# 1. Why immediate mode

Most GUI toolkits are **retained-mode**: you build a tree of widget objects
once, hand it to the toolkit, and then *mutate* that tree as your
application's state changes — `label.setText("...")`, `button.setEnabled
(false)`. The toolkit retains the tree between frames; your job is to keep
it in sync with your state by hand.

Frameworks like React soften this with a **virtual DOM**: you still describe
the UI as a function of state, but under the hood the framework diffs the
new description against the previous one and patches only what changed.
You get to write declaratively, at the cost of a diffing pass.

Blink is **immediate-mode**: there is no retained tree and no diff. Every
frame, your view function runs from scratch against the current state and
directly produces that frame's draw commands.

```
  Retained mode                    Immediate mode (Blink)
  ──────────────                   ──────────────────────

  build tree once                  every frame:
       │                             view(state) ──▶ draw commands
       ▼
  state changes
       │
       ▼
  mutate tree by hand               no tree persists between frames;
  (setText, setEnabled, ...)        state is the only thing that does
       │
       ▼
  toolkit redraws
  the mutated tree
```

There's no `label.setText`, because there's no `label` object sitting
around waiting to be told about a text change — next frame, the view just
runs again with the new state and draws the new text directly.

## What this buys you

* **No synchronization bugs.** A retained-mode tree can drift from your
  state if you forget to call a setter somewhere. In Blink, the view is a
  pure function of state — there's nothing to forget to update, because
  nothing is retained to go stale.
* **No diffing.** Unlike a virtual-DOM framework, Blink doesn't compare
  frames to figure out what changed — it just draws the current one. This
  is a big part of what keeps the model simple: there's no reconciliation
  algorithm to reason about.
* **Straightforward control flow.** An `if`/`case` in your view function
  *is* your conditional rendering — no special "conditional component" API
  needed.

## What it costs

* **The view runs every frame**, not just on change — some work your view
  does (a `map` over a list, building strings) happens repeatedly even when
  nothing relevant changed. In practice this is cheap for typical UI sizes,
  and Blink's layout and draw-command generation are designed to be fast
  enough that this isn't a bottleneck.
* **Anything that must survive between frames — has to be stored
  explicitly somewhere**, since nothing is retained implicitly. Blink
  provides two places for this: the application state you own (via
  messages — see [section 3](03-elements-and-messages.md)), and a small
  set of *presentational* slots Blink itself owns, like focus and scroll
  position (see [section 4](04-focus-and-timing.md)).

## Where this shows up in the API

Because there's no persistent widget tree, there are no widget objects to
hold a reference to. Instead:

* Controls are identified by a value *you* define (an `e` — see
  [section 3](03-elements-and-messages.md)), not an object handle, because
  there's no object for a handle to point to.
* The handful of things that genuinely need to persist across frames
  (focus, scroll, selection) live in a context Blink threads through the
  frame for you, keyed by that same element identity — not in the tree,
  because there is no tree to attach them to.

The next section walks through exactly what that threading looks like,
frame by frame.
