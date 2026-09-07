# Why immediate mode

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

* **The view cannot drift from state.** It is a pure function of state,
  recomputed every frame. There is no setter to forget to call, because
  nothing is retained for the view and the state to fall out of sync on.
* **Each frame draws directly from the current state.** Blink renders the
  current frame instead of comparing it to the previous one, so there is
  no reconciliation algorithm between state and screen.
* **Conditional rendering is ordinary Haskell.** An `if`/`case` in the view
  function is the conditional rendering; no separate "conditional
  component" API is needed.

## What it costs

* **The view runs every frame**, not just on change — some work your view
  does (a `map` over a list, building strings) happens repeatedly even when
  nothing relevant changed. In practice this is cheap for typical UI sizes,
  and Blink's layout and draw-command generation are designed to be fast
  enough that this isn't a bottleneck.
* **Anything that must survive between frames has to be stored
  explicitly somewhere**, since nothing is retained implicitly. The rest
  of this folder is about exactly that: the handful of capabilities
  `Blink.View` provides for state that needs to persist — or at least be
  reported — across the boundary between one frame and the next.

## Identifying controls and persisting state without a widget tree

Because there's no persistent widget tree, there are no widget objects to
hold a reference to. Instead:

* Controls are identified by a value *you* define, not an object handle,
  because there's no object for a handle to point to — see
  [section 2](02-identity.md).
* The handful of things that genuinely need to persist across frames
  (focus, scroll, selection) live in a context Blink threads through the
  frame for you, keyed by that same identity — not in a tree, because
  there is no tree to attach them to. See
  [section 4](04-effects.md) and [section 5](05-focus.md) for how and when
  those changes actually take effect.

Next: [section 2](02-identity.md) covers that identity value in detail —
why it's needed, and how Blink uses it.
