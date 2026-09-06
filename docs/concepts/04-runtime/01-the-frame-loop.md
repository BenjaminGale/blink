# 1. The frame loop

The previous areas covered what `Blink.UI`, `Element`, layout, controls,
and application-state updates each provide. This one zooms out to the
frame as a whole: Blink doesn't own the main loop — a backend (SDL2, in
the included demo) calls into Blink once per frame. Each call does the
same three things:

```
+---------------+     +---------------+     +---------------+
|   nextFrame-  |     |     runUI     |     |    extract    |
|    Context    | --> |   (walk the   | --> |    (draws,    |
| (advance ctx) |     |    UI tree)   |     |   messages)   |
+---------------+     +---------------+     +---------------+
        ^                                           |
        +-------------------------------------------+
     focus, scroll, and selection persist across frames
```

1. **Advance the context.** `nextFrameContext` takes the context left over
   from the previous frame and produces the starting point for this one:
   input state is refreshed, any focus/scroll/selection changes that were
   *queued* last frame get applied now (more on that distinction in
   [the effects concept](../01-immediate-mode-api/04-effects.md)), and the
   animation clock advances. On the very first frame there is no previous
   context, so `emptyUIContext` is used instead.
2. **Run the view.** `runUI` walks the UI tree your `view` function
   produces, threading that context through it. Controls read from it
   (is this element focused? what are the current bounds?) and write to it
   (append a draw command, queue a message, claim focus).
3. **Extract the results.** `getDrawCommands` pulls out what to render this
   frame; `getMessages` pulls out what the view asked to happen, in the
   order it asked for it.

The context produced by step 2 becomes next frame's starting point — that's
the arrow feeding back into step 1. This is the *only* thing that persists
between frames on Blink's side. Nothing else about "last frame" is
remembered; if it isn't in that context, it doesn't exist to Blink.

## What's actually inside that context

The context (`UIContext`) is not application data — it's the small set of
bookkeeping covered by
[`01-immediate-mode-api`](../01-immediate-mode-api/01-introduction.md)
that has to survive between frames for the immediate-mode model to work
at all:

* Which element (if any) currently holds keyboard focus, per focus scope.
* Scroll position and text selection, keyed by element ID.
* Hover/press/mouse-capture state for the frame just walked, so the *next*
  frame's controls can tell "was I hovered a moment ago?"
* The animation clock.

Your application's own state (`s` in `App e msg s`) is deliberately *not*
part of this context — see
[`03-app-state`](../03-app-state/01-introduction.md) for why that's a
separate mechanism with a different owner.

Next: [section 2](02-frame-management.md) covers *when* a backend actually
makes this call — the two ways `stepFrame` can be triggered, and why they
change what a single call does.
