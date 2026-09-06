# 3. `App` and the backend pipeline

[The frame loop](01-the-frame-loop.md) covered what happens inside a
single call to `stepFrame`. This section covers where that call comes
from — how your view, theme, and
[`update` function](../03-app-state/01-introduction.md) get bundled into
something a real backend can drive.

## `App`: the whole application as one value

Everything Blink needs to run is bundled into one `App` value:

```haskell
data App e msg s = App
  { startUp :: IO s
  , theme   :: s -> Theme e
  , view    :: s -> Element e msg
  , update  :: msg -> Update s ()
  }
```

`startUp` produces the initial state once, before the loop begins;
`theme` and `view` are read from that state each frame; `update` is
[the state-transition function](../03-app-state/01-introduction.md) covered
previously. None of these four fields know about each other directly —
`App` is just the record that lets the backend find all four in one
place.

## From `App` to frames on screen

Blink does not own the main loop — a backend calls into it. `App` is
turned into something a backend can drive by calling
`configureContinuous` or `configureEventDriven`, producing a
`BlinkHandle`; the backend then calls `stepFrame` once per loop iteration:

```
+------------------------------------------------+
| App { startUp, theme, view, update }            |
+------------------------------------------------+
                       |  configureContinuous /
                       |  configureEventDriven
                       v
+------------------------------------------------+
| BlinkHandle  (tracks current state s)           |
+------------------------------------------------+
                       |
                       |  backend calls stepFrame
                       |  once per loop iteration
                       v
+------------------------------------------------+
| FrameResult (draws, next state)                 |
+------------------------------------------------+
```

`BlinkHandle` is what threads the state between calls to `stepFrame` — the
backend's own loop only ever passes the handle around; it never sees or
stores `s` itself. Inside `stepFrame`, one call runs exactly the three
frame-loop steps from [the frame loop](01-the-frame-loop.md), then folds
every message the view emitted into the state with `update` before
handing back a `FrameResult`.

`configureContinuous` and `configureEventDriven` also decide *when*
`stepFrame` runs relative to input, which changes what a single call
actually does — see
[frame management](02-frame-management.md) for that distinction.

## Writing a new backend

Everything above is what a backend author needs to drive Blink; writing
one is a task, not a concept, so it isn't covered further here. See
[the backend guide](../../guides/writing-a-backend.md) for that, and the
included SDL2 backend (`app/`) as a working example.
