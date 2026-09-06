# 8. Application, update, and the backend loop

The previous sections cover the view side of Blink: how a single frame is
built, how it reports change via messages, and how presentational state
persists. This section closes the loop — how those messages actually
become a new application state, and how a frame gets triggered by a real
backend in the first place, rather than by an imagined caller.

## The `Update` monad

[Section 6](06-elements-and-messages.md) established that `update` folds
each queued message into your application state, once per message, in
emission order. `Update s` is the monad that runs in: a small
state-threading computation over your state `s`, with `get`/`put`/`gets`/
`modify` — the same shape as any state monad you've used before.

```haskell
data Msg = Increment | SetName Text

update :: Msg -> Update AppState ()
update Increment   = modify (\s -> s { counter = counter s + 1 })
update (SetName t) = modify (\s -> s { name = t })
```

There's deliberately nothing more to it than that. Unlike `UI` (which runs
in `IO` for text measurement, and threads a `UIContext` full of frame
bookkeeping), `Update` carries no `IO` and touches nothing but your own
state — an update handler can't accidentally depend on bounds, input, or
anything else that's the view's concern. If you find yourself wanting an
update handler to trigger a side effect (writing a file, making a network
call), that's a sign it belongs in your backend's loop, driven by the
resulting state, not inside `Update` itself.

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
`theme` and `view` are read from that state each frame; `update` is the
function just described. None of these four fields know about each other
directly — `App` is just the record that lets the backend find all four
in one place.

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
frame-loop steps from [section 5](05-the-frame-loop.md), then folds every
message the view emitted into the state with `update` before handing back
a `FrameResult`.

## Two ways a backend can drive that loop

Configuring `App` with `configureContinuous` or `configureEventDriven`
decides *when* `stepFrame` runs relative to input, which changes what a
single call actually does:

* **Continuous** (`configureContinuous`) — for a backend that redraws
  every iteration regardless of input, such as a game-style loop.
  `stepFrame` runs the view once, submits its draw commands, and folds any
  emitted messages into the state for the *next* call to see — so a state
  change made this frame isn't reflected on screen until the following
  one.
* **Event-driven** (`configureEventDriven`) — for a backend that blocks
  until an input event arrives. `stepFrame` runs the view twice: once to
  collect the messages that event produced, then again on the
  already-updated state, so the frame that actually gets drawn never shows
  something stale.

Either way, the messages produced and the state they fold into are exactly
what sections 1–5 describe — this choice only changes *when* one round of
that happens relative to a real input event, not what happens inside it.
See `Blink.App`'s Haddocks for the exact call sequence of each, and for
`FrameInput`/`FrameResult`, the types a backend actually constructs and
receives.

## Writing a new backend

Everything above is what a backend author needs to drive Blink; writing
one is a task, not a concept, so it isn't covered further here. Study the
included SDL2 backend (`app/`) as a working example if you're building a
new one.
