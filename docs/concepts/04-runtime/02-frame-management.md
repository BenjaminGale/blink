# Frame management

[The frame loop](01-the-frame-loop.md) describes what happens inside a
single call to `stepFrame`, the function a backend calls once per
iteration to advance Blink by a frame (see
[the next section](03-application-and-backend.md) for where `stepFrame`
comes from). This section covers when a backend makes that call, and why
the two ways of configuring it, `configureContinuous` and
`configureEventDriven`, change what a single call does, not just how often
it happens.

## Continuous: redraw every iteration, regardless of input

For a backend that redraws every iteration regardless of input, such as a
game-style loop or an animated UI, `stepFrame` runs the view once, submits
its draw commands, and folds any emitted messages into the state for the
next call to see:

```
Continuous
----------

  run view
     |
     v
  submit draws

  state changed by a message this frame isn't
  visible until the following frame's draws
```

A state change made this frame is real and gets folded in, but it isn't
drawn until the next iteration, producing one frame of visible lag between
an input and its on-screen effect. For a loop already redrawing
continuously regardless of input, that lag is usually imperceptible.

## Event-driven: block for input, never show stale state

For a backend that blocks until an input event arrives, that one-frame
lag would be far more noticeable, since the backend will not redraw again
a moment later to catch up. `configureEventDriven` avoids it by running
the view *twice* per `stepFrame` call: once to collect the messages that
event produced, then again on the already-updated state, before drawing
anything:

```
Event-driven
------------

  run view (pass 1)
     |
     v
  fold emitted messages
     |
     v
  run view again (pass 2)
     |
     v
  submit draws

  drawn state always reflects messages emitted
  this same frame -- never shows something stale
```

The `IO ()` callback `configureEventDriven` takes is unrelated to this
double pass; it is how Blink's animation ticker wakes a blocked backend
when a control has called `requiresAnimation`, covered in
[the guide on writing a backend](../../guides/writing-a-backend.md).

The second pass only runs when it can change what's drawn: on an ordinary
input frame that queued a message or `UiEffect`. A frame woken by the
animation ticker skips it even then, because the ticker is about to fire
again on the next iteration regardless — the one-tick lag a second pass
would otherwise correct is as imperceptible as continuous mode's inherent
one-frame lag. This keeps a running animation to a single view render per
tick instead of two.

## Choosing between them

Either way, the messages produced and the state they fold into are
exactly what the rest of these concepts describe. This choice only
changes when one round of that happens relative to a real input event,
not what happens inside it. Pick based on how your backend's own loop is
shaped: continuous if it already redraws unconditionally every iteration,
event-driven if it blocks waiting for something to happen. See
`Blink.App`'s Haddocks for the exact call sequence of each, and for
`FrameInput`/`FrameResult`, the types a backend actually constructs and
receives.
