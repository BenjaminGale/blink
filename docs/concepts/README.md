# Blink: core concepts

This is a narrative walkthrough of Blink's paradigm — immediate-mode
rendering, the frame loop, and how state and focus flow through it. It's
meant to be read start to finish by someone who has never seen the library,
without needing to read the source first.

For precise, per-function reference documentation, use the Haddocks
(`cabal haddock`), starting from the `Blink` module. This guide exists
because some of that reference material — correctly, for reference material
— assumes you already have the mental model. This is where you get it.

1. [Why immediate mode](01-why-immediate-mode.md) — what "rebuild the tree
   every frame" buys you, and what it costs, contrasted with retained-mode
   GUIs and virtual-DOM diffing.
2. [Declarative controls and the attribute mechanism](02-declarative-controls.md)
   — the widget/layout layer most views are actually written in, how it
   resolves down to the immediate-mode primitives, and how attribute lists
   are resolved.
3. [The frame loop](03-the-frame-loop.md) — the three steps every frame goes
   through, and what state survives from one frame to the next.
4. [Elements and messages](04-elements-and-messages.md) — how a view reports
   change without mutating anything, and why controls are identified by a
   value you define rather than an object reference.
5. [Focus and timing](05-focus-and-timing.md) — why some state changes take
   effect immediately, mid-frame, while others wait for the next one.
6. [Application, update, and the backend loop](06-application-and-backend.md)
   — how the `Update` monad turns messages into a new state, and how a
   real backend drives frames via `App` and `stepFrame`.

Each section builds on the ones before it — read in order the first time.

Once you have this model, [../guides/building-a-custom-control.md](../guides/building-a-custom-control.md)
puts it to work hand-writing a minimal button from these primitives.
