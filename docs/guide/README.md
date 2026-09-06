# Blink: a guided tour

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
2. [The frame loop](02-the-frame-loop.md) — the three steps every frame goes
   through, and what state survives from one frame to the next.
3. [Elements and messages](03-elements-and-messages.md) — how a view reports
   change without mutating anything, and why controls are identified by a
   value you define rather than an object reference.
4. [Focus and timing](04-focus-and-timing.md) — why some state changes take
   effect immediately, mid-frame, while others wait for the next one.
5. [Building a custom control](05-building-a-custom-control.md) — putting
   the previous four sections together to hand-write a minimal button.

Each section builds on the ones before it — read in order the first time.
