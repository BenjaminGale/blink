# Blink: core concepts

This is a narrative walkthrough of Blink's paradigm. It's meant to be read
start to finish by someone who has never seen the library, without needing
to read the source first.

For precise, per-function reference documentation, use the Haddocks
(`cabal haddock`), starting from the `Blink` module. This guide exists
because some of that reference material — correctly, for reference material
— assumes you already have the mental model. This is where you get it.

Read the three areas below in order — each builds on the ones before it —
then read each area's own files in order:

1. [`01-immediate-mode-api`](01-immediate-mode-api/01-introduction.md) —
   `Blink.UI`: the monad itself and the capabilities it directly provides
   (element identity, bounds, effects, focus).
2. [`02-elements`](02-elements/01-introduction.md) — `Element` and the
   attribute mechanism, and the two concrete systems built from them:
   `Blink.Layout`'s containers and `Blink.Controls`' ready-made widgets.
3. [`03-runtime`](03-runtime/01-the-frame-loop.md) — `Blink.App`/
   `Blink.Update`: the frame loop, the `Update` monad, and how a real
   backend drives it all.

Once you have this model, [`../guides`](../guides/README.md) covers
specific tasks — composing a layout, hand-writing a custom control, and
writing a new backend.
