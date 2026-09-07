# Bounds

## Bounds as a single rectangle

At the `Blink.View` level, "layout" is nothing more than a single
`Rectangle`, the current bounds, carried in context. Drawing and hit-testing
operate against whatever it currently is, with no separate coordinate
system to reconcile and no tree of positioned widgets to walk: a control
simply asks "what space do I currently have?" and draws within it. See
`Blink.View`'s Haddocks for the specific functions that read and draw
against the current bounds.

## Default sizing behaviour

By default, a `View` action doesn't shrink, centre, or otherwise adjust
those bounds for its own content — it fills the entire space its caller
happened to give it:

```
+------------------------------------------+
|                                          |
|                component                 |
|        (fills parent by default)         |
|                                          |
+------------------------------------------+
```

This is why a bare control placed directly under the root, with nothing
else configuring its size, ends up stretching to the whole window — there
is no implicit "shrink to content" behaviour to opt out of; filling is the
only thing that happens unless something changes the bounds first.

## A sub-tree can narrow its own bounds

The current bounds can be replaced for one sub-tree without affecting
anything outside it: a parent's own bounds are untouched, and the
narrower bounds apply only within the wrapped action. The same scoping
applies to clipping what a sub-tree draws, so content cannot spill over a
sibling. See `Blink.View`'s Haddocks for the functions that do this.

## Bounds are recomputed every frame

Bounds do not persist across frames. They are recalculated top-down from
the root bounds on every frame, so there is no per-element bookkeeping to
key by identity here, in contrast to focus and scroll state.

Measurement and clipping are the only vocabulary at this level. Requesting
a preferred size for a child, or splitting space among several children,
is handled by `Element`, one layer up — see
[`../02-elements/01-introduction.md`](../02-elements/01-introduction.md),
once the rest of this folder has been read.

Next: [section 4](04-effects.md) covers the first kind of state that
*does* persist — and how a view changes it without mutating anything
directly.
