# 3. Bounds

## There is no layout tree — just a rectangle

At the `Blink.View` level, "layout" is nothing more than a single
`Rectangle` — the *current bounds* — carried in context. `getBounds`
reads it; every drawing primitive (`fillRect`, `strokeRect`, `drawText`)
and every hit-test (`isRegionHit`) operates against whatever it currently
is, with no separate coordinate system to reconcile:

```haskell
myControl :: View e msg ()
myControl = do
  bounds <- getBounds
  fillRect (colourFor bounds)
```

There's no tree of positioned widgets for Blink to walk — a control simply
asks "what space do I currently have?" and draws within it.

## A plain action fills whatever it's given

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

## `withBounds` changes bounds for a sub-tree

`withBounds` is how that changes: it replaces the current bounds for the
view action passed to it, without affecting anything outside that action.
Nothing about `getBounds` elsewhere in the tree is disturbed — the
replacement is scoped exactly to the sub-tree `withBounds` wraps:

```
+----------------------------------------------------+
| parent bounds                                       |
|                                                      |
|     +--------------------------------------+         |
|     | withBounds smallerRect $ do            |         |
|     |   ...                                  |         |
|     | getBounds here == smallerRect          |         |
|     |                                        |         |
|     +--------------------------------------+         |
|                                                      |
+----------------------------------------------------+
```

`clipToCurrent` builds on the same idea for drawing rather than
measurement: it wraps a sub-tree so that anything it draws outside the
current bounds is discarded, rather than spilling over a sibling.

## Bounds don't persist — unlike what's next

Unlike element identity's bookkeeping, or the effects and focus state
covered next, bounds are not carried across frames at all: they're
recomputed top-down, fresh, every single frame, from whatever the root
bounds are this time. There's nothing to key by element identity here,
because there's nothing to remember between frames in the first place.

`getBounds`/`withBounds`/`clipToCurrent` are the entire vocabulary at this
level — there's no concept here of a child "requesting" a size, or of a
parent negotiating space among several children at once. That gap is
filled by `Element`, one layer up — see
[`../02-elements/01-introduction.md`](../02-elements/01-introduction.md), once
you've read the rest of this folder.

Next: [section 4](04-effects.md) covers the first kind of state that
*does* persist — and how a view changes it without mutating anything
directly.
