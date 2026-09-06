# 3. Layout primitives: current bounds

Before covering how `hBox`, `vBox`, and sizing constraints let you compose
multiple children ([section 4](04-layout-composition.md)), it's worth
seeing what "layout" means at the bare immediate-mode level, with none of
that machinery — because everything in section 4 is built on top of this,
not a replacement for it.

## There is no layout tree — just a rectangle

At the `Blink.UI` level, "layout" is nothing more than a single
`Rectangle` — the *current bounds* — carried in context. `getBounds`
reads it; every drawing primitive (`fillRect`, `strokeRect`, `drawText`)
and every hit-test (`isRegionHit`) operates against whatever it currently
is, with no separate coordinate system to reconcile:

```haskell
myControl :: UI e msg ()
myControl = do
  bounds <- getBounds
  fillRect (colourFor bounds)
```

There's no tree of positioned widgets for Blink to walk — a control simply
asks "what space do I currently have?" and draws within it.

## A plain action fills whatever it's given

By default, a `UI` action doesn't shrink, centre, or otherwise adjust
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
UI action passed to it, without affecting anything outside that action.
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

## Where this leaves you

`getBounds`/`withBounds`/`clipToCurrent` are the entire layout vocabulary
at this level — there's no concept here of a child "requesting" a size, or
of a parent negotiating space among several children at once. That's
precisely the gap [section 4](04-layout-composition.md) fills: `Element`
adds a size request on top of a plain `UI` action, which is what lets
`hBox`/`vBox` compute a `withBounds` rectangle for each child in the first
place, rather than every child just filling whatever it's handed.
