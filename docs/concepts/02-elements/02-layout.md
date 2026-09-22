# Composing containers

`hBox`, `vBox`, and `borderLayout` are `Blink.Layout`'s concrete
application of [`Element` and the attribute mechanism](01-introduction.md)
to one specific problem: arranging several children within one parent's
space. This section covers the rules each uses to do that; see
[the layout guide](../../guides/composing-a-layout.md) for putting them to
work building an actual screen.

## `hBox`/`vBox`: distributing space along one axis

Once every child's `Length` is resolved to a concrete number (via
`elMeasure` where needed, per
[the elements concept](01-introduction.md)), `hBox`/`vBox`
distribute space along the main axis with a small, fixed set of rules:
fixed-size children take exactly what they asked for, flexible children
split whatever's left equally (capped by their own `atMost`/`between`
ceiling if they have one), and the whole group is then positioned
according to the box's `alignment` (where leftover whitespace goes, or
which side clips on overflow). `Blink.Layout.Box`'s own Haddocks walk
through this with worked diagrams, now that you have the
`elMeasure`/`Available` vocabulary those examples assume.

## `borderLayout`: named regions instead of a single axis

`borderLayout` solves a different, more specific shape: a header, footer,
sidebars, and a content area, rather than a single row or column. Each
panel takes an `Element`. `top` and `bottom` are as tall as their element
asks, including `fitContent`, which measures it through `elMeasure` like
a box child; `left` and `right` are as wide as their element asks; and
`centre` fills whatever is left. See `Blink.Layout.Border`'s Haddocks for
the exact region diagram and clipping behaviour.

## Converting between `Element` and a bare `View` action

Box children and border panels both take `Element`s, but hand-written
`View` code and `borderLayout` itself are bare `View e msg ()` actions.
Composing the two together means converting between them at the boundary:

* **`Element` → `View e msg ()`**: `runElement`. Needed when something that
  already has a size request (a widget, or another `hBox`/`vBox`) runs
  inside code that only wants a plain action.
* **`View e msg ()` → `Element`**: `elementWithLayout`. Needed when a plain
  action needs to report a size request to a parent that expects one — a
  box's `children` list, or the `Element` a view function itself must
  return.

[The layout guide](../../guides/composing-a-layout.md) walks through this
conversion in a complete worked example.
