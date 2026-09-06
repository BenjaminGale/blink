# 2. Composing containers

`hBox`, `vBox`, and `borderLayout` are `Blink.View.Layout`'s concrete
application of [`Element` and the attribute mechanism](01-introduction.md)
to one specific problem: arranging several children within one parent's
space. This section covers the rules each uses to do that; see
[the layout guide](../../guides/composing-a-layout.md) for putting them to
work building an actual screen.

## `hBox`/`vBox`: distributing space along one axis

Once every child's `Length` is resolved to a concrete number (via
`elMeasure` where needed, per
[the elements concept](01-introduction.md)), `hBox`/`vBox`
distribute space along the main axis with a small, fixed set of rules —
fixed-size children take exactly what they asked for, flexible children
split whatever's left equally (capped by their own `atMost`/`between`
ceiling if they have one), and the whole group is then positioned
according to the box's `alignment` (where leftover whitespace goes, or
which side clips on overflow). This is worth reading in
`Blink.View.Layout.Box`'s own Haddocks rather than restating here — `hBox`'s
module documentation walks through it with worked diagrams, now that you
have the `elMeasure`/`Available` vocabulary those examples assume.

## `borderLayout`: named regions instead of a single axis

`borderLayout` solves a different, more specific shape: a header, footer,
sidebars, and a content area, rather than a single row or column. Its
panels (`top`/`bottom`/`left`/`right`/`centre`) each take a fixed size (or,
for `centre`, whatever's left) rather than negotiating through
`elMeasure` the way a box child does — there's nothing to measure when a
panel's size is already decided up front. See `Blink.View.Layout.Border`'s
Haddocks for the exact region diagram and clipping behaviour.

## Converting between `Element` and a bare `View` action

Because a box's children need to report a size request and a border
panel's content doesn't, they take different things: `children` wants
`[Element e msg]`, while `top`/`left`/`centre` want a bare `View e msg ()`.
Composing the two together means converting between them at the boundary:

* **`Element` → `View e msg ()`**: `runElement`. Needed when something that
  already has a size request (a widget, or another `hBox`/`vBox`) goes
  into a border panel, or anywhere else that only wants a plain action.
* **`View e msg ()` → `Element`**: `elementWithLayout`. Needed when a plain
  action needs to report a size request to a parent that expects one — a
  box's `children` list, or the `Element` a view function itself must
  return.

[The layout guide](../../guides/composing-a-layout.md) walks through this
conversion in a complete worked example.
