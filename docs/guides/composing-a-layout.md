# Composing a layout

The README's todo-list example shows one row and one column. This guide
walks through building something closer to a real screen — a settings
panel with a header, a sidebar, and a content area — to cover the layout
decisions that come up once you're nesting containers rather than using
one in isolation.

Read [the elements concept](../concepts/02-elements/01-introduction.md)
first if you haven't — this guide assumes you're comfortable with
attribute lists and `resolve`.

## Start from the shape, not the widgets

Before reaching for `hBox`/`vBox`, sketch the regions:

```
+------------------------------------------+
|                 header                    |
+---------------+----------------------------+
|               |                            |
|    sidebar    |          content           |
|               |                            |
+---------------+----------------------------+
```

That's a header spanning the top, with a fixed-width sidebar and a
flexible content area beneath it — exactly what `Blink.Layout.Border`'s
named regions are for, so reach for `borderLayout` at the outermost level
rather than hand-nesting `vBox`/`hBox` to get the same shape:

```haskell
import Blink.Layout (fill, Layout (..))
import Blink.Layout.Border (borderLayout, top, left, centre)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Element (elementWithLayout)

settingsView :: AppState -> Element ScreenId Msg
settingsView s = elementWithLayout (Layout fill fill TopLeft) $
  borderLayout
    [ top (headerBar s)
    , left (sidebar s)
    , centre (content s)
    ]
```

Each panel takes an `Element` and keeps its size on one axis: `top` and
`bottom` are as tall as their element asks and span the full width;
`left` and `right` are as wide as their element asks and fill the middle
row's height. `centre` fills whatever is left. So `headerBar` sets its
own height, either `height (exactly 48)` or `height fitContent` to fit
its content, and `sidebar` sets its own width. `borderLayout` itself
returns a bare `View e msg ()`, because it fills whatever space it's
given.

Since a view must return an `Element` (not a bare `View` action),
`elementWithLayout` wraps `settingsView`'s `borderLayout` back into one at
the top, declaring "fill whatever space the caller gives me" as its size
request — the only sensible request for something that's the whole
screen.

Leave a panel out of the list entirely and its neighbours expand to cover
the gap. You don't need an empty placeholder for a region you don't use.

## Filling the sidebar and content panels

Inside `sidebar`, use `vBox` the way the README's example does, giving the
box its own width and sizing its children relative to it:

```haskell
sidebar :: AppState -> Element ScreenId Msg
sidebar s = vBox
  [ width (exactly 200), spacing 4, margin 8
  , children
      [ button (SectionButton i) [text label, height (exactly 32), width fill]
      | (i, label) <- zip [0 ..] (sectionNames s)
      ]
  ]
```

`width fill` on each button is what makes them span the sidebar's full
200px, rather than shrinking to fit their label text — `fill` and
`exactly` are about the space *within* whatever slot a parent gives a
child, not about the screen as a whole. A child that should size itself
from its own content instead of the space available uses `fitContent`
(e.g. a button that should be only as wide as its label needs).

The three you'll reach for most:

* `exactly n` — a fixed size, ignoring whatever space is offered.
* `fill` — take all the space offered on that axis.
* `fitContent` — take exactly the space the widget's own content needs.

`atLeast`/`atMost`/`between` cover the cases in between (expand, but never
below/above a bound) — see `Blink.Layout.Constraints`'s Haddocks for exact
resolution behaviour against different amounts of available space.

## When children don't fit

Two independent things can happen when a box's children don't match the
space it has, and it's worth being able to tell them apart:

* **Children take less space than the box.** `alignment` on the box
  controls where the leftover whitespace goes (`TopLeft`, `Center`,
  `BottomRight` on the main axis) — see `Blink.Layout.Box`'s Haddocks for
  the worked diagrams of each.
* **Children take more space than the box.** The overflow is clipped, not
  scrolled — Blink's layout doesn't introduce scrolling on its own. A
  region that needs to scroll (a long settings list, say) needs a control
  built around `Blink.View`'s scroll primitives (`elmScrollStates`, see
  "Blink.View" for the accessors) rather than relying on `vBox` to do it for
  you.

## Converting between `Element` and a bare `View` action

`runElement` goes from an `Element` to a bare `View` action, and
`elementWithLayout` goes the other way. You need them whenever you mix
code that expects one with code that expects the other:

* **`Element` → `View e msg ()`**: `runElement`. Use this when something
  that already has a size request (a ready-made widget, or `hBox`/`vBox`)
  needs to run inside hand-written `View` code, without placing it in
  another sized slot.
* **`View e msg ()` → `Element`**: `elementWithLayout layout action`. Use
  this when a plain `View` action (hand-written primitives, or the result of
  `runElement`/`borderLayout`) needs to report a size request to a parent
  that expects one — a box's `children` list, or, as in `settingsView`
  above, the `Element` a view function itself must return.

Neither direction loses information: `runElement` runs the element in the
space it's given, and `elementWithLayout` supplies a size request for an
action that never had one.
