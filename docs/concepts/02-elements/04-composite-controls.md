# Composite controls: building more than one identity

[The control primitive](03-controls.md) covers `control`, and every widget
built on it takes a single element id. One value of your own `e` type is
its whole identity:

```haskell
button :: Ord e => e -> [Attribute (ButtonConfig e msg)] -> Element e msg
```

Some controls aren't one thing. `scrollBar` is a track, a decrement
button, and an increment button; `toggleButtonGroup`/`radioButtonGroup`
are one widget per item. [Identity](../01-immediate-mode-api/02-identity.md)
already established that hover, focus, and capture are tracked *per
element id*, so each of these pieces needs its own id: the decrement
button being hovered is a different fact from the track being hovered,
and Blink has to be able to tell them apart.

## The problem: the control doesn't know your `e`

A widget module can't ask for "four separate ids" directly. It's written
once, generically, against whatever `e` a given application picks, and it
has no way to invent values of a type it doesn't know. All it can define
is its own small, fixed vocabulary of *parts*:

```haskell
data ScrollBarPart
  = ScrollBarTrack
  | ScrollBarDecrement
  | ScrollBarIncrement
  | ScrollBar
  deriving (Eq, Ord, Show)
```

So instead of an id, it asks for a *function* that can turn any
`ScrollBarPart` into one of your `e` values:

```haskell
scrollBar :: Ord e => (ScrollBarPart -> e) -> [Attribute (ScrollBarConfig e msg)] -> Element e msg
```

Give it that function once, and `scrollBar` can build all four ids
itself, on demand, wherever it needs one.

## Where the function comes from

You never write this function by hand. You add one constructor to your
own element type, wrapping the part type:

```haskell
data ControlId = ...
             | VScrollCtl ScrollBarPart
             | HScrollCtl ScrollBarPart
             | SidebarPageButton (ToggleGroupPart Page)
             ...
```

This line does two things at once. It says "`VScrollCtl` applied to a
`ScrollBarPart` is one way to build a `ControlId`" (the usual meaning of a
data constructor), and because of that, it gives you, for free, a real,
callable function `VScrollCtl :: ScrollBarPart -> ControlId`. You didn't
write a function yourself; declaring the shape of data gave you one.
`VScrollCtl` is simultaneously a tag you can pattern-match on and a
function you can pass around.

## At the call site

```haskell
scrollBar VScrollCtl [scrollBarOrientation Vertical, height fill]
```

Naming the constructor reads like "the id," and that's a fine casual
mental model, but its type is a function. It's the recipe for all of
`scrollBar`'s ids, not any single one of them yet.

## Inside the control

Whenever `scrollBar` needs a concrete id for one part, say the track, it
applies the function it was given:

```haskell
tag ScrollBarTrack
```

This evaluates to `VScrollCtl ScrollBarTrack :: ControlId` (when called
from the vertical scrollbar), an ordinary, fully built value, usable for
hover/focus/capture lookup like any other id. `scrollBar` calls `tag` once
per part, internally, whenever it needs that part's id for the frame; you
never see the intermediate function from outside.

## Reading a signature

- A plain `e` (`button :: e -> ...`) means the control has one identity.
- A parenthesized function type (`(SomePart -> e) -> ...`) means it has
  several, collapsed into one argument: "give me a way to name each of my
  parts in your id type," not "give me an id."

The trigger is specifically "more than one *independently-tracked*
identity needed internally." Hover, focus, and mouse capture are all
tracked per element id (see [Identity](../01-immediate-mode-api/02-identity.md)),
so a part only needs its own id when it's genuinely a separate interactive
target: `scrollBar`'s decrement/increment buttons and track can each be
hovered, pressed, or (for the track) dragged independently of one
another, and each `toggleButtonGroup`/`radioButtonGroup` item is its own
focus target. Style-class selection is a separate mechanism: `ccStyleKey`
is a plain `Class` per part (`scrollBarTrackStyleKey`,
`scrollBarButtonStyleKey`), fixed and shared across every instance
regardless of `tag`, so it's not by itself a reason to build a second id.
A part with no independent interaction of its own doesn't need one.

## The "whole" is just another part

Once a control has several pieces, is there a way to refer to the
composite as a whole, for chrome behind the whole group, say? Blink's
composite controls answer this without adding a second mechanism: the
composite itself is a `control` too, wrapping the parts as children, so it
already needs one id of its own, for its own hover/press/focus tracking,
the same as any other control's id. You add that id as one more
constructor in the same part type, rather than inventing a separate
mechanism for it. `ScrollBarPart` has `ScrollBar` alongside its three
pieces, and `ToggleGroupPart` has `ToggleGroup` alongside
`ToggleGroupItem`:

```haskell
data ToggleGroupPart a
  = ToggleGroup
  | ToggleGroupItem a
  deriving (Eq, Ord, Show)
```

`tag ToggleGroup` is the id the group's own wrapping `control` renders
under. Its style still resolves from a fixed `Class`
(`toggleButtonGroupStyleKey`/`radioButtonGroupStyleKey`), same as any
part's; the id itself is only for interaction bookkeeping. For
`scrollBar`, that bookkeeping use extends to one more thing: `tag
ScrollBar` doubles as the `Blink.View.ScrollState` key the current
position is stored under. A caller that needs to read a scrollbar's
position from elsewhere uses the identical id, `tag ScrollBar`, passed to
`getScrollState`, so no separate attribute needs to expose it.

The composite's own id is deliberately not itself a keyboard focus target
(`NotFocusable`, fixed rather than attr-settable). Tab moves directly
between the composite's focusable parts (each `toggleButtonGroup` item),
or skips the composite entirely (`scrollBar`, never part of Tab order),
the same as if the wrapping control weren't there.

## Data-parameterized parts

A part type isn't limited to a fixed enum. `ToggleGroupPart a` is generic
over the group's own item type: `ToggleGroupItem` tags an item by its own
data value rather than its position in the list, so reordering or
inserting items elsewhere never disturbs another item's hover/focus/
capture state:

```haskell
radioButtonGroup RadioOption [items ["Small", "Medium", "Large"], ...]
```

Here `RadioOption :: ToggleGroupPart Text -> ControlId`, and each item's
id is `RadioOption (ToggleGroupItem "Medium")` and so on, built the same
way as the fixed-part case, just with the part itself carrying data.

## What doesn't get a part today

Not every visually distinct piece of a control gets its own part.
`scrollBar`'s thumb is drawn inside the track's control with no id of its
own, sharing the track's hover/drag state, and `slider` (one draggable
region with a thumb drawn inside it) has no part type at all. Whether
pieces like these should become independently tracked, and how a theme
would target a sub-part's look when it has no id of its own, are both
still open; see `IDEAS.md`.

## Summary

- One identity needed: a plain `e` argument.
- More than one: a fixed `data ...Part = ...` vocabulary and a
  `(Part -> e)` argument, satisfied by naming one constructor from your own
  element type (`VScrollCtl`, `RadioOption`, ...), never a hand-written
  function.
- The composite's own id, when it needs one, is just another constructor
  in the same part type, not a separate mechanism.

See `Blink.Controls.ScrollBar` and `Blink.Controls.ToggleGroup` for the
two worked examples referenced throughout this page.
