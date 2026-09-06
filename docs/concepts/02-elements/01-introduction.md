# 1. `Element` and the attribute mechanism

[`01-immediate-mode-api`](../01-immediate-mode-api/01-introduction.md)
covered what `Blink.UI` itself provides: a monad that draws directly
against a current-bounds rectangle, with identity, effects, and focus as
its only persistent bookkeeping. In practice, you'll rarely write at that
level directly. Both of the systems built on top of it —
[`Blink.UI.Layout`](02-layout.md)'s containers and
[`Blink.UI.Controls`](03-controls.md)'s ready-made widgets — share one
common piece of machinery: `Element`, and the attribute mechanism used to
configure one.

```haskell
button AddButton [text "Add", onActivated (post AddItem), width (exactly 80)]
```

Every ready-made control and every `hBox`/`vBox` already *is* one of
these. Nothing about immediate mode changes because you're using either —
it's the same rebuild-every-frame model, just assembled for you instead of
hand-written the primitive-by-primitive way
[the custom-control guide](../../guides/building-a-custom-control.md) does
by hand:

```
+------------------------------------------------+
| button AddButton                                |
|   [ text "Add"                                  |
|   , onActivated (post AddItem)                  |
|   , width (exactly 80)                          |
|   ]                                             |
+------------------------------------------------+
                        |
            resolved into a UI action,
               rebuilt fresh every frame
                        v
+------------------------------------------------+
| isRegionHit, registerMouseOver, setFocus,       |
| fillRect, drawText, emit, ...                   |
+------------------------------------------------+
```

## `Element`: a size request alongside the action

Bounds ([`01-immediate-mode-api/03-bounds.md`](../01-immediate-mode-api/03-bounds.md))
gave every `UI` action a rectangle to draw within, but no way to *ask* for
a particular size — a plain action just fills whatever it's given.
`Element` is the type that adds that ask:

```haskell
data Element e msg = Element
  { elLayout  :: Layout                     -- this element's size request
  , elMeasure :: MeasureCtx -> UI e msg Size -- its preferred size, if asked
  , elRun     :: UI e msg ()                 -- runs it for this frame
  }
```

You don't construct one by hand unless you're placing a hand-written `UI`
action as a container child (`elementWithLayout`, covered in
[the layout guide](../../guides/composing-a-layout.md)). The `elLayout`
field is a `Layout`: a `Length` for each axis (`exactly n`, `fill`,
`atLeast`/`atMost`/`between`, or `fitContent`) plus an alignment — see
`Blink.UI.Layout.Constraints`'s Haddocks for the exact resolution rules of
each, worked through with diagrams.

Most `Length`s (`exactly`, `fill`, `atLeast`, ...) can be resolved from
available space alone — a fixed number, or "whatever's offered." One
can't: `fitContent`, which means "exactly as much as this element's own
content needs," and there's no way to know that without asking the
element itself. That's what `elMeasure` is for.

## Why measuring needs its own pass, with its own rules

A container with a `fitContent` child (or one that's itself sizing to
content) has to call that child's `elMeasure` *before* it can decide how
much space to give it — which means, for that one child, the container
runs its size-reporting logic once as a pure question ("how big do you
want to be?") before running it again as the real thing ("here's your
space, draw yourself"):

```
Container's elRun for this frame
  |
  |  1. resolveLength calls each child's elMeasure
  |     to ask its preferred size along the main axis
  |     (must not draw, emit, or claim focus --
  |      may run speculatively, more than once,
  |      and never commits to a real frame)
  v
Container now knows every child's resolved size
  |
  |  2. arrange children within the space available
  |     (see the layout and controls areas for the
  |     specifics of how each does this)
  v
Container calls withBounds + elRun for each child in its
final slot -- this IS a committed frame: drawing,
focus claims, mouse capture, and emitted effects
here all count
```

This is why `elMeasure`'s type confines it to computing a `Size` and
nothing else — it must not draw, emit an effect, or claim focus, because
it may be called speculatively, more than once, or not at all, depending
on whether anything actually asked for a content-dependent size this
frame. Writing a custom container that measures children needs to respect
this; writing an ordinary widget generally doesn't, since
`noIntrinsicSize` (report zero, or whatever space is offered) is the right
answer unless your widget's natural size genuinely depends on its content
(text, primarily).

## Two kinds of "no space to give"

A container itself sizing to content (`fitContent` on the container, or
one nested inside another content-sized container) has a second wrinkle:
along the axis it's still figuring out, it doesn't have a number to offer
its own children as "available space" — not because there's *no* room,
but because *how much* room isn't decided yet. `Available` is the type
that keeps these apart:

```haskell
data Available = Bounded Double | Unbounded
```

`Bounded n` is an ordinary answer: exactly this much space. `Unbounded`
means "no ceiling has been decided yet" — a child asked for its natural
size in this situation takes exactly what it needs, since there's nothing
to fill or clamp against. Conflating the two would make "I have zero
pixels" indistinguishable from "ask me anything," which would make a
content-sizing container either collapse its children to nothing or hand
them an arbitrary number that means nothing.

## Attributes: a list instead of a record

Configuring an `Element`-producing widget with a list rather than a
record — `[text "Add", width (exactly 80)]` instead of a record with a
`text` and `width` field — is what lets you build that list with ordinary
list machinery (`++`, list comprehensions, conditionally including an
attribute at all) rather than fighting a fixed record shape.

Under the hood, each attribute is just a function from a widget's config
to a new config, and configuring a widget folds the whole list over a
starting default, left to right:

```haskell
resolve defaultLayoutConfig
  [ width fill          --  1: width := Fill
  , width (exactly 80)  --  2: width := Exactly 80  (overrides 1)
  ]

-- final config: width = Exactly 80
```

The practical consequence: **later entries in the list win**. If two
attributes set the same field, whichever comes last in the list is the one
that takes effect — there's no error, no merge, just plain left-to-right
override. This matters most when composing attribute lists programmatically
(a shared list of "base" attributes with a caller's overrides appended
after it).

The same handful of attributes — `width`, `height`, `align`, `onClicked`,
and so on — work across both layout containers and controls, not because
each widget redeclares them, but because each widget's config nests the
shared config those attributes target (layout config, element config)
somewhere inside its own. You don't need to know the mechanics of that
nesting to use it; it's why `width (exactly 80)` reads identically
whether you're sizing a button, a text input, or a whole box.

## Where this leaves you

`Element` and the attribute mechanism are the shared substrate — the next
two files are each a concrete application of it:
[Composing containers](02-layout.md) covers how `hBox`/`vBox`/`borderLayout`
use it to arrange multiple children, and
[The control primitive](03-controls.md) covers how every ready-made widget
uses it to get consistent focus, chrome, and
mouse handling.
