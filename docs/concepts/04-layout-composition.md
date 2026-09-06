# 4. Layout composition: `Element`, measurement, and containers

[Section 3](03-layout-primitives.md) covered layout at the bare
immediate-mode level: a single current-bounds rectangle, and a plain `UI`
action that fills whatever it's given. That's enough for one control in
isolation, but it has no way to answer a question a container like `hBox`
has to ask constantly: "how much space does *this* child actually want,
so I can decide how to split my space among all of them?" `Element` is
the type that adds an answer.

## `Element`: a size request alongside the action

```haskell
data Element e msg = Element
  { elLayout  :: Layout                     -- this element's size request
  , elMeasure :: MeasureCtx -> UI e msg Size -- its preferred size, if asked
  , elRun     :: UI e msg ()                 -- runs it for this frame
  }
```

Every ready-made control (`button`, `checkbox`, ...) and every
`hBox`/`vBox` already produces one of these — you don't construct one by
hand unless you're placing a hand-written `UI` action as a container child
(`elementWithLayout`, covered in
[the layout-composition guide](../guides/composing-a-layout.md)). The
`elLayout` field is a `Layout`: a `Length` for each axis (`exactly n`,
`fill`, `atLeast`/`atMost`/`between`, or `fitContent`) plus an alignment —
see `Blink.Layout.Constraints`'s Haddocks for the exact resolution rules
of each, worked through with diagrams.

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
Box's elRun for this frame
  |
  |  1. resolveLength calls each child's elMeasure
  |     to ask its preferred size along the main axis
  |     (must not draw, emit, or claim focus --
  |      may run speculatively, more than once,
  |      and never commits to a real frame)
  v
Box now knows every child's resolved size
  |
  |  2. distribute leftover space among flexible
  |     children (surplus-distribution algorithm)
  v
Box calls withBounds + elRun for each child in its
final slot -- this IS a committed frame: drawing,
focus claims, mouse capture, and emitted messages
here all count
```

This is why `elMeasure`'s type confines it to computing a `Size` and
nothing else — it must not draw, emit a message, or claim focus, because
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

## Composing many children: what a box actually does

Once every child's `Length` is resolved to a concrete number (via
`elMeasure` where needed), `hBox`/`vBox` distribute space along the main
axis with a small, fixed set of rules — fixed-size children take exactly
what they asked for, flexible children split whatever's left equally
(capped by their own `atMost`/`between` ceiling if they have one), and the
whole group is then positioned according to the box's `alignment`. This is
worth reading in `Blink.Layout.Box`'s own Haddocks rather than restating
here — `hBox`'s module documentation walks through it with the same kind
of worked diagrams as `layoutWithConstraints`, now that you have the
`elMeasure`/`Available` vocabulary those examples assume.

## Where this leaves you

`Element` is what turns "a `UI` action that fills whatever bounds it's
given" ([section 3](03-layout-primitives.md)) into something a container
can arrange alongside others — by giving it a size request, and a
measurement path for when that request depends on content. See
[the layout-composition guide](../guides/composing-a-layout.md) for this
in practice: choosing between `exactly`/`fill`/`fitContent`, nesting boxes
inside `borderLayout`'s named regions, and converting between `Element`
and a bare `UI` action at the boundaries between them.
