# 2. Declarative controls and the attribute mechanism

[Section 1](01-why-immediate-mode.md) explained immediate mode in terms of
the lowest level of Blink: a view function that produces draw commands
directly, with nothing retained between frames. In practice, you'll rarely
write at that level. Most views are built from `Blink.Controls`'
ready-made widgets and `Blink.Layout`'s containers, configured with a list
of attributes:

```haskell
button AddButton [text "Add", onActivated (post AddItem), width (exactly 80)]
```

This section covers that layer — the one you'll actually spend most of
your time in — and how it relates to the immediate-mode primitives the
rest of these concepts describe.

## Two layers, one output

`button`, `checkbox`, `hBox`, and every other combinator in
`Blink.Controls`/`Blink.Layout` are not a different way of building a UI —
they're a declarative *front end* for exactly the same immediate-mode
`UI` actions from `Blink.UI`. Each one is rebuilt fresh every frame, same
as everything else in [section 1](01-why-immediate-mode.md), and resolves
down to the same primitives:

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

Nothing about immediate mode changes because you're using the declarative
layer — it's the same rebuild-every-frame model, just assembled for you
instead of hand-written the primitive-by-primitive way
[the custom-control guide](../guides/building-a-custom-control.md) does by
hand. You reach for the primitives directly only when a widget's shape
doesn't fit what `Blink.Controls` already offers.

## Attributes: a list instead of a record

Configuring a widget with a list rather than a record — `[text "Add",
width (exactly 80)]` instead of a record with a `text` and `width`
field — is what lets you build that list with ordinary list machinery
(`++`, list comprehensions, conditionally including an attribute at all)
rather than fighting a fixed record shape.

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
and so on — work across many unrelated widgets, not because each widget
redeclares them, but because each widget's config nests the shared config
those attributes target (layout config, element config) somewhere inside
its own. You don't need to know the mechanics of that nesting to use it;
it's why `width (exactly 80)` reads identically whether you're sizing a
button, a text input, or a whole box.

## Where this leaves you

You can build an entire application — the todo list in the top-level
README is a complete example — using only this declarative layer and never
call an immediate-mode primitive directly. The remaining sections
([the frame loop](03-the-frame-loop.md),
[elements and messages](04-elements-and-messages.md), and
[focus and timing](05-focus-and-timing.md)) describe the layer underneath
that this one compiles into — worth understanding even if you never write
a `UI` action by hand, since it's what every declarative control you use is
actually doing each frame.
