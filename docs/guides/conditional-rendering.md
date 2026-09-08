# Conditional rendering

There's no special combinator for "render this or that" or "render this
only if." A view function is a plain Haskell function returning an
`Element`, and a `children` list is a plain `[Element e msg]`
([the elements concept](../concepts/02-elements/01-introduction.md)) — so
ordinary Haskell control flow already does the whole job. This guide is
just a tour of the shapes that come up, plus `emptyElement`, the one small
addition to the API for the case ordinary control flow doesn't cover on
its own.

## Branching between two elements — `if`

When both branches produce a real element of the same type, a plain `if`
is all you need:

```haskell
statusView :: AppState -> Element ScreenId Msg
statusView s = if isConnected s
  then label ConnLabel [text "Connected", style connectedStyle]
  else label ConnLabel [text "Offline", style offlineStyle]
```

## Optionally including a child — list comprehension guard

Because `children` just wants a list, leaving an element out entirely is a
guard clause, not a special case:

```haskell
toolbarView :: AppState -> Element ScreenId Msg
toolbarView s = hBox
  [ spacing 8
  , children $
      [ button SaveBtn [text "Save", onActivated (post Save)] ]
      ++ [ button DeleteBtn [text "Delete", onActivated (post Delete)] | canDelete s ]
  ]
```

## Rendering from a `Maybe` — `maybe`/`mapMaybe`

The same idea extended to data that's already optional:

```haskell
sidebarView :: AppState -> Element ScreenId Msg
sidebarView s = vBox
  [ spacing 4
  , children $ mapMaybe id
      [ Just (label TitleLabel [text "Details"])
      , detailRow <$> selectedItem s
      ]
  ]
```

## When the slot must stay occupied — `emptyElement`

The three shapes above all work by changing how many elements go into a
list — fine for a box, whose children negotiate space among themselves.
`borderLayout`'s named regions and a fixed-index list of children (say,
one row per tab, where index `i` matters) don't have that flexibility:
leaving a branch out isn't an option, so the fallback needs to be an
actual `Element` — one that takes up no space and draws nothing.
`spacer` doesn't fit here: it *fills* whatever room it's given, which
would push everything else in the list around. `emptyElement` is `0x0` and
non-drawing, so it holds a slot without visually existing:

```haskell
bannerView :: AppState -> Element ScreenId Msg
bannerView s = vBox
  [ children
      [ if hasError s
          then errorBanner (errorMessage s)
          else emptyElement -- reserves nothing, draws nothing, but keeps this vBox slot
      , mainContent s
      ]
  ]
```

## Summary

Reach for plain Haskell first — `if`, list comprehension guards, `maybe`/
`mapMaybe` — since `Element` and `children` are ordinary values, not a
bespoke DSL. `emptyElement` is the one addition worth knowing about, for
the specific case where a branch has nothing to draw but still needs to
produce a same-shaped `Element` rather than being left out of a list.
