# Blink

Blink is a declarative, functional GUI library for Haskell desktop
applications. You describe your interface as a tree of elements built from
your application's state, and Blink handles turning that into what's drawn
on screen and how it responds to input. That declarative layer sits on top
of a lower-level immediate-mode API, which is there if you need to
hand-write a custom control. Blink doesn't own the main loop itself; a
backend, such as the SDL2 one used in the included demo, drives the loop
and calls into Blink.

## Features

- **Declarative view API** — You describe your interface as a tree of
  elements built from your application's state, and Blink handles turning
  that into what's drawn on screen and how it responds to input. That
  declarative layer sits on top of a lower-level immediate-mode API, which
  you can use directly to hand-write a custom control when the ready-made
  ones don't fit.
- **Immediate-mode API** — Underneath the declarative layer is a `UI` monad
  you can write against directly: a state-threading computation with access
  to bounds, input, the active theme, and focus, letting you append draw
  commands and queue state changes by hand. Reach for it when you're
  building a custom control that the ready-made ones and layout combinators
  can't express.
- **Rebuilt every frame** — The whole UI tree is rebuilt from your
  application state each frame, rather than a retained tree you update
  incrementally, so what you see is always a pure function of the current
  state with no diffing to reason about. You can drive this continuously,
  redrawing every frame regardless of input, which suits backends like
  animated UIs, or only when something's changed, which suits backends
  that block on events. Either way, you drive it the same way, one step
  per iteration of your loop.
- **Controls** — Blink ships with a standard set of interactive controls:
  buttons, checkboxes, toggles, radio buttons, text inputs, sliders,
  progress bars, labels, dividers, and repeat buttons.
- **Layout** — Elements are arranged and sized declaratively rather than by
  hand-placing coordinates. You can lay children out in rows and columns
  with spacing and alignment, size them by rule (fill available space, an
  exact size, a minimum, a maximum, or fit their content), and split space
  into named regions, like a sidebar next to a main content area.
- **Theming** — Themes are built from a small palette of colours plus a set
  of styles per control state, computed from your application state each
  frame. Every control can be styled individually or by class, and can fall
  back to a shared default.
- **Backend-agnostic** — Blink doesn't own the main loop. A backend
  assembles input for a frame, calls into Blink, and draws the result, so
  Blink can run on top of whatever windowing and rendering setup you're
  using. The included demo uses SDL2.

## Example

A minimal todo list, showing the shape of a Blink application:

```haskell
import Blink
import Blink.UI.Element (Element)

data ControlId = NewItemInput | AddButton | ItemCheckbox Int
  deriving (Eq, Ord)

data Msg
  = SetNewItemText Text
  | AddItem
  | ToggleItem Int Bool

data AppState = AppState { newItemText :: Text, items :: [(Text, Bool)] }

todoView :: AppState -> Element ControlId Msg
todoView s = vBox
  [ spacing 8, margin 12
  , children
      [ hBox
          [ spacing 8
          , children
              [ textInput NewItemInput [value (newItemText s), onInput (postWith SetNewItemText), width Fill]
              , button AddButton [text "Add", onActivated (post AddItem), width (Exactly 80)]
              ]
          ]
      , vBox
          [ spacing 4
          , children
              [ checkbox (ItemCheckbox i)
                  [text label, isSelected done, onSelectedChanged (postWith (ToggleItem i)), height (Exactly 24)]
              | (i, (label, done)) <- zip [0 ..] (items s)
              ]
          ]
      ]
  ]

todoUpdate :: Msg -> Update AppState ()
todoUpdate msg = case msg of
  SetNewItemText t -> modify $ \s -> s { newItemText = t }
  AddItem           -> modify $ \s -> s { items = items s ++ [(newItemText s, False)], newItemText = "" }
  ToggleItem i done -> modify $ \s -> s
    { items = [ if j == i then (t, done) else item | (j, item@(t, _)) <- zip [0 ..] (items s) ] }

app :: App ControlId Msg AppState
app = App
  { startUp = pure (AppState "" [])
  , theme   = const myTheme
  , view    = todoView
  , update  = todoUpdate
  }
```

`ControlId` identifies each interactive control, used to look up styles and
route keyboard focus. A single constructor can cover a whole family of
controls — `ItemCheckbox Int` stands for every item's checkbox rather than
one constructor per row.

`Msg` is what the view emits. `onInput (postWith SetNewItemText)` queues an
updated draft as you type, `onActivated (post AddItem)` queues `AddItem` on
click, and `onSelectedChanged (postWith (ToggleItem i))` queues which item
changed and its new state.

`view` takes `AppState` and rebuilds the full `Element` tree from it every
frame — `items` in `todoView` is just read straight off `AppState`, with no
manual diffing. `update` then folds each queued `Msg` into the state once the
frame completes, in emission order.

Pass `app` to `configureContinuous` or `configureEventDriven` to get a
`BlinkHandle`, then drive it with `stepFrame` each iteration of your
platform's event loop.

See the top-level `Blink` module's Haddock documentation for the full module
guide and package entry point.

## Getting Started

### Dependencies

Install the SDL2 and SDL2 TTF development libraries:

```
sudo apt-get install libsdl2-dev libsdl2-ttf-dev
```

### Running the demo

```
cabal run demo
```
