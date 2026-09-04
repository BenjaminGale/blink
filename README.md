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

- **Declarative view API** — A lightweight, list-based syntax for
  configuring controls and layout, covering appearance, sizing, and event
  handling together in one uniform way.

- **Message-driven updates** — Controls emit typed messages on interaction
  rather than mutating state directly; a single `update` function folds
  every message queued during a frame into your application state once the
  frame completes, in emission order.

- **Immediate-mode API** — A lower-level, state-threading API for
  hand-writing custom controls, with direct access to bounds, input, the
  active theme, and focus.

- **Rebuilt every frame** — A per-frame rendering model: the whole UI tree
  is recomputed from your application state every frame, either
  continuously or only when input arrives, depending on how the backend
  drives it.

- **Continuous or event-driven** — Two frame-driving modes: one that
  redraws on every iteration regardless of input, suited to backends like
  animated UIs, and one that only redraws when something's changed,
  suited to backends that block on events.

- **Controls** — A standard set of interactive controls: buttons,
  checkboxes, toggles, radio buttons, text inputs, sliders, progress bars,
  labels, dividers, and repeat buttons.

- **Layout** — A layout system combining row and column arrangement with
  spacing and alignment, size constraints (fill, exact size, minimum,
  maximum, fit-to-content), and named regions for things like a sidebar
  next to a main content area.

- **Theming** — A theme system built from a small colour palette and a set
  of per-control-state styles, computed from your application state each
  frame, with per-instance or per-class overrides and a shared default.

- **Backend-agnostic** — A backend-driven architecture, with no main loop
  of its own, so it can run on top of whatever windowing and rendering
  setup you're using. The included demo uses SDL2.

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

### Running the tests

```
cabal test
```

### Building the documentation

```
cabal haddock
```

## License

Blink is licensed under Apache-2.0. See [LICENSE](LICENSE).
