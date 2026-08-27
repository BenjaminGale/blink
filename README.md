# Blink

Blink is an immediate-mode UI library for Haskell desktop applications: the
whole UI tree is rebuilt from scratch every frame as a pure function of your
application state, rather than retained and mutated incrementally. State
changes only through modifiers dispatched by the UI. Blink does not own the
main loop — a backend (SDL2, in the included demo) drives the loop and calls
into Blink once per frame.

## Example

A minimal todo list, showing the shape of a Blink application:

```haskell
data Element = NewItemInput | AddButton | ItemCheckbox Int
  deriving (Eq, Ord)

data Msg
  = SetNewItemText Text
  | AddItem
  | ToggleItem Int Bool

data AppState = AppState { newItemText :: Text, items :: [(Text, Bool)] }

todoView :: AppState -> UI Element Msg ()
todoView s = runElement $ vBox
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

app :: App Element Msg AppState
app = App
  { startUp = pure (AppState "" [])
  , theme   = const myTheme
  , view    = todoView
  , update  = todoUpdate
  }
```

`Element` identifies each interactive control, used to look up styles and
route keyboard focus — note `ItemCheckbox Int`, one constructor covering
every item's checkbox rather than a fixed control per row. `Msg` is what the
view emits: `onInput (postWith SetNewItemText)` queues an updated draft as
you type, `onActivated (post AddItem)` queues `AddItem` on click, and
`onSelectedChanged (postWith (ToggleItem i))` queues which item changed and
its new state. `AppState` is the application state, passed into `view`
explicitly each frame and rebuilt into a fresh `Element`/child list every
time — there's no manual diffing, since `items` in the view is just read
straight off `AppState`. `update` folds each queued `Msg` into the state once
the frame completes, in emission order. Pass `app` to `configureContinuous`
or `configureEventDriven` to get a `BlinkHandle`, then drive it with
`stepFrame` each iteration of your platform's event loop.

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
