# Blink

Blink is an immediate-mode UI library for Haskell desktop applications: the
whole UI tree is rebuilt from scratch every frame as a pure function of your
application state, rather than retained and mutated incrementally. State
changes only through modifiers dispatched by the UI. Blink does not own the
main loop — a backend (SDL2, in the included demo) drives the loop and calls
into Blink once per frame.

## Example

A minimal counter, showing the shape of a Blink application:

```haskell
data Element = Increment | CountLabel
  deriving (Eq, Ord)

view :: UI Element Int ()
view = vBox defaultBoxConfig
  [ ( Layout Fill (Exactly 24) TopLeft, do
        count <- getAppState
        label CountLabel (T.pack (show count))
    )
  , ( Layout Fill (Exactly 32) TopLeft, do
        clicked <- button Increment "+1"
        when clicked $ dispatch (+ 1)
    )
  ]

app :: App Element Int
app = App { startUp = pure 0, theme = const myTheme, view = view }
```

`Element` identifies each interactive control, used to look up styles and
route keyboard focus. `Int` is the application state — `view` reads it with
`getAppState` and queues changes with `dispatch`; the host applies them once
the frame completes. Pass `app` to `configureContinuous` or
`configureEventDriven` to get a `BlinkHandle`, then drive it with `stepFrame`
each iteration of your platform's event loop.

## Modules

  * `Blink.App` — application definition and backend integration.
  * `Blink.UI` — the UI monad: drawing, interaction, focus, and style queries.
  * `Blink.Controls` — ready-made controls: buttons, text inputs, checkboxes,
    progress bars, and labels.
  * `Blink.Layout` — box layout and constraint-based sizing.
  * `Blink.Style` — themes and per-state styles.
  * `Blink.Rendering` — the draw command list produced each frame.
  * `Blink.Geometry` — primitive geometry types.
  * `Blink.Input` — raw keyboard and mouse types assembled by the backend
    each frame.

See each module's Haddock documentation for details; the top-level `Blink`
module re-exports everything above and is the package's entry point.

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
