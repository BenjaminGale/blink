{-# LANGUAGE DisambiguateRecordFields #-}
{- |
Module: Blink.App

= Application structure

'App' describes a complete Blink application using an Elm-style architecture:
the UI is a pure function of state, and the only way to change state is by
handling commands dispatched by the UI.

@
data App e s c = App
  { startUp :: IO s            -- ^ produce the initial state
  , theme   :: s -> Theme e    -- ^ derive the active theme
  , view    :: s -> UI e c ()  -- ^ render the current state
  , update  :: c -> Update s c () -- ^ handle a dispatched command
  }
@

  * @e@ is the element type — a sum type identifying each interactive control
    (see "Blink.UI").
  * @s@ is the application state, owned entirely by the host; the UI tree
    receives it as a read-only argument and never mutates it directly.
  * @c@ is the command type — how the view signals state changes back to the
    application (see "Blink.UI").

= Update

'App.update' maps each command to an 'Update' action (see "Blink.Update").
Commands produced during a frame are applied in dispatch order; the resulting
state is used from the next frame onward.

= Backend

'Backend' is a thin platform abstraction over a windowing system. It supplies
event collection, a close signal, window dimensions, and a draw-list renderer.
'FrameMode' controls how the draw list is produced each iteration.

= Frame mode

  * 'EventDriven' — after processing the view's commands, a second render pass
    runs on the updated state so the displayed frame always reflects the latest
    state. Preferred for most applications.
  * 'Continuous' — the draw list from the first pass is submitted immediately.
    Use when the view is redrawn every frame regardless of input.

= Running the loop

'runApp' calls 'startUp', builds the initial context, then drives the render
loop until the backend's 'shouldClose' returns 'True'.
-}
module Blink.App
  ( -- * Application
    App (..)
  , runApp
    -- * Backend
  , Backend (..)
  , FrameMode (..)
  ) where

import Blink.Rendering (DrawCommand)
import Blink.Geometry (Point (..), Size (..), rectOrigin, resizeRect)
import Blink.Input (ButtonState (..), InputState (..))
import Blink.Style (Theme)
import Blink.UI (FocusState (..), UI, UIContext (..), emptyUIContext, nextFrameContext, runUI, getDrawCommands, getCommands, ctxThemeChangeRequested)
import Blink.Update (Update, execCommands)
import Control.Monad (unless)

-- | Controls how the draw list for each frame is produced.
data FrameMode
  = EventDriven
    -- ^ After processing the view's commands, runs a second render pass on the
    -- updated state. The frame displayed always reflects the latest state.
  | Continuous
    -- ^ Submits the draw list from the first render pass immediately. Suitable
    -- for continuously-animated content that redraws every frame.

-- | Platform abstraction over the windowing system. Implement one per target
-- platform (e.g. GLFW, SDL).
data Backend = Backend
  { collectEvents :: IO InputState
    -- ^ Gather mouse, button, and keyboard events for the current frame.
  , shouldClose :: IO Bool
    -- ^ Return 'True' to exit the render loop.
  , windowSize :: IO Size
    -- ^ The current dimensions of the window's drawing area.
  , render :: [DrawCommand] -> IO ()
    -- ^ Submit a frame's draw list to the renderer.
  , frameMode :: FrameMode
    -- ^ Whether to use event-driven or continuous rendering.
  }

-- | Describes a complete Blink application.
--
-- @e@ is the element type, @s@ the application state, and @c@ the command
-- type. See "Blink.UI" for an explanation of element IDs and commands.
data App e s c = App
  { startUp :: IO s
    -- ^ Produces the initial application state before the render loop begins.
  , theme :: s -> Theme e
    -- ^ Derives the active 'Theme' from the current state. Called each frame,
    -- allowing the theme to change in response to state changes.
  , view :: s -> UI e c ()
    -- ^ Renders the current state as a 'UI' tree. Called once or twice per
    -- frame depending on the 'FrameMode'.
  , update :: c -> Update s c ()
    -- ^ Handles a command dispatched by the view, returning an 'Update' action
    -- that transforms the state. Applied to each command in dispatch order.
  }

-- | Initialises the application and enters the render loop, driving the
-- backend until 'shouldClose' returns 'True'.
runApp :: Backend -> App e s c -> IO ()
runApp backend app = do
  state <- startUp app
  let initialCtx = emptyUIContext
        rectOrigin
        (InputState (Point 0 0) ButtonUp [] [])
        (Blink.App.theme app state)
  loop backend app state initialCtx

loop :: Backend -> App e s c -> s -> UIContext e c -> IO ()
loop backend app state ctx = do
  events <- collectEvents backend
  close <- shouldClose backend
  unless close $ do
      size <- windowSize backend
      let winRect = resizeRect size rectOrigin
          frameCtx = (nextFrameContext winRect events ctx) { ctxTheme = Blink.App.theme app state }
          processedCtx = snd $ runUI (view app state) frameCtx
          focusState = ctxFocusState processedCtx
          nextFocus = if focusedThisFrame focusState then focusedElement focusState else Nothing
          state' = execCommands (update app) (getCommands processedCtx) state
          (drawCalls', nextCtx) = case frameMode backend of
            EventDriven ->
              let newTheme = if ctxThemeChangeRequested processedCtx
                               then Blink.App.theme app state'
                               else ctxTheme processedCtx
                  freshCtx = (nextFrameContext winRect (events { keyEvents = [], typedText = [] }) processedCtx)
                    { ctxFocusState = focusState { focusedElement = nextFocus }
                    , ctxTheme = newTheme
                    }
                  renderedCtx = snd $ runUI (view app state') freshCtx
              in (getDrawCommands renderedCtx, renderedCtx { ctxFocusState = focusState { focusedElement = nextFocus } })
            Continuous -> (getDrawCommands processedCtx, processedCtx { ctxFocusState = focusState { focusedElement = nextFocus } })
      render backend drawCalls'
      loop backend app state' nextCtx
