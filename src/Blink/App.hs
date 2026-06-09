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

= Backend integration

'BlinkHandle' is the interface the backend uses each frame. Obtain one via
'configureContinuous' or 'configureEventDriven', then drive the loop yourself:

@
loop handle state = do
  waitForPlatformEvents
  input  <- collectFrameInput
  result <- stepFrame handle input state
  case result of
    Continue draws state' -> render draws >> loop handle state'
    Quit     draws _      -> render draws
@

= Configuration

  * 'configureContinuous'  — for backends that redraw every frame regardless of
    input (e.g. game-style loops). Draw commands from the first render pass are
    submitted immediately.
  * 'configureEventDriven' — for backends that block on events. After processing
    commands, a second render pass runs on the updated state so the displayed
    frame always reflects the latest state. The 'IO ()' callback is invoked when
    async work completes, allowing the backend to unblock its event wait (e.g.
    @glfwPostEmptyEvent@).

= Text measurement

'TextMeasurer' is provided at configure time and made available to the UI monad
for cursor positioning and layout. Construct one from your platform's font API
and pass it to 'configureContinuous' or 'configureEventDriven'.
-}
module Blink.App
  ( -- * Application
    App (..)
    -- * Handle
  , BlinkHandle (..)
    -- * Configuration
  , configureContinuous
  , configureEventDriven
    -- * Frame types
  , FrameInput (..)
  , FrameResult (..)
    -- * Text measurement
  , TextMeasurer (..)
  , FontSpec (..)
  , FontMetrics (..)
  ) where

import Control.Concurrent (forkIO)
import Data.IORef
import Data.Text (Text)

import Blink.Geometry (Point, Size, rectOrigin, resizeRect)
import Blink.Input (ButtonState, KeyEvent, InputState (..))
import Blink.Rendering (DrawCommand)
import Blink.Style (Theme)
import Blink.UI
  ( UI, UIContext (..), FocusState (..)
  , emptyUIContext, nextFrameContext
  , runUI, getDrawCommands, getCommands
  , ctxThemeChangeRequested
  )
import Blink.Update (Update, runUpdate)

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
    -- frame depending on the render mode.
  , update :: c -> Update s c ()
    -- ^ Handles a command dispatched by the view, returning an 'Update' action
    -- that transforms the state. Applied to each command in dispatch order.
  }

-- | All per-frame inputs from the platform, assembled by the backend.
data FrameInput = FrameInput
  { mousePosition :: Point
  , mouseButton   :: ButtonState
  , keyEvents     :: [KeyEvent]
  , typedText     :: [Text]
  , windowSize    :: Size
  , quitRequested :: Bool
  }

-- | The result of processing a single frame. Draw commands are always included
-- so the backend can render the final frame before exiting on 'Quit'.
data FrameResult s
  = Continue [DrawCommand] s
  | Quit     [DrawCommand] s

-- | The interface the backend uses each frame.
data BlinkHandle s = BlinkHandle
  { initState :: IO s
    -- ^ Produces the initial application state.
  , stepFrame :: FrameInput -> s -> IO (FrameResult s)
    -- ^ Processes one frame: drains the async queue, runs the view and update
    -- cycle, forks any async effects, and returns draw commands with new state.
  }

-- | Identifies a font for text measurement.
data FontSpec = FontSpec
  { fontPath :: FilePath
  , fontSize :: Int
  } deriving (Eq, Ord, Show)

-- | Font-level metrics independent of content.
data FontMetrics = FontMetrics
  { lineHeight :: Float
  , ascender   :: Float
  , descender  :: Float
  }

-- | Text measurement operations provided by the backend at configure time.
data TextMeasurer = TextMeasurer
  { measureFont  :: FontSpec -> IO FontMetrics
    -- ^ Font-level metrics; used to determine control height before content is known.
  , measureText  :: Text -> FontSpec -> IO Size
    -- ^ Total bounds of a string; used for layout.
  , charOffset   :: Text -> FontSpec -> Int -> IO Float
    -- ^ X offset at a character index; used for cursor positioning.
  , charAtOffset :: Text -> FontSpec -> Float -> IO Int
    -- ^ Character index at a pixel offset; used for mouse hit testing.
  }

-- | Produces a 'BlinkHandle' for a continuous render backend. The draw list
-- from the first render pass is submitted immediately each frame.
configureContinuous :: App e s c -> TextMeasurer -> IO (BlinkHandle s)
configureContinuous app _measurer = do
  asyncQueue <- newIORef []
  ctxRef     <- newIORef Nothing
  pure BlinkHandle
    { initState = startUp app
    , stepFrame = doStep False app asyncQueue ctxRef (pure ())
    }

-- | Produces a 'BlinkHandle' for an event-driven backend. After processing
-- commands, a second render pass runs on the updated state. The 'IO ()' callback
-- is called when async work completes so the backend can unblock its event wait.
configureEventDriven :: App e s c -> IO () -> TextMeasurer -> IO (BlinkHandle s)
configureEventDriven app notify _measurer = do
  asyncQueue <- newIORef []
  ctxRef     <- newIORef Nothing
  pure BlinkHandle
    { initState = startUp app
    , stepFrame = doStep True app asyncQueue ctxRef notify
    }

doStep
  :: Bool
  -> App e s c
  -> IORef [c]
  -> IORef (Maybe (UIContext e c))
  -> IO ()
  -> FrameInput
  -> s
  -> IO (FrameResult s)
doStep eventDriven app asyncQueue ctxRef notify fi state = do
  let winRect    = resizeRect (windowSize fi) rectOrigin
      inputState = toInputState fi

  mCtx <- readIORef ctxRef
  let ctx = case mCtx of
        Nothing -> emptyUIContext winRect inputState (theme app state)
        Just c  -> (nextFrameContext winRect inputState c)
                     { ctxTheme = theme app state }

  -- Drain the async command queue (oldest first)
  asyncCmds <- atomicModifyIORef asyncQueue (\q -> ([], reverse q))

  -- First render pass
  let ((), processedCtx) = runUI (view app state) ctx

  -- Collect and process all commands
  let uiCmds  = getCommands processedCtx
      allCmds = asyncCmds ++ uiCmds
      (state', effects) = runCommands (update app) allCmds state

  -- Fork async effects; each result is enqueued and the callback is invoked
  mapM_ (forkEffect asyncQueue notify) effects

  -- Determine stable focus from the first pass
  let focusState = ctxFocusState processedCtx
      nextFocus  = if focusedThisFrame focusState
                   then focusedElement focusState
                   else Nothing

  -- Produce draw commands and the context to persist for the next frame
  (drawCmds, nextCtx) <-
    if eventDriven
      then do
        let newTheme     = if ctxThemeChangeRequested processedCtx
                           then theme app state'
                           else ctxTheme processedCtx
            clearedInput = clearKeyEvents inputState
            freshCtx     = (nextFrameContext winRect clearedInput processedCtx)
              { ctxFocusState = focusState { focusedElement = nextFocus }
              , ctxTheme      = newTheme
              }
            ((), renderedCtx) = runUI (view app state') freshCtx
            storedCtx         = renderedCtx
              { ctxFocusState = focusState { focusedElement = nextFocus } }
        pure (getDrawCommands renderedCtx, storedCtx)
      else
        pure
          ( getDrawCommands processedCtx
          , processedCtx { ctxFocusState = focusState { focusedElement = nextFocus } }
          )

  writeIORef ctxRef (Just nextCtx)

  pure $ if quitRequested fi
    then Quit drawCmds state'
    else Continue drawCmds state'

-- | Convert a 'FrameInput' to the 'InputState' the UI monad uses internally.
-- Uses positional matching to sidestep overlapping field names.
toInputState :: FrameInput -> InputState
toInputState (FrameInput mp mb kes txt _ _) = InputState mp mb kes txt

-- | Clear keyboard and text events from an 'InputState' for the second render
-- pass in event-driven mode.
clearKeyEvents :: InputState -> InputState
clearKeyEvents (InputState mp lb _ _) = InputState mp lb [] []

-- | Run a list of commands through the update function, collecting the final
-- state and any async effects produced along the way.
runCommands :: (c -> Update s c ()) -> [c] -> s -> (s, [IO c])
runCommands updateFn cmds s0 = foldl step (s0, []) cmds
  where
    step (s, effs) cmd =
      let ((), s', newEffects) = runUpdate (updateFn cmd) s
      in (s', effs ++ newEffects)

-- | Fork an async effect: run the action, enqueue the result, then notify.
forkEffect :: IORef [c] -> IO () -> IO c -> IO ()
forkEffect queue notify action = do
  _ <- forkIO $ do
    cmd <- action
    atomicModifyIORef' queue (\q -> (cmd : q, ()))
    notify
  pure ()
