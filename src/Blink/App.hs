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

= Configuration

Pass an 'App' to 'configureContinuous' or 'configureEventDriven' to obtain a
'BlinkHandle'. Choose based on how the backend's render loop is driven:

  * 'configureContinuous'  — for backends that redraw every frame regardless of
    input (e.g. game-style loops). Draw commands from the first render pass are
    submitted immediately each frame.
  * 'configureEventDriven' — for backends that block on events. After processing
    commands, a second render pass runs on the updated state so the displayed
    frame always reflects the latest state. The 'IO ()' callback is invoked when
    async work completes, allowing the backend to unblock its event wait (e.g.
    @glfwPostEmptyEvent@).

= Backend integration

'BlinkHandle' is the interface the backend uses each frame. Call 'initState'
once to obtain the initial application state, then drive the render loop by
calling 'stepFrame' each iteration with a 'FrameInput' assembled from platform
events:

@
loop handle state = do
  waitForPlatformEvents
  input  <- collectFrameInput
  result <- stepFrame handle input state
  case result of
    Continue draws state' -> render draws >> loop handle state'
    Quit     draws _      -> render draws
@

Draw commands are included in both 'Continue' and 'Quit' so the backend can
render the final frame before exiting.

= Quit flow

Set 'quitRequested' in 'FrameInput' when the platform detects a close signal
(e.g. the window's close button). 'stepFrame' returns 'Quit' on the same frame.

= Async updates

'Blink.Update.effect' queues an @IO c@ action. 'stepFrame' forks each queued
action, posts the resulting command @c@ to an internal queue, and calls the
async notification callback so the backend can unblock its event wait.
Async commands are drained at the start of the next 'stepFrame' call, before
UI-driven commands, and flow through the normal 'update' cycle.

= Text measurement

'TextMeasurer' is provided at configure time for cursor positioning and layout.
Construct one from your platform's font API and pass it to 'configureContinuous'
or 'configureEventDriven'.
-}
module Blink.App
  ( -- * Application
    App (..)
    -- * Configuration
  , configureContinuous
  , configureEventDriven
    -- * Handle
  , BlinkHandle (..)
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
    -- ^ Handles a command dispatched by the view. Applied to each command in
    -- dispatch order; async commands are processed before UI-driven ones.
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

-- | The interface the backend uses each frame. Obtain via 'configureContinuous'
-- or 'configureEventDriven'.
data BlinkHandle s = BlinkHandle
  { initState :: IO s
    -- ^ Produces the initial application state. Call once before entering the
    -- render loop.
  , stepFrame :: FrameInput -> s -> IO (FrameResult s)
    -- ^ Processes one frame: drains the async command queue, runs the view,
    -- applies all commands through 'update', forks any async effects, and
    -- returns draw commands paired with the new state.
  }

-- | All per-frame inputs from the platform, assembled by the backend each
-- iteration before calling 'stepFrame'.
data FrameInput = FrameInput
  { mousePosition :: Point
    -- ^ Cursor position in window coordinates.
  , mouseButton   :: ButtonState
    -- ^ State of the primary (left) mouse button.
  , keyEvents     :: [KeyEvent]
    -- ^ Keyboard events for this frame.
  , typedText     :: [Text]
    -- ^ Text input events for this frame, in the order they were received.
  , windowSize    :: Size
    -- ^ Current dimensions of the window's drawing area.
  , quitRequested :: Bool
    -- ^ Set to 'True' when the platform signals that the window should close.
    -- 'stepFrame' returns 'Quit' on the same frame this is first set.
  }

-- | The result of processing a single frame.
data FrameResult s
  = Continue [DrawCommand] s
    -- ^ Normal frame. Render the draw commands and loop with the new state.
  | Quit [DrawCommand] s
    -- ^ The application has quit. Render the draw commands (the final frame)
    -- then exit the loop.

-- | Identifies a font for text measurement.
data FontSpec = FontSpec
  { fontPath :: FilePath
    -- ^ Path to the font file.
  , fontSize :: Int
    -- ^ Point size.
  } deriving (Eq, Ord, Show)

-- | Font-level metrics that do not depend on the string content. Retrieved via
-- 'TextMeasurer.measureFont'.
data FontMetrics = FontMetrics
  { lineHeight :: Float
    -- ^ Distance between consecutive baselines.
  , ascender   :: Float
    -- ^ Distance from the baseline to the top of the tallest glyph.
  , descender  :: Float
    -- ^ Distance from the baseline to the bottom of the deepest descender.
    -- Typically negative.
  }

-- | Text measurement operations provided by the backend at configure time.
-- All operations are in @IO@ because they may invoke the platform's font
-- renderer.
data TextMeasurer = TextMeasurer
  { measureFont  :: FontSpec -> IO FontMetrics
    -- ^ Returns font-level metrics. Used during layout to determine control
    -- height before the content string is known.
  , measureText  :: Text -> FontSpec -> IO Size
    -- ^ Returns the total bounding box of a string. Used for layout.
  , charOffset   :: Text -> FontSpec -> Int -> IO Float
    -- ^ Returns the x offset (in pixels) at a given character index, measured
    -- from the start of the string. Used for cursor positioning.
  , charAtOffset :: Text -> FontSpec -> Float -> IO Int
    -- ^ Returns the character index closest to a given x offset. Used for
    -- mapping a mouse click position back to a character index.
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

-- Uses positional matching to sidestep overlapping field names with InputState.
toInputState :: FrameInput -> InputState
toInputState (FrameInput mp mb kes txt _ _) = InputState mp mb kes txt

-- Clears keyboard and text events for the second render pass in event-driven mode.
clearKeyEvents :: InputState -> InputState
clearKeyEvents (InputState mp lb _ _) = InputState mp lb [] []

runCommands :: (c -> Update s c ()) -> [c] -> s -> (s, [IO c])
runCommands updateFn cmds s0 = foldl step (s0, []) cmds
  where
    step (s, effs) cmd =
      let ((), s', newEffects) = runUpdate (updateFn cmd) s
      in (s', effs ++ newEffects)

forkEffect :: IORef [c] -> IO () -> IO c -> IO ()
forkEffect queue notify action = do
  _ <- forkIO $ do
    cmd <- action
    atomicModifyIORef' queue (\q -> (cmd : q, ()))
    notify
  pure ()
