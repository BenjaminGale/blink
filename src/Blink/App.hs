{-# LANGUAGE DisambiguateRecordFields #-}
{- |
Module: Blink.App

= Application structure

'App' describes a complete Blink application: the UI is a pure function of
state, and the only way to change state is by queueing modifiers with
'Blink.UI.dispatch' and 'Blink.UI.dispatchAsync'.

@
data App e u s = App
  { startUp :: IO s
  , initialUIState :: u
  , theme :: s -> Theme e
  , view :: UI e u s ()
  }
@

  * @e@ is the element type — a sum type identifying each interactive control
    (see "Blink.UI").
  * @u@ is the UI state record — presentation state owned by the controls
    themselves (scroll positions and the like), persisted across frames inside
    the 'UIContext'. Use 'Blink.Controls.StandardControls' when only the
    standard controls need it (see "Blink.UI").
  * @s@ is the application state, owned by the host. The UI tree reads it with
    'Blink.UI.getAppState' and never mutates it directly; modifiers queued
    with 'Blink.UI.dispatch' are applied once the frame completes.

= Configuration

Pass an 'App' to 'configureContinuous' or 'configureEventDriven' to obtain a
'BlinkHandle'. Choose based on how the backend's render loop is driven:

  * 'configureContinuous'  — for backends that redraw every frame regardless of
    input (e.g. game-style loops). Draw commands from the first render pass are
    submitted immediately each frame.
  * 'configureEventDriven' — for backends that block on events. After applying
    the frame's dispatched modifiers, a second render pass runs on the updated
    state so the displayed frame always reflects the latest state. The 'IO ()'
    callback is invoked when async work completes, allowing the backend to
    unblock its event wait (e.g. @glfwPostEmptyEvent@).

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

'Blink.UI.dispatchAsync' queues a job @s -> IO (s -> s)@. 'stepFrame' forks
each queued job with the frame's post-dispatch state, posts the modifier the
job returns to an internal queue, and calls the async notification callback so
the backend can unblock its event wait. Posted modifiers are applied at the
start of the next 'stepFrame' call, before the render pass.

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
import Data.List (foldl')
import Data.Text (Text)

import Blink.Geometry (Point, Size, rectOrigin, resizeRect)
import Blink.Input (ButtonState, KeyEvent, InputState (..))
import Blink.Rendering (DrawCommand)
import Blink.Style (Theme)
import Blink.UI
  ( UI, UIContext (..), FocusState (..)
  , emptyUIContext, nextFrameContext
  , runUI, getDrawCommands, applyDispatches, getAsyncJobs
  )

-- | Describes a complete Blink application.
--
-- @e@ is the element type, @u@ the UI state record, and @s@ the application
-- state. See "Blink.UI" for an explanation of element IDs, UI state, and
-- application state.
data App e u s = App
  { startUp :: IO s
    -- ^ Produces the initial application state before the render loop begins.
  , initialUIState :: u
    -- ^ The UI state record as it should be on the first frame. Controls
    -- read and write it through 'Blink.UI.getUIState' and
    -- 'Blink.UI.modifyUIState'; it persists across frames inside the
    -- 'UIContext'.
  , theme :: s -> Theme e
    -- ^ Derives the active 'Theme' from the current state. Called each frame,
    -- allowing the theme to change in response to state changes.
  , view :: UI e u s ()
    -- ^ The UI tree. Reads the application state with 'Blink.UI.getAppState'
    -- and queues changes with 'Blink.UI.dispatch'; run once or twice per
    -- frame depending on the render mode.
  }

-- | Produces a 'BlinkHandle' for a continuous render backend. The draw list
-- from the first render pass is submitted immediately each frame.
configureContinuous :: App e u s -> TextMeasurer -> IO (BlinkHandle s)
configureContinuous app _measurer = do
  asyncQueue <- newIORef []
  ctxRef     <- newIORef Nothing
  pure BlinkHandle
    { initState = startUp app
    , stepFrame = doStep False app asyncQueue ctxRef (pure ())
    }

-- | Produces a 'BlinkHandle' for an event-driven backend. After applying the
-- frame's dispatched modifiers, a second render pass runs on the updated state.
-- The 'IO ()' callback is called when async work completes so the backend can
-- unblock its event wait.
configureEventDriven :: App e u s -> IO () -> TextMeasurer -> IO (BlinkHandle s)
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
    -- ^ Processes one frame: applies modifiers posted by completed async
    -- jobs, runs the view, applies the frame's dispatched modifiers, forks
    -- any async jobs, and returns draw commands paired with the new state.
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
  -> App e u s
  -> IORef [s -> s]
  -> IORef (Maybe (UIContext e u s))
  -> IO ()
  -> FrameInput
  -> s
  -> IO (FrameResult s)
doStep eventDriven app asyncQueue ctxRef notify fi state0 = do
  let winRect = resizeRect (windowSize fi) rectOrigin
      inputState = toInputState fi

  -- Apply modifiers posted by completed async jobs (oldest first)
  asyncMods <- atomicModifyIORef asyncQueue (\q -> ([], reverse q))
  let state = foldl' (flip ($)) state0 asyncMods

  mCtx <- readIORef ctxRef
  let ctx = case mCtx of
        Nothing -> emptyUIContext winRect inputState (theme app state) (initialUIState app) state
        Just c -> (nextFrameContext winRect inputState c)
          { ctxTheme = theme app state
          , ctxAppState = state
          }

  -- First render pass
  let ((), processedCtx) = runUI (view app) ctx

  -- Apply the modifiers dispatched during the frame
  let state' = applyDispatches processedCtx

  -- Fork async jobs with the post-dispatch state; each completed job's
  -- modifier is enqueued and the callback is invoked
  mapM_ (forkJob asyncQueue notify state') (getAsyncJobs processedCtx)

  -- Determine stable focus from the first pass
  let focusState = ctxFocusState processedCtx
      nextFocus = if focusedThisFrame focusState
                  then focusedElement focusState
                  else Nothing

  -- Produce draw commands and the context to persist for the next frame
  (drawCmds, nextCtx) <-
    if eventDriven
      then do
        let newTheme = if ctxThemeChangeRequested processedCtx
                       then theme app state'
                       else ctxTheme processedCtx
            clearedInput = clearKeyEvents inputState
            freshCtx = (nextFrameContext winRect clearedInput processedCtx)
              { ctxFocusState = focusState { focusedElement = nextFocus }
              , ctxTheme = newTheme
              , ctxAppState = state'
              }
            ((), renderedCtx) = runUI (view app) freshCtx
            storedCtx = renderedCtx
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

forkJob :: IORef [s -> s] -> IO () -> s -> (s -> IO (s -> s)) -> IO ()
forkJob queue notify s job = do
  _ <- forkIO $ do
    f <- job s
    atomicModifyIORef' queue (\q -> (f : q, ()))
    notify
  pure ()
