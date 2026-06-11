{- |
Module: Blink.App

= Application structure

'App' bundles everything Blink needs to run: the startup action that
produces the initial state, the starting UI state, a function from state
to 'Theme', and the UI tree.

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

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void)
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Blink.Geometry (Point, Rectangle, Size, rectOrigin, resizeRect)
import Blink.Input (ButtonState, KeyEvent, InputState (..))
import Blink.Rendering (DrawCommand)
import Blink.Style (Theme)
import Blink.UI
  ( UI, UIContext (..)
  , AnimationState (..)
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
configureContinuous :: Eq e => App e u s -> TextMeasurer -> IO (BlinkHandle s)
configureContinuous app _measurer = do
  refs <- AppRefs <$> newIORef [] <*> newIORef Nothing <*> newIORef False <*> newIORef Nothing
  pure BlinkHandle
    { initState = startUp app
    , stepFrame = doStepContinuous app refs
    }

-- | Produces a 'BlinkHandle' for an event-driven backend. After applying the
-- frame's dispatched modifiers, a second render pass runs on the updated state.
-- The 'IO ()' callback is called when async work completes so the backend can
-- unblock its event wait.
configureEventDriven :: Eq e => App e u s -> IO () -> TextMeasurer -> IO (BlinkHandle s)
configureEventDriven app notify _measurer = do
  refs <- AppRefs <$> newIORef [] <*> newIORef Nothing <*> newIORef False <*> newIORef Nothing
  pure BlinkHandle
    { initState = startUp app
    , stepFrame = doStepEventDriven app refs notify
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
  , quitRequested   :: Bool
    -- ^ Set to 'True' when the platform signals that the window should close.
    -- 'stepFrame' returns 'Quit' on the same frame this is first set.
  , isAnimationTick :: Bool
    -- ^ Set to 'True' when this frame was triggered by the animation ticker
    -- rather than a platform input event. Blink's ticker calls the @notify@
    -- callback passed to 'configureEventDriven'; backends should detect that
    -- wake-up and set this field accordingly.
  }

-- | The result of processing a single frame.
data FrameResult s
  = Continue [DrawCommand] s
    -- ^ Normal frame. Render the draw commands and loop with the new state.
  | Quit [DrawCommand] s
    -- ^ The application has quit. Render the draw commands (the final frame)
    -- then exit the loop.

-- | Identifies a font for text measurement. Passed to 'measureFont',
-- 'measureText', 'charOffset', and 'charAtOffset' to select the font.
data FontSpec = FontSpec
  { fontPath :: FilePath
    -- ^ Path to the font file.
  , fontSize :: Int
    -- ^ Point size.
  } deriving (Eq, Ord, Show)

-- | Font-level metrics returned by 'measureFont' for a given 'FontSpec'.
-- These metrics do not depend on string content.
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

-- Mutable state shared across frames, allocated once at configure time.
data AppRefs e u s = AppRefs
  { refsAsyncQueue :: IORef [s -> s]
    -- Modifiers posted by completed async jobs, waiting to be applied at the
    -- start of the next frame. Accumulated in LIFO order; reversed on drain.
  , refsCtx        :: IORef (Maybe (UIContext e u s))
    -- The UIContext carried over from the previous frame. 'Nothing' on the
    -- first frame, causing 'runFrame' to build a fresh context via
    -- 'emptyUIContext'.
  , refsAnimActive :: IORef Bool
    -- Written at the end of each frame. The running ticker thread reads this
    -- to decide whether to continue looping or exit. A False->True edge
    -- causes a new ticker thread to be forked.
  , refsLastFrame  :: IORef (Maybe Word64)
    -- Monotonic nanosecond timestamp of the previous frame, used to compute
    -- the wall-clock delta. Only accessed inside 'runFrame', which is called
    -- sequentially, so no concurrent access concerns.
  }

buildCtx :: Eq e => App e u s -> Rectangle -> InputState -> Float -> Bool -> s -> Maybe (UIContext e u s) -> UIContext e u s
buildCtx app winRect inputState delta isAnimTick state mCtx =
  let animState = AnimationState { animDelta = delta, animIsTick = isAnimTick }
      ctx = case mCtx of
        Nothing -> emptyUIContext winRect inputState (theme app state) (initialUIState app) state
        Just c  -> (nextFrameContext winRect inputState c)
          { ctxTheme    = theme app state
          , ctxAppState = state
          }
  in ctx { ctxAnimation = animState }

runFrame
  :: Eq e
  => App e u s
  -> AppRefs e u s
  -> IO ()
  -> FrameInput
  -> s
  -> IO (UIContext e u s, s)
runFrame app refs notify input prevState = do
  let winRect    = resizeRect (windowSize input) rectOrigin
      inputState = toInputState input

  asyncMods <- atomicModifyIORef (refsAsyncQueue refs) (\q -> ([], reverse q))
  let state = foldl' (flip ($)) prevState asyncMods

  delta <- sampleDelta (refsLastFrame refs) (isAnimationTick input)

  mCtx <- readIORef (refsCtx refs)
  let ctx  = buildCtx app winRect inputState delta (isAnimationTick input) state mCtx
      ((), ctx') = runUI (view app) ctx
      state'     = applyDispatches ctx'

  mapM_ (forkJob (refsAsyncQueue refs) notify state') (getAsyncJobs ctx')

  pure (ctx', state')

doStepContinuous :: Eq e => App e u s -> AppRefs e u s -> FrameInput -> s -> IO (FrameResult s)
doStepContinuous app refs input prevState = do
  (ctx', state') <- runFrame app refs (pure ()) input prevState
  writeIORef (refsCtx refs) (Just ctx')
  pure $ toResult input (getDrawCommands ctx') state'

doStepEventDriven :: Eq e => App e u s -> AppRefs e u s -> IO () -> FrameInput -> s -> IO (FrameResult s)
doStepEventDriven app refs notify input prevState = do
  (firstPassCtx, state') <- runFrame app refs notify input prevState
  let winRect    = resizeRect (windowSize input) rectOrigin
      inputState = toInputState input
      freshCtx   =
          withAppState state'
        . withTheme (theme app state')
        . nextFrameContext winRect (clearKeyEvents inputState)
        $ firstPassCtx
      ((), renderedCtx) = runUI (view app) freshCtx
  writeIORef (refsCtx refs) (Just renderedCtx)
  wasActive <- readIORef (refsAnimActive refs)
  let nowActive = ctxRequiresAnimation renderedCtx
  writeIORef (refsAnimActive refs) nowActive
  when (not wasActive && nowActive) $
    forkAnimationTicker (refsAnimActive refs) notify
  pure $ toResult input (getDrawCommands renderedCtx) state'

toResult :: FrameInput -> [DrawCommand] -> s -> FrameResult s
toResult input draws state
  | quitRequested input = Quit draws state
  | otherwise           = Continue draws state

toInputState :: FrameInput -> InputState
toInputState fi = InputState
  { inputMousePosition = mousePosition fi
  , inputLeftButton    = mouseButton fi
  , inputKeyEvents     = keyEvents fi
  , inputTypedText     = typedText fi
  }

-- Clears keyboard and text events for the second render pass in event-driven mode.
clearKeyEvents :: InputState -> InputState
clearKeyEvents is = is { inputKeyEvents = [], inputTypedText = [] }

withTheme :: Theme e -> UIContext e u s -> UIContext e u s
withTheme t ctx = ctx { ctxTheme = t }

withAppState :: s -> UIContext e u s -> UIContext e u s
withAppState s ctx = ctx { ctxAppState = s }

forkJob :: IORef [s -> s] -> IO () -> s -> (s -> IO (s -> s)) -> IO ()
forkJob queue notify s job = do
  _ <- forkIO $ do
    f <- job s
    atomicModifyIORef' queue (\q -> (f : q, ()))
    notify
  pure ()

sampleDelta :: IORef (Maybe Word64) -> Bool -> IO Float
sampleDelta _ False = pure 0
sampleDelta lastFrameRef True = do
  now   <- getMonotonicTimeNSec
  mLast <- readIORef lastFrameRef
  writeIORef lastFrameRef (Just now)
  pure $ case mLast of
    Nothing   -> 0
    Just prev -> min 0.1 $ fromIntegral (now - prev) / 1.0e9

sixtyHzMicros :: Int
sixtyHzMicros = 16667

forkAnimationTicker :: IORef Bool -> IO () -> IO ()
forkAnimationTicker animActive notify = void $ forkIO tick
  where
    tick = do
      threadDelay sixtyHzMicros
      active <- readIORef animActive
      when active $ do
        notify
        tick
