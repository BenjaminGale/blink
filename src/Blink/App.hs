{- |
Module: Blink.App

= Application structure

'App' bundles everything Blink needs to run: the startup action that
produces the initial state, a function from state to 'Theme', the UI tree,
and the message handler.

@
data App e msg s = App
  { startUp :: IO s
  , theme   :: s -> Theme e
  , view    :: s -> UI e msg ()
  , update  :: msg -> Update s ()
  }
@

  * @e@ is the element type — a sum type identifying each interactive control
    (see "Blink.UI").
  * @msg@ is the type of messages the view emits (see "Blink.UI").
  * @s@ is the application state, owned by the host and passed into 'view'
    explicitly each frame. The view never mutates it directly; it queues
    @msg@ values with 'Blink.UI.emit', which @update@ folds into the state
    once the frame completes, in emission order.

= Configuration

Pass an 'App' to 'configureContinuous' or 'configureEventDriven' to obtain a
'BlinkHandle'. Choose based on how the backend's render loop is driven:

  * 'configureContinuous'  — for backends that redraw every frame regardless of
    input (e.g. game-style loops). Draw commands from the first render pass are
    submitted immediately each frame.
  * 'configureEventDriven' — for backends that block on events. After folding
    the frame's emitted messages into the state, a second render pass runs on
    the updated state so the displayed frame always reflects the latest
    state. The 'IO ()' callback is invoked when the animation ticker fires,
    allowing the backend to unblock its event wait (e.g. @glfwPostEmptyEvent@).

>  Continuous:                          Event-driven:
>
>  run view -----> submit draws         run view (pass 1)
>       (stale state may flash              |
>        briefly on the next frame          v
>        redraw instead)               fold emitted messages
>                                            |
>                                            v
>                                       run view again (pass 2)
>                                            |
>                                            v
>                                       submit draws
>                                       (never shows stale state)

= Backend integration

'BlinkHandle' is the interface the backend uses each frame. Drive the render
loop by calling 'stepFrame' each iteration with a 'FrameInput' assembled from
platform events:

@
loop handle = do
  waitForPlatformEvents
  input  <- collectFrameInput
  result <- stepFrame handle input
  case result of
    Continue draws _ -> render draws >> loop handle
    Quit     draws _ -> render draws
@

Draw commands are included in both 'Continue' and 'Quit' so the backend can
render the final frame before exiting.

= Quit flow

Set 'quitRequested' in 'FrameInput' when the platform detects a close signal
(e.g. the window's close button). 'stepFrame' returns 'Quit' on the same frame.

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
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void)
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Blink.Geometry (Point (..), Rectangle, Size (..), rectFromSize)
import Blink.Input (KeyEvent, InputState (..))
import Blink.Rendering (DrawCommand, TextMeasurer (..))
import Blink.Style (Theme)
import Blink.UI
  ( UI, UIContext
  , AnimationState (animElapsed)
  , mkAnimationState
  , emptyUIContext, nextFrameContext, rerenderContext
  , runUI, getDrawCommands, getMessages
  , contextAnimation, contextRequiresAnimation
  )
import Blink.Update (Update, runUpdate)

-- | Describes a complete Blink application.
--
-- @e@ is the element type, @msg@ the type of messages emitted by the view,
-- and @s@ the application state. See "Blink.UI" for an explanation of
-- element IDs and messages.
data App e msg s = App
  { startUp :: IO s
    -- ^ Produces the initial application state before the render loop begins.
  , theme :: s -> Theme e
    -- ^ Derives the active 'Theme' from the current state. Called each frame,
    -- allowing the theme to change in response to state changes.
  , view :: s -> UI e msg ()
    -- ^ The UI tree, given the application state as it was at the start of
    -- the frame. Queues messages with 'Blink.UI.emit'; run once or twice per
    -- frame depending on the render mode.
  , update :: msg -> Update s ()
    -- ^ Folds one message emitted by 'view' into the application state.
    -- Every message queued during a frame is applied in emission order.
  }

-- | Produces a 'BlinkHandle' for a continuous render backend. The draw list
-- from the first render pass is submitted immediately each frame.
configureContinuous :: Ord e => App e msg s -> TextMeasurer -> IO (BlinkHandle s)
configureContinuous app measurer = do
  s <- startUp app
  refs <- AppRefs
    <$> newIORef (emptyUIContext (rectFromSize (Size 0 0)) emptyInputState (theme app s) measurer)
    <*> newIORef s
    <*> newIORef False
    <*> newIORef Nothing
  pure BlinkHandle { stepFrame = doStepContinuous app refs }

-- | Produces a 'BlinkHandle' for an event-driven backend. The 'IO ()'
-- callback is called when the animation ticker fires so the backend can
-- unblock its event wait.
configureEventDriven :: Ord e => App e msg s -> IO () -> TextMeasurer -> IO (BlinkHandle s)
configureEventDriven app notify measurer = do
  s <- startUp app
  refs <- AppRefs
    <$> newIORef (emptyUIContext (rectFromSize (Size 0 0)) emptyInputState (theme app s) measurer)
    <*> newIORef s
    <*> newIORef False
    <*> newIORef Nothing
  pure BlinkHandle { stepFrame = doStepEventDriven app refs notify }

-- | The interface the backend uses each frame. Obtain via 'configureContinuous'
-- or 'configureEventDriven'.
data BlinkHandle s = BlinkHandle
  { stepFrame :: FrameInput -> IO (FrameResult s)
    -- ^ Processes one frame: runs the view, folds the frame's emitted
    -- messages into the state via @update@, and returns draw commands
    -- paired with the new state.
  }

-- | All per-frame inputs from the platform, assembled by the backend each
-- iteration before calling 'stepFrame'.
data FrameInput = FrameInput
  { mousePosition :: Point
    -- ^ Cursor position in window coordinates.
  , mouseButtonDown :: Bool
    -- ^ 'True' while the primary (left) mouse button is physically held.
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

-- | Mutable state carried between frames, allocated once at configure time
-- and threaded through every 'stepFrame' call via closure.
data AppRefs e msg s = AppRefs
  { refsCtx        :: IORef (UIContext e msg)
    -- The UIContext carried over from the previous frame.
  , refsState      :: IORef s
    -- The application state as of the end of the previous frame.
  , refsAnimActive :: IORef Bool
    -- Written at the end of each frame. The running ticker thread reads this
    -- to decide whether to continue looping or exit. A False->True edge
    -- causes a new ticker thread to be forked.
  , refsLastFrame  :: IORef (Maybe Word64)
    -- Monotonic nanosecond timestamp of the previous frame, used to compute
    -- the wall-clock delta. Only accessed inside 'runFrame', which is called
    -- sequentially, so no concurrent access concerns.
  }

buildCtx :: Ord e => App e msg s -> Rectangle -> InputState -> Float -> Bool -> s -> UIContext e msg -> UIContext e msg
buildCtx app winRect inputState delta isAnimTick state prevCtx =
  let elapsed   = animElapsed (contextAnimation prevCtx) + delta
      animState = mkAnimationState delta elapsed isAnimTick
  in nextFrameContext winRect inputState (theme app state) animState prevCtx

runFrame
  :: Ord e
  => App e msg s
  -> AppRefs e msg s
  -> FrameInput
  -> IO (UIContext e msg, s)
runFrame app refs input = do
  let winRect    = rectFromSize (windowSize input)
      inputState = toInputState input

  state <- readIORef (refsState refs)

  delta <- sampleDelta (refsLastFrame refs) (isAnimationTick input)

  prevCtx <- readIORef (refsCtx refs)
  let ctx = buildCtx app winRect inputState delta (isAnimationTick input) state prevCtx
  ((), ctx') <- runUI (view app state) ctx
  let state' = foldl' (\s msg -> runUpdate (update app msg) s) state (getMessages ctx')

  writeIORef (refsState refs) state'

  pure (ctx', state')

doStepContinuous :: Ord e => App e msg s -> AppRefs e msg s -> FrameInput -> IO (FrameResult s)
doStepContinuous app refs input = do
  (ctx', state') <- runFrame app refs input
  writeIORef (refsCtx refs) ctx'
  pure $ toResult input (getDrawCommands ctx') state'

doStepEventDriven :: Ord e => App e msg s -> AppRefs e msg s -> IO () -> FrameInput -> IO (FrameResult s)
doStepEventDriven app refs notify input = do
  (firstPassCtx, state') <- runFrame app refs input
  let winRect    = rectFromSize (windowSize input)
      inputState = toInputState input
      freshCtx   = rerenderContext winRect (clearKeyEvents inputState)
                     (theme app state') (contextAnimation firstPassCtx) firstPassCtx
  ((), renderedCtx) <- runUI (view app state') freshCtx
  writeIORef (refsCtx refs) renderedCtx
  wasActive <- readIORef (refsAnimActive refs)
  let nowActive = contextRequiresAnimation renderedCtx
  writeIORef (refsAnimActive refs) nowActive
  when (not wasActive && nowActive) $
    forkAnimationTicker (refsAnimActive refs) notify
  pure $ toResult input (getDrawCommands renderedCtx) state'

toResult :: FrameInput -> [DrawCommand] -> s -> FrameResult s
toResult input draws state
  | quitRequested input = Quit draws state
  | otherwise           = Continue draws state

emptyInputState :: InputState
emptyInputState = InputState
  { inputMousePosition  = Point 0 0
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

toInputState :: FrameInput -> InputState
toInputState fi = InputState
  { inputMousePosition  = mousePosition fi
  , inputLeftButtonDown = mouseButtonDown fi
  , inputKeyEvents      = keyEvents fi
  , inputTypedText      = typedText fi
  }

-- Clears keyboard and text events for the second render pass in event-driven mode.
clearKeyEvents :: InputState -> InputState
clearKeyEvents is = is { inputKeyEvents = [], inputTypedText = [] }

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
