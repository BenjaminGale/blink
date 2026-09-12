{- |
Module: Blink.App

= Application structure

'App' bundles everything Blink needs to run: the startup action that
produces the initial state, a function from state to 'Theme', the view tree,
and the message handler.

@
data App e msg s = App
  { startUp :: IO s
  , theme   :: s -> Theme e
  , view    :: s -> Element e msg
  , update  :: msg -> Update s msg ()
  }
@

  * @e@ is the element type — a sum type identifying each interactive control
    (see "Blink.View").
  * @msg@ is the type of messages the view emits (see "Blink.View").
  * @s@ is the application state, owned by the host and passed into 'view'
    explicitly each frame. The view never mutates it directly; it queues
    @msg@ values with 'Blink.View.emit', which @update@ folds into the state
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

= Commands

An 'Blink.Update.Update' handler can request a 'Cmd' -- an 'IO' action run
off the frame thread, whose result is folded back into the state as an
ordinary message on a later frame. Delivery goes through a 'MsgQueue': a
completing 'Cmd' calls 'enqueueMsg', and 'stepFrame' calls 'drainMsgs' once
per frame before folding messages into the state. Blink has no opinion on
the queue's underlying data structure or backpressure policy -- the backend
supplies one, typically built on "Control.Concurrent.STM"'s @TBQueue@.

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
    -- * Commands
  , MsgQueue (..)
    -- * Text measurement
  , TextMeasurer (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void)
import Data.IORef
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Blink.Cmd (Cmd, runCmd)
import Blink.Geometry (Point (..), Rectangle, Size (..), rectFromSize)
import Blink.Input (KeyEvent, InputState (..), advanceButton)
import Blink.View.Context (ctxMouse)
import Blink.Rendering (DrawCommand, TextMeasurer (..))
import Blink.Style (Theme)
import Blink.View
  ( ViewContext
  , AnimationState (animElapsed)
  , mkAnimationState
  , emptyViewContext, nextFrameContext, rerenderContext
  , runView, getDrawCommands, getMessages, hasPendingUiEffects
  , contextAnimation, contextRequiresAnimation
  )
import Blink.Element (Element, runElement)
import Blink.Update (Update, runUpdateCmds)

-- | Describes a complete Blink application.
--
-- @e@ is the element type, @msg@ the type of messages emitted by the view,
-- and @s@ the application state. See "Blink.View" for an explanation of
-- element IDs and messages.
data App e msg s = App
  { startUp :: IO s
    -- ^ Produces the initial application state before the render loop begins.
  , theme :: s -> Theme e
    -- ^ Derives the active 'Theme' from the current state. Called each frame,
    -- allowing the theme to change in response to state changes.
  , view :: s -> Element e msg
    -- ^ The view tree, given the application state as it was at the start of
    -- the frame. Queues messages with 'Blink.View.emit'; run once or twice per
    -- frame depending on the render mode.
  , update :: msg -> Update s msg ()
    -- ^ Folds one message emitted by 'view' into the application state.
    -- Every message queued during a frame is applied in emission order. May
    -- request a 'Cmd' via 'Blink.Update.cmd' -- see \"Commands\" above.
  }

-- | Delivery mechanism for messages produced by a 'Cmd' completing off the
-- frame thread. A completing 'Cmd' calls 'enqueueMsg'; 'stepFrame' calls
-- 'drainMsgs' once per frame, non-blocking, before folding messages into the
-- state. This is deliberately just an interface: Blink has no opinion on the
-- underlying data structure or its backpressure policy, so the backend
-- supplies one -- e.g. built on "Control.Concurrent.STM"'s @TBQueue@.
data MsgQueue msg = MsgQueue
  { enqueueMsg :: msg -> IO ()
    -- ^ Called once, off the frame thread, when a 'Cmd' completes.
  , drainMsgs :: IO [msg]
    -- ^ Called once per frame, on the frame thread. Must not block --
    -- return the messages available right now (empty if none).
  }

-- | Produces a 'BlinkHandle' for a continuous render backend. The draw list
-- from the first render pass is submitted immediately each frame.
configureContinuous :: Ord e => App e msg s -> MsgQueue msg -> TextMeasurer -> IO (BlinkHandle s)
configureContinuous app queue measurer = do
  s <- startUp app
  refs <- AppRefs
    <$> newIORef (emptyViewContext (rectFromSize (Size 0 0)) emptyInputState (theme app s) measurer)
    <*> newIORef s
    <*> newIORef False
    <*> newIORef Nothing
  pure BlinkHandle { stepFrame = doStepContinuous app refs queue }

-- | Produces a 'BlinkHandle' for an event-driven backend. The 'IO ()'
-- callback is called when the animation ticker fires, or when a 'Cmd'
-- completes, so the backend can unblock its event wait.
configureEventDriven :: Ord e => App e msg s -> MsgQueue msg -> IO () -> TextMeasurer -> IO (BlinkHandle s)
configureEventDriven app queue notify measurer = do
  s <- startUp app
  refs <- AppRefs
    <$> newIORef (emptyViewContext (rectFromSize (Size 0 0)) emptyInputState (theme app s) measurer)
    <*> newIORef s
    <*> newIORef False
    <*> newIORef Nothing
  pure BlinkHandle { stepFrame = doStepEventDriven app refs queue notify }

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
  , wheelDelta    :: Double
    -- ^ Vertical mouse wheel movement for this frame -- see
    -- 'Blink.Input.inputWheelDelta'.
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
  { refsCtx        :: IORef (ViewContext e msg)
    -- The ViewContext carried over from the previous frame.
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

buildCtx :: Ord e => App e msg s -> Rectangle -> InputState -> Float -> Bool -> s -> ViewContext e msg -> ViewContext e msg
buildCtx app winRect inputState delta isAnimTick state prevCtx =
  let elapsed   = animElapsed (contextAnimation prevCtx) + delta
      animState = mkAnimationState delta elapsed isAnimTick
  in nextFrameContext winRect inputState (theme app state) animState prevCtx

-- | Folds a batch of messages into state via @update@, in order, collecting
-- every 'Cmd' any of them requested along the way.
foldMsgs :: App e msg s -> s -> [msg] -> (s, [Cmd msg])
foldMsgs app = go []
  where
    go cs s []       = (s, cs)
    go cs s (m : ms) =
      let (s', cs') = runUpdateCmds (update app m) s
      in go (cs ++ cs') s' ms

-- | Forks each 'Cmd', posting its result to @queue@ and calling @notify@
-- once it completes -- the same wake-up path 'forkAnimationTicker' already
-- uses, so an event-driven backend blocked on its event wait unblocks as
-- soon as a result is ready. @notify@ is a no-op in continuous mode, which
-- will pick the result up on its own next frame regardless.
dispatchCmds :: MsgQueue msg -> IO () -> [Cmd msg] -> IO ()
dispatchCmds queue notify = mapM_ $ \c -> void $ forkIO $ do
  m <- runCmd c
  enqueueMsg queue m
  notify

runFrame
  :: Ord e
  => App e msg s
  -> AppRefs e msg s
  -> MsgQueue msg
  -> IO ()
  -> FrameInput
  -> IO (ViewContext e msg, s)
runFrame app refs queue notify input = do
  let winRect    = rectFromSize (windowSize input)
      inputState = toInputState input

  state <- readIORef (refsState refs)

  delta <- sampleDelta (refsLastFrame refs) (isAnimationTick input)

  prevCtx <- readIORef (refsCtx refs)
  let ctx = buildCtx app winRect inputState delta (isAnimationTick input) state prevCtx
  ((), ctx') <- runView (runElement (view app state)) ctx
  -- Cmd results that completed since the last frame are treated as having
  -- happened before this frame's own view emissions.
  pending <- drainMsgs queue
  let (state', cmds) = foldMsgs app state (pending ++ getMessages ctx')

  dispatchCmds queue notify cmds
  writeIORef (refsState refs) state'

  pure (ctx', state')

doStepContinuous :: Ord e => App e msg s -> AppRefs e msg s -> MsgQueue msg -> FrameInput -> IO (FrameResult s)
doStepContinuous app refs queue input = do
  (ctx', state') <- runFrame app refs queue (pure ()) input
  writeIORef (refsCtx refs) ctx'
  pure $ toResult input (getDrawCommands ctx') state'

doStepEventDriven :: Ord e => App e msg s -> AppRefs e msg s -> MsgQueue msg -> IO () -> FrameInput -> IO (FrameResult s)
doStepEventDriven app refs queue notify input = do
  (firstPassCtx, state1) <- runFrame app refs queue notify input
  (renderedCtx, state2) <-
    if null (getMessages firstPassCtx) && not (hasPendingUiEffects firstPassCtx)
      -- Nothing was queued, so nothing about the app or view state changed —
      -- a second pass would run the same view against the same state and
      -- input and produce byte-identical output. Reuse the first pass's
      -- context and draws instead of paying for a pointless re-render.
      then pure (firstPassCtx, state1)
      else do
        let winRect     = rectFromSize (windowSize input)
            inputState  = toInputState input
            rerendered  = rerenderContext winRect (clearKeyEvents inputState)
                            (theme app state1) (contextAnimation firstPassCtx) firstPassCtx
            freshCtx    = suppressFreshButtonEdge inputState rerendered
        (_, ctx2) <- runView (runElement (view app state1)) freshCtx
        -- A deferred effect settling on this second pass (e.g. a focus
        -- change taking effect) can itself emit messages that never appear
        -- anywhere else -- fold them into state too rather than silently
        -- dropping them. A third pass to re-render against the result is
        -- deliberately not done: re-running an already-settled deferred
        -- focus change through another 'rerenderContext' re-emits the same
        -- gained/lost messages again (verified against a real click-to-focus
        -- case), so looping here would re-deliver duplicates every further
        -- pass instead of converging. The accepted trade-off is that this
        -- one frame's draws (rendered against 'state1') can lag one frame
        -- behind whatever folding these messages changes in state -- the
        -- same one-frame staleness continuous mode already has, just
        -- reached from the second pass instead of the first.
        let (state2, cmds2) = foldMsgs app state1 (getMessages ctx2)
        dispatchCmds queue notify cmds2
        pure (ctx2, state2)
  writeIORef (refsCtx refs) renderedCtx
  writeIORef (refsState refs) state2
  wasActive <- readIORef (refsAnimActive refs)
  let nowActive = contextRequiresAnimation renderedCtx
  writeIORef (refsAnimActive refs) nowActive
  when (not wasActive && nowActive) $
    forkAnimationTicker (refsAnimActive refs) notify
  pure $ toResult input (getDrawCommands renderedCtx) state2

-- | Collapses a fresh button edge ('Blink.Input.ButtonDown', 'Blink.Input.ButtonReleased')
-- into its continuing counterpart ('Blink.Input.ButtonHeld', 'Blink.Input.ButtonUp') as
-- read against @input@, leaving whatever capture is currently assigned
-- unchanged. 'doStepEventDriven' uses this for its second, re-render pass:
-- the first pass already reacted to this input's fresh press/release once
-- (e.g. a click toggled a control); re-running the same view against the
-- literal same input a second time would see the identical fresh edge
-- again and react to it a second time -- e.g. a checkbox re-reads its own
-- (now-updated) selected state and toggles back. Collapsing the edge here
-- leaves level state (is the button currently down, is something still
-- captured) untouched, so drag/hover rendering is unaffected -- only the
-- "this is a fresh press/release" fact is suppressed for this rerender.
suppressFreshButtonEdge :: InputState -> ViewContext e msg -> ViewContext e msg
suppressFreshButtonEdge input ctx =
  ctx { ctxMouse = advanceButton isDown isDown (ctxMouse ctx) }
  where
    isDown = inputLeftButtonDown input

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
  , inputWheelDelta     = 0
  }

toInputState :: FrameInput -> InputState
toInputState fi = InputState
  { inputMousePosition  = mousePosition fi
  , inputLeftButtonDown = mouseButtonDown fi
  , inputKeyEvents      = keyEvents fi
  , inputTypedText      = typedText fi
  , inputWheelDelta     = wheelDelta fi
  }

-- Clears keyboard, text, and wheel events for the second render pass in
-- event-driven mode -- each is a discrete, one-frame occurrence (like a key
-- press) rather than level state (like a button held down), so re-running
-- the same input a second time must not re-apply it.
clearKeyEvents :: InputState -> InputState
clearKeyEvents is = is { inputKeyEvents = [], inputTypedText = [], inputWheelDelta = 0 }

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
