{-# LANGUAGE OverloadedStrings #-}
-- | A small DSL for driving a 'UI' action through a sequence of real,
-- simulated input frames instead of poking 'UIContext' fields directly.
--
-- A test provides a chunk of UI (a 'UI' action), a list of /setup/
-- 'Interaction's that simulate real input to reach a starting state, and a
-- list of /test/ 'Interaction's whose resulting messages, draw commands, and
-- final context are what the test actually asserts on.
--
-- Every 'Interaction' expands to one or more real 'InputState' frames,
-- driven through the same 'nextFrameContext' \/ 'runUI' machinery
-- production code uses — there is no shortcut path that skips a real frame
-- transition. A synthetic pre-condition that no real input sequence through
-- the action under test could produce on its own (e.g. "some other,
-- unrelated element already holds focus\/capture") is not something this
-- module tries to fake: compose a second real control into the action under
-- test instead, and reach that state with real 'Interaction's against it.
-- Precise scroll\/selection seeding, which is impractical to reach via a
-- pixel-accurate drag, goes through the already-public 'emitUi' \/
-- 'UiEffect' API in a preliminary 'runInteractions' call instead, e.g.
--
-- @
-- seeded <- runInteractions bounds (mkCtx noInput) (emitUi (ScrollTo TestControl 0.5)) [] []
-- result <- runInteractions bounds (resultContext seeded) (scrollBar ...) [] testInteractions
-- @
--
-- (a call with both interaction lists empty still runs @action@ once, so the
-- 'emitUi' above actually queues its effect, which the trailing settle then
-- applies).
module Blink.Interaction
  ( Interaction (..)
  , InteractionResult (..)
  , runInteractions
  ) where

import Data.Text (Text)
import Blink.Geometry (Point, Rectangle)
import Blink.Input (Key (..), Modifier (..), InputState (..), KeyEvent (..))
import Blink.Rendering (DrawCommand)
import Blink.UI

-- | One simulated real-input step. A list of these is expanded into one or
-- more raw 'InputState' frames, each driven through 'nextFrameContext' \/
-- 'runUI' in turn.
data Interaction
  = MoveTo Point
    -- ^ One frame: reposition the mouse, button state unchanged.
  | MouseDown Point
    -- ^ One frame: mouse at the given point, button held down.
  | MouseUp Point
    -- ^ One frame: mouse at the given point, button released.
  | Click
    -- ^ 'MouseDown' then 'MouseUp' at the current tracked position (2 frames).
  | ClickAt Point
    -- ^ 'MouseDown' then 'MouseUp' at the given point (2 frames).
  | DragTo Point
    -- ^ One frame: mouse at the given point, button forced down — continues
    -- a drag started by an earlier 'MouseDown'.
  | PressKey Key [Modifier]
    -- ^ One frame: a single 'KeyEvent', mouse\/button unchanged.
  | TypeText Text
    -- ^ One frame: the given text delivered as typed input, mouse\/button unchanged.
  | Tab
    -- ^ 'PressKey' 'KeyTab' with no modifiers.
  | ShiftTab
    -- ^ 'PressKey' 'KeyTab' with 'Shift' held.
  | Wait Int
    -- ^ N idle frames: input unchanged, no key events or typed text. Lets a
    -- deferred 'UiEffect' cross a real frame boundary, or simulates elapsed
    -- frames for hover\/mouse-over checks. @n <= 0@ behaves as @Wait 1@ — an
    -- 'Interaction' always advances at least one real frame.
  deriving (Eq, Show)

-- | The outcome of driving a 'UI' action through 'runInteractions'.
data InteractionResult e msg a = InteractionResult
  { resultValue    :: a
    -- ^ The value produced by the final test-phase frame.
  , resultContext  :: UIContext e msg
    -- ^ The context after the test phase, with pending 'UiEffect's applied.
  , resultMessages :: [msg]
    -- ^ Every message emitted during the test phase, across every simulated
    -- frame, in order — not just the final frame's, since 'FrameOutputs'
    -- resets each frame and a multi-frame interaction (e.g. a drag) can emit
    -- on more than one of them.
  , resultDraws    :: [DrawCommand]
    -- ^ Draw commands from the final test-phase frame.
  }

-- | Drives @action@ through @setup@ (messages discarded, only the resulting
-- context carried forward) and then @test@ (messages accumulated across
-- every frame, draws taken from the last), starting from @seed@ — a
-- 'UIContext' obtained from 'emptyUIContext' or a prior 'runInteractions'
-- call's 'resultContext'. @action@ is re-run once per simulated frame, the
-- same way a real host re-runs its view every frame; the test phase always
-- runs @action@ at least once, even when @test@ is @[]@, so its value\/draws
-- are always well defined and any effect it queues is captured. @bounds@ is
-- held fixed across every simulated frame.
runInteractions
  :: Ord e
  => Rectangle
  -> UIContext e msg
  -> UI e msg a
  -> [Interaction]
  -> [Interaction]
  -> IO (InteractionResult e msg a)
runInteractions bounds seed action setupIxns testIxns = do
  setupCtx <- runSetup seed setupIxns
  (a, msgs, draws, finalCtx) <- runTest setupCtx testFrames
  pure InteractionResult
    { resultValue    = a
    , resultContext  = settle finalCtx
    , resultMessages = msgs
    , resultDraws    = draws
    }
  where
    testFrames = if null testIxns then [Wait 1] else testIxns
    settle ctx = applyUiEffects (getUiEffects ctx) ctx

    step ctx frame = runUI action (nextFrameContext bounds frame (contextTheme ctx) (contextAnimation ctx) ctx)

    -- Setup: discard every frame's value/messages/draws, keep only the
    -- resulting context. Re-derives the "current" InputState from the
    -- running context before expanding each interaction, so a later
    -- interaction sees where an earlier one actually left the mouse/button.
    runSetup ctx []         = pure ctx
    runSetup ctx (ix : ixs) = do
      ctx' <- runFrames ctx (expand (contextInput ctx) ix)
      runSetup ctx' ixs
      where
        runFrames c []       = pure c
        runFrames c (f : fs) = do
          (_, c') <- step c f
          runFrames c' fs

    -- Test: same frame-by-frame advance as setup, but keeps the last frame's
    -- value/draws and accumulates messages across every frame of every
    -- interaction. 'ixs' is always non-empty here (guaranteed by 'testFrames'
    -- above), so the final frame's value is always produced.
    runTest ctx [ix]       = runFramesTest ctx (expand (contextInput ctx) ix)
    runTest ctx (ix : ixs) = do
      (_, msgs1, _, ctx') <- runFramesTest ctx (expand (contextInput ctx) ix)
      (a, msgs2, draws, ctxFinal) <- runTest ctx' ixs
      pure (a, msgs1 ++ msgs2, draws, ctxFinal)
    runTest _ [] = error "runInteractions: internal invariant violated — test interaction list was empty"

    runFramesTest ctx [f] = do
      (a, ctx') <- step ctx f
      pure (a, getMessages ctx', getDrawCommands ctx', ctx')
    runFramesTest ctx (f : fs) = do
      (_, ctx') <- step ctx f
      (a, msgs, draws, ctxFinal) <- runFramesTest ctx' fs
      pure (a, getMessages ctx' ++ msgs, draws, ctxFinal)
    runFramesTest _ [] = error "runInteractions: internal invariant violated — expand returned no frames"

-- | Expands one 'Interaction' into the raw 'InputState' frame(s) it needs,
-- given the last submitted 'InputState' (for carrying forward mouse
-- position\/button state — key events and typed text never carry forward).
-- Never returns an empty list.
expand :: InputState -> Interaction -> [InputState]
expand cur ix = case ix of
  MoveTo p     -> [bare { inputMousePosition = p }]
  MouseDown p  -> [bare { inputMousePosition = p, inputLeftButtonDown = True }]
  MouseUp p    -> [bare { inputMousePosition = p, inputLeftButtonDown = False }]
  Click        -> expand cur (ClickAt (inputMousePosition cur))
  ClickAt p    -> [bare { inputMousePosition = p, inputLeftButtonDown = True }
                  ,bare { inputMousePosition = p, inputLeftButtonDown = False }]
  DragTo p     -> [bare { inputMousePosition = p, inputLeftButtonDown = True }]
  PressKey k m -> [bare { inputKeyEvents = [KeyEvent k m] }]
  TypeText t   -> [bare { inputTypedText = [t] }]
  Tab          -> expand cur (PressKey KeyTab [])
  ShiftTab     -> expand cur (PressKey KeyTab [Shift])
  Wait n       -> replicate (max 1 n) bare
  where
    bare = cur { inputKeyEvents = [], inputTypedText = [] }
