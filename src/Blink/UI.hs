{- |
Module: Blink.UI

= The UI monad

'UI' is the core abstraction in Blink: a state-threading computation
parameterised over an /element type/ @e@, a /UI state type/ @u@, and an
/application state type/ @s@.

@
newtype UI e s a = UI { runUI :: UIContext e s -> (a, UIContext e s) }
@

Composing 'UI' actions with '>>=', '>>' and 'mapM_' builds a UI tree. Each
node in the tree reads from the shared 'UIContext' (bounds, input, theme, focus
state) and may append draw commands to it or queue application state changes.

= Element identity

Every interactive control is identified by a value of type @e@, typically a
sum type with one constructor per control:

@
data MyElem = OkButton | CancelButton | NameInput
  deriving (Eq, Ord)
@

Element IDs are used to look up styles from the active 'Theme', to track hover
and press state within a frame, and to route keyboard events to the focused
control.

= Application state

The application state @s@ lives in the context alongside the UI state.
'getAppState' reads the value the frame started with. Changes are queued
rather than applied immediately: 'dispatch' appends a modifier that the host
applies once the frame completes, so 'getAppState' later in the same frame
still sees the pre-dispatch value. 'dispatchAsync' queues an IO job that the
host forks after the frame; the modifier it returns is applied to whatever
state exists when the job completes.

= Scroll and selection state

Some controls carry presentation state that is no business of the application
— a scrollbar's position, a text input's cursor. This state is baked directly
into the 'UIContext' via 'ctxElements': scroll positions are stored in
'elmScrollStates' (a @Map e 'ScrollState'@) and selections in 'elmSelections'
(a @Map e ['Selection']@). Both maps are keyed by element ID, populate lazily
on first write, and persist across frames. Controls read and write them through
'getScrollState' \/ 'setScrollState' and 'getSelections' \/ 'setSelections';
the application never sees the traffic. Changes take effect immediately:
later reads in the same frame see the new value.

= The render loop

Each frame follows the same three steps:

  1. Build a fresh 'UIContext' with 'emptyUIContext' (first frame) or advance an
     existing one with 'nextFrameContext'.
  2. Run the UI tree via 'runUI'.
  3. Pass the resulting context to 'getDrawCommands' to obtain the renderer
     input, and to 'applyDispatches' and 'getAsyncJobs' to advance the
     application state.

= Drawing

'fillRect', 'strokeRect', and 'drawText' all operate on the /current bounds/
returned by 'getBounds'. 'withBounds' temporarily replaces the current bounds
for a sub-tree — used internally by the layout system. 'clipToCurrent' wraps a
sub-tree in a clip region matching the current bounds; drawing outside the
region is discarded.

= Interaction

Interaction queries are scoped to an element ID and are only meaningful after
the element has registered a hover hit via 'setHovered' (or the high-level
'control' helper, which does this automatically):

  * 'isHovered' — the mouse is inside the element's bounds.
  * 'isPressed' — the left button is held while the element is hovered.
  * 'isClicked' — the left button was just released while the element is hovered.

'regionHit' is the lower-level primitive: it checks whether the mouse is within
the /current bounds/, without reference to any element ID.

= Focus and keyboard navigation

At most one element holds keyboard focus at a time, tracked in 'FocusState'.

  * 'isFocused' \/ 'setFocus' \/ 'clearFocus' — query and update focus.
  * 'consumeKey' — remove a key event from the frame's queue so that it is not
    handled by multiple controls in the same frame.

Tab and Shift-Tab navigation between controls is managed automatically by
'control' in "Blink.Controls".

= Styles

'getStyle' resolves the active 'Style' for an element given its current
interaction state. Disabled takes priority over pressed, which takes priority
over hovered, which takes priority over focused; the normal style is the
fallback. 'getStyleSet' returns all states at once for cases where more than
one is needed simultaneously.

= Disabled state

'disableWhen' marks an entire sub-tree as disabled. Disabled controls render
normally but ignore all input. 'whenEnabled' is a guard that skips its body
when disabled.
-}
module Blink.UI
  ( -- * The UI monad
    UI (..)
  , FocusState (..)
  , UIContext (..)
  , InteractionState (..)
  , ElementState (..)
  , FrameOutputs (..)
    -- * Re-export for convenience
  , TextMeasurer (..)
  , noOpTextMeasurer
    -- * The render loop
  , emptyUIContext
  , nextFrameContext
  , getDrawCommands
  , applyDispatches
  , getAsyncJobs
    -- * Application state
  , getAppState
  , dispatch
  , dispatchAsync
    -- * Scroll state
  , ScrollState (..)
  , getScrollState
  , setScrollState
  , clampScrollPos
    -- * Selections
  , Selection (..)
  , getSelections
  , setSelections
  , getSelection
  , setSelection
  , selectionLow
  , selectionHigh
  , selectionHasExtent
  , cursor
  , collapseToLow
  , collapseToHigh
  , collapseToActive
  , extendActive
    -- * Bounds
  , getBounds
  , withBounds
    -- * Drawing
  , fillRect
  , strokeRect
  , drawText
  , clipToCurrent
  , withBackground
  , withBorder
    -- * Interaction
  , getInput
  , getMousePos
  , regionHit
  , isHovered
  , setHovered
  , isButtonDown
  , isButtonReleased
  , isClicked
  , isPressed
  , isDragging
  , isMouseFree
  , getCapturedElement
    -- * Focus and keyboard navigation
  , getFocus
  , isFocused
  , setFocus
  , setFocusWhen
  , clearFocus
  , consumeKey
  , getPreviousTabStop
  , setPreviousTabStop
    -- * Styles
  , getStyleSet
  , getStyle
    -- * Text measurement
  , charOffsetUI
  , charAtOffsetUI
    -- * Disabled state
  , isDisabled
  , disableWhen
  , whenEnabled
    -- * Animation
  , AnimationState (..)
  , requiresAnimation
  , withAnimationFrame
  , getAnimDelta
  , getAnimElapsed
  ) where

import Control.Monad (when, unless, guard)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (isNothing, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), isVisible, TextAlign (..), DrawCommand (..), TextMeasurer (..), noOpTextMeasurer)
import Blink.Geometry (Point, Rectangle, containsPoint, intersectRect)
import Blink.Input (Key (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))

-- | Per-frame animation state threaded through the 'UIContext'. Set by the
-- backend at the start of each frame; read by 'withAnimationFrame' and
-- 'getAnimDelta'.
data AnimationState = AnimationState
  { animDelta   :: Float
    -- ^ Wall-clock seconds elapsed since the previous frame, clamped to
    -- 100 ms. Zero on the first frame.
  , animElapsed :: Float
    -- ^ Total wall-clock seconds elapsed since the application started,
    -- accumulated from 'animDelta' each frame.
  , animIsTick  :: Bool
    -- ^ 'True' when this frame was triggered by the animation ticker rather
    -- than a platform input event.
  }

-- | Per-instance scroll position in @[0, 1]@.
newtype ScrollState = ScrollState { scrollPosition :: Double }
  deriving (Eq, Ord, Show)

-- | A contiguous selection or cursor within a linear sequence. The selected
-- range is @(min anchor active, max anchor active)@. When @anchor == active@
-- the selection is a cursor with no extent.
data Selection = Selection
  { selectionAnchor :: Int  -- ^ The fixed end.
  , selectionActive :: Int  -- ^ The moving end (cursor position).
  }
  deriving (Eq, Show)

-- | Tracks which element holds keyboard focus and whether it was visited
-- during the current frame's render pass.
data FocusState e = FocusState
  { focusedElement   :: Maybe e
    -- ^ The element that currently holds focus, or 'Nothing' if no element is focused.
  , focusedThisFrame :: Bool
    -- ^ 'True' if the focused element was encountered during this frame's render pass.
    -- Used to clear stale focus when a focused element is no longer present in the UI.
  }

-- | Per-frame interactive targeting state: which element the mouse is over,
-- which holds capture during a drag, which has keyboard focus, and which was
-- the most recent tab stop. Reset and carried forward by 'nextFrameContext'.
-- | Per-frame mouse button interaction state, derived by 'nextInteractionFrame'
-- from the previous and current raw button-down values. Four states arise from
-- the two-frame comparison:
--
-- @
-- prev  curr  ixnButtonDown  ixnButtonReleased  meaning
-- ----  ----  -------------  -----------------  -------
-- F     F     False          False              Up       — not held, no event
-- F     T     True           False              Pressed  — went down this frame
-- T     T     True           False              Down     — held, no event
-- T     F     False          True               Released — went up this frame
-- @
--
-- Controls read 'ixnButtonDown' for press state and 'ixnButtonReleased' for
-- click detection. Capture is held through Released so that drag-release can be
-- distinguished from a plain click.
data InteractionState e = InteractionState
  { ixnHovered         :: Maybe e
  , ixnCaptured        :: Maybe e
  , ixnFocus           :: FocusState e
  , ixnPrevTabStop     :: Maybe e
  , ixnButtonDown      :: Bool
    -- ^ 'True' when the left button is currently held (Pressed or Down state).
  , ixnButtonReleased  :: Bool
    -- ^ 'True' on the one frame the left button transitions from held to up.
  }

-- | Cross-frame per-element presentation state. Persists unchanged across
-- frames; never exposed to the application.
data ElementState e = ElementState
  { elmScrollStates :: Map.Map e ScrollState
  , elmSelections   :: Map.Map e [Selection]
  }

-- | Outputs accumulated during a single frame: draw commands, queued state
-- modifiers, async jobs, and the animation continuation flag. Reset to empty
-- at the start of each frame by 'nextFrameContext'.
data FrameOutputs s = FrameOutputs
  { outDrawCommands     :: [DrawCommand]
  , outDispatches       :: [s -> s]
  , outAsyncJobs        :: [s -> IO (s -> s)]
  , outRequiresAnimation :: Bool
  }

-- | The frame context threaded through every 'UI' computation. Carries the
-- current bounds, input state, active theme, accumulated draw commands, focus
-- state, scroll and selection state, and the application state with its
-- queued modifiers. Construct with 'emptyUIContext' or 'nextFrameContext';
-- extract results with 'getDrawCommands', 'applyDispatches', and 'getAsyncJobs'.
--
-- [@e@] Element identity type — identifies focusable\/hoverable controls.
-- [@s@] Application state type — read with 'getAppState', modified via
-- 'dispatch' and 'dispatchAsync'.
data UIContext e s = UIContext
  { ctxBounds          :: Rectangle
  , ctxInput           :: InputState
  , ctxTheme           :: Theme e
  , ctxAppState        :: s
  , ctxDisabled        :: Bool
  , ctxInteractionClip :: Maybe Rectangle
    -- ^ When set, 'regionHit' additionally requires the mouse to fall within
    -- this rectangle. Set by 'clipToCurrent' and restored on exit, so it
    -- tracks the innermost enclosing clip region.
  , ctxAnimation       :: AnimationState
    -- ^ Per-frame animation state: wall-clock delta and tick flag. Set by the
    -- backend at the start of each frame via 'buildCtx'.
  , ctxTextMeasure     :: TextMeasurer
    -- ^ Text measurement service supplied at configure time. Controls call
    -- 'charOffsetUI' and 'charAtOffsetUI' rather than accessing this directly.
  , ctxInteraction     :: InteractionState e
  , ctxElements        :: ElementState e
  , ctxOutputs         :: FrameOutputs s
  }

-- | The UI monad. A state-threading computation in 'IO' that reads from a
-- 'UIContext' and emits draw commands and application state modifiers as a
-- side effect. Use the 'Functor', 'Applicative', and 'Monad' instances to
-- compose UI trees. See 'control' and "Blink.Controls" for higher-level
-- building blocks.
--
-- [@e@] Element identity type.
-- [@s@] Application state type.
-- [@a@] Result type.
newtype UI e s a = UI { runUI :: UIContext e s -> IO (a, UIContext e s) }

instance Functor (UI e s) where
  fmap f (UI g) = UI $ \ctx -> do
    (a, ctx') <- g ctx
    pure (f a, ctx')

instance Applicative (UI e s) where
  pure a = UI $ \ctx -> pure (a, ctx)
  UI f <*> UI x = UI $ \ctx -> do
    (g, ctx')  <- f ctx
    (a, ctx'') <- x ctx'
    pure (g a, ctx'')

instance Monad (UI e s) where
  return = pure
  UI x >>= f = UI $ \ctx -> do
    (a, ctx') <- x ctx
    runUI (f a) ctx'

emptyInteractionState :: InteractionState e
emptyInteractionState = InteractionState
  { ixnHovered        = Nothing
  , ixnCaptured       = Nothing
  , ixnFocus          = FocusState { focusedElement = Nothing, focusedThisFrame = False }
  , ixnPrevTabStop    = Nothing
  , ixnButtonDown     = False
  , ixnButtonReleased = False
  }

emptyFrameOutputs :: FrameOutputs s
emptyFrameOutputs = FrameOutputs
  { outDrawCommands      = []
  , outDispatches        = []
  , outAsyncJobs         = []
  , outRequiresAnimation = False
  }

-- | Constructs the initial 'UIContext' for the first frame.
emptyUIContext :: Rectangle -> InputState -> Theme e -> s -> TextMeasurer -> UIContext e s
emptyUIContext bounds input thm appState measurer = UIContext
  { ctxBounds          = bounds
  , ctxInput           = input
  , ctxTheme           = thm
  , ctxAppState        = appState
  , ctxDisabled        = False
  , ctxInteractionClip = Nothing
  , ctxAnimation       = AnimationState { animDelta = 0, animElapsed = 0, animIsTick = False }
  , ctxTextMeasure     = measurer
  , ctxInteraction     = emptyInteractionState { ixnButtonDown = inputLeftButtonDown input }
  , ctxElements        = ElementState { elmScrollStates = Map.empty, elmSelections = Map.empty }
  , ctxOutputs         = emptyFrameOutputs
  }

-- | Advances the context to the next frame. Resets per-frame state (draw
-- commands, hover element, queued dispatches and async jobs, and the
-- focus-visited flag) while preserving cross-frame state (theme, focus
-- element, scroll state, selections, application state, and tab-stop bookkeeping).
nextFrameContext :: Rectangle -> InputState -> UIContext e s -> UIContext e s
nextFrameContext bounds input ctx = ctx
  { ctxBounds      = bounds
  , ctxInput       = input
  , ctxInteraction = nextInteractionFrame
      (inputLeftButtonDown (ctxInput ctx))
      (inputLeftButtonDown input)
      (ctxInteraction ctx)
  , ctxOutputs     = emptyFrameOutputs
  }

-- | Advances 'InteractionState' to the next frame: clears hover, derives
-- button transition state from the previous and current raw down values,
-- advances capture, and carries focus forward only if it was visited this frame.
-- The previous tab stop is preserved for Shift-Tab navigation.
nextInteractionFrame :: Bool -> Bool -> InteractionState e -> InteractionState e
nextInteractionFrame prevDown currDown ixn = ixn
  { ixnHovered        = Nothing
  , ixnButtonDown     = currDown
  , ixnButtonReleased = prevDown && not currDown
  , ixnCaptured       = nextCapture prevDown currDown (ixnCaptured ixn)
  , ixnFocus          = nextFocusFrame (ixnFocus ixn)
  }

gets :: (UIContext e s -> a) -> UI e s a
gets f = UI $ \ctx -> pure (f ctx, ctx)

modify :: (UIContext e s -> UIContext e s) -> UI e s ()
modify f = UI $ \ctx -> pure ((), f ctx)

modifyIxn :: (InteractionState e -> InteractionState e) -> UI e s ()
modifyIxn f = modify $ \ctx -> ctx { ctxInteraction = f (ctxInteraction ctx) }

modifyElm :: (ElementState e -> ElementState e) -> UI e s ()
modifyElm f = modify $ \ctx -> ctx { ctxElements = f (ctxElements ctx) }

modifyOut :: (FrameOutputs s -> FrameOutputs s) -> UI e s ()
modifyOut f = modify $ \ctx -> ctx { ctxOutputs = f (ctxOutputs ctx) }

-- | The current scroll position for the given element, in @[0, 1]@. Returns
-- @0@ when no position has been recorded yet.
getScrollState :: Ord e => e -> UI e s Double
getScrollState eid = gets $ \ctx ->
  scrollPosition (Map.findWithDefault (ScrollState 0) eid (elmScrollStates (ctxElements ctx)))

-- | Overwrites the scroll position for the given element. The change takes
-- effect immediately: later reads in the same frame see the new value.
setScrollState :: Ord e => e -> Double -> UI e s ()
setScrollState eid v = modifyElm $ \elm ->
  elm { elmScrollStates = Map.insert eid (ScrollState v) (elmScrollStates elm) }

-- | All selections for the given element. Returns @[]@ when none have been recorded.
getSelections :: Ord e => e -> UI e s [Selection]
getSelections eid = gets $ \ctx ->
  Map.findWithDefault [] eid (elmSelections (ctxElements ctx))

-- | Overwrites the selection list for the given element. The change takes
-- effect immediately.
setSelections :: Ord e => e -> [Selection] -> UI e s ()
setSelections eid ss = modifyElm $ \elm ->
  elm { elmSelections = Map.insert eid ss (elmSelections elm) }

-- | The first selection for the given element, or 'Nothing'.
getSelection :: Ord e => e -> UI e s (Maybe Selection)
getSelection eid = listToMaybe <$> getSelections eid

-- | Sets a single selection, replacing any existing selections for the element.
setSelection :: Ord e => e -> Selection -> UI e s ()
setSelection eid s = setSelections eid [s]

-- | The lower bound of the selected range: @min selectionAnchor selectionActive@.
selectionLow :: Selection -> Int
selectionLow s = min (selectionAnchor s) (selectionActive s)

-- | The upper bound of the selected range: @max selectionAnchor selectionActive@.
selectionHigh :: Selection -> Int
selectionHigh s = max (selectionAnchor s) (selectionActive s)

-- | 'True' when the selection has non-zero extent (anchor ≠ active).
selectionHasExtent :: Selection -> Bool
selectionHasExtent s = selectionAnchor s /= selectionActive s

-- | A cursor with no selection extent. Equivalent to @'Selection' n n@.
cursor :: Int -> Selection
cursor n = Selection n n

-- | Collapse the selection to a cursor at the lower bound.
collapseToLow :: Selection -> Selection
collapseToLow = cursor . selectionLow

-- | Collapse the selection to a cursor at the upper bound.
collapseToHigh :: Selection -> Selection
collapseToHigh = cursor . selectionHigh

-- | Collapse the selection to a cursor at the active (moving) end.
collapseToActive :: Selection -> Selection
collapseToActive = cursor . selectionActive

-- | Apply a function to the active end, keeping the anchor fixed.
extendActive :: (Int -> Int) -> Selection -> Selection
extendActive f s = s { selectionActive = f (selectionActive s) }

-- | Clamp a scroll position to @[0, 1]@.
clampScrollPos :: Double -> Double
clampScrollPos = max 0 . min 1

-- | The current layout rectangle. Set by the layout system via 'withBounds'.
getBounds :: UI e s Rectangle
getBounds = gets ctxBounds

-- | The current mouse cursor position in window coordinates.
getMousePos :: UI e s Point
getMousePos = inputMousePosition <$> getInput

-- | The raw input state for the current frame.
getInput :: UI e s InputState
getInput = gets ctxInput

-- | Removes all events for the given key from the current frame's key queue,
-- preventing other controls from handling the same keypress.
consumeKey :: Key -> UI e s ()
consumeKey k = modify $ \ctx ->
  let input = ctxInput ctx
  in ctx { ctxInput = input { inputKeyEvents = filter (\e -> key e /= k) (inputKeyEvents input) } }

-- | The element that was the most recent tab stop before the current one,
-- used by 'control' to implement Shift-Tab navigation.
getPreviousTabStop :: UI e s (Maybe e)
getPreviousTabStop = gets (ixnPrevTabStop . ctxInteraction)

-- | Records the current element as the previous tab stop. Called automatically
-- by 'control'; call manually when building custom focusable controls.
setPreviousTabStop :: e -> UI e s ()
setPreviousTabStop eid = modifyIxn $ \ixn -> ixn { ixnPrevTabStop = Just eid }

getTheme :: UI e s (Theme e)
getTheme = gets ctxTheme

-- | Returns all style variants for the given element. Falls back to the theme's
-- default style when no element-specific style is registered.
getStyleSet :: Ord e => e -> UI e s StyleSet
getStyleSet eid = do
  t <- getTheme
  return $ Map.findWithDefault (themeDefaultStyle t) eid (themeElementStyles t)

-- | Resolves the active 'Style' for an element given its current interaction
-- state. Priority: disabled > pressed > hovered > focused > normal.
getStyle :: Ord e => e -> UI e s Style
getStyle eid = do
  styles <- getStyleSet eid
  isDis  <- isDisabled
  isHov  <- isHovered eid
  isFoc  <- isFocused eid
  isPrs  <- isPressed eid
  let candidates =
        [ guard isDis $> styleSetDisabled styles
        , guard isPrs $> styleSetPressed  styles
        , guard isHov $> styleSetHovered  styles
        , guard isFoc $> styleSetFocused  styles
        ]
  return $ fromMaybe (styleSetNormal styles) (asum candidates)

-- | 'True' when the given element is the current hover target.
isHovered :: Eq e => e -> UI e s Bool
isHovered eid = (== Just eid) <$> gets (ixnHovered . ctxInteraction)

-- | 'True' when the left button is currently held (Pressed or Down state).
isButtonDown :: UI e s Bool
isButtonDown = gets (ixnButtonDown . ctxInteraction)

-- | 'True' on the one frame the left button transitions from held to up.
isButtonReleased :: UI e s Bool
isButtonReleased = gets (ixnButtonReleased . ctxInteraction)

-- | 'True' when the element is hovered and the left button was just released.
isClicked :: Eq e => e -> UI e s Bool
isClicked eid = do
  isHov     <- isHovered eid
  released  <- gets (ixnButtonReleased . ctxInteraction)
  return (isHov && released)

-- | 'True' when the element is hovered and the left button is held down.
isPressed :: Eq e => e -> UI e s Bool
isPressed eid = do
  isHov <- isHovered eid
  down  <- gets (ixnButtonDown . ctxInteraction)
  return (isHov && down)

-- | Derives the next frame's captured element from the button transition.
-- Capture is held while the button is down and through the release frame so
-- that 'applyFocus' can distinguish a drag release from a plain click.
-- Cleared once the button is fully up (both prev and curr false).
-- Acquisition — setting capture in the first place — happens in 'setHovered'.
nextCapture :: Bool -> Bool -> Maybe e -> Maybe e
nextCapture prevDown currDown existing
  | prevDown || currDown = existing
  | otherwise            = Nothing

-- | 'True' on every frame that the given element is being dragged — from the
-- initial press through to release.
isDragging :: Eq e => e -> UI e s Bool
isDragging eid = (== Just eid) <$> gets (ixnCaptured . ctxInteraction)

-- | The element that currently holds mouse capture, or 'Nothing' when no drag
-- is in progress. Exported for control authors that need to inspect capture
-- state directly, e.g. when implementing focus-on-click without using 'control'.
getCapturedElement :: UI e s (Maybe e)
getCapturedElement = gets (ixnCaptured . ctxInteraction)

-- | Registers the element as the current hover target. Also acquires mouse
-- capture for it if the left button is currently down and nothing is captured
-- yet, making this the first point of capture for that press.
setHovered :: e -> UI e s ()
setHovered eid = modify $ \ctx ->
  let ixn  = ctxInteraction ctx
      ixn' = ixn { ixnHovered = Just eid }
  in ctx { ctxInteraction =
       if ixnButtonDown ixn && isNothing (ixnCaptured ixn)
       then ixn' { ixnCaptured = Just eid }
       else ixn' }

getFocus :: UI e s (Maybe e)
getFocus = gets (focusedElement . ixnFocus . ctxInteraction)

-- | 'True' when the given element holds keyboard focus.
isFocused :: Eq e => e -> UI e s Bool
isFocused eid = (== Just eid) <$> getFocus

-- | Transfers keyboard focus to the given element.
setFocus :: e -> UI e s ()
setFocus eid = modifyIxn $ \ixn ->
  ixn { ixnFocus = FocusState { focusedElement = Just eid, focusedThisFrame = True } }

-- | Transfers keyboard focus to the given element when the condition is 'True'.
setFocusWhen :: Bool -> e -> UI e s ()
setFocusWhen b eid = when b (setFocus eid)

-- | Removes keyboard focus from all elements.
clearFocus :: UI e s ()
clearFocus = modifyIxn $ \ixn -> ixn { ixnFocus = (ixnFocus ixn) { focusedElement = Nothing } }

-- | Advances a 'FocusState' to the next frame: carries focus forward if it
-- was explicitly set this frame, otherwise clears it. Used by 'nextInteractionFrame'.
nextFocusFrame :: FocusState e -> FocusState e
nextFocusFrame fs = FocusState
  { focusedElement   = if focusedThisFrame fs
                       then focusedElement fs
                       else Nothing
  , focusedThisFrame = False
  }

-- | Runs a sub-tree within a different bounding rectangle. The previous bounds
-- are restored when the sub-tree completes. Used by the layout system to
-- assign each child its allocated slot.
withBounds :: Rectangle -> UI e s a -> UI e s a
withBounds r (UI f) = UI $ \ctx -> do
  (a, ctx') <- f (ctx { ctxBounds = r })
  pure (a, ctx' { ctxBounds = ctxBounds ctx })

-- | 'True' when the current sub-tree has been marked disabled.
isDisabled :: UI e s Bool
isDisabled = gets ctxDisabled

-- | Marks a sub-tree as disabled when the condition is 'True'. The flag is
-- restored to its previous value once the sub-tree completes.
disableWhen :: Bool -> UI e s a -> UI e s a
disableWhen True (UI f) = UI $ \ctx -> do
  (a, ctx') <- f (ctx { ctxDisabled = True })
  pure (a, ctx' { ctxDisabled = ctxDisabled ctx })
disableWhen False action = action

draw :: DrawCommand -> UI e s ()
draw cmd = modifyOut $ \out -> out { outDrawCommands = cmd : outDrawCommands out }

-- | Fills the current bounds with a solid colour.
fillRect :: Colour -> UI e s ()
fillRect colour = do
  r <- getBounds
  draw $ FillRect r colour

-- | Strokes the border of the current bounds with the given colour and line width.
strokeRect :: Colour -> Double -> UI e s ()
strokeRect colour width = do
  r <- getBounds
  draw $ StrokeRect r colour width

-- | Renders text within the current bounds using the given colour and alignment.
drawText :: Colour -> TextAlign -> Text -> UI e s ()
drawText colour align text = do
  r <- getBounds
  draw $ DrawText r text colour align

-- | Wraps a sub-tree in a clip region matching the current bounds. Draw
-- commands produced by the sub-tree that fall outside the region are discarded,
-- and mouse hit-testing is also restricted to the same region.
clipToCurrent :: UI e s a -> UI e s a
clipToCurrent (UI f) = UI $ \ctx -> do
  let r       = ctxBounds ctx
      newClip = maybe r (intersectRect r) (ctxInteractionClip ctx)
      ctx'    = ctx { ctxInteractionClip = Just newClip
                    , ctxOutputs = (ctxOutputs ctx)
                        { outDrawCommands = PushClip r : outDrawCommands (ctxOutputs ctx) } }
  (a, ctx'') <- f ctx'
  let ctx''' = ctx'' { ctxOutputs = (ctxOutputs ctx'')
                         { outDrawCommands = PopClip : outDrawCommands (ctxOutputs ctx'') }
                     , ctxInteractionClip = ctxInteractionClip ctx }
  pure (a, ctx''')

-- | Fills the current bounds with @colour@ then runs @content@ on top.
-- Skips the fill when @colour@ is fully transparent.
withBackground :: Colour -> UI e s a -> UI e s a
withBackground colour content = do
  when (isVisible colour) $ fillRect colour
  content

-- | Runs @content@, then strokes a border around the current bounds on top.
-- Drawing the border after content ensures it is always visible over children.
withBorder :: Colour -> Double -> UI e s a -> UI e s a
withBorder colour width content = do
  result <- content
  strokeRect colour width
  pure result

-- | The application state as it was at the start of the frame. Modifiers
-- queued with 'dispatch' do not affect the value seen by later calls in the
-- same frame; changes become visible from the next frame onward.
getAppState :: UI e s s
getAppState = gets ctxAppState

-- | Queues a modifier to be applied to the application state once the frame
-- completes. Modifiers are applied in dispatch order by 'applyDispatches'.
dispatch :: (s -> s) -> UI e s ()
dispatch f = modifyOut $ \out -> out { outDispatches = f : outDispatches out }

-- | Queues an asynchronous job. The host forks the job once the frame
-- completes, passing it the post-dispatch application state; the modifier the
-- job returns is applied to whatever state exists when it finishes.
dispatchAsync :: (s -> IO (s -> s)) -> UI e s ()
dispatchAsync job = modifyOut $ \out -> out { outAsyncJobs = job : outAsyncJobs out }

-- | Extracts the draw commands produced during the frame, in submission order.
getDrawCommands :: UIContext e s -> [DrawCommand]
getDrawCommands = reverse . outDrawCommands . ctxOutputs

-- | Applies the modifiers queued with 'dispatch' during the frame to the
-- frame's application state, in dispatch order.
applyDispatches :: UIContext e s -> s
applyDispatches ctx =
  foldl' (flip ($)) (ctxAppState ctx) (reverse (outDispatches (ctxOutputs ctx)))

-- | Extracts the asynchronous jobs queued with 'dispatchAsync' during the
-- frame, in dispatch order.
getAsyncJobs :: UIContext e s -> [s -> IO (s -> s)]
getAsyncJobs = reverse . outAsyncJobs . ctxOutputs

-- | 'True' when the mouse cursor is within the current bounds and within the
-- active interaction clip region (set by 'clipToCurrent').
regionHit :: UI e s Bool
regionHit = do
  r    <- getBounds
  p    <- getMousePos
  clip <- gets ctxInteractionClip
  return $ containsPoint p r && maybe True (containsPoint p) clip

-- | Skips its argument entirely when the current sub-tree is disabled.
whenEnabled :: UI e s () -> UI e s ()
whenEnabled ui = do
  disabled <- isDisabled
  unless disabled ui

-- | 'True' when no element currently holds mouse capture — i.e. no drag is
-- in progress. Use alongside 'isDragging' to decide whether a control should
-- respond to hover: @free || dragging@ allows hover when the mouse is
-- uncontested or when this element itself owns the capture.
isMouseFree :: UI e s Bool
isMouseFree = isNothing <$> gets (ixnCaptured . ctxInteraction)

-- | Signals that animation should continue running. Call unconditionally on
-- every frame from any component that needs animation, including frames not
-- triggered by the ticker. Keeps 'refsAnimActive' set so the ticker does not
-- go quiet while the component is visible.
requiresAnimation :: UI e s ()
requiresAnimation = modifyOut $ \out -> out { outRequiresAnimation = True }

-- | Runs @action@ only on frames triggered by the animation ticker. On frames
-- triggered by mouse movement, keyboard input, or other platform events, this
-- is a no-op. Pair with 'requiresAnimation' so the ticker keeps firing.
withAnimationFrame :: UI e s () -> UI e s ()
withAnimationFrame action = do
  isTick <- gets (animIsTick . ctxAnimation)
  when isTick action

-- | Wall-clock seconds elapsed since the previous frame, clamped to 100 ms.
-- Zero on the first frame. Use inside 'withAnimationFrame' to advance
-- animation state by the correct amount regardless of ticker jitter.
getAnimDelta :: UI e s Float
getAnimDelta = gets (animDelta . ctxAnimation)

-- | Total wall-clock seconds elapsed since the application started.
-- Derived by accumulating 'animDelta' each frame; use this to compute
-- animation phase without storing per-component state.
getAnimElapsed :: UI e s Float
getAnimElapsed = gets (animElapsed . ctxAnimation)

-- | Returns the x offset (pixels) of character index @n@ from the start of
-- @text@, using the backend's text measurer.
charOffsetUI :: Text -> Int -> UI e s Float
charOffsetUI text n = UI $ \ctx -> do
  v <- tmCharOffset (ctxTextMeasure ctx) text n
  pure (v, ctx)

-- | Returns the character index closest to x offset @x@ in @text@, using the
-- backend's text measurer.
charAtOffsetUI :: Text -> Float -> UI e s Int
charAtOffsetUI text x = UI $ \ctx -> do
  v <- tmCharAtOffset (ctxTextMeasure ctx) text x
  pure (v, ctx)
