{- |
Module: Blink.UI

= The UI monad

'UI' is the core abstraction in Blink: a state-threading computation
parameterised over an /element type/ @e@, a /UI state type/ @u@, and an
/application state type/ @s@.

@
newtype UI e u s a = UI { runUI :: UIContext e u s -> (a, UIContext e u s) }
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

= UI state

Some controls carry presentation state that is no business of the
application — a scrollbar's position, a text input's cursor index. This state
lives in a single user-supplied record of type @u@, stored in the 'UIContext'
and preserved across frames:

@
data MyUIState = MyUIState { sidebarScroll :: Double }
@

A control reads the record with 'getUIState' and writes updates back with
'modifyUIState'; the application never sees the traffic. Unlike application
state, UI state changes take effect immediately: later reads in the same
frame see the new value. Applications with no stateful controls use @()@.

The record may hold whatever the application's controls need, including maps
keyed by element ID for state shared by a family of controls. The standard
controls bundle their state in "Blink.Controls"' @StandardControls@ record,
which can serve as @u@ directly.

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
  * 'isKeyPressed' — the element is focused and a matching key event is present.

'regionHit' is the lower-level primitive: it checks whether the mouse is within
the /current bounds/, without reference to any element ID.

= Focus and keyboard navigation

At most one element holds keyboard focus at a time, tracked in 'FocusState'.

  * 'isFocused' \/ 'setFocus' \/ 'clearFocus' — query and update focus.
  * 'whenFocused' — run an action only when an element is focused.
  * 'consumeKey' — remove a key event from the frame's queue so that it is not
    handled by multiple controls in the same frame.

Tab and Shift-Tab navigation between controls is managed automatically by
'control'.

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

= Building controls

'control' is the high-level entry point for interactive controls: it applies
hover detection, focus management, tab navigation, and style-aware rendering in
one call. 'renderControl' provides the rendering half alone, for display-only
elements that should not participate in interaction.
-}
module Blink.UI
  ( -- * The UI monad
    UI (..)
  , FocusState (..)
  , UIContext (..)
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
    -- * UI state
  , getUIState
  , modifyUIState
    -- * Bounds
  , getBounds
  , withBounds
    -- * Drawing
  , fillRect
  , strokeRect
  , drawText
  , clipToCurrent
    -- * Interaction
  , getInput
  , getMousePos
  , getLeftButton
  , regionHit
  , isHovered
  , setHovered
  , isClicked
  , isPressed
  , isDragging
  , isActivatedBy
    -- * Focus and keyboard navigation
  , isFocused
  , setFocus
  , setFocusWhen
  , clearFocus
  , whenFocused
  , isKeyPressed
  , consumeKey
  , getPreviousTabStop
  , setPreviousTabStop
    -- * Styles
  , getStyleSet
  , getStyle
    -- * Disabled state
  , isDisabled
  , disableWhen
  , whenEnabled
    -- * Animation
  , AnimationState (..)
  , requiresAnimation
  , withAnimationFrame
  , getAnimDelta
    -- * Building controls
  , control
  , renderControl
  ) where

import Control.Monad (when, unless, guard)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find, foldl')
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), isOpaque, TextAlign (..), DrawCommand (..))
import Blink.Geometry (Point, Rectangle, containsPoint, insetRect)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))

-- | Per-frame animation state threaded through the 'UIContext'. Set by the
-- backend at the start of each frame; read by 'withAnimationFrame' and
-- 'getAnimDelta'.
data AnimationState = AnimationState
  { animDelta  :: Float
    -- ^ Wall-clock seconds elapsed since the previous frame, clamped to
    -- 100 ms. Zero on the first frame.
  , animIsTick :: Bool
    -- ^ 'True' when this frame was triggered by the animation ticker rather
    -- than a platform input event.
  }

-- | Tracks which element holds keyboard focus and whether it was visited
-- during the current frame's render pass.
data FocusState e = FocusState
  { focusedElement   :: Maybe e
    -- ^ The element that currently holds focus, or 'Nothing' if no element is focused.
  , focusedThisFrame :: Bool
    -- ^ 'True' if the focused element was encountered during this frame's render pass.
    -- Used to clear stale focus when a focused element is no longer present in the UI.
  }

-- | The frame context threaded through every 'UI' computation. Carries the
-- current bounds, input state, active theme, accumulated draw commands, focus
-- state, UI state, and the application state with its queued modifiers.
-- Construct with 'emptyUIContext' or 'nextFrameContext'; extract results with
-- 'getDrawCommands', 'applyDispatches', and 'getAsyncJobs'.
--
-- [@e@] Element identity type — identifies focusable\/hoverable controls.
-- [@u@] UI state type — holds per-control presentation state across frames.
-- [@s@] Application state type — read with 'getAppState', modified via
-- 'dispatch' and 'dispatchAsync'.
data UIContext e u s = UIContext
  { ctxBounds :: Rectangle
  , ctxInput :: InputState
  , ctxTheme :: Theme e
  , ctxDrawCommands :: [DrawCommand]
  , ctxHoveredElement :: Maybe e
  , ctxCapturedElement :: Maybe e
  , ctxFocusState :: FocusState e
  , ctxPreviousTabStop :: Maybe e
  , ctxUIState :: u
  , ctxAppState :: s
  , ctxDispatches :: [s -> s]
  , ctxAsyncJobs :: [s -> IO (s -> s)]
  , ctxDisabled :: Bool
  , ctxAnimation :: AnimationState
    -- ^ Per-frame animation state: wall-clock delta and tick flag. Set by the
    -- backend at the start of each frame via 'buildCtx'.
  , ctxRequiresAnimation :: Bool
    -- ^ Set to 'True' by any component calling 'requiresAnimation'. Read at
    -- the end of the frame to decide whether to keep the ticker active.
    -- Reset to 'False' at the start of each frame by 'nextFrameContext'.
  }

-- | The UI monad. A pure state-threading computation that reads from a
-- 'UIContext' and emits draw commands and application state modifiers as a
-- side effect. Use the 'Functor', 'Applicative', and 'Monad' instances to
-- compose UI trees. See 'control' and "Blink.Controls" for higher-level
-- building blocks.
--
-- [@e@] Element identity type.
-- [@u@] UI state type.
-- [@s@] Application state type.
-- [@a@] Result type.
newtype UI e u s a = UI { runUI :: UIContext e u s -> (a, UIContext e u s) }

instance Functor (UI e u s) where
  fmap f (UI g) = UI $ \ctx ->
    let (a, ctx') = g ctx
    in (f a, ctx')

instance Applicative (UI e u s) where
  pure a = UI $ \ctx -> (a, ctx)
  UI f <*> UI x = UI $ \ctx ->
    let (g, ctx') = f ctx
        (a, ctx'') = x ctx'
    in (g a, ctx'')

instance Monad (UI e u s) where
  return = pure
  UI x >>= f = UI $ \ctx ->
    let (a, ctx') = x ctx
        UI g = f a
    in g ctx'

-- | Constructs the initial 'UIContext' for the first frame.
emptyUIContext :: Rectangle -> InputState -> Theme e -> u -> s -> UIContext e u s
emptyUIContext bounds input thm uiState appState = UIContext
  { ctxBounds = bounds
  , ctxInput = input
  , ctxTheme = thm
  , ctxDrawCommands = []
  , ctxHoveredElement = Nothing
  , ctxCapturedElement = Nothing
  , ctxFocusState = FocusState { focusedElement = Nothing, focusedThisFrame = False }
  , ctxPreviousTabStop = Nothing
  , ctxUIState = uiState
  , ctxAppState = appState
  , ctxDispatches = []
  , ctxAsyncJobs = []
  , ctxDisabled = False
  , ctxAnimation = AnimationState { animDelta = 0, animIsTick = False }
  , ctxRequiresAnimation = False
  }

-- | Advances the context to the next frame. Resets per-frame state (draw
-- commands, hover element, queued dispatches and async jobs, and the
-- focus-visited flag) while preserving cross-frame state (theme, focus
-- element, UI state, application state, and tab-stop bookkeeping).
nextFrameContext :: Rectangle -> InputState -> UIContext e u s -> UIContext e u s
nextFrameContext bounds input ctx = ctx
  { ctxBounds = bounds
  , ctxInput = input
  , ctxDrawCommands = []
  , ctxHoveredElement = Nothing
  , ctxCapturedElement = nextCapture (inputLeftButton input) (ctxCapturedElement ctx)
  , ctxFocusState = nextFocusFrame (ctxFocusState ctx)
  , ctxDispatches = []
  , ctxAsyncJobs = []
  , ctxRequiresAnimation = False
  }

gets :: (UIContext e u s -> a) -> UI e u s a
gets f = UI $ \ctx -> (f ctx, ctx)

modify :: (UIContext e u s -> UIContext e u s) -> UI e u s ()
modify f = UI $ \ctx -> ((), f ctx)

-- | The UI state record @u@, as accumulated so far this frame.
getUIState :: UI e u s u
getUIState = gets ctxUIState

-- | Applies a function to the UI state record. Unlike 'dispatch', the change
-- takes effect immediately: later reads in the same frame see the new value.
modifyUIState :: (u -> u) -> UI e u s ()
modifyUIState f = modify $ \ctx -> ctx { ctxUIState = f (ctxUIState ctx) }

-- | The current layout rectangle. Set by the layout system via 'withBounds'.
getBounds :: UI e u s Rectangle
getBounds = gets ctxBounds

-- | The current mouse cursor position in window coordinates.
getMousePos :: UI e u s Point
getMousePos = inputMousePosition <$> getInput

-- | The current state of the primary (left) mouse button.
getLeftButton :: UI e u s ButtonState
getLeftButton = inputLeftButton <$> getInput

-- | The raw input state for the current frame.
getInput :: UI e u s InputState
getInput = gets ctxInput

-- | Removes all events for the given key from the current frame's key queue,
-- preventing other controls from handling the same keypress.
consumeKey :: Key -> UI e u s ()
consumeKey k = modify $ \ctx ->
  let input = ctxInput ctx
  in ctx { ctxInput = input { inputKeyEvents = filter (\e -> key e /= k) (inputKeyEvents input) } }

-- | The element that was the most recent tab stop before the current one,
-- used by 'control' to implement Shift-Tab navigation.
getPreviousTabStop :: UI e u s (Maybe e)
getPreviousTabStop = gets ctxPreviousTabStop

-- | Records the current element as the previous tab stop. Called automatically
-- by 'control'; call manually when building custom focusable controls.
setPreviousTabStop :: e -> UI e u s ()
setPreviousTabStop eid = modify $ \ctx -> ctx { ctxPreviousTabStop = Just eid }

getTheme :: UI e u s (Theme e)
getTheme = gets ctxTheme

-- | Returns all style variants for the given element. Falls back to the theme's
-- default style when no element-specific style is registered.
getStyleSet :: Ord e => e -> UI e u s StyleSet
getStyleSet eid = do
  t <- getTheme
  pure $ Map.findWithDefault (themeDefaultStyle t) eid (themeElementStyles t)

-- | Resolves the active 'Style' for an element given its current interaction
-- state. Priority: disabled > pressed > hovered > focused > normal.
getStyle :: Ord e => e -> UI e u s Style
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
  pure $ fromMaybe (styleSetNormal styles) (asum candidates)

-- | 'True' when the given element is the current hover target.
isHovered :: Eq e => e -> UI e u s Bool
isHovered eid = (== Just eid) <$> gets ctxHoveredElement

-- | 'True' when the element is hovered and the left button was just released.
isClicked :: Eq e => e -> UI e u s Bool
isClicked eid = do
  isHov <- isHovered eid
  btn   <- getLeftButton
  pure (isHov && btn == ButtonReleased)

-- | 'True' when the element is hovered and the left button is held down.
isPressed :: Eq e => e -> UI e u s Bool
isPressed eid = do
  isHov <- isHovered eid
  btn   <- getLeftButton
  pure (isHov && btn == ButtonDown)

-- | 'True' when the element is clicked or any of the given keys are pressed
-- while it is focused, and the element is not disabled. Use this to implement
-- the activation behaviour of interactive controls.
isActivatedBy :: (Eq e, Ord e) => [Key] -> e -> UI e u s Bool
isActivatedBy keys eid = do
  clicked  <- isClicked eid
  keyPress <- any id <$> mapM (isKeyPressed eid) keys
  disabled <- isDisabled
  pure (not disabled && (clicked || keyPress))

-- | Derives the next frame's captured element from the current button state.
-- Capture is carried forward while the button is held and survives through
-- ButtonReleased so that 'applyFocus' can distinguish a drag release (mouse
-- on a different element) from a plain click. Cleared on ButtonUp.
-- Acquisition — setting capture in the first place — happens in 'setHovered'.
nextCapture :: ButtonState -> Maybe e -> Maybe e
nextCapture ButtonDown    existing = existing
nextCapture ButtonReleased existing = existing
nextCapture _             _        = Nothing

-- | 'True' on every frame that the given element is being dragged — from the
-- initial press through to release.
isDragging :: Eq e => e -> UI e u s Bool
isDragging eid = (== Just eid) <$> gets ctxCapturedElement


-- | Runs an action only when the given element holds keyboard focus.
whenFocused :: Eq e => e -> UI e u s () -> UI e u s ()
whenFocused eid action = isFocused eid >>= \f -> when f action

-- | 'True' when the element holds focus and a key event for @k@ is present
-- in the current frame's input queue.
isKeyPressed :: Eq e => e -> Key -> UI e u s Bool
isKeyPressed eid k = do
  hasFoc <- isFocused eid
  pressed <- any (\e -> key e == k) . inputKeyEvents <$> getInput
  pure (hasFoc && pressed)

-- | Registers the element as the current hover target. Also acquires mouse
-- capture for it if the left button is currently down and nothing is captured
-- yet, making this the first point of capture for that press.
setHovered :: e -> UI e u s ()
setHovered eid = modify $ \ctx ->
  let ctx' = ctx { ctxHoveredElement = Just eid }
  in if inputLeftButton (ctxInput ctx) == ButtonDown && isNothing (ctxCapturedElement ctx)
     then ctx' { ctxCapturedElement = Just eid }
     else ctx'

getFocus :: UI e u s (Maybe e)
getFocus = gets (focusedElement . ctxFocusState)

-- | 'True' when the given element holds keyboard focus.
isFocused :: Eq e => e -> UI e u s Bool
isFocused eid = (== Just eid) <$> getFocus

-- | Transfers keyboard focus to the given element.
setFocus :: e -> UI e u s ()
setFocus eid = modify $ \ctx -> ctx { ctxFocusState = FocusState { focusedElement = Just eid, focusedThisFrame = True } }

-- | Transfers keyboard focus to the given element when the condition is 'True'.
setFocusWhen :: Bool -> e -> UI e u s ()
setFocusWhen b eid = when b (setFocus eid)

-- | Removes keyboard focus from all elements.
clearFocus :: UI e u s ()
clearFocus = modify $ \ctx -> ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = Nothing } }

-- | Advances a 'FocusState' to the next frame: carries focus forward if it
-- was explicitly set this frame, otherwise clears it. Used by 'nextFrameContext'.
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
withBounds :: Rectangle -> UI e u s a -> UI e u s a
withBounds r (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxBounds = r })
  in (a, ctx' { ctxBounds = ctxBounds ctx })

-- | 'True' when the current sub-tree has been marked disabled.
isDisabled :: UI e u s Bool
isDisabled = gets ctxDisabled

-- | Marks a sub-tree as disabled when the condition is 'True'. The flag is
-- restored to its previous value once the sub-tree completes.
disableWhen :: Bool -> UI e u s a -> UI e u s a
disableWhen True (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxDisabled = True })
  in (a, ctx' { ctxDisabled = ctxDisabled ctx })
disableWhen False action = action

draw :: DrawCommand -> UI e u s ()
draw cmd = modify $ \ctx -> ctx { ctxDrawCommands = cmd : ctxDrawCommands ctx }

-- | Fills the current bounds with a solid colour.
fillRect :: Colour -> UI e u s ()
fillRect colour = do
  r <- getBounds
  draw $ FillRect r colour

-- | Strokes the border of the current bounds with the given colour and line width.
strokeRect :: Colour -> Double -> UI e u s ()
strokeRect colour width = do
  r <- getBounds
  draw $ StrokeRect r colour width

-- | Renders text within the current bounds using the given colour and alignment.
drawText :: Colour -> TextAlign -> Text -> UI e u s ()
drawText colour align text = do
  r <- getBounds
  draw $ DrawText r text colour align

-- | Wraps a sub-tree in a clip region matching the current bounds. Draw
-- commands produced by the sub-tree that fall outside the region are discarded.
clipToCurrent :: UI e u s a -> UI e u s a
clipToCurrent action = do
  r <- getBounds
  draw $ PushClip r
  result <- action
  draw PopClip
  return result

-- | The application state as it was at the start of the frame. Modifiers
-- queued with 'dispatch' do not affect the value seen by later calls in the
-- same frame; changes become visible from the next frame onward.
getAppState :: UI e u s s
getAppState = gets ctxAppState

-- | Queues a modifier to be applied to the application state once the frame
-- completes. Modifiers are applied in dispatch order by 'applyDispatches'.
dispatch :: (s -> s) -> UI e u s ()
dispatch f = modify $ \ctx -> ctx { ctxDispatches = f : ctxDispatches ctx }

-- | Queues an asynchronous job. The host forks the job once the frame
-- completes, passing it the post-dispatch application state; the modifier the
-- job returns is applied to whatever state exists when it finishes.
dispatchAsync :: (s -> IO (s -> s)) -> UI e u s ()
dispatchAsync job = modify $ \ctx -> ctx { ctxAsyncJobs = job : ctxAsyncJobs ctx }

-- | Extracts the draw commands produced during the frame, in submission order.
getDrawCommands :: UIContext e u s -> [DrawCommand]
getDrawCommands = reverse . ctxDrawCommands

-- | Applies the modifiers queued with 'dispatch' during the frame to the
-- frame's application state, in dispatch order.
applyDispatches :: UIContext e u s -> s
applyDispatches ctx = foldl' (flip ($)) (ctxAppState ctx) (reverse (ctxDispatches ctx))

-- | Extracts the asynchronous jobs queued with 'dispatchAsync' during the
-- frame, in dispatch order.
getAsyncJobs :: UIContext e u s -> [s -> IO (s -> s)]
getAsyncJobs = reverse . ctxAsyncJobs

-- | 'True' when the mouse cursor is within the current bounds.
regionHit :: UI e u s Bool
regionHit = do
  r <- getBounds
  p <- getMousePos
  return $ containsPoint p r

-- | Skips its argument entirely when the current sub-tree is disabled.
whenEnabled :: UI e u s () -> UI e u s ()
whenEnabled ui = do
  isDisabl <- isDisabled
  unless isDisabl $ do
    ui

applyHover :: (Eq e, Ord e) => e -> UI e u s ()
applyHover eid = do
  whenEnabled $ do
    s <- getStyle eid
    r <- getBounds
    let bgRect = insetRect (styleMargin s) r
    isHit <- withBounds bgRect regionHit
    when isHit $ do
      setHovered eid

applyFocus :: (Eq e, Ord e) => e -> UI e u s ()
applyFocus eid = do
  whenEnabled $ do
    currentFocus <- getFocus
    isHit        <- isHovered eid
    btn          <- getLeftButton
    captured     <- gets ctxCapturedElement
    let nothingIsFocused  = isNothing currentFocus
        isRequestingFocus = currentFocus == Just eid
        -- A drag release is when the button is released over a different
        -- element than the one that was captured. Focus should not transfer
        -- in that case — the drag origin retains focus.
        isDragRelease = btn == ButtonReleased && isJust captured && captured /= Just eid
        wasClicked    = isHit && btn == ButtonReleased && not isDragRelease
    setFocusWhen ((nothingIsFocused || isRequestingFocus || wasClicked) && not isDragRelease) eid

applyTabNavigation :: (Eq e, Ord e) => e -> UI e u s ()
applyTabNavigation eid = do
  hasFocus <- isFocused eid
  input    <- getInput
  prevCtrl <- getPreviousTabStop
  let tabKey          = find (\e -> key e == KeyTab) (inputKeyEvents input)
      tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
      shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
  when (hasFocus && tabPressed) $ do
    clearFocus
    consumeKey KeyTab
  when (hasFocus && shiftTabPressed && isJust prevCtrl) $ do
    setFocus (fromJust prevCtrl)
    consumeKey KeyTab
  whenEnabled $ do
    setPreviousTabStop eid

-- | Signals that animation should continue running. Call unconditionally on
-- every frame from any component that needs animation, including frames not
-- triggered by the ticker. Keeps 'refsAnimActive' set so the ticker does not
-- go quiet while the component is visible.
requiresAnimation :: UI e u s ()
requiresAnimation = modify $ \ctx -> ctx { ctxRequiresAnimation = True }

-- | Runs @action@ only on frames triggered by the animation ticker. On frames
-- triggered by mouse movement, keyboard input, or other platform events, this
-- is a no-op. Pair with 'requiresAnimation' so the ticker keeps firing.
withAnimationFrame :: UI e u s () -> UI e u s ()
withAnimationFrame action = do
  isTick <- gets (animIsTick . ctxAnimation)
  when isTick action

-- | Wall-clock seconds elapsed since the previous frame, clamped to 100 ms.
-- Zero on the first frame. Use inside 'withAnimationFrame' to advance
-- animation state by the correct amount regardless of ticker jitter.
getAnimDelta :: UI e u s Float
getAnimDelta = gets (animDelta . ctxAnimation)

-- | Style-aware rendering for a control. Applies the element's margin, draws
-- its background and border, and runs @content@ within the padded content
-- rectangle. Does not perform hover detection, focus management, or tab
-- navigation — use this for display-only elements that should not participate
-- in interaction. See 'control' for the interactive counterpart.
renderControl :: Ord e => e -> UI e u s () -> UI e u s ()
renderControl eid content = do
  style <- getStyle eid
  r     <- getBounds
  let bgRect      = insetRect (styleMargin style) r
      contentRect = insetRect (stylePadding style) bgRect
  when (isOpaque (styleBackground style)) $ do
    withBounds bgRect $ do
      fillRect (styleBackground style)
  case styleBorderColour style of
    Just c  -> withBounds bgRect $ do
      strokeRect c (styleBorderWidth style)
    Nothing -> return ()
  withBounds contentRect $ clipToCurrent content

-- | The standard entry point for interactive controls. Applies hover detection,
-- focus management, and Tab\/Shift-Tab navigation, then delegates to
-- 'renderControl' for style-aware rendering. @content@ runs inside the padded
-- content rectangle.
--
-- @
-- control eid $ do
--   style <- getStyle eid
--   drawText (styleTextColour style) (styleTextAlign style) label
-- @
control :: (Eq e, Ord e) => e -> UI e u s () -> UI e u s ()
control eid content = do
  applyHover eid
  applyFocus eid
  applyTabNavigation eid
  renderControl eid content
