{- |
Module: Blink.UI

= The UI monad

'UI' is the core abstraction in Blink: a state-threading computation
parameterised over an /element type/ @e@, a /UI state type/ @s@, and a
/command type/ @c@.

@
newtype UI e s c a = UI { runUI :: UIContext e s c -> (a, UIContext e s c) }
@

Composing 'UI' actions with '>>=', '>>' and 'mapM_' builds a UI tree. Each
node in the tree reads from the shared 'UIContext' (bounds, input, theme, focus
state) and may append draw commands or application commands to it.

= Element identity

Every interactive control is identified by a value of type @e@, typically a
sum type with one constructor per control:

@
data MyElem = OkButton | CancelButton | NameField
  deriving (Eq, Ord)
@

Element IDs are used to look up styles from the active 'Theme', to track hover
and press state within a frame, and to route keyboard events to the focused
control.

= Commands

Controls communicate with the application by /dispatching/ values of type @c@.
'dispatch' appends a command; 'getCommands' retrieves them in order after the
frame completes.

@
data MyCmd = Submit | Cancel | NameChanged Text
@

= UI state

Some controls carry presentation state that is no business of the
application's update function — a scrollbar's position, a text input's cursor
index. This state lives in a single user-supplied record of type @s@, stored
in the 'UIContext' and preserved across frames:

@
data MyUIState = MyUIState { sidebarScroll :: Double }
@

A control is granted access to its slice of the record through a 'Field' — a
first-class getter\/setter pair. The control reads the current value with
'useField' and writes updates back with 'setField'; the application never sees
the traffic. Applications with no stateful controls use @()@.

@
scrollBar = scrollBarBuilder (Field sidebarScroll (\\v s -> s { sidebarScroll = v }))
@

The record may hold whatever the application's controls need, including maps
keyed by element ID for state shared by a family of controls.

= The render loop

Each frame follows the same three steps:

  1. Build a fresh 'UIContext' with 'emptyUIContext' (first frame) or advance an
     existing one with 'nextFrameContext'.
  2. Run the UI tree via 'runUI'.
  3. Pass the resulting context to 'getDrawCommands' and 'getCommands' to obtain
     the renderer input and application events.

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
  , getCommands
    -- * UI state
  , Field (..)
  , useField
  , setField
  , modifyField
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
  , captureElement
  , isCapturedBy
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
    -- * Commands
  , dispatch
  , changeTheme
    -- * Building controls
  , control
  , renderControl
  ) where

import Control.Monad (when, unless, guard)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), isOpaque, TextAlign (..), DrawCommand (..))
import Blink.Geometry (Point, Rectangle, containsPoint, insetRect)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))

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
-- state, UI state, and queued application commands. Construct with
-- 'emptyUIContext' or 'nextFrameContext'; extract results with
-- 'getDrawCommands' and 'getCommands'.
--
-- [@e@] Element identity type — identifies focusable\/hoverable controls.
-- [@s@] UI state type — holds per-control presentation state across frames.
-- [@c@] Command type — values dispatched to the application via 'dispatch'.
data UIContext e s c = UIContext
  { ctxBounds :: Rectangle
  , ctxInput :: InputState
  , ctxTheme :: Theme e
  , ctxDrawCommands :: [DrawCommand]
  , ctxHoveredElement :: Maybe e
  , ctxCapturedElement :: Maybe e
  , ctxFocusState :: FocusState e
  , ctxPreviousTabStop :: Maybe e
  , ctxUIState :: s
  , ctxCommands :: [c]
  , ctxDisabled :: Bool
  , ctxThemeChangeRequested :: Bool
  }

-- | The UI monad. A pure state-threading computation that reads from a
-- 'UIContext' and emits draw commands and application commands as a side
-- effect. Use the 'Functor', 'Applicative', and 'Monad' instances to compose
-- UI trees. See 'control' and "Blink.Controls" for higher-level building blocks.
--
-- [@e@] Element identity type.
-- [@s@] UI state type.
-- [@c@] Command type.
-- [@a@] Result type.
newtype UI e s c a = UI { runUI :: UIContext e s c -> (a, UIContext e s c) }

instance Functor (UI e s c) where
  fmap f (UI g) = UI $ \ctx ->
    let (a, ctx') = g ctx
    in (f a, ctx')

instance Applicative (UI e s c) where
  pure a = UI $ \ctx -> (a, ctx)
  UI f <*> UI x = UI $ \ctx ->
    let (g, ctx') = f ctx
        (a, ctx'') = x ctx'
    in (g a, ctx'')

instance Monad (UI e s c) where
  return = pure
  UI x >>= f = UI $ \ctx ->
    let (a, ctx') = x ctx
        UI g = f a
    in g ctx'

-- | Constructs the initial 'UIContext' for the first frame.
emptyUIContext :: Rectangle -> InputState -> Theme e -> s -> UIContext e s c
emptyUIContext bounds input thm uiState = UIContext
  { ctxBounds = bounds
  , ctxInput = input
  , ctxTheme = thm
  , ctxDrawCommands = []
  , ctxHoveredElement = Nothing
  , ctxCapturedElement = Nothing
  , ctxFocusState = FocusState { focusedElement = Nothing, focusedThisFrame = False }
  , ctxPreviousTabStop = Nothing
  , ctxUIState = uiState
  , ctxCommands = []
  , ctxDisabled = False
  , ctxThemeChangeRequested = False
  }

-- | Advances the context to the next frame. Resets per-frame state (draw
-- commands, hover element, application commands, and the focus-visited flag)
-- while preserving cross-frame state (theme, focus element, UI state, and
-- tab-stop bookkeeping).
nextFrameContext :: Rectangle -> InputState -> UIContext e s c -> UIContext e s c
nextFrameContext bounds input ctx = ctx
  { ctxBounds           = bounds
  , ctxInput            = input
  , ctxDrawCommands     = []
  , ctxHoveredElement   = Nothing
  , ctxCapturedElement  = if inputLeftButton input == ButtonDown then ctxCapturedElement ctx else Nothing
  , ctxFocusState       = (ctxFocusState ctx) { focusedThisFrame = False }
  , ctxCommands             = []
  , ctxThemeChangeRequested = False
  }

gets :: (UIContext e s c -> a) -> UI e s c a
gets f = UI $ \ctx -> (f ctx, ctx)

modify :: (UIContext e s c -> UIContext e s c) -> UI e s c ()
modify f = UI $ \ctx -> ((), f ctx)

-- | A first-class record field: a getter\/setter pair granting a control
-- access to its slice of the user-supplied UI state record @s@. Stateful
-- controls take a 'Field' as their first argument so that applications can
-- configure them once by partial application:
--
-- @
-- scrollBar = scrollBarBuilder (Field sidebarScroll (\\v s -> s { sidebarScroll = v }))
-- @
--
-- [@s@] UI state record type.
-- [@a@] Type of the field being accessed.
data Field s a = Field
  { fieldGet :: s -> a
  , fieldSet :: a -> s -> s
  }

-- | Reads the field's current value from the UI state.
useField :: Field s a -> UI e s c a
useField f = gets (fieldGet f . ctxUIState)

-- | Writes a new value for the field into the UI state.
setField :: Field s a -> a -> UI e s c ()
setField f a = modify $ \ctx -> ctx { ctxUIState = fieldSet f a (ctxUIState ctx) }

-- | Applies a function to the field's current value.
modifyField :: Field s a -> (a -> a) -> UI e s c ()
modifyField f g = useField f >>= setField f . g

-- | The current layout rectangle. Set by the layout system via 'withBounds'.
getBounds :: UI e s c Rectangle
getBounds = gets ctxBounds

-- | The current mouse cursor position in window coordinates.
getMousePos :: UI e s c Point
getMousePos = inputMousePosition <$> getInput

-- | The current state of the primary (left) mouse button.
getLeftButton :: UI e s c ButtonState
getLeftButton = inputLeftButton <$> getInput

-- | The raw input state for the current frame.
getInput :: UI e s c InputState
getInput = gets ctxInput

-- | Removes all events for the given key from the current frame's key queue,
-- preventing other controls from handling the same keypress.
consumeKey :: Key -> UI e s c ()
consumeKey k = modify $ \ctx ->
  let input = ctxInput ctx
  in ctx { ctxInput = input { inputKeyEvents = filter (\e -> key e /= k) (inputKeyEvents input) } }

-- | The element that was the most recent tab stop before the current one,
-- used by 'control' to implement Shift-Tab navigation.
getPreviousTabStop :: UI e s c (Maybe e)
getPreviousTabStop = gets ctxPreviousTabStop

-- | Records the current element as the previous tab stop. Called automatically
-- by 'control'; call manually when building custom focusable controls.
setPreviousTabStop :: e -> UI e s c ()
setPreviousTabStop eid = modify $ \ctx -> ctx { ctxPreviousTabStop = Just eid }

getTheme :: UI e s c (Theme e)
getTheme = gets ctxTheme

-- | Returns all style variants for the given element. Falls back to the theme's
-- default style when no element-specific style is registered.
getStyleSet :: Ord e => e -> UI e s c StyleSet
getStyleSet eid = do
  t <- getTheme
  pure $ Map.findWithDefault (themeDefaultStyle t) eid (themeElementStyles t)

-- | Resolves the active 'Style' for an element given its current interaction
-- state. Priority: disabled > pressed > hovered > focused > normal.
getStyle :: Ord e => e -> UI e s c Style
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
isHovered :: Eq e => e -> UI e s c Bool
isHovered eid = (== Just eid) <$> gets ctxHoveredElement

-- | 'True' when the element is hovered and the left button was just released.
isClicked :: Eq e => e -> UI e s c Bool
isClicked eid = do
  isHov <- isHovered eid
  btn   <- getLeftButton
  pure (isHov && btn == ButtonReleased)

-- | 'True' when the element is hovered and the left button is held down.
isPressed :: Eq e => e -> UI e s c Bool
isPressed eid = do
  isHov <- isHovered eid
  btn   <- getLeftButton
  pure (isHov && btn == ButtonDown)

-- | Claims mouse capture for the given element. While captured, 'isCapturedBy'
-- returns 'True' even when the mouse moves outside the element's bounds.
-- Capture is released automatically when the left button is no longer held.
captureElement :: e -> UI e s c ()
captureElement eid = modify $ \ctx -> ctx { ctxCapturedElement = Just eid }

-- | 'True' when the given element holds mouse capture.
isCapturedBy :: Eq e => e -> UI e s c Bool
isCapturedBy eid = (== Just eid) <$> gets ctxCapturedElement

-- | Runs an action only when the given element holds keyboard focus.
whenFocused :: Eq e => e -> UI e s c () -> UI e s c ()
whenFocused eid action = isFocused eid >>= \f -> when f action

-- | 'True' when the element holds focus and a key event for @k@ is present
-- in the current frame's input queue.
isKeyPressed :: Eq e => e -> Key -> UI e s c Bool
isKeyPressed eid k = do
  hasFoc <- isFocused eid
  pressed <- any (\e -> key e == k) . inputKeyEvents <$> getInput
  pure (hasFoc && pressed)

-- | Registers the element as the current hover target. Typically called
-- automatically by 'control' after a 'regionHit' check.
setHovered :: e -> UI e s c ()
setHovered eid = modify $ \ctx -> ctx { ctxHoveredElement = Just eid }

getFocus :: UI e s c (Maybe e)
getFocus = gets (focusedElement . ctxFocusState)

-- | 'True' when the given element holds keyboard focus.
isFocused :: Eq e => e -> UI e s c Bool
isFocused eid = (== Just eid) <$> getFocus

-- | Transfers keyboard focus to the given element.
setFocus :: e -> UI e s c ()
setFocus eid = modify $ \ctx -> ctx { ctxFocusState = FocusState { focusedElement = Just eid, focusedThisFrame = True } }

-- | Transfers keyboard focus to the given element when the condition is 'True'.
setFocusWhen :: Bool -> e -> UI e s c ()
setFocusWhen b eid = when b (setFocus eid)

-- | Removes keyboard focus from all elements.
clearFocus :: UI e s c ()
clearFocus = modify $ \ctx -> ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = Nothing } }

-- | Runs a sub-tree within a different bounding rectangle. The previous bounds
-- are restored when the sub-tree completes. Used by the layout system to
-- assign each child its allocated slot.
withBounds :: Rectangle -> UI e s c a -> UI e s c a
withBounds r (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxBounds = r })
  in (a, ctx' { ctxBounds = ctxBounds ctx })

-- | 'True' when the current sub-tree has been marked disabled.
isDisabled :: UI e s c Bool
isDisabled = gets ctxDisabled

-- | Marks a sub-tree as disabled when the condition is 'True'. The flag is
-- restored to its previous value once the sub-tree completes.
disableWhen :: Bool -> UI e s c a -> UI e s c a
disableWhen True (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxDisabled = True })
  in (a, ctx' { ctxDisabled = ctxDisabled ctx })
disableWhen False action = action

draw :: DrawCommand -> UI e s c ()
draw cmd = modify $ \ctx -> ctx { ctxDrawCommands = cmd : ctxDrawCommands ctx }

-- | Fills the current bounds with a solid colour.
fillRect :: Colour -> UI e s c ()
fillRect colour = do
  r <- getBounds
  draw $ FillRect r colour

-- | Strokes the border of the current bounds with the given colour and line width.
strokeRect :: Colour -> Double -> UI e s c ()
strokeRect colour width = do
  r <- getBounds
  draw $ StrokeRect r colour width

-- | Renders text within the current bounds using the given colour and alignment.
drawText :: Colour -> TextAlign -> Text -> UI e s c ()
drawText colour align text = do
  r <- getBounds
  draw $ DrawText r text colour align

-- | Wraps a sub-tree in a clip region matching the current bounds. Draw
-- commands produced by the sub-tree that fall outside the region are discarded.
clipToCurrent :: UI e s c a -> UI e s c a
clipToCurrent action = do
  r <- getBounds
  draw $ PushClip r
  result <- action
  draw PopClip
  return result

-- | Appends a command to the frame's command queue. Retrieve with 'getCommands'
-- after the frame completes.
dispatch :: c -> UI e s c ()
dispatch cmd = modify $ \ctx -> ctx { ctxCommands = cmd : ctxCommands ctx }

-- | Signals that the application has requested a theme switch. The host checks
-- 'ctxThemeChangeRequested' after the frame and acts accordingly.
changeTheme :: UI e s c ()
changeTheme = modify $ \ctx -> ctx { ctxThemeChangeRequested = True }

-- | Extracts the draw commands produced during the frame, in submission order.
getDrawCommands :: UIContext e s c -> [DrawCommand]
getDrawCommands = reverse . ctxDrawCommands

-- | Extracts the application commands dispatched during the frame, in the order
-- they were dispatched.
getCommands :: UIContext e s c -> [c]
getCommands = reverse . ctxCommands

-- | 'True' when the mouse cursor is within the current bounds.
regionHit :: UI e s c Bool
regionHit = do
  r <- getBounds
  containsPoint r <$> getMousePos

-- | Skips its argument entirely when the current sub-tree is disabled.
whenEnabled :: UI e s c () -> UI e s c ()
whenEnabled ui = do
  isDisabl <- isDisabled
  unless isDisabl $ do
    ui

applyHover :: (Eq e, Ord e) => e -> UI e s c ()
applyHover eid = do
  whenEnabled $ do
    s <- getStyle eid
    r <- getBounds
    let bgRect = insetRect (styleMargin s) r
    isHit <- withBounds bgRect regionHit
    when isHit $ do
      setHovered eid

applyFocus :: (Eq e, Ord e) => e -> UI e s c ()
applyFocus eid = do
  whenEnabled $ do
    currentFocus <- getFocus
    isHit        <- isHovered eid
    btn          <- getLeftButton
    let nothingIsFocused  = isNothing currentFocus
        isRequestingFocus = currentFocus == Just eid
        wasClicked        = isHit && btn == ButtonReleased
    setFocusWhen (nothingIsFocused || isRequestingFocus || wasClicked) eid

applyTabNavigation :: (Eq e, Ord e) => e -> UI e s c ()
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

-- | Style-aware rendering for a control. Applies the element's margin, draws
-- its background and border, and runs @content@ within the padded content
-- rectangle. Does not perform hover detection, focus management, or tab
-- navigation — use this for display-only elements that should not participate
-- in interaction. See 'control' for the interactive counterpart.
renderControl :: Ord e => e -> UI e s c () -> UI e s c ()
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
control :: (Eq e, Ord e) => e -> UI e s c () -> UI e s c ()
control eid content = do
  applyHover eid
  applyFocus eid
  applyTabNavigation eid
  renderControl eid content
