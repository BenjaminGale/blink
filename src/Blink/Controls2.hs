module Blink.Controls2
  ( Attr
  , configure
  , fire
  , onAny
  , configAny
  , ControlEvent (..)
  , HasControlEvent (..)
  , onFocusGained
  , onFocusLost
  , onMouseEnter
  , onMouseExit
  , FocusOnClick (..)
  , tabStop
  , focusOnClick
  , isMouseOver
  , renderChrome
  , measureChrome
  , applyMouseOver
  , applyFocus
  , control
  , isKeyPressed
  , whenFocused
  , isActivatedBy
  , activatable
  ) where

import Control.Monad (forM_, when)
import Data.List (find, foldl')
import Data.Maybe (isJust, isNothing)

import Blink.Geometry (Insets (..), borderInsets, insetRect, noBorder)
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Layout (Length (..))
import Blink.Style (Style (..))
import Blink.UI

data ControlEvent
  = FocusGained
  | FocusLost
  | MouseEntered
  | MouseExited
  deriving (Eq, Show)

class HasControlEvent ev where
  liftControl  :: ControlEvent -> ev
  matchControl :: ev -> Maybe ControlEvent

data FocusOnClick e
  = FocusSelf
  | FocusTarget e
  | NoFocus
  deriving (Eq, Show)

-- | Configuration shared by every control, regardless of that control's own
-- 'Config': whether Tab lands on it ('tabStop') and what clicking it does to
-- focus ('focusOnClick').
data ControlConfig e = ControlConfig
  { ccTabStop      :: Bool
  , ccFocusOnClick :: FocusOnClick e
  }

defaultControlConfig :: ControlConfig e
defaultControlConfig = ControlConfig { ccTabStop = True, ccFocusOnClick = FocusSelf }

data Attr e ev msg cfg
  = On (ev -> [Out e msg])
  | Config (cfg -> cfg)
  | Shared (ControlConfig e -> ControlConfig e)

configure :: cfg -> [Attr e ev msg cfg] -> cfg
configure = foldl' apply
  where
    apply cfg (Config f) = f cfg
    apply cfg _          = cfg

controlConfig :: [Attr e ev msg cfg] -> ControlConfig e
controlConfig = foldl' apply defaultControlConfig
  where
    apply cc (Shared f) = f cc
    apply cc _          = cc

fire :: [Attr e ev msg cfg] -> [ev] -> UI e msg ()
fire attrs evs = forM_ evs $ \ev -> forM_ handlers $ \h -> mapM_ dispatch (h ev)
  where
    handlers = [h | On h <- attrs]
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

onAny :: (ev -> [Out e msg]) -> Attr e ev msg cfg
onAny = On

configAny :: (cfg -> cfg) -> Attr e ev msg cfg
configAny = Config

onFocusGained :: HasControlEvent ev => msg -> Attr e ev msg cfg
onFocusGained msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just FocusGained]

onFocusLost :: HasControlEvent ev => msg -> Attr e ev msg cfg
onFocusLost msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just FocusLost]

onMouseEnter :: HasControlEvent ev => msg -> Attr e ev msg cfg
onMouseEnter msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just MouseEntered]

onMouseExit :: HasControlEvent ev => msg -> Attr e ev msg cfg
onMouseExit msg = onAny $ \ev -> [OutMsg msg | matchControl ev == Just MouseExited]

tabStop :: Bool -> Attr e ev msg cfg
tabStop b = Shared $ \cc -> cc { ccTabStop = b }

focusOnClick :: FocusOnClick e -> Attr e ev msg cfg
focusOnClick foc = Shared $ \cc -> cc { ccFocusOnClick = foc }

-- | 'True' when the mouse is over the element's background rectangle (bounds
-- inset by its margin) — the control-specific hit area. Built on 'isRegionHit';
-- a pure geometric test, so unlike the legacy single-owner hover field, many
-- elements can each independently be "over" in the same frame.
isMouseOver :: Ord e => e -> UI e msg Bool
isMouseOver eid = do
  s <- getStyle eid
  r <- getBounds
  withBounds (insetRect (styleMargin s) r) isRegionHit

-- | Registers the element as moused over and as hot (for drag continuation)
-- when the mouse is within its bounds, and fires 'onMouseEnter'\/'onMouseExit'
-- by comparing against last frame's result.
applyMouseOver :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg ()
applyMouseOver eid attrs = do
  wasOver  <- wasMouseOverLastFrame eid
  disabled <- isDisabled
  free     <- isMouseFree
  dragging <- isDragging eid
  hit      <- isMouseOver eid
  let isOver = not disabled && (free || dragging) && hit
  when isOver $ do
    registerMouseOver eid
    setHot eid
  fire attrs $ concat
    [ [liftControl MouseEntered | not wasOver && isOver]
    , [liftControl MouseExited  | wasOver && not isOver]
    ]

-- | Applies focus rules (take focus, hand off to a target, or leave it, per
-- 'focusOnClick') together with Tab\/Shift-Tab navigation (per 'tabStop'),
-- firing 'onFocusGained'\/'onFocusLost'. These are one primitive, not two,
-- because a Tab press's effect on this element's own focus must be visible
-- in the same before\/after bracket as the click\/retain logic for those
-- events to fire correctly — split into two separately-callable primitives,
-- a Tab-driven loss would go undetected: 'applyFocus' alone would fire
-- nothing (it returns before the loss happens), and by the time anything
-- calls it again, this same element's own auto-claim (nothing is focused,
-- so I'll take it) would have silently reclaimed focus first, masking the
-- transition entirely.
applyFocus :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg ()
applyFocus eid attrs = do
  wasFocused <- isFocused eid
  applyFocusRules
  applyTabKeys
  nowFocused <- isFocused eid
  fire attrs $ concat
    [ [liftControl FocusGained | not wasFocused && nowFocused]
    , [liftControl FocusLost   | wasFocused && not nowFocused]
    ]
  where
    cc = controlConfig attrs

    -- Takes focus on a click, hands it to a target, or leaves it, per 'focusOnClick'.
    applyFocusRules = whenEnabled $ do
      currentFocus <- getFocus
      isHit        <- isMouseOver eid
      released     <- isButtonReleased
      captured     <- getCapturedElement
      let nothingIsFocused = isNothing currentFocus
          isRetainingFocus = currentFocus == Just eid
          isDragRelease    = released && isJust captured && captured /= Just eid
          wasClicked       = isHit && released && not isDragRelease
          autoClaim        = case ccFocusOnClick cc of
            FocusSelf -> nothingIsFocused && not isDragRelease
            _         -> False
      if isRetainingFocus || autoClaim
        then setFocus eid
        else when (wasClicked && not isDragRelease) $ case ccFocusOnClick cc of
          FocusSelf     -> setFocus eid
          FocusTarget t -> setFocus t
          NoFocus       -> pure ()

    -- Handles Tab and Shift-Tab and registers the element as a tab stop,
    -- subject to 'tabStop'. An element with @tabStop False@ still gives up
    -- focus normally on Tab if it happens to hold it, but is skipped by
    -- Shift-Tab from whatever comes after it, since it never records itself
    -- as the previous tab stop.
    applyTabKeys = whenEnabled $ do
      hasFocus <- isFocused eid
      input    <- getInput
      prevCtrl <- getPreviousTabStop
      let tabKey          = find (\e -> key e == KeyTab) (inputKeyEvents input)
          tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
          shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
      when (hasFocus && tabPressed) $ do
        clearFocus
        consumeKey KeyTab
      when (hasFocus && shiftTabPressed) $
        forM_ prevCtrl $ \prev -> do
          setFocus prev
          consumeKey KeyTab
      when (ccTabStop cc) $ setPreviousTabStop eid

measureChrome :: Ord e => e -> UI e msg (Length, Length)
measureChrome eid = do
  style <- getStyle eid
  let m  = styleMargin style
      p  = stylePadding style
      be = case styleBorderColour style of
             Just _  -> borderInsets (styleBorderEdges style)
             Nothing -> borderInsets noBorder
      dw = leftInset m + rightInset m + leftInset be + rightInset be + leftInset p + rightInset p
      dh = topInset m  + bottomInset m  + topInset be + bottomInset be + topInset p  + bottomInset p
  pure (Exactly dw, Exactly dh)

renderChrome :: Ord e => e -> UI e msg () -> UI e msg ()
renderChrome eid content = do
  style <- getStyle eid
  r     <- getBounds
  let bg          = insetRect (styleMargin style) r
      borderRect  = case styleBorderColour style of
                      Just _  -> insetRect (borderInsets (styleBorderEdges style)) bg
                      Nothing -> bg
      contentRect = insetRect (stylePadding style) borderRect
      inner       = withBounds contentRect $ clipToCurrent content
  withBounds bg $
    withBackground (styleBackground style) $
    case styleBorderColour style of
      Just c  -> withBorder c (styleBorderEdges style) inner
      Nothing -> inner

-- | The standard entry point for interactive controls: applies mouse-over
-- (with hot/drag-continuation), focus and tab navigation, then renders
-- chrome around @content@.
control :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg () -> UI e msg ()
control eid attrs content = do
  applyMouseOver eid attrs
  applyFocus eid attrs
  renderChrome eid content

-- | 'True' when the element holds focus and a key event for @k@ is present
-- in the current frame's input queue.
isKeyPressed :: Eq e => e -> Key -> UI e msg Bool
isKeyPressed eid k = do
  hasFoc  <- isFocused eid
  pressed <- any (\e -> key e == k) . inputKeyEvents <$> getInput
  pure (hasFoc && pressed)

-- | Runs @action@ only when the given element holds keyboard focus.
whenFocused :: Eq e => e -> UI e msg () -> UI e msg ()
whenFocused eid action = isFocused eid >>= \f -> when f action

-- | 'True' when the element is a valid click target this frame: not
-- disabled, eligible for mouse-over (mouse free, or this element itself
-- holds capture), geometrically hit, and the button was just released.
-- Mirrors 'applyMouseOver's own gating, so a click can't activate a control
-- while a different control is mid-drag. Not exported: 'isMouseOver' is
-- geometric and has no registration state of its own to query directly the
-- way the legacy @isHovered@ did, so this is assembled fresh here rather
-- than reused from "Blink.UI".
isClickedOver :: Ord e => e -> UI e msg Bool
isClickedOver eid = do
  disabled <- isDisabled
  free     <- isMouseFree
  dragging <- isDragging eid
  hit      <- isMouseOver eid
  released <- isButtonReleased
  pure (not disabled && (free || dragging) && hit && released)

-- | 'True' when the element was clicked, or one of @keys@ was pressed while
-- it held focus, and it is not disabled.
isActivatedBy :: Ord e => e -> [Key] -> UI e msg Bool
isActivatedBy eid keys = do
  clicked  <- isClickedOver eid
  keyPress <- or <$> mapM (isKeyPressed eid) keys
  disabled <- isDisabled
  pure (not disabled && (clicked || keyPress))

-- | 'control' plus 'isActivatedBy': runs @draw@ as a normal interactive
-- control, then reports whether it was activated (a click or one of @keys@)
-- this frame.
activatable :: (Ord e, HasControlEvent ev) => e -> [Attr e ev msg cfg] -> [Key] -> UI e msg () -> UI e msg Bool
activatable eid attrs keys draw = do
  control eid attrs draw
  isActivatedBy eid keys
