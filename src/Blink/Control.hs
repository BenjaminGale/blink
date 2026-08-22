{-# LANGUAGE MultiParamTypeClasses #-}
-- | The standard entry point for interactive controls: manages keyboard
-- focus, reports raw mouse\/keyboard\/focus events to the given attrs, and
-- draws styled chrome around the control's content.
--
-- Focus is claimed automatically when nothing else holds it (subject to
-- 'ccIsTabStop'), reaffirmed each frame while held, and given up on Tab. A
-- click (per 'FocusOnClick') or Shift-Tab can also hand focus to a
-- /different/ specific element; that takes effect the frame after it
-- happens rather than immediately, so whichever element is gaining or
-- losing focus reports it consistently regardless of render order -- see
-- 'Blink.Element.FocusGained'\/'Blink.Element.FocusLost'. A disabled
-- control neither claims focus nor reacts to clicks or Tab.
--
-- Scope is always root for now.
module Blink.Control
  ( control
  , getStyle
  ) where

import Control.Monad (forM_, guard, when)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromMaybe)

import Blink.Attributes
  ( Attr, fire
  , HasControlConfig (..), ControlConfig (..), FocusOnClick (..), autoClaimsFocus
  )
import Blink.Element (ElementEvent (..), HasElementEvent (..), element, onClicked)
import Blink.Geometry (Rectangle, insetRect, borderInsets)
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

-- | The control-specific hit area: the current bounds inset by the
-- element's margin -- the margin itself is never part of the control, so a
-- mouse position within it counts as "outside" for hovering, clicking, and
-- focus-claiming alike.
--
-- Uses the element's /normal/ margin, not the margin of whichever style
-- variant is currently active, since real themes don't vary margin by
-- state and 'getStyle' would otherwise depend on its own result.
marginInsetBounds :: Ord e => e -> UI e msg Rectangle
marginInsetBounds eid = do
  ss <- getStyleSet eid
  r  <- getBounds
  pure (insetRect (styleMargin (styleSetNormal ss)) r)

-- | 'True' when the mouse is over the element's background rectangle (see
-- @marginInsetBounds@) -- the control-specific hit area.
isMouseOver :: Ord e => e -> UI e msg Bool
isMouseOver eid = do
  hitBounds <- marginInsetBounds eid
  withBounds hitBounds isRegionHit

-- | 'True' when nothing else holds mouse capture, or this element itself
-- does (a drag in progress on this element doesn't count as contention).
isMouseFreeFor :: Eq e => e -> UI e msg Bool
isMouseFreeFor eid = do
  capturedByMe <- isDragging eid
  (|| capturedByMe) <$> isMouseFree

-- | 'True' when the element is hovered (per 'isMouseOver'), enabled, and
-- the mouse isn't contested by another element's drag.
isMouseTarget :: Ord e => e -> UI e msg Bool
isMouseTarget eid = do
  disabled    <- isDisabled
  hit         <- isMouseOver eid
  uncontested <- isMouseFreeFor eid
  pure (not disabled && hit && uncontested)

-- | 'True' when the element is hovered, enabled, not contested by another
-- element's drag, and the left button is currently held down.
isPressed :: Ord e => e -> UI e msg Bool
isPressed eid = do
  isTarget <- isMouseTarget eid
  down     <- isButtonDown
  pure (isTarget && down)

-- | Resolves the active 'Style' for an element given its current
-- interaction state. Priority: disabled > pressed > hovered > focused >
-- normal.
getStyle :: Ord e => e -> UI e msg Style
getStyle eid = do
  styles <- getStyleSet eid
  isDis  <- isDisabled
  isHov  <- isMouseOver eid
  isFoc  <- isFocused eid
  isPrs  <- isPressed eid
  let candidates =
        [ guard isDis $> styleSetDisabled styles
        , guard isPrs $> styleSetPressed  styles
        , guard isHov $> styleSetHovered  styles
        , guard isFoc $> styleSetFocused  styles
        ]
  pure $ fromMaybe (styleSetNormal styles) (asum candidates)

-- | Draws an element's background and border from the resolved style, then
-- runs @content@ clipped to the remaining space inside the padding.
styledElement :: Ord e => e -> UI e msg () -> UI e msg ()
styledElement eid content = do
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

-- | 'True' when this element should take focus with nothing having asked
-- for it: opted into auto-claiming (per 'autoClaimsFocus'), nothing else is
-- currently focused, and the mouse isn't contested by another element's
-- drag.
canAutoClaim :: (Ord e, HasControlConfig e cfg) => e -> cfg -> UI e msg Bool
canAutoClaim eid cfg = do
  nothingIsFocused <- isNothingFocused <$> getFocus
  uncontested      <- isMouseFreeFor eid
  pure (autoClaimsFocus (controlConfig cfg) && nothingIsFocused && uncontested)

-- | Manages this element's keyboard focus (see the module header), reports
-- its raw mouse\/keyboard\/focus events to @attrs@, and draws its chrome
-- around @content@. Hovering, clicking, and focus-claiming all respect the
-- same margin-inset hit area chrome resolution uses -- the margin itself
-- never counts as "on" the control.
control :: (Ord e, HasControlConfig e cfg, HasElementEvent ev) => e -> cfg -> [Attr e ev msg cfg] -> UI e msg () -> UI e msg ()
control eid cfg attrs content = disableWhen (not (ccEnabled cc)) $ do
  wasFocused <- isFocused eid
  applySelfFocus
  applyTabKeys wasFocused
  nowFocused <- isFocused eid
  fire attrs $ map liftElementEvent $ concat
    [ [FocusGained | not wasFocused && nowFocused]
    , [FocusLost   | wasFocused && not nowFocused]
    ]
  hitBounds <- marginInsetBounds eid
  withBounds hitBounds $ element eid (attrs ++ clickFocusReaction)
  when (ccIsTabStop cc) $ setPreviousTabStop eid
  styledElement eid content
  where
    cc = controlConfig cfg

    -- Immediate, not deferred: needed so that when several controls are
    -- simultaneously eligible, only the first one to render claims focus.
    applySelfFocus = whenEnabled $ do
      isRetaining <- isFocused eid
      auto        <- canAutoClaim eid cfg
      when (isRetaining || auto) $ setFocus eid

    -- Tab gives up focus immediately; Shift-Tab hands it to a specific
    -- other element one frame later, so that element reports its own
    -- FocusGained correctly regardless of where it renders.
    applyTabKeys wasFocused = whenEnabled $ do
      input    <- getInput
      prevCtrl <- getPreviousTabStop
      let tabKey          = find (\e -> key e == KeyTab) (inputKeyEvents input)
          tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
          shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
      when (wasFocused && tabPressed) $ do
        clearFocus
        consumeKey KeyTab
      when (wasFocused && shiftTabPressed) $
        forM_ prevCtrl $ \prev -> do
          requestFocus Nothing prev
          consumeKey KeyTab

    -- A click hands focus to whichever element FocusOnClick names, taking
    -- effect one frame later (see the module header).
    clickFocusReaction = case ccFocusOnClick cc of
      FocusSelf     -> [onClicked (\() -> [OutUi (Focus Nothing eid)])]
      FocusTarget t -> [onClicked (\() -> [OutUi (Focus Nothing t)])]
      NoFocus       -> []
