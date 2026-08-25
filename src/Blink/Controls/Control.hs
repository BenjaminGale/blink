{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The upper of the two building-block layers every widget in
-- "Blink.Controls" is built from -- see "Blink.Controls.Element" for the
-- one underneath, and its own module header for the attribute mechanism
-- both layers share.
--
-- A control ('controlBase') wraps an element with focus management (claim
-- on render while nothing else holds it, give up on Tab, hand focus
-- elsewhere on Shift-Tab or a click, per 'FocusOnClick') and themed chrome
-- (background, border, padding, resolved from a 'Blink.Style.StyleKey' and
-- the element's own hover\/press\/focus state) around whatever content its
-- 'ControlConfig' carries. Per "a layer fires only what it originates",
-- 'controlBase' never dispatches an element event itself -- the one
-- side-effect it applies off a click (moving focus, per 'FocusOnClick') is
-- a direct 'UiEffect', not a handler call.
module Blink.Controls.Control
  ( -- * Re-exported from Blink.Controls.Element
    Attr (..)
  , resolve
  , EventHandler
  , KeyEventHandler
  , ElementConfig (..)
  , ElementInteraction (..)
  , HasElementConfig (..)
  , defaultElementConfig
  , elementBase
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  , runHandlers

    -- * Control
  , FocusOnClick (..)
  , ControlConfig (..)
  , ControlInteraction (..)
  , HasControlConfig (..)
  , defaultControlConfig
  , controlBase

    -- ** Control attributes
  , isFocusable
  , isEnabled
  , style
  , StyleKey (..)
  ) where

import Control.Monad (forM_, guard, when)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromMaybe)

import Blink.Controls.Element
import Blink.Geometry (Rectangle, borderInsets, insetRect)
import Blink.Input (InputState (..), Key, KeyEvent (..), Modifier)
import Blink.Style (Style (..), StyleKey (..), StyleSet (..))
import Blink.UI

-- * Control

-- | What clicking a control does to focus -- set by record update on
-- 'ccFocusOnClick'.
data FocusOnClick e
  = FocusSelf
    -- ^ The control takes focus itself (the default for interactive controls).
  | FocusTarget e
    -- ^ The control hands focus to a different element instead of taking it
    -- itself -- e.g. a label redirecting focus onto its target.
  | NoFocus
    -- ^ Clicking the control has no effect on focus at all.
  deriving (Eq, Show)

-- | Every capability a control resolves before rendering: whether it's
-- focusable and enabled, its style, what clicking it does to focus, its
-- content, and the element event handlers wrapped up inside it.
data ControlConfig e msg = ControlConfig
  { ccIsFocusable  :: Bool
  , ccIsEnabled    :: Bool
  , ccStyleKey     :: StyleKey e
  , ccFocusOnClick :: FocusOnClick e
  , ccContent      :: UI e msg ()
  , ccEvents       :: ElementConfig e msg
  }

-- | Focusable, enabled, styled via an arbitrary placeholder key (always
-- overridden -- every real caller of 'controlBase' supplies its own via
-- 'style'), taking focus itself on click, rendering nothing, and with no
-- event handlers registered.
defaultControlConfig :: ControlConfig e msg
defaultControlConfig = ControlConfig
  { ccIsFocusable  = True
  , ccIsEnabled    = True
  , ccStyleKey     = Class ""
  , ccFocusOnClick = FocusSelf
  , ccContent      = pure ()
  , ccEvents       = defaultElementConfig
  }

-- | What 'controlBase' reports back: the wrapped element's own
-- 'ElementInteraction', and the 'Style' it resolved and drew with this
-- frame.
data ControlInteraction e msg = ControlInteraction
  { ciElement :: ElementInteraction
  , ciStyle   :: Style
  }

-- | Implemented by any config type that nests a 'ControlConfig', letting a
-- control attribute (e.g. 'isFocusable') be applied to it directly. Also
-- gives every such config an 'HasElementConfig' instance for free, one hop
-- further in through 'ccEvents'.
class HasControlConfig e msg cfg | cfg -> e msg where
  overControl :: Attr (ControlConfig e msg) -> Attr cfg

instance HasControlConfig e msg (ControlConfig e msg) where
  overControl = id

instance HasElementConfig e msg (ControlConfig e msg) where
  overElement (Attr f) = Attr (\cc -> cc { ccEvents = f (ccEvents cc) })

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both.
isFocusable :: HasControlConfig e msg cfg => Bool -> Attr cfg
isFocusable b = overControl (Attr (\cc -> cc { ccIsFocusable = b }))

-- | Whether the control responds to input at all. A disabled control still
-- renders (in its disabled style) but ignores hover, clicks, key presses,
-- and focus, and is skipped by Tab\/Shift-Tab. Defaults to 'True'.
isEnabled :: HasControlConfig e msg cfg => Bool -> Attr cfg
isEnabled b = overControl (Attr (\cc -> cc { ccIsEnabled = b }))

-- | Which 'StyleKey' this control resolves its style from. Defaults to a
-- 'Class' named after the control; pass 'ElementId' to theme this one
-- instance differently, or a different 'Class' to group it with others.
style :: HasControlConfig e msg cfg => StyleKey e -> Attr cfg
style k = overControl (Attr (\cc -> cc { ccStyleKey = k }))

-- | Which way, if any, focus just moved, for 'controlBase's own immediate
-- self-claim\/self-give-up notifications -- distinct from the deferred
-- focus handoffs 'elementBase' itself detects via 'getFocusChange'.
data FocusTransition = FocusUnchanged | GainedFocus | LostFocus

focusTransition :: Bool -> Bool -> FocusTransition
focusTransition was now
  | not was && now = GainedFocus
  | was && not now  = LostFocus
  | otherwise       = FocusUnchanged

-- | Fires 'ecOnFocusGained'\/'ecOnFocusLost' directly from a given
-- 'FocusTransition', immediately rather than through 'elementBase's own
-- deferred detection -- for a control's self-claim (auto-claiming focus by
-- rendering first) or immediate self-give-up (Tab), neither of which goes
-- through the 'Focus'\/'ClearFocus' 'UiEffect' 'elementBase' watches for.
fireFocusChangeDirect :: ElementConfig e msg -> FocusTransition -> UI e msg ()
fireFocusChangeDirect ec t = case t of
  GainedFocus    -> runHandlers (ecOnFocusGained ec) ()
  LostFocus      -> runHandlers (ecOnFocusLost ec) ()
  FocusUnchanged -> pure ()

-- | The control-specific hit area: the current bounds inset by the
-- element's margin -- the margin itself is never part of the control, so a
-- mouse position within it counts as "outside" for hovering, clicking, and
-- focus-claiming alike.
--
-- Uses the element's /normal/ margin, not the margin of whichever style
-- variant is currently active, since real themes don't vary margin by
-- state and resolving the active style would otherwise depend on its own
-- result.
marginInsetBounds :: Ord e => StyleKey e -> UI e msg Rectangle
marginInsetBounds styleKey = do
  ss <- getStyleSet styleKey
  r  <- getBounds
  pure (insetRect (styleMargin (styleSetNormal ss)) r)

-- | Picks the active 'Style' variant given the element's disabled\/held\/
-- hovered\/focused state. Priority: disabled > held > hovered > focused >
-- normal.
resolveStyle :: StyleSet -> Bool -> Bool -> Bool -> Bool -> Style
resolveStyle styles disabled held hovered focused =
  fromMaybe (styleSetNormal styles) $ asum
    [ guard disabled $> styleSetDisabled styles
    , guard held     $> styleSetPressed  styles
    , guard hovered  $> styleSetHovered  styles
    , guard focused  $> styleSetFocused  styles
    ]

-- | Draws a control's background and border from its resolved style, then
-- runs @body@ clipped to the remaining space inside the padding, with that
-- same style available via 'currentStyle' so content never needs to
-- resolve its own copy.
renderStyled :: Style -> UI e msg () -> UI e msg ()
renderStyled s body = do
  r <- getBounds
  let bg          = insetRect (styleMargin s) r
      borderRect  = case styleBorderColour s of
                      Just _  -> insetRect (borderInsets (styleBorderEdges s)) bg
                      Nothing -> bg
      contentRect = insetRect (stylePadding s) borderRect
      inner       = withBounds contentRect $ clipToCurrent (withStyle s body)
  withBounds bg $
    withBackground (styleBackground s) $
    case styleBorderColour s of
      Just c  -> withBorder c (styleBorderEdges s) inner
      Nothing -> inner

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all
-- (@ccIsFocusable@) and configured to take focus itself on click ('FocusSelf').
autoClaimsFocus :: Eq e => FocusOnClick e -> ControlConfig e msg -> Bool
autoClaimsFocus foc cc = ccIsFocusable cc && foc == FocusSelf

-- | 'True' when this element should take focus with nothing having asked
-- for it: opted into auto-claiming (per 'autoClaimsFocus'), nothing else is
-- currently focused, and the mouse isn't contested by another element's
-- drag.
canAutoClaim :: Ord e => e -> FocusOnClick e -> ControlConfig e msg -> UI e msg Bool
canAutoClaim eid foc cc = do
  nothingIsFocused <- isNothingFocused <$> getFocus
  uncontested      <- isMouseFreeFor eid
  pure (autoClaimsFocus foc cc && nothingIsFocused && uncontested)

-- | Gives up focus immediately when @wasFocused@ and one of @advanceKeys@
-- was just pressed; hands focus to the previous tab stop, one frame later,
-- when @wasFocused@ and one of @retreatKeys@ was pressed instead (deferred
-- so that whichever element is gaining or losing focus reports it
-- consistently regardless of render order). Consumes whichever specific
-- key matched, so nothing else reacts to the same press. Disabled controls
-- never react.
advanceOrRetreat :: Bool -> [(Key, [Modifier])] -> [(Key, [Modifier])] -> UI e msg Bool
advanceOrRetreat wasFocused advanceKeys retreatKeys = do
  disabled <- isDisabled
  if disabled then pure False else do
    evs      <- inputKeyEvents <$> getInput
    prevCtrl <- getPreviousTabStop
    scopeId  <- getCurrentScope
    let advanceHit = find (\e -> (key e, modifiers e) `elem` advanceKeys) evs
        retreatHit = find (\e -> (key e, modifiers e) `elem` retreatKeys) evs
    case (wasFocused, advanceHit, retreatHit) of
      (True, Just e, _) -> clearFocus >> consumeKey (key e) $> True
      (True, _, Just e) -> forM_ prevCtrl (requestFocus scopeId) >> consumeKey (key e) $> False
      _                 -> pure False

-- | Manages this element's keyboard focus, reports its element interaction,
-- and draws its chrome around its content -- all configured entirely by
-- @cc@. Hovering, clicking, and focus-claiming all respect the same
-- margin-inset hit area chrome resolution uses -- the margin itself never
-- counts as "on" the control.
--
-- Per "a layer fires only what it originates", never dispatches an element
-- event itself: the click-to-focus effect it applies (per 'FocusOnClick')
-- is a direct 'UiEffect', read off the wrapped element's own 'eiClicked'.
controlBase :: Ord e => e -> ControlConfig e msg -> UI e msg (ControlInteraction e msg)
controlBase eid cc = disableWhen (not (ccIsEnabled cc)) $ do
  wasFocused   <- isFocused eid
  currentScope <- getCurrentScope
  applySelfFocus wasFocused
  applyNavigationKeys wasFocused
  nowFocused <- isFocused eid
  fireFocusChangeDirect (ccEvents cc) (focusTransition wasFocused nowFocused)
  hitBounds <- marginInsetBounds styleKey
  ei        <- withBounds hitBounds (elementBase eid (ccEvents cc))
  when (eiClicked ei) (applyClickFocus currentScope)
  styles   <- getStyleSet styleKey
  disabled <- isDisabled
  let s = resolveStyle styles disabled (eiHeld ei) (eiHovered ei) (eiFocused ei)
  renderStyled s (ccContent cc)
  when (ccIsFocusable cc && not disabled) (setPreviousTabStop eid)
  pure (ControlInteraction ei s)
  where
    styleKey = ccStyleKey cc
    foc      = ccFocusOnClick cc

    -- Immediate, not deferred: needed so that when several controls are
    -- simultaneously eligible, only the first one to render claims focus.
    applySelfFocus wasFocused = whenEnabled $ do
      auto <- canAutoClaim eid foc cc
      when (wasFocused || auto) (setFocus eid)

    -- This control's own reaction to whichever keys are currently ambient
    -- (plain Tab\/Shift-Tab, unless some enclosing container has
    -- redefined them).
    applyNavigationKeys wasFocused = do
      keys <- getNavigationKeys
      () <$ advanceOrRetreat wasFocused (navAdvance keys) (navRetreat keys)

    -- A click hands focus to whichever element FocusOnClick names, taking
    -- effect one frame later (see 'elementBase's own deferred detection via
    -- 'getFocusChange'). Targets whichever scope was ambient when this
    -- control itself rendered (@currentScope@, read once up front), not
    -- always root.
    applyClickFocus currentScope = case foc of
      FocusSelf     | ccIsFocusable cc -> emitUi (Focus currentScope eid)
                    | otherwise        -> pure ()
      FocusTarget t -> emitUi (Focus currentScope t)
      NoFocus       -> pure ()
