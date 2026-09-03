{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    Attribute (..)
  , resolve
  , EventHandler
  , KeyEventHandler
  , ElementConfig (..)
  , ElementInteraction (..)
  , HasElementConfig (..)
  , MouseActivation (..)
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
  , mouseActivation
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

    -- * Measurement
  , chromeInsets
  , measureChrome
  ) where

import Control.Monad (forM_, when)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set

import Blink.Controls.Element
import Blink.Geometry (Insets (..), Orientation (..), Rectangle, Size, borderInsets, inflate, insetRect)
import Blink.Input (InputState (..), Key, KeyEvent (..), Modifier)
import Blink.Layout.Constraints (MeasureCtx (..), shrink)
import Blink.Style (Metrics (..), Style (..), StyleKey (..), StyleSet (..), VisualState (..), resolveStyle)
import Blink.UI
import Blink.UI.Element (Element (..))

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
  , ccActiveStates :: Set VisualState
    -- ^ Extra 'VisualState's contributed by a wrapping layer (e.g.
    -- 'Blink.Controls.Toggle.toggleBase' setting a checked\/unchecked
    -- pseudo-state), unioned with the common\/focus states 'controlBase'
    -- derives itself. Defaults to empty.
  , ccContent      :: UI e msg ()
  , ccElement      :: ElementConfig e msg
  }

-- | Focusable, enabled, styled via an arbitrary placeholder key (always
-- overridden -- every real caller of 'controlBase' supplies its own via
-- 'style'), taking focus itself on click, no extra active states,
-- rendering nothing, and with no event handlers registered.
defaultControlConfig :: ControlConfig e msg
defaultControlConfig = ControlConfig
  { ccIsFocusable  = True
  , ccIsEnabled    = True
  , ccStyleKey     = Class ""
  , ccFocusOnClick = FocusSelf
  , ccActiveStates = Set.empty
  , ccContent      = pure ()
  , ccElement      = defaultElementConfig
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
-- further in through 'ccElement'.
class HasControlConfig e msg cfg | cfg -> e msg where
  overControl :: Attribute (ControlConfig e msg) -> Attribute cfg

instance HasControlConfig e msg (ControlConfig e msg) where
  overControl = id

instance HasElementConfig e msg (ControlConfig e msg) where
  overElement (Attribute f) = Attribute (\cc -> cc { ccElement = f (ccElement cc) })

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both.
isFocusable :: HasControlConfig e msg cfg => Bool -> Attribute cfg
isFocusable b = overControl (Attribute (\cc -> cc { ccIsFocusable = b }))

-- | Whether the control responds to input at all. A disabled control still
-- renders (in its disabled style) but ignores hover, clicks, key presses,
-- and focus, and is skipped by Tab\/Shift-Tab. Defaults to 'True'.
isEnabled :: HasControlConfig e msg cfg => Bool -> Attribute cfg
isEnabled b = overControl (Attribute (\cc -> cc { ccIsEnabled = b }))

-- | Which 'StyleKey' this control resolves its style from. Defaults to a
-- 'Class' named after the control; pass 'ElementId' to theme this one
-- instance differently, or a different 'Class' to group it with others.
style :: HasControlConfig e msg cfg => StyleKey e -> Attribute cfg
style k = overControl (Attribute (\cc -> cc { ccStyleKey = k }))

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
-- Takes 'Metrics' directly (independent of interaction state, since real
-- themes don't vary margin by state), so this never depends on the
-- active 'Style'.
marginInsetBounds :: Metrics -> UI e msg Rectangle
marginInsetBounds m = do
  r <- getBounds
  pure (insetRect (metricsMargin m) r)

-- | The common\/focus 'VisualState's derived from a control's own disabled
-- reading and its wrapped element's held\/hovered\/focused reading, before
-- any 'ccActiveStates' contributed by a wrapping layer are unioned in.
intrinsicStates :: Bool -> ElementInteraction -> Set VisualState
intrinsicStates disabled ei = Set.fromList
  [ common
  , if eiFocused ei then FocusFocused else FocusUnfocused
  ]
  where
    common
      | disabled       = CommonDisabled
      | eiHeld ei      = CommonPressed
      | eiHovered ei   = CommonMouseOver
      | otherwise      = CommonNormal

-- | Draws a control's background and border from its resolved 'Metrics'
-- and 'Style', then runs @body@ clipped to the remaining space inside the
-- padding, with that same style\/metrics available via 'currentStyle'\/
-- 'currentMetrics' so content never needs to resolve its own copy.
renderStyled :: Metrics -> Style -> UI e msg () -> UI e msg ()
renderStyled m s body = do
  r <- getBounds
  let bg          = insetRect (metricsMargin m) r
      borderRect  = insetRect (borderContribution m s) bg
      contentRect = insetRect (metricsPadding m) borderRect
      inner       = withBounds contentRect $ clipToCurrent (withMetrics m (withStyle s body))
  withBounds bg $
    withBackground (styleBackground s) $
    case styleBorderColour s of
      Just c  -> withBorder c (metricsBorderEdges m) inner
      Nothing -> inner

-- | The space a control's border occupies: its 'Metrics' edges when
-- 'styleBorderColour' is set, or none at all when it isn't -- an unset
-- border draws (and occupies) nothing. Shared by @renderStyled@ and
-- 'chromeInsets' so the two can never drift.
borderContribution :: Metrics -> Style -> Insets
borderContribution m s = case styleBorderColour s of
  Just _  -> borderInsets (metricsBorderEdges m)
  Nothing -> mempty

-- | The combined margin\/border\/padding a control's chrome occupies,
-- outside-in. Shared by @renderStyled@ (which insets by it) and
-- 'measureChrome' (which inflates by it), so the two can never drift.
chromeInsets :: Metrics -> Style -> Insets
chromeInsets m s = metricsMargin m <> borderContribution m s <> metricsPadding m

-- | Measures a control wrapping a single child: offers @child@ the
-- interior left over after its chrome (so it wraps within the padding, not
-- across it), then inflates the answer back out by that same chrome.
--
-- Always resolves chrome from 'styleBase' -- the un-overridden style --
-- never the active interaction-state variant: themes don't vary geometry by
-- state, and resolving the active variant would make a control's size
-- depend on hover\/press\/focus (a button that grows when the pointer
-- touches it, or reflows its row when clicked).
measureChrome :: Ord e => StyleKey e -> Element e msg -> MeasureCtx -> UI e msg Size
measureChrome k child ctx = do
  (m, styleSet) <- getStyleSet k
  let insets = chromeInsets m (styleBase styleSet)
  sz <- elMeasure child ctx
    { measureMain  = shrink (axisInset (measureAxis ctx) insets) (measureMain ctx)
    , measureCross = shrink (axisInset (otherAxis (measureAxis ctx)) insets) (measureCross ctx)
    }
  pure (inflate insets sz)
  where
    axisInset Horizontal ins = leftInset ins + rightInset ins
    axisInset Vertical   ins = topInset ins + bottomInset ins
    otherAxis Horizontal = Vertical
    otherAxis Vertical   = Horizontal

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
advanceOrRetreat :: Bool -> [(Key, [Modifier])] -> [(Key, [Modifier])] -> UI e msg ()
advanceOrRetreat wasFocused advanceKeys retreatKeys = do
  disabled <- isDisabled
  when (not disabled) $ do
    evs      <- inputKeyEvents <$> getInput
    prevCtrl <- getPreviousTabStop
    scopeId  <- getCurrentScope
    let advanceHit = find (\e -> (key e, modifiers e) `elem` advanceKeys) evs
        retreatHit = find (\e -> (key e, modifiers e) `elem` retreatKeys) evs
    case (wasFocused, advanceHit, retreatHit) of
      (True, Just e, _) -> clearFocus >> consumeKey (key e)
      (True, _, Just e) -> forM_ prevCtrl (requestFocus scopeId) >> consumeKey (key e)
      _                 -> pure ()

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
  fireFocusChangeDirect (ccElement cc) (focusTransition wasFocused nowFocused)
  (m, styles) <- getStyleSet styleKey
  hitBounds   <- marginInsetBounds m
  ei          <- withBounds hitBounds (elementBase eid (ccElement cc))
  when (eiClicked ei) (applyClickFocus currentScope)
  disabled    <- isDisabled
  let active = intrinsicStates disabled ei `Set.union` ccActiveStates cc
      s      = resolveStyle styles active
  renderStyled m s (ccContent cc)
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
      advanceOrRetreat wasFocused (navAdvance keys) (navRetreat keys)

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
