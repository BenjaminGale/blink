{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The two building-block layers every widget in "Blink.Controls" is built
-- from, plus the attribute mechanism that lets one attribute function work
-- across every widget that nests the config it targets.
--
-- = The attribute mechanism
--
-- An attribute is @'Attr' cfg@: a function @cfg -> cfg@, wrapped in a
-- newtype so it can be given its own instances. 'resolve' folds a list of
-- them over a starting config left to right, so a later attribute setting
-- the same field overrides an earlier one.
--
-- Each layer's own config type gets a one-method typeclass
-- (@HasElementConfig@, @HasControlConfig@) so an attribute defined once
-- against that layer's config (e.g. 'onClicked' against 'ElementConfig')
-- can be applied to any config that nests one, however deep, via the
-- class's @over...@ method. A config that itself /is/ the target type
-- delegates with 'id'; a config that nests one delegates by rewriting just
-- that field. 'over...' composes through arbitrary nesting depth, so an
-- attribute's reach is decided by where its field sits, not by which
-- instances a widget declines to declare.
--
-- = Element and control
--
-- An element ('elementBase') watches whether the pointer is over it,
-- whether a mouse button is pressed or released on it, what keys are typed
-- while it holds focus, and whether focus moves onto or off of it, and
-- returns all of that as an 'ElementInteraction' -- firing the matching
-- handler from its 'ElementConfig' for each, once, in one place, after
-- every flag has been computed.
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
module Blink.Controls.Core
  ( -- * Attributes
    Attr (..)
  , resolve

    -- * Element layer
  , EventHandler
  , KeyEventHandler
  , ElementConfig (..)
  , ElementInteraction (..)
  , HasElementConfig (..)
  , defaultElementConfig
  , elementBase

    -- ** Element attributes
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost

    -- * Control layer
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

    -- * Handler plumbing
  , runHandlers
  ) where

import Control.Monad (forM_, guard, when)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find, foldl')
import Data.Maybe (fromMaybe)

import Blink.Geometry (Rectangle, borderInsets, insetRect)
import Blink.Input (ButtonState (..), InputState (..), Key, KeyEvent (..), Modifier, Mouse (..), captureOf)
import Blink.Style (Style (..), StyleKey (..), StyleSet (..))
import Blink.UI

-- * Attributes

-- | A single field update on @cfg@, applied by 'resolve'.
newtype Attr cfg = Attr { runAttr :: cfg -> cfg }

-- | Folds a list of attributes over a starting config, left to right -- a
-- later attribute setting the same field overrides an earlier one.
resolve :: cfg -> [Attr cfg] -> cfg
resolve = foldl' (\cfg (Attr f) -> f cfg)

-- * Element layer

-- | A handler for an element event with no data of its own.
type EventHandler e msg = () -> [Out e msg]

-- | A handler for 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Out e msg]

-- | One element's handlers, grouped by event.
data ElementConfig e msg = ElementConfig
  { ecOnClicked      :: [EventHandler e msg]
  , ecOnFocusGained  :: [EventHandler e msg]
  , ecOnFocusLost    :: [EventHandler e msg]
  , ecOnMouseEntered :: [EventHandler e msg]
  , ecOnMouseExited  :: [EventHandler e msg]
  , ecOnMouseDown    :: [EventHandler e msg]
  , ecOnMouseUp      :: [EventHandler e msg]
  , ecOnKeyPressed   :: [KeyEventHandler e msg]
  }

-- | Every field empty: no handlers registered for anything.
defaultElementConfig :: ElementConfig e msg
defaultElementConfig = ElementConfig [] [] [] [] [] [] [] []

-- | What 'elementBase' reports about the current frame's mouse, keyboard,
-- and focus activity: three steady interaction states (@eiHovered@,
-- @eiHeld@, @eiFocused@), and the discrete events that fired this frame.
data ElementInteraction = ElementInteraction
  { eiHovered       :: Bool
  , eiHeld          :: Bool
  , eiFocused       :: Bool
  , eiMouseEntered  :: Bool
  , eiMouseExited   :: Bool
  , eiMouseDown     :: Bool
  , eiMouseUp       :: Bool
  , eiClicked       :: Bool
  , eiFocusGained   :: Bool
  , eiFocusLost     :: Bool
  , eiKeysPressed   :: [KeyEvent]
  }

-- | Implemented by any config type that nests an 'ElementConfig', letting
-- an element attribute (e.g. 'onClicked') be applied to it directly. Every
-- instance but the base case delegates one hop into its own nested field.
class HasElementConfig e msg cfg | cfg -> e msg where
  overElement :: Attr (ElementConfig e msg) -> Attr cfg

instance HasElementConfig e msg (ElementConfig e msg) where
  overElement = id

-- | Reacts when the pointer starts being over the element this frame.
onMouseEntered :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onMouseEntered f = overElement (Attr (\ec -> ec { ecOnMouseEntered = ecOnMouseEntered ec ++ [f] }))

-- | Reacts when the pointer stops being over the element this frame.
onMouseExited :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onMouseExited f = overElement (Attr (\ec -> ec { ecOnMouseExited = ecOnMouseExited ec ++ [f] }))

-- | Reacts when the mouse button goes down while the element is hit.
onMouseDown :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onMouseDown f = overElement (Attr (\ec -> ec { ecOnMouseDown = ecOnMouseDown ec ++ [f] }))

-- | Reacts when the mouse button comes up while the element is hit, even
-- if the press started elsewhere. See 'onClicked' for the click-only
-- version.
onMouseUp :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onMouseUp f = overElement (Attr (\ec -> ec { ecOnMouseUp = ecOnMouseUp ec ++ [f] }))

-- | Reacts when the element is clicked: the mouse button pressed and
-- released on it without leaving. Mouse-only -- see
-- 'Blink.Controls.Attrs.onActivated' for the event that also fires on
-- Enter while a button-like control holds focus.
onClicked :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onClicked f = overElement (Attr (\ec -> ec { ecOnClicked = ecOnClicked ec ++ [f] }))

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasElementConfig e msg cfg => KeyEventHandler e msg -> Attr cfg
onKeyPressed f = overElement (Attr (\ec -> ec { ecOnKeyPressed = ecOnKeyPressed ec ++ [f] }))

-- | Reacts when the element is named the winner of a focus transfer.
onFocusGained :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onFocusGained f = overElement (Attr (\ec -> ec { ecOnFocusGained = ecOnFocusGained ec ++ [f] }))

-- | Reacts when the element loses focus, whether to a transfer or a clear.
onFocusLost :: HasElementConfig e msg cfg => EventHandler e msg -> Attr cfg
onFocusLost f = overElement (Attr (\ec -> ec { ecOnFocusLost = ecOnFocusLost ec ++ [f] }))

-- | Runs every handler in @hs@ on @a@, dispatching the resulting 'Out's.
runHandlers :: [a -> [Out e msg]] -> a -> UI e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | 'True' when nothing else holds mouse capture, or this element itself
-- does (a drag in progress on this element doesn't count as contention).
isMouseFreeFor :: Eq e => e -> UI e msg Bool
isMouseFreeFor eid = do
  capturedByMe <- isDragging eid
  (|| capturedByMe) <$> isMouseFree

-- | Watches this element's hover, mouse-button, keyboard, and focus
-- activity for the current frame -- within whatever bounds and scope its
-- caller has established via 'withBounds' -- and calls the matching
-- handler in @ec@ for each. Returns the full picture as an
-- 'ElementInteraction' so a caller built on top (e.g. 'controlBase') can
-- read the same flags back without re-querying the context itself.
elementBase :: Ord e => e -> ElementConfig e msg -> UI e msg ElementInteraction
elementBase eid ec = do
  disabled <- isDisabled
  hit      <- isRegionHit
  let hovered = not disabled && hit
  when hovered $ do
    registerMouseOver eid
    acquireCapture eid
  wasOver <- wasMouseOverLastFrame eid
  let mouseEntered = not wasOver && hovered
      mouseExited  = wasOver && not hovered

  mouse <- getMouse
  let eligible     = not disabled && hit
      capturedByMe = captureOf (mouseButton mouse) == MouseCapturedBy eid
      mouseDown    = eligible && isButtonDownEvent (mouseButton mouse)
      mouseUp      = eligible && isButtonReleasedEvent (mouseButton mouse)
      clicked      = mouseUp && capturedByMe

  free <- isMouseFreeFor eid
  down <- isButtonDown
  let held = not disabled && hit && free && down

  focused <- isFocused eid
  input   <- getInput
  let keysPressed = if not disabled && focused then inputKeyEvents input else []

  change <- getFocusChange
  let focusGained = maybe False (\fc -> focusChangeTo fc == Just eid) change
      focusLost   = maybe False (\fc -> focusChangeFrom fc == Just eid) change

  let interaction = ElementInteraction
        { eiHovered      = hovered
        , eiHeld         = held
        , eiFocused      = focused
        , eiMouseEntered = mouseEntered
        , eiMouseExited  = mouseExited
        , eiMouseDown    = mouseDown
        , eiMouseUp      = mouseUp
        , eiClicked      = clicked
        , eiFocusGained  = focusGained
        , eiFocusLost    = focusLost
        , eiKeysPressed  = keysPressed
        }

  fireElementEvents ec interaction
  pure interaction
  where
    isButtonDownEvent (ButtonDown _) = True
    isButtonDownEvent _              = False
    isButtonReleasedEvent (ButtonReleased _) = True
    isButtonReleasedEvent _                  = False

-- | Fires the handler matching each flag set on @ei@ -- the one place
-- 'elementBase' dispatches anything, run once every flag has been
-- computed.
fireElementEvents :: ElementConfig e msg -> ElementInteraction -> UI e msg ()
fireElementEvents ec ei = do
  when (eiMouseEntered ei) $ runHandlers (ecOnMouseEntered ec) ()
  when (eiMouseExited  ei) $ runHandlers (ecOnMouseExited  ec) ()
  when (eiMouseDown    ei) $ runHandlers (ecOnMouseDown    ec) ()
  when (eiMouseUp      ei) $ runHandlers (ecOnMouseUp      ec) ()
  when (eiClicked      ei) $ runHandlers (ecOnClicked      ec) ()
  mapM_ (runHandlers (ecOnKeyPressed ec)) (eiKeysPressed ei)
  when (eiFocusGained  ei) $ runHandlers (ecOnFocusGained ec) ()
  when (eiFocusLost    ei) $ runHandlers (ecOnFocusLost   ec) ()

-- * Control layer

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
