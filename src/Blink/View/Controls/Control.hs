{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The building block every widget in "Blink.View.Controls" is built
-- from. Re-exports "Blink.View.Attribute"'s 'Attribute'\/'resolve'
-- mechanism, which every config type here (and "Blink.View.Layout.Box") is
-- configured through.
--
-- = The attribute mechanism
--
-- Each layer's own config type gets a one-method typeclass
-- (@HasElementConfig@, 'HasControlConfig') so an attribute defined once
-- against that layer's config (e.g. 'onClicked' against 'ElementConfig')
-- can be applied to any config that nests one, however deep, via the
-- class's @over...@ method. A config that itself /is/ the target type
-- delegates with 'id'; a config that nests one delegates by rewriting just
-- that field. 'over...' composes through arbitrary nesting depth, so an
-- attribute's reach is decided by where its field sits, not by which
-- instances a widget declines to declare.
--
-- = Element
--
-- An element ('elementBase') watches whether the pointer is over it,
-- whether a mouse button is pressed or released on it, what keys are typed
-- while it holds focus, and whether focus moves onto or off of it, and
-- returns all of that as an 'ElementInteraction' -- firing the matching
-- handler from its 'ElementConfig' for each, once, in one place, after
-- every flag has been computed.
--
-- = Control
--
-- A control ('controlBase') wraps an element with focus management (claim
-- on render while nothing else holds it, give up on Tab, hand focus to the
-- previous tab stop on Shift-Tab, take focus itself on a mouse-down when
-- 'isFocusable') and themed chrome (background, border, padding, resolved
-- from a 'Blink.View.Style.StyleKey' and the element's own hover\/press\/focus
-- state) around whatever content its 'ControlConfig' carries. Per "a layer
-- fires only what it originates", 'controlBase' never dispatches an
-- element event itself -- the one side-effect it applies off a click
-- (self-focus on mouse-down) is a direct 'UiEffect', not a handler call.
module Blink.View.Controls.Control
  ( -- * Attributes
    Attribute (..)
  , resolve

    -- * Element
  , EventHandler
  , KeyEventHandler
  , ElementConfig (..)
  , ElementInteraction (..)
  , HasElementConfig (..)
  , MouseActivation (..)
  , defaultElementConfig
  , elementBase
  , elementId
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
  , post
  , postWith

    -- * Control
  , ControlConfig (..)
  , ControlInteraction (..)
  , HasControlConfig (..)
  , defaultControlConfig
  , controlBase
  , focusTargetOnClick

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

import Blink.View.Attribute (Attribute (..), resolve)
import Blink.Geometry (Insets (..), Orientation (..), Rectangle, Size, borderInsets, inflate, insetRect)
import Blink.Input (ButtonState (..), InputState (..), Key, KeyEvent (..), Modifier, Mouse (..), captureOf)
import Blink.View.Layout.Constraints (MeasureCtx (..), shrink)
import Blink.View.Style (Metrics (..), Style (..), StyleKey (..), StyleSet (..), VisualState (..), resolveStyle)
import Blink.View
import Blink.View.Element (Element (..))

-- * Element

-- | A handler for an element event with no data of its own.
type EventHandler e msg = () -> [Out e msg]

-- | A handler for 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Out e msg]

-- | How an element's own mouse-button activity translates into
-- 'eiClicked' -- and, transitively, into 'controlBase's own click-to-focus.
-- A control declares this once, up front, as part of its own interaction
-- model; it is never inferred from what a particular drag happened to do.
data MouseActivation
  = ClickActivated
    -- ^ Activated only by a release that lands back within the element's
    -- bounds -- the default. Dragging off before releasing cancels the
    -- press without side effects, the conventional way to back out of a
    -- click (buttons, checkboxes, and most other controls).
  | CaptureActivated
    -- ^ Activated by any release while the element still holds mouse
    -- capture, even once the pointer has left its bounds. For a control
    -- whose drag movement is itself the interaction (e.g.
    -- 'Blink.View.Controls.Slider.slider'), the value has already changed by
    -- the time the button comes up, so the release should count regardless
    -- of where the pointer ends up -- plain capture without this would
    -- just be a drag with no activation at all.
  deriving (Eq, Show)

-- | One element's handlers, grouped by event, plus its 'MouseActivation'
-- and its identity ('ecElementId').
data ElementConfig e msg = ElementConfig
  { ecElementId       :: Maybe e
  , ecOnMouseEntered  :: [EventHandler e msg]
  , ecOnMouseExited   :: [EventHandler e msg]
  , ecOnMouseDown     :: [EventHandler e msg]
  , ecOnMouseUp       :: [EventHandler e msg]
  , ecOnClicked       :: [EventHandler e msg]
  , ecOnKeyPressed    :: [KeyEventHandler e msg]
  , ecOnFocusGained   :: [EventHandler e msg]
  , ecOnFocusLost     :: [EventHandler e msg]
  , ecMouseActivation :: MouseActivation
  }

-- | No 'ecElementId', every handler field empty, 'ClickActivated' for
-- 'ecMouseActivation'.
defaultElementConfig :: ElementConfig e msg
defaultElementConfig = ElementConfig
  { ecElementId       = Nothing
  , ecOnMouseEntered  = []
  , ecOnMouseExited   = []
  , ecOnMouseDown     = []
  , ecOnMouseUp       = []
  , ecOnClicked       = []
  , ecOnKeyPressed    = []
  , ecOnFocusGained   = []
  , ecOnFocusLost     = []
  , ecMouseActivation = ClickActivated
  }

-- | What 'elementBase' reports about the current frame's mouse, keyboard,
-- and focus activity: three steady interaction states (@eiHovered@,
-- @eiHeld@, @eiFocused@), and the discrete events that fired this frame.
--
-- @eiClicked@'s exact trigger depends on the 'ElementConfig's own
-- 'ecMouseActivation': a release back within bounds by default, or (for
-- 'CaptureActivated') any release while this element still holds capture,
-- even outside them.
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

-- | Field-by-field: @||@ for flags, concatenation for key events. Combining
-- a report against 'mempty' leaves it unchanged, so a report that only
-- knows about some fields can be merged with others covering the rest.
instance Semigroup ElementInteraction where
  a <> b = ElementInteraction
    { eiHovered      = eiHovered a      || eiHovered b
    , eiHeld         = eiHeld a         || eiHeld b
    , eiFocused      = eiFocused a      || eiFocused b
    , eiMouseEntered = eiMouseEntered a || eiMouseEntered b
    , eiMouseExited  = eiMouseExited a  || eiMouseExited b
    , eiMouseDown    = eiMouseDown a    || eiMouseDown b
    , eiMouseUp      = eiMouseUp a      || eiMouseUp b
    , eiClicked      = eiClicked a      || eiClicked b
    , eiFocusGained  = eiFocusGained a  || eiFocusGained b
    , eiFocusLost    = eiFocusLost a    || eiFocusLost b
    , eiKeysPressed  = eiKeysPressed a  ++ eiKeysPressed b
    }

instance Monoid ElementInteraction where
  mempty = ElementInteraction False False False False False False False False False False []

-- | Implemented by any config type that nests an 'ElementConfig', letting
-- an element attribute (e.g. 'onClicked') be applied to it directly. Every
-- instance but the base case delegates one hop into its own nested field.
class HasElementConfig e msg cfg | cfg -> e msg where
  overElement :: Attribute (ElementConfig e msg) -> Attribute cfg

instance HasElementConfig e msg (ElementConfig e msg) where
  overElement = id

-- | Appends a handler to whichever 'ElementConfig' field @get@\/@set@
-- address, wrapping the result as an 'Attribute'. The shared plumbing
-- behind every @onX@ builder below.
addHandler :: HasElementConfig e msg cfg
           => (ElementConfig e msg -> [h]) -> (ElementConfig e msg -> [h] -> ElementConfig e msg)
           -> h -> Attribute cfg
addHandler get set h = overElement (Attribute (\ec -> set ec (get ec ++ [h])))

-- | Gives the element a stable identity, keying its hover\/capture\/focus
-- tracking across frames -- see 'elementBase'. Unset by default, in which
-- case the element raises no events and takes no part in focus at all,
-- regardless of any handler attached to it.
elementId :: HasElementConfig e msg cfg => e -> Attribute cfg
elementId eid = overElement (Attribute (\ec -> ec { ecElementId = Just eid }))

-- | Reacts when the pointer starts being over the element this frame.
onMouseEntered :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseEntered = addHandler ecOnMouseEntered (\ec hs -> ec { ecOnMouseEntered = hs })

-- | Reacts when the pointer stops being over the element this frame.
onMouseExited :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseExited = addHandler ecOnMouseExited (\ec hs -> ec { ecOnMouseExited = hs })

-- | Reacts when the mouse button goes down while the element is hit.
onMouseDown :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseDown = addHandler ecOnMouseDown (\ec hs -> ec { ecOnMouseDown = hs })

-- | Reacts when the mouse button comes up while the element is hit, even
-- if the press started elsewhere. See 'onClicked' for the click-only
-- version.
onMouseUp :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseUp = addHandler ecOnMouseUp (\ec hs -> ec { ecOnMouseUp = hs })

-- | Reacts when the element is clicked -- by default (see
-- 'MouseActivation'), the mouse button pressed and released on it without
-- leaving; a 'CaptureActivated' element instead fires this on any release
-- while it still holds capture, even outside its bounds. Mouse-only -- see
-- 'Blink.View.Controls.Button.onActivated' for the event that also fires on
-- Enter while a button-like control holds focus.
onClicked :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onClicked = addHandler ecOnClicked (\ec hs -> ec { ecOnClicked = hs })

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasElementConfig e msg cfg => KeyEventHandler e msg -> Attribute cfg
onKeyPressed = addHandler ecOnKeyPressed (\ec hs -> ec { ecOnKeyPressed = hs })

-- | Reacts when the element is named the winner of a focus transfer.
onFocusGained :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onFocusGained = addHandler ecOnFocusGained (\ec hs -> ec { ecOnFocusGained = hs })

-- | Reacts when the element loses focus, whether to a transfer or a clear.
onFocusLost :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onFocusLost = addHandler ecOnFocusLost (\ec hs -> ec { ecOnFocusLost = hs })

-- | Which way this element's own mouse-button activity turns into a click
-- -- see 'MouseActivation'. Defaults to 'ClickActivated'.
mouseActivation :: HasElementConfig e msg cfg => MouseActivation -> Attribute cfg
mouseActivation a = overElement (Attribute (\ec -> ec { ecMouseActivation = a }))

-- | Runs every handler in @hs@ on @a@, dispatching the resulting 'Out's.
runHandlers :: [a -> [Out e msg]] -> a -> View e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | Builds a reaction (an 'EventHandler'\/'Blink.View.Controls.Toggle.onSelectedChanged'-shaped
-- function into @['Out' e msg]@) that emits @msg@, ignoring whatever data
-- the triggering event carried.
post :: msg -> a -> [Out e msg]
post msg = const [OutMsg msg]

-- | Builds a reaction that emits @f a@ -- uses the triggering event's own
-- data to build the message.
postWith :: (a -> msg) -> a -> [Out e msg]
postWith f a = [OutMsg (f a)]

-- | 'True' when nothing else holds mouse capture, or this element itself
-- does (a drag in progress on this element doesn't count as contention).
-- Shared by 'controlBase's own auto-claim logic.
isMouseFreeFor :: Eq e => e -> View e msg Bool
isMouseFreeFor eid = do
  capturedByMe <- isDragging eid
  (|| capturedByMe) <$> isMouseFree

-- | Watches this element's hover, mouse-button, keyboard, and focus
-- activity for the current frame -- within whatever bounds and scope its
-- caller has established via 'withBounds' -- and calls the matching
-- handler in @ec@ for each. Returns the full picture as an
-- 'ElementInteraction' so a caller built on top (e.g. 'controlBase') can
-- read the same flags back without re-querying the context itself. @ec@'s
-- own 'ecMouseActivation' decides what counts as a click -- see
-- 'MouseActivation'.
--
-- Identified by @ec@'s own 'ecElementId' (see 'elementId'). With no id set,
-- this raises no events and reports 'mempty' -- fully inert, not an error.
elementBase :: Ord e => ElementConfig e msg -> View e msg ElementInteraction
elementBase ec = case ecElementId ec of
  Nothing  -> pure mempty
  Just eid -> do
    disabled <- isDisabled
    hit      <- isRegionHit
    let eligible = not disabled && hit

    hoverI <- watchHover eid eligible
    mouseI <- watchMouseButton eid (ecMouseActivation ec) eligible
    focusI <- watchFocus eid disabled

    let interaction = hoverI <> mouseI <> focusI

    fireElementEvents ec interaction
    pure interaction

-- | Registers this frame's hover, claiming mouse-over and capture when the
-- element is eligible, and reports hovered plus the enter\/exit edges
-- against last frame's hover state. Only fills in 'eiHovered',
-- 'eiMouseEntered', and 'eiMouseExited' -- the rest is 'mempty'.
watchHover :: Ord e => e -> Bool -> View e msg ElementInteraction
watchHover eid eligible = do
  when eligible $ do
    registerMouseOver eid
    acquireCapture eid
  wasOver <- wasMouseOverLastFrame eid
  let mouseEntered = not wasOver && eligible
      mouseExited  = wasOver && not eligible
  pure mempty
    { eiHovered      = eligible
    , eiMouseEntered = mouseEntered
    , eiMouseExited  = mouseExited
    }

-- | Reports this frame's mouse-button activity against the element: down,
-- up (always bounds-gated), click, and held (button down with capture free
-- or held by this element). Only fills in 'eiMouseDown', 'eiMouseUp',
-- 'eiClicked', and 'eiHeld' -- the rest is 'mempty'.
--
-- @clicked@ reads differently depending on @activation@: for
-- 'ClickActivated', a release only counts while still within bounds (the
-- same release 'eiMouseUp' reports); for 'CaptureActivated', a release
-- while this element holds capture counts regardless of bounds.
watchMouseButton :: Eq e => e -> MouseActivation -> Bool -> View e msg ElementInteraction
watchMouseButton eid activation eligible = do
  mouse <- getMouse
  let capturedByMe  = captureOf (mouseButton mouse) == MouseCapturedBy eid
      releasedEvent = isButtonReleasedEvent (mouseButton mouse)
      mouseDown     = eligible && isButtonDownEvent (mouseButton mouse)
      mouseUp       = eligible && releasedEvent
      clicked       = capturedByMe && case activation of
        ClickActivated   -> mouseUp
        CaptureActivated -> releasedEvent

  free <- isMouseFreeFor eid
  down <- isButtonDown
  let held = eligible && free && down

  pure mempty
    { eiMouseDown = mouseDown
    , eiMouseUp   = mouseUp
    , eiClicked   = clicked
    , eiHeld      = held
    }
  where
    isButtonDownEvent (ButtonDown _) = True
    isButtonDownEvent _              = False
    isButtonReleasedEvent (ButtonReleased _) = True
    isButtonReleasedEvent _                  = False

-- | Reports whether the element is focused this frame, the key events it
-- received (empty when disabled or unfocused), and whether a focus
-- transfer this frame named it winner or loser. Only fills in
-- 'eiFocused', 'eiFocusGained', 'eiFocusLost', and 'eiKeysPressed' -- the
-- rest is 'mempty'.
watchFocus :: Eq e => e -> Bool -> View e msg ElementInteraction
watchFocus eid disabled = do
  focused <- isFocused eid
  input   <- getInput
  let keysPressed = if not disabled && focused then inputKeyEvents input else []

  focusGained <- hasGainedFocus eid
  focusLost   <- hasLostFocus eid

  pure mempty
    { eiFocused     = focused
    , eiFocusGained = focusGained
    , eiFocusLost   = focusLost
    , eiKeysPressed = keysPressed
    }

-- | Fires the handler matching each flag set on @ei@ -- the one place
-- 'elementBase' dispatches anything, run once every flag has been
-- computed.
fireElementEvents :: ElementConfig e msg -> ElementInteraction -> View e msg ()
fireElementEvents ec ei = do
  mapM_ (\(fired, hs) -> when fired $ runHandlers hs ()) events
  mapM_ (runHandlers (ecOnKeyPressed ec)) (eiKeysPressed ei)
  where
    events =
      [ (eiMouseEntered ei, ecOnMouseEntered ec)
      , (eiMouseExited  ei, ecOnMouseExited  ec)
      , (eiMouseDown    ei, ecOnMouseDown    ec)
      , (eiMouseUp      ei, ecOnMouseUp      ec)
      , (eiClicked      ei, ecOnClicked      ec)
      , (eiFocusGained  ei, ecOnFocusGained  ec)
      , (eiFocusLost    ei, ecOnFocusLost    ec)
      ]

-- * Control

-- | Every capability a control resolves before rendering: whether it's
-- focusable and enabled, its style, its content, and the element event
-- handlers wrapped up inside it.
data ControlConfig e msg = ControlConfig
  { ccIsFocusable  :: Bool
  , ccIsEnabled    :: Bool
  , ccStyleKey     :: StyleKey e
  , ccActiveStates :: Set VisualState
    -- ^ Extra 'VisualState's contributed by a wrapping layer (e.g.
    -- 'Blink.View.Controls.Toggle.toggleBase' setting a checked\/unchecked
    -- pseudo-state), unioned with the common\/focus states 'controlBase'
    -- derives itself. Defaults to empty.
  , ccContent      :: View e msg ()
  , ccElement      :: ElementConfig e msg
  }

-- | Focusable, enabled, styled via an arbitrary placeholder key (always
-- overridden -- every real caller of 'controlBase' supplies its own via
-- 'style'), no extra active states, rendering nothing, and with no event
-- handlers registered.
defaultControlConfig :: ControlConfig e msg
defaultControlConfig = ControlConfig
  { ccIsFocusable  = True
  , ccIsEnabled    = True
  , ccStyleKey     = Class ""
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
-- focus handoffs 'elementBase' itself detects via 'hasGainedFocus'\/'hasLostFocus'.
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
fireFocusChangeDirect :: ElementConfig e msg -> FocusTransition -> View e msg ()
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
marginInsetBounds :: Metrics -> View e msg Rectangle
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
renderStyled :: Metrics -> Style -> View e msg () -> View e msg ()
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
measureChrome :: Ord e => StyleKey e -> Element e msg -> MeasureCtx -> View e msg Size
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
-- while nothing else holds it: opted into keyboard focus at all, via
-- @ccIsFocusable@.
autoClaimsFocus :: ControlConfig e msg -> Bool
autoClaimsFocus cc = ccIsFocusable cc

-- | 'True' when this element should take focus with nothing having asked
-- for it: opted into auto-claiming (per 'autoClaimsFocus'), nothing else is
-- currently focused, and the mouse isn't contested by another element's
-- drag.
canAutoClaim :: Ord e => e -> ControlConfig e msg -> View e msg Bool
canAutoClaim eid cc = do
  nothingIsFocused <- isNothingFocused <$> getFocus
  uncontested      <- isMouseFreeFor eid
  pure (autoClaimsFocus cc && nothingIsFocused && uncontested)

-- | Gives up focus immediately when @wasFocused@ and one of @advanceKeys@
-- was just pressed; hands focus to the previous tab stop, one frame later,
-- when @wasFocused@ and one of @retreatKeys@ was pressed instead (deferred
-- so that whichever element is gaining or losing focus reports it
-- consistently regardless of render order). Consumes whichever specific
-- key matched, so nothing else reacts to the same press. Disabled controls
-- never react.
advanceOrRetreat :: Bool -> [(Key, [Modifier])] -> [(Key, [Modifier])] -> View e msg ()
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
-- event itself: the self-focus-on-click effect it applies (when
-- @ccIsFocusable@) is a direct 'UiEffect', read off the wrapped element's
-- own 'eiMouseDown' -- so focus moves on press, before any drag or release
-- decides whether the press itself counts as a click.
--
-- Identified by @cc@'s own element config ('ecElementId', see 'elementId').
-- With no id set, the control still renders, in its resting (undisabled,
-- unfocused, unhovered) style, but claims no focus, tracks no hover or
-- press, and reports 'mempty' for its element interaction.
controlBase :: Ord e => ControlConfig e msg -> View e msg (ControlInteraction e msg)
controlBase cc = disableWhen (not (ccIsEnabled cc)) $
  case ecElementId (ccElement cc) of
    Nothing  -> renderInert
    Just eid -> renderTracked eid
  where
    styleKey = ccStyleKey cc

    renderInert = do
      (m, styles) <- getStyleSet styleKey
      disabled    <- isDisabled
      let active = intrinsicStates disabled mempty `Set.union` ccActiveStates cc
          s      = resolveStyle styles active
      renderStyled m s (ccContent cc)
      pure (ControlInteraction mempty s)

    renderTracked eid = do
      disabled <- isDisabled
      -- A click can't focus this control directly while disabled, but a
      -- 'Blink.View.Controls.Label.target' pointed at it isn't stopped that
      -- way -- so reject a freshly arrived grant here too.
      freshlyGranted <- hasGainedFocus eid
      when (disabled && freshlyGranted) disclaimFocus
      wasFocused   <- isFocused eid
      currentScope <- getCurrentScope
      applySelfFocus eid wasFocused
      applyNavigationKeys wasFocused
      nowFocused <- isFocused eid
      fireFocusChangeDirect (ccElement cc) (focusTransition wasFocused nowFocused)
      (m, styles) <- getStyleSet styleKey
      hitBounds   <- marginInsetBounds m
      ei          <- withBounds hitBounds (elementBase (ccElement cc))
      when (eiMouseDown ei && ccIsFocusable cc) (emitUi (Focus currentScope eid))
      let active = intrinsicStates disabled ei `Set.union` ccActiveStates cc
          s      = resolveStyle styles active
      renderStyled m s (ccContent cc)
      when (ccIsFocusable cc && not disabled) (setPreviousTabStop eid)
      pure (ControlInteraction ei s)

    -- Immediate, not deferred: needed so that when several controls are
    -- simultaneously eligible, only the first one to render claims focus.
    applySelfFocus eid wasFocused = whenEnabled $ do
      auto <- canAutoClaim eid cc
      when (wasFocused || auto) (setFocus eid)

    -- This control's own reaction to whichever keys are currently ambient
    -- (plain Tab\/Shift-Tab, unless some enclosing container has
    -- redefined them).
    applyNavigationKeys wasFocused = do
      keys <- getNavigationKeys
      advanceOrRetreat wasFocused (navAdvance keys) (navRetreat keys)

-- | Sends focus to @target@, in @scope@, one frame after @ei@ reports a
-- click on the control that produced it (a release back within bounds, or
-- -- for a 'CaptureActivated' element -- any release while it still holds
-- capture). For a control that hands focus to a different element than
-- itself when clicked -- e.g. 'Blink.View.Controls.Label.label' redirecting
-- onto its 'Blink.View.Controls.Label.target'.
focusTargetOnClick :: Maybe e -> e -> ElementInteraction -> View e msg ()
focusTargetOnClick scope target ei = when (eiClicked ei) (emitUi (Focus scope target))
