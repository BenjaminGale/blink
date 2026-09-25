{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The building block every widget in "Blink.Controls" is built
-- from. Re-exports "Blink.Element"'s 'Attribute'\/'resolve'
-- mechanism, which every config type here (and "Blink.Layout.Box") is
-- configured through.
--
-- = The attribute mechanism
--
-- Every widget's own config type gets a one-method typeclass instance
-- ('HasControlConfig') so an attribute defined once against 'ControlConfig'
-- (e.g. 'onClicked') can be applied to any config that nests one, however
-- deep, via the class's 'overControl' method. A config that itself /is/
-- 'ControlConfig' delegates with 'id'; a config that nests one delegates by
-- rewriting just that field. 'overControl' composes through arbitrary
-- nesting depth.
--
-- 'style' and 'isEnabled' apply to any config with a 'HasControlConfig'
-- instance. The event-handler attributes ('onClicked' and the rest) also
-- need a 'HasEventHandlers' instance, which a widget declares only if it
-- reports those events. 'elementId', 'focusPolicy' and 'mouseActivation'
-- apply to 'ControlConfig' alone: a widget built on 'control' sets those
-- fields itself, so no widget's attribute list accepts them.
--
-- = Control
--
-- A control ('control') watches whether the pointer is over it, whether a
-- mouse button is pressed or released on it, what keys are typed while it
-- holds focus, and whether focus moves onto or off of it -- firing the
-- matching handler from @cc@'s own handler fields for each, once, in one
-- place, after every flag has been computed -- manages its own keyboard
-- focus (claim on render while nothing else holds it, give up on Tab, hand
-- focus to the previous tab stop on Shift-Tab, and\/or take focus itself on
-- a mouse-down, per its 'FocusPolicy' and 'FocusOptions'), and draws themed
-- chrome (background,
-- border, padding, resolved from a 'Blink.Style.StyleKey' and its own
-- hover\/press\/focus state) around whatever content its 'ControlConfig'
-- carries. Per "a layer fires only what it originates", 'control' never
-- dispatches a raw event itself -- the one side-effect it applies off a
-- click (self-focus on mouse-down) is a direct 'UiEffect', not a handler
-- call.
module Blink.Controls.Control
  ( -- * Attributes
    Attribute (..)
  , resolve

    -- * Raw events
  , EventHandler
  , KeyEventHandler
  , MouseActivation (..)
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
  , HasEventHandlers
  , defaultControlConfig
  , control
  , focusTargetOnClick

    -- ** Control attributes
  , isEnabled
  , style
  , StyleKey (..)

    -- * Focus
  , FocusPolicy (..)
  , FocusOptions (..)
  , defaultFocusOptions
  , focusPolicy

    -- * Measurement
  , chromeInsets
  , measureChrome

    -- * Elements
  , chromeElement
  , controlElement

    -- * Parts
  , commonState
  , partStyle
  , drawPart
  ) where

import Control.Monad (forM_, void, when)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set

import Blink.Geometry
  ( Insets (..), Orientation (..), Rectangle, Size, CornerRadii, Border, BorderLayer (..)
  , borderInsets, inflate, insetRect, uniformRadii
  )
import Blink.Input (ButtonState (..), InputState (..), Key, KeyEvent (..), Modifier, Mouse (..), captureOf)
import Blink.Layout.Constraints (Layout, MeasureCtx (..), shrink)
import Blink.Style (Metrics (..), Style (..), StyleKey (..), StyleSet (..), VisualState (..), resolveStyle)
import Blink.View
import Blink.View.Context (Effect (..))
import Blink.View.Drawing (withClip, withBackground, withBorder)
import Blink.Element (Attribute (..), Element (..), resolve)

-- * Raw events

-- | A handler for an element event with no data of its own.
type EventHandler e msg = () -> [Effect e msg]

-- | A handler for 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Effect e msg]

-- | How a control's own mouse-button activity translates into 'ciClicked'
-- -- and, transitively, into 'control's own click-to-focus. A control
-- declares this once, up front, as part of its own interaction model; it
-- is never inferred from what a particular drag happened to do.
data MouseActivation
  = ClickActivated
    -- ^ Activated only by a release that lands back within the control's
    -- bounds -- the default. Dragging off before releasing cancels the
    -- press without side effects, the conventional way to back out of a
    -- click (buttons, checkboxes, and most other controls).
  | CaptureActivated
    -- ^ Activated by any release while the control still holds mouse
    -- capture, even once the pointer has left its bounds. For a control
    -- whose drag movement is itself the interaction (e.g.
    -- 'Blink.Controls.Slider.slider'), the value has already changed by
    -- the time the button comes up, so the release should count regardless
    -- of where the pointer ends up -- plain capture without this would
    -- just be a drag with no activation at all.
  deriving (Eq, Show)

-- | Appends a handler to whichever 'ControlConfig' field @get@\/@set@
-- address, wrapping the result as an 'Attribute'. The shared plumbing
-- behind every @onX@ builder below.
addHandler :: (HasControlConfig e msg cfg, HasEventHandlers cfg)
           => (ControlConfig e msg -> [h]) -> (ControlConfig e msg -> [h] -> ControlConfig e msg)
           -> h -> Attribute cfg
addHandler get set h = overControl (Attribute (\cc -> set cc (get cc ++ [h])))

-- | Gives the control a stable identity, keying its hover\/capture\/focus
-- tracking across frames -- see 'control'. Unset by default, in which case
-- the control raises no events and takes no part in focus at all,
-- regardless of any handler attached to it.
elementId :: e -> Attribute (ControlConfig e msg)
elementId eid = Attribute (\cc -> cc { ccElementId = Just eid })

-- | Reacts when the pointer starts being over the control this frame.
onMouseEntered :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onMouseEntered = addHandler ccOnMouseEntered (\cc hs -> cc { ccOnMouseEntered = hs })

-- | Reacts when the pointer stops being over the control this frame.
onMouseExited :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onMouseExited = addHandler ccOnMouseExited (\cc hs -> cc { ccOnMouseExited = hs })

-- | Reacts when the mouse button goes down while the control is hit.
onMouseDown :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onMouseDown = addHandler ccOnMouseDown (\cc hs -> cc { ccOnMouseDown = hs })

-- | Reacts when the mouse button comes up while the control is hit, even
-- if the press started elsewhere. See 'onClicked' for the click-only
-- version.
onMouseUp :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onMouseUp = addHandler ccOnMouseUp (\cc hs -> cc { ccOnMouseUp = hs })

-- | Reacts when the control is clicked -- by default (see
-- 'MouseActivation'), the mouse button pressed and released on it without
-- leaving; a 'CaptureActivated' control instead fires this on any release
-- while it still holds capture, even outside its bounds. Mouse-only -- see
-- 'Blink.Controls.Button.onActivated' for the event that also fires on
-- Enter while a button-like control holds focus.
onClicked :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onClicked = addHandler ccOnClicked (\cc hs -> cc { ccOnClicked = hs })

-- | Reacts to a key event while the control holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => KeyEventHandler e msg -> Attribute cfg
onKeyPressed = addHandler ccOnKeyPressed (\cc hs -> cc { ccOnKeyPressed = hs })

-- | Reacts when the control is named the winner of a focus transfer.
onFocusGained :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onFocusGained = addHandler ccOnFocusGained (\cc hs -> cc { ccOnFocusGained = hs })

-- | Reacts when the control loses focus, whether to a transfer or a clear.
onFocusLost :: (HasControlConfig e msg cfg, HasEventHandlers cfg) => EventHandler e msg -> Attribute cfg
onFocusLost = addHandler ccOnFocusLost (\cc hs -> cc { ccOnFocusLost = hs })

-- | Which way this control's own mouse-button activity turns into a click
-- -- see 'MouseActivation'. Defaults to 'ClickActivated'.
mouseActivation :: MouseActivation -> Attribute (ControlConfig e msg)
mouseActivation a = Attribute (\cc -> cc { ccMouseActivation = a })

-- | Runs every handler in @hs@ on @a@, dispatching the resulting 'Effect's.
runHandlers :: [a -> [Effect e msg]] -> a -> View e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (EffectMsg msg) = emit msg
    dispatch (EffectUi eff)  = emitUi eff

-- | Builds a reaction (an 'EventHandler'\/'Blink.Controls.ToggleButton.onSelectedChanged'-shaped
-- function into @['Effect' e msg]@) that emits @msg@, ignoring whatever data
-- the triggering event carried.
post :: msg -> a -> [Effect e msg]
post msg = const [EffectMsg msg]

-- | Builds a reaction that emits @f a@ -- uses the triggering event's own
-- data to build the message.
postWith :: (a -> msg) -> a -> [Effect e msg]
postWith f a = [EffectMsg (f a)]

-- | 'True' when nothing else holds mouse capture, or this control itself
-- does (a drag in progress on this control doesn't count as contention), or
-- the enclosing 'Blink.View.Focus.withFocusScope' scope does. That last case
-- matters for a click landing on a scope's own background (not any child):
-- the composite itself acquires capture first (its content hasn't rendered,
-- and so hasn't had a chance to claim the point out from under it, until
-- afterwards -- see 'Blink.View.Mouse.isOccludedFor'), and that capture is
-- held for the whole press. Without this, every child's own auto-claim
-- would read as contested by its own composite for that entire press, so
-- nothing could claim the scope's freshly-granted focus until release.
-- Shared by 'control's own auto-claim logic.
isMouseFreeFor :: Eq e => e -> View e msg Bool
isMouseFreeFor eid = do
  capturedByMe    <- isDragging eid
  scope           <- getCurrentScope
  capturedByScope <- maybe (pure False) isDragging scope
  (|| capturedByMe || capturedByScope) <$> isMouseFree

-- | 'watchHover's own report: hovered, plus the enter\/exit edges against
-- last frame's hover state.
data HoverInteraction = HoverInteraction
  { hiHovered      :: Bool
  , hiMouseEntered :: Bool
  , hiMouseExited  :: Bool
  }

-- | Registers this frame's hover, and claims capture unless the control is
-- @occluded@ -- something else (per last frame's registered hit-rects) sat
-- on top of it here, see 'isOccludedFor'. Reports hovered plus the
-- enter\/exit edges against last frame's hover state; hover itself is
-- unconditional on occlusion (any number of nested\/overlapping elements
-- can be "hovered" at once, by design), only capture-claiming backs off.
-- @eligible@ is expected to already fold in 'isMouseFreeFor'.
watchHover :: Ord e => e -> Bool -> Bool -> View e msg HoverInteraction
watchHover eid eligible occluded = do
  when eligible $ do
    registerMouseOver eid
    registerHitRect eid
    when (not occluded) (acquireCapture eid)
  wasOver <- wasMouseOverLastFrame eid
  pure HoverInteraction
    { hiHovered      = eligible
    , hiMouseEntered = not wasOver && eligible
    , hiMouseExited  = wasOver && not eligible
    }

-- | 'watchMouseButton's own report: down, up (always bounds-gated), click,
-- and held (button down with capture free or held by this control).
data MouseButtonInteraction = MouseButtonInteraction
  { mbiMouseDown :: Bool
  , mbiMouseUp   :: Bool
  , mbiClicked   :: Bool
  , mbiHeld      :: Bool
  }

-- | Reports this frame's mouse-button activity against the control.
--
-- @clicked@ reads differently depending on @activation@: for
-- 'ClickActivated', a release only counts while still within bounds (the
-- same release 'mbiMouseUp' reports); for 'CaptureActivated', a release
-- while this control holds capture counts regardless of bounds.
--
-- @held@ additionally requires @not occluded@ -- not just deferring to
-- 'isMouseFreeFor' -- because within the single frame capture is first
-- claimed, an occluded ancestor's own check runs before the nested\/topmost
-- element that will actually claim capture has had a chance to (an
-- ancestor's own interaction is watched before it renders whatever content
-- 'control' gives it, see 'control'), so capture would otherwise still
-- read "free" for the ancestor on that frame even though it correctly
-- declined to acquire it itself.
watchMouseButton :: Eq e => e -> MouseActivation -> Bool -> Bool -> View e msg MouseButtonInteraction
watchMouseButton eid activation eligible occluded = do
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
  let held = eligible && not occluded && free && down

  pure MouseButtonInteraction
    { mbiMouseDown = mouseDown
    , mbiMouseUp   = mouseUp
    , mbiClicked   = clicked
    , mbiHeld      = held
    }
  where
    isButtonDownEvent (ButtonDown _) = True
    isButtonDownEvent _              = False
    isButtonReleasedEvent (ButtonReleased _) = True
    isButtonReleasedEvent _                  = False

-- | 'watchFocus's own report: whether the control is focused this frame,
-- the key events it received, and whether a focus transfer this frame
-- named it winner or loser.
data FocusInteraction = FocusInteraction
  { fiFocused     :: Bool
  , fiFocusGained :: Bool
  , fiFocusLost   :: Bool
  , fiKeysPressed :: [KeyEvent]
  }

-- | Reports whether the control is focused this frame, the key events it
-- received (empty when disabled or unfocused), and whether a focus
-- transfer this frame named it winner or loser.
watchFocus :: Eq e => e -> Bool -> View e msg FocusInteraction
watchFocus eid disabled = do
  focused <- isFocused eid
  input   <- getInput
  let keysPressed = if not disabled && focused then inputKeyEvents input else []

  focusGained <- hasGainedFocus eid
  focusLost   <- hasLostFocus eid

  pure FocusInteraction
    { fiFocused     = focused
    , fiFocusGained = focusGained
    , fiFocusLost   = focusLost
    , fiKeysPressed = keysPressed
    }

-- | Fires the handler matching each flag set on @ci@ -- the one place raw
-- events are dispatched, run once every flag has been computed.
fireElementEvents :: ControlConfig e msg -> ControlInteraction e msg -> View e msg ()
fireElementEvents cc ci = do
  mapM_ (\(fired, hs) -> when fired $ runHandlers hs ()) events
  mapM_ (runHandlers (ccOnKeyPressed cc)) (ciKeysPressed ci)
  where
    events =
      [ (ciMouseEntered ci, ccOnMouseEntered cc)
      , (ciMouseExited  ci, ccOnMouseExited  cc)
      , (ciMouseDown    ci, ccOnMouseDown    cc)
      , (ciMouseUp      ci, ccOnMouseUp      cc)
      , (ciClicked      ci, ccOnClicked      cc)
      , (ciFocusGained  ci, ccOnFocusGained  cc)
      , (ciFocusLost    ci, ccOnFocusLost    cc)
      ]

-- * Focus

-- | Whether a control's own identity participates in keyboard focus.
-- Attached via the @ccFocusPolicy@ field of 'ControlConfig' (defaulting to
-- @'Focusable' 'defaultFocusOptions'@, matching every control's original,
-- single-flag behavior). A widget built on 'control' sets this field
-- itself (e.g. 'Blink.Controls.Label.label' always being 'NotFocusable');
-- no widget's attribute list accepts 'focusPolicy'.
--
-- A composite with its own distinctly-identified children (a list, a
-- tree, an editable row) that needs to give Tab a different meaning while
-- one of them is active calls 'Blink.View.Focus.withFocusScope' directly
-- around that content, rather than declaring it here. It writes whatever
-- entry, exit, and navigation behaviour it needs, typically a handful of
-- 'Blink.View.Focus.setFocus'\/'Blink.View.Focus.requestFocus' calls
-- reacting to its own key handling, as ordinary code, the same way it
-- already manages any other per-instance state.
data FocusPolicy
  = NotFocusable
    -- ^ This control's own id never becomes a focus target: no auto-claim
    -- by rendering first, no Tab\/Shift-Tab landing on it, no click-to-
    -- focus, and it's never handed off to as the previous tab stop. For
    -- pure layout containers, and widgets like 'Blink.Controls.Label.label'
    -- that are never themselves a stop. Still reachable programmatically
    -- (a direct 'Blink.View.Focus.setFocus'\/'Blink.View.Focus.requestFocus'
    -- from a UI effect isn't gated by this at all).
  | Focusable FocusOptions
    -- ^ This control's own id is a focus target, with 'FocusOptions'
    -- choosing which of the two ways it can be reached. Neither flag gates
    -- programmatic focus -- both 'False' just means the only way in is a
    -- direct 'Blink.View.Focus.setFocus'\/'Blink.View.Focus.requestFocus'.
  deriving (Eq, Show)

-- | Which of the two ordinary ways a 'Focusable' control's own id can
-- become the focus target. Both 'True' (via 'defaultFocusOptions')
-- reproduces every focusable control's original, single-flag behavior.
data FocusOptions = FocusOptions
  { focusIsTabStop :: Bool
    -- ^ Whether this control participates in keyboard tab order at all:
    -- auto-claimed by rendering first while nothing else holds focus,
    -- given up on Tab, landed on via Shift-Tab, and recorded as the
    -- previous tab stop for its neighbours to hand off to. 'False' removes
    -- it from Tab\/Shift-Tab traversal entirely, in both directions -- not
    -- just from auto-claiming.
  , focusIsClickToFocus :: Bool
    -- ^ Whether a mouse-down on this control claims focus for it.
  } deriving (Eq, Show)

-- | Both flags 'True' -- every focusable control's original behavior:
-- auto-claiming, full Tab\/Shift-Tab participation, and click-to-focus.
defaultFocusOptions :: FocusOptions
defaultFocusOptions = FocusOptions
  { focusIsTabStop      = True
  , focusIsClickToFocus = True
  }

-- * Control

-- | Every capability a control resolves before rendering: its identity and
-- raw-event handlers, whether it's focusable and enabled, its style, and
-- its content.
data ControlConfig e msg = ControlConfig
  { ccElementId       :: Maybe e
  , ccOnMouseEntered  :: [EventHandler e msg]
  , ccOnMouseExited   :: [EventHandler e msg]
  , ccOnMouseDown     :: [EventHandler e msg]
  , ccOnMouseUp       :: [EventHandler e msg]
  , ccOnClicked       :: [EventHandler e msg]
  , ccOnKeyPressed    :: [KeyEventHandler e msg]
  , ccOnFocusGained   :: [EventHandler e msg]
  , ccOnFocusLost     :: [EventHandler e msg]
  , ccMouseActivation :: MouseActivation
  , ccIsEnabled       :: Bool
  , ccStyleKey        :: StyleKey e
  , ccActiveStates    :: Set VisualState
    -- ^ Extra 'VisualState's contributed by a wrapping layer (e.g.
    -- 'Blink.Controls.ToggleButton.toggleBase' setting a checked\/unchecked
    -- pseudo-state), unioned with the common\/focus states 'control'
    -- derives itself. Defaults to empty.
  , ccContent         :: ControlInteraction e msg -> View e msg ()
    -- ^ Renders the control's content, given this same frame's own
    -- 'ControlInteraction' -- already fully computed by the time this
    -- runs, so content reads facts like 'ciFocused'\/'ciKeysPressed'\/
    -- 'ciIsCaptured' off it directly rather than re-deriving them via
    -- 'isFocused'\/'getInput'\/'isDragging' itself.
  , ccFocusPolicy     :: FocusPolicy
    -- ^ Whether, and how, this control's own identity participates in
    -- keyboard focus. See 'focusPolicy'.
  }

-- | No identity, every handler field empty, 'ClickActivated', enabled,
-- styled via an arbitrary placeholder key (always overridden -- every real
-- caller of 'control' supplies its own via 'style'), no extra active
-- states, rendering nothing, and @'Focusable' 'defaultFocusOptions'@.
defaultControlConfig :: ControlConfig e msg
defaultControlConfig = ControlConfig
  { ccElementId       = Nothing
  , ccOnMouseEntered  = []
  , ccOnMouseExited   = []
  , ccOnMouseDown     = []
  , ccOnMouseUp       = []
  , ccOnClicked       = []
  , ccOnKeyPressed    = []
  , ccOnFocusGained   = []
  , ccOnFocusLost     = []
  , ccMouseActivation = ClickActivated
  , ccIsEnabled       = True
  , ccStyleKey        = Class ""
  , ccActiveStates    = Set.empty
  , ccContent         = const (pure ())
  , ccFocusPolicy     = Focusable defaultFocusOptions
  }

-- | What 'control' reports back: three steady interaction states
-- (@ciHovered@, @ciHeld@, @ciFocused@), the discrete events that fired
-- this frame, and the 'Style' it resolved and drew with.
--
-- @ciClicked@'s exact trigger depends on 'ControlConfig's own
-- 'ccMouseActivation': a release back within bounds by default, or (for
-- 'CaptureActivated') any release while this control still holds capture,
-- even outside them.
data ControlInteraction e msg = ControlInteraction
  { ciHovered      :: Bool
  , ciHeld         :: Bool
  , ciFocused      :: Bool
  , ciMouseEntered :: Bool
  , ciMouseExited  :: Bool
  , ciMouseDown    :: Bool
  , ciMouseUp      :: Bool
  , ciClicked      :: Bool
  , ciFocusGained  :: Bool
    -- ^ Focus arrived at this control, however it got there: a click, Tab,
    -- Shift-Tab, a direct request, or claiming focus by rendering first.
  , ciFocusLost    :: Bool
    -- ^ Focus left this control, however it went.
  , ciKeysPressed  :: [KeyEvent]
  , ciIsCaptured   :: Bool
    -- ^ Whether this control holds mouse capture -- a press that started
    -- on it and hasn't been released, wherever the pointer is now.
  , ciCaptureStarted :: Bool
    -- ^ True only when the press that gives this control capture begins,
    -- so content can tell a new drag from one in progress.
  , ciDisabled     :: Bool
    -- ^ Whether this control is disabled, either directly
    -- ('Blink.Controls.Control.isEnabled') or via an enclosing
    -- 'Blink.View.disableWhen'. Content that only animates while live (e.g.
    -- 'Blink.Controls.ProgressBar.progressBar's indeterminate band)
    -- should check this rather than calling 'Blink.View.isDisabled' itself.
  , ciStyle        :: Style
  }

-- | The report a disabled or unidentified control gets: nothing was
-- watched, so nothing is hovered, held, focused, or reports any event --
-- drawn with @s@ regardless.
noInteraction :: Style -> ControlInteraction e msg
noInteraction s = ControlInteraction
  { ciHovered      = False
  , ciHeld         = False
  , ciFocused      = False
  , ciMouseEntered = False
  , ciMouseExited  = False
  , ciMouseDown    = False
  , ciMouseUp      = False
  , ciClicked      = False
  , ciFocusGained  = False
  , ciFocusLost    = False
  , ciKeysPressed  = []
  , ciIsCaptured   = False
  , ciCaptureStarted = False
  , ciDisabled     = False
  , ciStyle        = s
  }

-- | Implemented by any config type that nests a 'ControlConfig', letting
-- 'style' and 'isEnabled' be applied to it directly. Every instance but the
-- base case delegates one hop into its own nested field.
class HasControlConfig e msg cfg | cfg -> e msg where
  overControl :: Attribute (ControlConfig e msg) -> Attribute cfg

instance HasControlConfig e msg (ControlConfig e msg) where
  overControl = id

-- | Implemented by a config whose widget reports raw pointer, key and
-- focus events, letting 'onClicked' and the other event-handler
-- attributes be applied to it.
class HasEventHandlers cfg

instance HasEventHandlers (ControlConfig e msg)

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

-- | Whether, and how, this control's own identity participates in
-- keyboard focus -- see 'FocusPolicy'. Defaults to
-- @'Focusable' 'defaultFocusOptions'@, matching every control's original
-- behavior: Tab\/Shift-Tab cycling onto it, and auto-claiming focus by
-- rendering first while nothing else holds it.
focusPolicy :: FocusPolicy -> Attribute (ControlConfig e msg)
focusPolicy p = Attribute (\cc -> cc { ccFocusPolicy = p })

-- | Which way, if any, focus just moved, for 'control's own immediate
-- self-claim\/self-give-up notifications -- distinct from the deferred
-- focus handoffs 'control' itself detects via 'hasGainedFocus'\/'hasLostFocus'.
data FocusTransition = FocusUnchanged | GainedFocus | LostFocus

isGained, isLost :: FocusTransition -> Bool
isGained GainedFocus = True
isGained _           = False
isLost LostFocus = True
isLost _         = False

focusTransition :: Bool -> Bool -> FocusTransition
focusTransition was now
  | not was && now = GainedFocus
  | was && not now  = LostFocus
  | otherwise       = FocusUnchanged

-- | Fires 'ccOnFocusGained'\/'ccOnFocusLost' directly from a given
-- 'FocusTransition', immediately rather than through the deferred
-- detection watching raw focus transitions uses -- for a control's
-- self-claim (auto-claiming focus by rendering first) or immediate
-- self-give-up (Tab), neither of which goes through the
-- @Focus@\/@ClearFocus@ 'UiEffect' that deferred detection watches for.
fireFocusChangeDirect :: ControlConfig e msg -> FocusTransition -> View e msg ()
fireFocusChangeDirect cc t = case t of
  GainedFocus    -> runHandlers (ccOnFocusGained cc) ()
  LostFocus      -> runHandlers (ccOnFocusLost cc) ()
  FocusUnchanged -> pure ()

-- | The control-specific hit area: the current bounds inset by the
-- control's margin -- the margin itself is never part of the control, so a
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
-- reading and its held\/hovered\/focused reading, before any
-- 'ccActiveStates' contributed by a wrapping layer are unioned in.
intrinsicStates :: Bool -> ControlInteraction e msg -> Set VisualState
intrinsicStates disabled ci = Set.fromList
  [ commonState disabled (ciHeld ci) (ciHovered ci)
  , if ciFocused ci then FocusFocused else FocusUnfocused
  ]

-- | The common 'VisualState' for something @disabled@, @pressed@, or
-- @hovered@, checked in that order.
commonState :: Bool -> Bool -> Bool -> VisualState
commonState disabled pressed hovered
  | disabled  = CommonDisabled
  | pressed   = CommonPressed
  | hovered   = CommonMouseOver
  | otherwise = CommonNormal

-- | Draws a control's background and border from its resolved 'Metrics'
-- and 'Style', then runs @body@ clipped to the remaining space inside the
-- padding, with that same style\/metrics available via 'currentStyle'\/
-- 'currentMetrics' so content never needs to resolve its own copy.
renderStyled :: Metrics -> Style -> View e msg () -> View e msg ()
renderStyled m s body = do
  r <- getBounds
  let bg          = insetRect (metricsMargin m) r
      borderRect  = insetRect (borderContribution s) bg
      contentRect = insetRect (metricsPadding m) borderRect
      inner       = withBounds contentRect $ withClip (withMetrics m (withStyle s body))
  withBounds bg $
    withBackground (backgroundRadii (styleBorder s)) (styleBackground s) $
    withBorder (styleBorder s) inner

-- | The corner radii a control's background fill should be clipped to --
-- whichever border layer sits flush with the control's own edge
-- ('layerOffset' 0), since @bg@ (what the fill actually draws into) is
-- exactly that layer's rectangle; an outer decorative layer offset
-- further out (e.g. a focus ring) has no bearing on where the background
-- itself should round off. A plain square fill (no radius) when there's
-- no border, or no layer sits at offset 0.
backgroundRadii :: Border -> CornerRadii
backgroundRadii border = case [ layerRadii l | l <- border, layerOffset l == 0 ] of
  (radii : _) -> radii
  []          -> uniformRadii 0

-- | The space a control's border occupies: based on whichever of its
-- layers extends furthest out (see 'borderInsets'), or none at all
-- for an empty border. Shared by @renderStyled@ and 'chromeInsets' so
-- the two can never drift.
borderContribution :: Style -> Insets
borderContribution s = borderInsets (styleBorder s)

-- | The combined margin\/border\/padding a control's chrome occupies,
-- outside-in. Shared by @renderStyled@ (which insets by it) and
-- 'measureChrome' (which inflates by it), so the two can never drift.
chromeInsets :: Metrics -> Style -> Insets
chromeInsets m s = metricsMargin m <> borderContribution s <> metricsPadding m

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

-- | Whether this control participates in Tab\/Shift-Tab traversal at all
-- (see 'focusIsTabStop').
isTabStop :: FocusPolicy -> Bool
isTabStop NotFocusable        = False
isTabStop (Focusable options) = focusIsTabStop options

-- | Whether a mouse-down on this control claims focus for it (see
-- 'focusIsClickToFocus').
isClickToFocus :: FocusPolicy -> Bool
isClickToFocus NotFocusable        = False
isClickToFocus (Focusable options) = focusIsClickToFocus options

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: a tab stop, via @ccFocusPolicy@.
autoClaimsFocus :: ControlConfig e msg -> Bool
autoClaimsFocus cc = isTabStop (ccFocusPolicy cc)

-- | 'True' when this control should take focus with nothing having asked
-- for it: opted into auto-claiming (per 'autoClaimsFocus'), nothing else is
-- currently focused, and the mouse isn't contested by another control's
-- drag.
canAutoClaim :: Ord e => e -> ControlConfig e msg -> View e msg Bool
canAutoClaim eid cc = do
  nothingIsFocused <- isNothingFocused <$> getFocus
  uncontested      <- isMouseFreeFor eid
  pure (autoClaimsFocus cc && nothingIsFocused && uncontested)

-- | Gives up focus immediately when @wasFocused@ and one of @advanceKeys@
-- was just pressed; hands focus to the previous tab stop, one frame later,
-- when @wasFocused@ and one of @retreatKeys@ was pressed instead (deferred
-- so that whichever control is gaining or losing focus reports it
-- consistently regardless of render order). Consumes whichever specific
-- key matched, so nothing else reacts to the same press. Disabled controls
-- never react.
--
-- A retreat whose 'previousTabStop' equals the current scope's own id
-- means there's no real predecessor to hand off to in this scope. That
-- case leaves the key unconsumed instead of swallowing it, so whatever
-- encloses this scope gets a chance to react to it once this scope
-- closes. A composite that wants Shift-Tab to stop at its own first
-- child, rather than wrap to its last, seeds 'previousTabStop' with its
-- own scope id before rendering that child, so no descendant's id can
-- ever match it.
advanceOrRetreat :: Eq e => Bool -> [(Key, [Modifier])] -> [(Key, [Modifier])] -> View e msg ()
advanceOrRetreat wasFocused advanceKeys retreatKeys = do
  disabled <- isDisabled
  when (not disabled) $ do
    evs      <- inputKeyEvents <$> getInput
    prevCtrl <- getPreviousTabStop
    scopeId  <- getCurrentScope
    let advanceHit = find (\e -> (key e, modifiers e) `elem` advanceKeys) evs
        retreatHit = find (\e -> (key e, modifiers e) `elem` retreatKeys) evs
    case (wasFocused, advanceHit, retreatHit) of
      (True, Just e, _)
        -> clearFocus >> consumeKey (key e)
      (True, _, Just e)
        | prevCtrl /= scopeId -> forM_ prevCtrl (requestFocus scopeId) >> consumeKey (key e)
      _ -> pure ()

-- | Watches this control's hover, mouse-button, keyboard, and focus
-- activity for the current frame, manages its keyboard focus, reports the
-- raw interaction it saw, and draws its chrome around its content -- all
-- configured entirely by @cc@. Hovering, clicking, and focus-claiming all
-- respect the same margin-inset hit area chrome resolution uses -- the
-- margin itself never counts as "on" the control.
--
-- Per "a layer fires only what it originates", never dispatches a raw
-- event itself: the self-focus-on-click effect it applies (when
-- @ccFocusPolicy@'s 'focusIsClickToFocus' is set) is a direct 'UiEffect', read
-- off its own 'ciMouseDown' -- so focus moves on press, before any drag or
-- release decides whether the press itself counts as a click. Only fired
-- while this control isn't already focused: it's a "give me focus" request
-- for when something else (or nothing) currently holds it, not a periodic
-- reaffirmation -- @applySelfFocus@ already reaffirms an existing claim
-- every frame, immediately. Re-requesting it anyway on every click would
-- queue a real 'Blink.View.Focus.requestFocus' \/ redirect even when
-- nothing is actually moving, which a composite scope built on
-- 'Blink.View.Focus.withFocusScope' can't tell apart from a genuine
-- arrival from outside (see 'Blink.View.Focus.hasGainedFocus'), spuriously
-- re-triggering any reset-on-entry behaviour it does itself on every click
-- anywhere in an already-focused scope, not just ones that actually enter
-- it.
--
-- Identified by @cc@'s own 'ccElementId' (see 'elementId'). With no id set,
-- the control still renders, in its resting (undisabled, unfocused,
-- unhovered) style, but claims no focus, tracks no hover or press, and
-- reports @noInteraction@.
control :: Ord e => ControlConfig e msg -> View e msg (ControlInteraction e msg)
control cc = disableWhen (not (ccIsEnabled cc)) $
  case ccElementId cc of
    Nothing  -> renderInert
    Just eid -> renderTracked eid
  where
    styleKey = ccStyleKey cc

    renderInert = do
      (m, styles) <- getStyleSet styleKey
      disabled    <- isDisabled
      let active = intrinsicStates disabled (noInteraction (styleBase styles)) `Set.union` ccActiveStates cc
          s      = resolveStyle styles active
          interaction = (noInteraction s) { ciDisabled = disabled }
      renderStyled m s (ccContent cc interaction)
      pure interaction

    renderTracked eid = do
      disabled <- isDisabled
      -- A click can't focus this control directly while disabled, but a
      -- 'Blink.Controls.Label.target' pointed at it isn't stopped that
      -- way -- so reject a freshly arrived grant here too.
      freshlyGranted <- hasGainedFocus eid
      when (disabled && freshlyGranted) disclaimFocus
      wasFocused   <- isFocused eid
      currentScope <- getCurrentScope
      applySelfFocus eid wasFocused
      -- NotFocusable can still be the ambient focus (a scope's own id) -- skip Tab so the scope handles it first.
      when (ccFocusPolicy cc /= NotFocusable) (applyNavigationKeys wasFocused)
      nowFocused <- isFocused eid
      fireFocusChangeDirect cc (focusTransition wasFocused nowFocused)
      (m, styles) <- getStyleSet styleKey
      hitBounds   <- marginInsetBounds m
      raw         <- withBounds hitBounds (watchInteraction eid disabled (styleBase styles))
      when (ciMouseDown raw && isClickToFocus (ccFocusPolicy cc) && not nowFocused) (requestFocus currentScope eid)
      let active = intrinsicStates disabled raw `Set.union` ccActiveStates cc
          s      = resolveStyle styles active
          -- A self-claim or Tab give-up fires its handlers above, directly,
          -- rather than through @raw@'s own focus events -- so it's only
          -- added to the reported events here, after those have fired.
          transition = focusTransition wasFocused nowFocused
          final = raw
            { ciStyle       = s
            , ciFocusGained = ciFocusGained raw || isGained transition
            , ciFocusLost   = ciFocusLost raw || isLost transition
            }
      renderStyled m s (ccContent cc final)
      when (isTabStop (ccFocusPolicy cc) && not disabled) (setPreviousTabStop eid)
      pure final

    -- The raw hover\/mouse-button\/keyboard\/focus watching every
    -- identified control does, regardless of its focus-management or
    -- chrome -- assembles each watcher's own report into one
    -- 'ControlInteraction' (@placeholderStyle@ standing in for 'ciStyle'
    -- until the caller resolves and overwrites it, since style resolution
    -- itself depends on these flags), fires @cc@'s own handlers, and
    -- reports the full picture.
    watchInteraction eid disabled placeholderStyle = do
      -- Read before 'watchHover' can freshly 'acquireCapture', so a capture
      -- acquired below is reported as starting -- see 'ciCaptureStarted'.
      wasCaptured <- isDragging eid
      hit         <- isRegionHit
      let eligible = not disabled && hit
      occluded        <- if eligible then isOccludedFor eid else pure False
      occludedByPopup <- if eligible then isOccludedByPopupFor eid else pure False
      free     <- isMouseFreeFor eid
      -- registerHitRect must run unconditionally here, or isOccludedFor
      -- loses this element's entry the very next frame and click-through
      -- protection undoes itself; popup occlusion only masks hover below.
      hoverI0 <- watchHover eid (eligible && free) occluded
      let hoverI
            | occludedByPopup = hoverI0 { hiHovered = False, hiMouseEntered = False }
            | otherwise       = hoverI0
      mouseI0 <- watchMouseButton eid (ccMouseActivation cc) eligible occluded
      -- Unlike 'ciClicked' (already protected, via 'acquireCapture' backing
      -- off under 'occluded'), a fresh press was never gated by occlusion
      -- at all -- so a popup-covered control's own click-to-focus
      -- ('label's 'target' included) would otherwise still fire on a press
      -- that visually lands on the popup on top of it.
      let mouseI
            | occludedByPopup = mouseI0 { mbiMouseDown = False }
            | otherwise       = mouseI0
      focusI <- watchFocus eid disabled
      captured <- isDragging eid
      let interaction = (noInteraction placeholderStyle)
            { ciHovered      = hiHovered hoverI
            , ciMouseEntered = hiMouseEntered hoverI
            , ciMouseExited  = hiMouseExited hoverI
            , ciMouseDown    = mbiMouseDown mouseI
            , ciMouseUp      = mbiMouseUp mouseI
            , ciClicked      = mbiClicked mouseI
            , ciHeld         = mbiHeld mouseI
            , ciFocused      = fiFocused focusI
            , ciFocusGained  = fiFocusGained focusI
            , ciFocusLost    = fiFocusLost focusI
            , ciKeysPressed  = fiKeysPressed focusI
            , ciIsCaptured   = captured
            , ciCaptureStarted = captured && not wasCaptured
            , ciDisabled     = disabled
            }
      fireElementEvents cc interaction
      pure interaction

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
-- -- for a 'CaptureActivated' control -- any release while it still holds
-- capture). For a control that hands focus to a different control than
-- itself when clicked -- e.g. 'Blink.Controls.Label.label' redirecting
-- onto its 'Blink.Controls.Label.target'.
focusTargetOnClick :: Maybe e -> e -> ControlInteraction e msg -> View e msg ()
focusTargetOnClick scope target ci = when (ciClicked ci) (requestFocus scope target)

-- | An element laid out by @layout@ that measures as @content@ wrapped in
-- the chrome @styleKey@ resolves to, and runs @run@.
chromeElement :: Ord e => Layout -> StyleKey e -> Element e msg -> View e msg () -> Element e msg
chromeElement layout styleKey content run = Element
  { elLayout  = layout
  , elMeasure = measureChrome styleKey content
  , elRun     = run
  }

-- | 'chromeElement' for a widget whose run is @ctrl@ alone, measuring its
-- chrome from the same style key @ctrl@ draws with.
controlElement :: Ord e => Layout -> Element e msg -> ControlConfig e msg -> Element e msg
controlElement layout content ctrl = chromeElement layout (ccStyleKey ctrl) content (void (control ctrl))

-- | The style a part of a control draws with: @partKey@'s style resolved for
-- @states@. A part is a region a control draws inside itself (a slider's
-- thumb, a progress bar's fill) with its own 'StyleKey', so a theme styles
-- it separately from the control around it.
partStyle :: Ord e => StyleKey e -> Set VisualState -> View e msg Style
partStyle partKey states = do
  (_, styleSet) <- getStyleSet partKey
  pure (resolveStyle styleSet states)

-- | Draws a part into the current bounds: the background and border of
-- @partKey@'s style resolved for @states@ (see 'partStyle').
drawPart :: Ord e => StyleKey e -> Set VisualState -> View e msg ()
drawPart partKey states = do
  s <- partStyle partKey states
  withBackground (backgroundRadii (styleBorder s)) (styleBackground s) (withBorder (styleBorder s) (pure ()))
