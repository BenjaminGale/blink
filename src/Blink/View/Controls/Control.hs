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
-- Every widget's own config type gets a one-method typeclass instance
-- ('HasControlConfig') so an attribute defined once against 'ControlConfig'
-- (e.g. 'onClicked') can be applied to any config that nests one, however
-- deep, via the class's 'overControl' method. A config that itself /is/
-- 'ControlConfig' delegates with 'id'; a config that nests one delegates by
-- rewriting just that field. 'overControl' composes through arbitrary
-- nesting depth, so an attribute's reach is decided by where its field
-- sits, not by which instances a widget declines to declare.
--
-- = Control
--
-- A control ('control') watches whether the pointer is over it, whether a
-- mouse button is pressed or released on it, what keys are typed while it
-- holds focus, and whether focus moves onto or off of it -- firing the
-- matching handler from @cc@'s own handler fields for each, once, in one
-- place, after every flag has been computed -- manages its own keyboard
-- focus (claim on render while nothing else holds it, give up on Tab, hand
-- focus to the previous tab stop on Shift-Tab, take focus itself on a
-- mouse-down when 'Focusable'), and draws themed chrome (background,
-- border, padding, resolved from a 'Blink.View.Style.StyleKey' and its own
-- hover\/press\/focus state) around whatever content its 'ControlConfig'
-- carries. Per "a layer fires only what it originates", 'control' never
-- dispatches a raw event itself -- the one side-effect it applies off a
-- click (self-focus on mouse-down) is a direct 'UiEffect', not a handler
-- call.
module Blink.View.Controls.Control
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
  , defaultControlConfig
  , control
  , focusTargetOnClick

    -- ** Control attributes
  , isEnabled
  , style
  , StyleKey (..)

    -- * Focus scope
  , FocusPolicy (..)
  , ChildNavigation (..)
  , ContainedNavigation (..)
  , WrapPolicy (..)
  , EntryPolicy (..)
  , focusPolicy

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

-- * Raw events

-- | A handler for an element event with no data of its own.
type EventHandler e msg = () -> [Out e msg]

-- | A handler for 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Out e msg]

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
    -- 'Blink.View.Controls.Slider.slider'), the value has already changed by
    -- the time the button comes up, so the release should count regardless
    -- of where the pointer ends up -- plain capture without this would
    -- just be a drag with no activation at all.
  deriving (Eq, Show)

-- | Appends a handler to whichever 'ControlConfig' field @get@\/@set@
-- address, wrapping the result as an 'Attribute'. The shared plumbing
-- behind every @onX@ builder below.
addHandler :: HasControlConfig e msg cfg
           => (ControlConfig e msg -> [h]) -> (ControlConfig e msg -> [h] -> ControlConfig e msg)
           -> h -> Attribute cfg
addHandler get set h = overControl (Attribute (\cc -> set cc (get cc ++ [h])))

-- | Gives the control a stable identity, keying its hover\/capture\/focus
-- tracking across frames -- see 'control'. Unset by default, in which case
-- the control raises no events and takes no part in focus at all,
-- regardless of any handler attached to it.
elementId :: HasControlConfig e msg cfg => e -> Attribute cfg
elementId eid = overControl (Attribute (\cc -> cc { ccElementId = Just eid }))

-- | Reacts when the pointer starts being over the control this frame.
onMouseEntered :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseEntered = addHandler ccOnMouseEntered (\cc hs -> cc { ccOnMouseEntered = hs })

-- | Reacts when the pointer stops being over the control this frame.
onMouseExited :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseExited = addHandler ccOnMouseExited (\cc hs -> cc { ccOnMouseExited = hs })

-- | Reacts when the mouse button goes down while the control is hit.
onMouseDown :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseDown = addHandler ccOnMouseDown (\cc hs -> cc { ccOnMouseDown = hs })

-- | Reacts when the mouse button comes up while the control is hit, even
-- if the press started elsewhere. See 'onClicked' for the click-only
-- version.
onMouseUp :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseUp = addHandler ccOnMouseUp (\cc hs -> cc { ccOnMouseUp = hs })

-- | Reacts when the control is clicked -- by default (see
-- 'MouseActivation'), the mouse button pressed and released on it without
-- leaving; a 'CaptureActivated' control instead fires this on any release
-- while it still holds capture, even outside its bounds. Mouse-only -- see
-- 'Blink.View.Controls.Button.onActivated' for the event that also fires on
-- Enter while a button-like control holds focus.
onClicked :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onClicked = addHandler ccOnClicked (\cc hs -> cc { ccOnClicked = hs })

-- | Reacts to a key event while the control holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasControlConfig e msg cfg => KeyEventHandler e msg -> Attribute cfg
onKeyPressed = addHandler ccOnKeyPressed (\cc hs -> cc { ccOnKeyPressed = hs })

-- | Reacts when the control is named the winner of a focus transfer.
onFocusGained :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onFocusGained = addHandler ccOnFocusGained (\cc hs -> cc { ccOnFocusGained = hs })

-- | Reacts when the control loses focus, whether to a transfer or a clear.
onFocusLost :: HasControlConfig e msg cfg => EventHandler e msg -> Attribute cfg
onFocusLost = addHandler ccOnFocusLost (\cc hs -> cc { ccOnFocusLost = hs })

-- | Which way this control's own mouse-button activity turns into a click
-- -- see 'MouseActivation'. Defaults to 'ClickActivated'.
mouseActivation :: HasControlConfig e msg cfg => MouseActivation -> Attribute cfg
mouseActivation a = overControl (Attribute (\cc -> cc { ccMouseActivation = a }))

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

-- | 'True' when nothing else holds mouse capture, or this control itself
-- does (a drag in progress on this control doesn't count as contention).
-- Shared by 'control's own auto-claim logic.
isMouseFreeFor :: Eq e => e -> View e msg Bool
isMouseFreeFor eid = do
  capturedByMe <- isDragging eid
  (|| capturedByMe) <$> isMouseFree

-- | 'watchHover's own report: hovered, plus the enter\/exit edges against
-- last frame's hover state.
data HoverInteraction = HoverInteraction
  { hiHovered      :: Bool
  , hiMouseEntered :: Bool
  , hiMouseExited  :: Bool
  }

-- | Registers this frame's hover, claiming mouse-over and capture when the
-- control is eligible, and reports hovered plus the enter\/exit edges
-- against last frame's hover state.
watchHover :: Ord e => e -> Bool -> View e msg HoverInteraction
watchHover eid eligible = do
  when eligible $ do
    registerMouseOver eid
    acquireCapture eid
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
watchMouseButton :: Eq e => e -> MouseActivation -> Bool -> View e msg MouseButtonInteraction
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

-- * Focus scope

-- | Whether, and how, a control's own identity participates in keyboard
-- focus. Attached via the @ccFocusPolicy@ field of 'ControlConfig'
-- (defaulting to 'Focusable', matching every control's existing
-- behavior). Unlike other 'ControlConfig' fields, a widget that needs a
-- fixed value (e.g. 'Blink.View.Controls.Label.label' always being
-- 'NotFocusable') sets it unconditionally after resolving attrs, the same
-- way it already pins other fixed behavior -- there is no separate,
-- narrower attribute for this; 'focusPolicy' is the only one, and any
-- widget is free to override whatever a caller passed through it.
data FocusPolicy
  = NotFocusable
    -- ^ This control's own id never becomes a focus target: no auto-claim
    -- by rendering first, no Tab\/Shift-Tab landing on it, no click-to-
    -- focus. For pure layout containers, and widgets like
    -- 'Blink.View.Controls.Label.label' that are never themselves a stop.
  | Focusable
    -- ^ This control's own id is exactly one focus stop, entered and left
    -- the way every focusable control already works: auto-claimed by
    -- rendering first while nothing else holds focus, given up on Tab,
    -- taken on click. The default.
  | FocusScope ChildNavigation
    -- ^ Also exactly one focus stop from outside; 'ChildNavigation'
    -- decides what Tab does once focus is inside -- keep moving through
    -- this control's own distinctly-identified children ('Continue'), or
    -- stay contained within them using a separate key scheme
    -- ('Contained').
  deriving (Eq, Show)

-- | What Tab does once focus is inside a 'FocusScope'.
data ChildNavigation
  = Continue
    -- ^ Tab keeps moving through this control's own children and on into
    -- the surrounding order at the ends, exactly as if this control
    -- weren't there at all. No scope of its own is needed for this.
  | Contained ContainedNavigation
    -- ^ Tab never reaches the children at all -- it stays owned by this
    -- control as a single stop, the same as 'Focusable'. Movement between
    -- children uses its own separate key scheme instead.
  deriving (Eq, Show)

-- | The key scheme and behaviour for 'Contained' navigation.
data ContainedNavigation = ContainedNavigation
  { navForward  :: (Key, [Modifier])
    -- ^ Moves to the next child. Never Tab -- that stays owned by this
    -- control as a whole.
  , navBackward :: (Key, [Modifier])
    -- ^ Moves to the previous child.
  , navWrap     :: WrapPolicy
  , navEntry    :: EntryPolicy
  } deriving (Eq, Show)

-- | What happens when 'ContainedNavigation' runs off the first\/last
-- child.
data WrapPolicy
  = WrapCycle
    -- ^ Wraps around to the opposite child.
  | WrapStop
    -- ^ Stops there -- the boundary child stays focused. Tab remains the
    -- only way to leave a 'Contained' scope.
  deriving (Eq, Show)

-- | Which child 'Contained' navigation targets when the scope gains focus
-- from outside (a Tab arriving at the boundary) with nothing already
-- focused inside.
data EntryPolicy
  = EnterRemembered
    -- ^ Targets whichever child was focused last time the scope was live
    -- (persisted indefinitely, independent of focus decay).
  | EnterFirst
    -- ^ Ignores history; whichever child is first eligible to auto-claim
    -- during this render gets it, the same way focus already resolves
    -- today.
  deriving (Eq, Show)

-- | Wraps a control's own content per its resolved 'FocusPolicy'. Every
-- control (leaf or composite) runs its content through this, uniformly --
-- 'FocusPolicy' decides what, if anything, changes for whatever renders
-- inside.
--
-- 'NotFocusable' and 'Focusable' need no scope machinery at all: neither
-- changes whether this control's own id is a focus target for anything
-- beyond the ordinary claim\/give-up logic 'control' already does outside
-- this function (see @autoClaimsFocus@ and the click-to-focus\/Tab
-- handling in 'control' itself), and neither has any nested,
-- distinctly-identified children to act on. A real scope (established via
-- 'withFocusScope') is only needed for 'FocusScope', since that's the one
-- case where something inside -- a distinct child id -- needs Tab handed
-- to it, or hidden from it, differently than this control's own id. Also
-- why a real scope can't just be established unconditionally for every
-- control: a control's own content routinely reads its own focus\/key
-- state directly (e.g. 'Blink.View.Controls.TextInput.textInput' checking
-- 'isFocused' on its own id), and wrapping that in a scope keyed by the
-- same id would swap the ambient out from under those checks.
--
-- Both 'FocusScope' cases still need the caller ('control' itself) to
-- skip this control's own ordinary Tab\/Shift-Tab handling before @body@
-- runs -- run unconditionally there, it would see focus-within as "I'm
-- focused" and give up the instant Tab is pressed, before any child ever
-- gets a chance to react. 'runFocusScope' runs it itself instead, once
-- @body@ has already had its turn -- see there.
applyFocusPolicy :: Ord e => e -> FocusPolicy -> View e msg a -> View e msg a
applyFocusPolicy eid policy body = case policy of
  NotFocusable            -> body
  Focusable               -> body
  FocusScope Continue     -> runFocusScope eid Nothing WrapStop body
  FocusScope (Contained nav) ->
    runFocusScope eid (Just (NavigationKeys [navForward nav] [navBackward nav], navEntry nav)) (navWrap nav) body

-- | Runs @body@ (a 'FocusScope' control's content) inside its own focus
-- scope, then this control's own reaction to whichever Tab\/Shift-Tab
-- keys are ambient *outside* it -- captured before anything below can
-- replace them, so it always means real Tab\/Shift-Tab regardless of
-- @keys@. @keys@, when given, replaces the pair @body@'s own descendants
-- see with the 'Contained' scheme's own. 'Nothing' ('Continue') leaves
-- the ambient keys alone, since children should behave exactly as if
-- this control weren't there.
--
-- 'EnterRemembered' needs no code here at all: 'withFocusScope' already
-- keeps this scope's own child claim alive for as long as this control
-- keeps rendering, live or not (see its own docs) -- so whichever child
-- was last selected is simply still there, the ordinary way, whenever
-- this control is freshly Tab-ed back onto. 'EnterFirst' is the one that
-- needs to actively do something: on the frame this control itself is
-- freshly given outer focus, it discards whatever's remembered so the
-- ordinary auto-claim mechanism picks whichever child is first eligible,
-- fresh, exactly as if nothing had ever been selected before.
--
-- 'BlockFreshClaim': this scope's id is always the same id 'control'
-- already claims (or doesn't) against the *enclosing* ambient via its own
-- ordinary self-claim, before this function ever runs. 'AllowFreshClaim'
-- would let the scope itself self-claim a second, uncontrolled time
-- whenever nothing else is focused, bypassing that ordinary claim
-- entirely.
--
-- 'WrapStop' also seeds the scope's own previous-tab-stop with this
-- scope's own id, ahead of @body@ -- a value no descendant's id can ever
-- equal, so 'advanceOrRetreat' reads it as "nothing to retreat to in
-- here" (the same idiom 'Blink.View.withFocusScope' itself uses for its
-- 'Blink.View.BlockFreshClaim' placeholder: when nothing else can honestly
-- stand for "unset", a value nothing inside can ever be does instead).
-- Without this, the scope's persisted previous-tab-stop is always
-- whichever child rendered last (every focusable child claims it as it
-- renders, unconditionally), so the *first* child to retreat would
-- otherwise wrap around to the last one -- the behaviour 'WrapCycle'
-- wants, but not 'WrapStop'. A child that does have a real predecessor
-- this frame (it isn't the first to render) overwrites this placeholder
-- with a real id before any retreat check can see it, so an ordinary
-- internal move (e.g. second child back to first) is untouched.
--
-- TODO: revisit -- this placeholder is a workaround for there being no
-- exported way to write a literal @Nothing@ into 'previousTabStop' (only
-- 'setPreviousTabStop', which always writes @Just@); a cleaner primitive
-- upstream could remove the need for it.
--
-- After @body@ runs, if nothing is claimed inside any more, @wrap@
-- decides what happens: 'WrapStop' gives up this control's own claim too
-- (letting the surrounding order pick up where it left off, the same way
-- an ordinary control already does on Tab); 'WrapCycle' does nothing --
-- the scope stays claimed but empty, so the ordinary auto-claim already
-- built into every control simply refills it with whichever child renders
-- first next time, the same mechanism that resolves 'EnterFirst'.
--
-- Finally, this control runs its own ordinary 'advanceOrRetreat' against
-- the ambient keys captured up front -- the *same* call for either
-- 'ChildNavigation', letting each produce the right outcome on its own
-- rather than branching on which one this is:
--
--   ['Contained']: descendants never see Tab\/Shift-Tab at all (@keys@
--   replaced them), so this call always finds the key exactly as it
--   arrived and reacts every time, regardless of which child (if any) is
--   selected inside -- Tab\/Shift-Tab always leaves the scope, as a
--   single ordinary stop.
--
--   ['Continue']: descendants see and react to real Tab\/Shift-Tab
--   directly. An advancing descendant always consumes it immediately
--   (whether or not it was the last one) -- the @wrap@\/@stillFocused@
--   check above, not this call, is what notices the boundary was reached
--   for that direction. A retreating descendant consumes it too, unless
--   it hit the placeholder above -- so this call only ever finds a live
--   retreat key at that same boundary, and is a no-op everywhere else.
runFocusScope :: Ord e => e -> Maybe (NavigationKeys, EntryPolicy) -> WrapPolicy -> View e msg a -> View e msg a
runFocusScope eid mKeysEntry wrap body = do
  ambientKeys <- getNavigationKeys
  justEntered <- hasGainedFocus eid
  maybe id (withNavigationKeys . fst) mKeysEntry $ do
    (a, stillFocused) <- withFocusScope eid BlockFreshClaim $ do
      case mKeysEntry of
        Just (_, EnterFirst) | justEntered -> clearFocus
        _ -> pure ()
      when (wrap == WrapStop) (setPreviousTabStop eid)
      a <- body
      case mKeysEntry of
        Just _ | wrap == WrapStop -> do
          mPrev      <- getPreviousTabStop
          stillHasIt <- maybe (pure False) isFocused mPrev
          when (not stillHasIt) (forM_ mPrev setFocus)
        _ -> pure ()
      focused <- not . isNothingFocused <$> getFocus
      pure (a, focused)
    when (wrap == WrapStop && not stillFocused) clearFocus
    wasFocused <- isFocused eid
    advanceOrRetreat wasFocused (navAdvance ambientKeys) (navRetreat ambientKeys)
    pure a

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
    -- 'Blink.View.Controls.Toggle.toggleBase' setting a checked\/unchecked
    -- pseudo-state), unioned with the common\/focus states 'control'
    -- derives itself. Defaults to empty.
  , ccContent         :: ControlInteraction e msg -> View e msg ()
    -- ^ Renders the control's content, given this same frame's own
    -- 'ControlInteraction' -- already fully computed by the time this
    -- runs, so content reads facts like 'ciFocused'\/'ciKeysPressed'\/
    -- 'ciWasDragging' off it directly rather than re-deriving them via
    -- 'isFocused'\/'getInput'\/'isDragging' itself.
  , ccFocusPolicy     :: FocusPolicy
    -- ^ Whether, and how, this control's own identity participates in
    -- keyboard focus. See 'focusPolicy'.
  }

-- | No identity, every handler field empty, 'ClickActivated', enabled,
-- styled via an arbitrary placeholder key (always overridden -- every real
-- caller of 'control' supplies its own via 'style'), no extra active
-- states, rendering nothing, and 'Focusable'.
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
  , ccFocusPolicy     = Focusable
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
  , ciFocusLost    :: Bool
  , ciKeysPressed  :: [KeyEvent]
  , ciWasDragging  :: Bool
    -- ^ Whether this control already held mouse capture as of the *start*
    -- of this frame's processing, before anything this frame (including a
    -- fresh capture acquired this same frame) could change it. Lets
    -- content distinguish "continuing an existing drag" from "a fresh
    -- grab just starting" -- a fact only available from before the frame
    -- began, the same reason a plain live 'isDragging' read from inside
    -- content can't recover it.
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
  , ciWasDragging  = False
  , ciStyle        = s
  }

-- | Implemented by any config type that nests a 'ControlConfig', letting a
-- control attribute (e.g. 'focusPolicy', 'onClicked') be applied to it
-- directly. Every instance but the base case delegates one hop into its
-- own nested field.
class HasControlConfig e msg cfg | cfg -> e msg where
  overControl :: Attribute (ControlConfig e msg) -> Attribute cfg

instance HasControlConfig e msg (ControlConfig e msg) where
  overControl = id

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
-- keyboard focus -- see 'FocusPolicy'. Defaults to 'Focusable', matching
-- every control's existing behavior: Tab\/Shift-Tab cycling onto it, and
-- auto-claiming focus by rendering first while nothing else holds it.
-- There is no separate on\/off flag for this -- a widget that needs a
-- fixed value (e.g. 'Blink.View.Controls.Label.label' always being
-- 'NotFocusable') sets it unconditionally after resolving attrs, the same
-- way it already pins other fixed behavior.
focusPolicy :: HasControlConfig e msg cfg => FocusPolicy -> Attribute cfg
focusPolicy p = overControl (Attribute (\cc -> cc { ccFocusPolicy = p }))

-- | Which way, if any, focus just moved, for 'control's own immediate
-- self-claim\/self-give-up notifications -- distinct from the deferred
-- focus handoffs 'control' itself detects via 'hasGainedFocus'\/'hasLostFocus'.
data FocusTransition = FocusUnchanged | GainedFocus | LostFocus

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
-- 'Focus'\/'ClearFocus' 'UiEffect' that deferred detection watches for.
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
  [ common
  , if ciFocused ci then FocusFocused else FocusUnfocused
  ]
  where
    common
      | disabled       = CommonDisabled
      | ciHeld ci      = CommonPressed
      | ciHovered ci   = CommonMouseOver
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

-- | Whether this control's own id is ever a focus target at all -- 'False'
-- only for 'NotFocusable'; both 'Focusable' and 'FocusScope' present as
-- exactly one stop from outside.
isFocusable :: FocusPolicy -> Bool
isFocusable NotFocusable  = False
isFocusable Focusable     = True
isFocusable FocusScope {} = True

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all, via
-- @ccFocusPolicy@.
autoClaimsFocus :: ControlConfig e msg -> Bool
autoClaimsFocus cc = isFocusable (ccFocusPolicy cc)

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
-- (the placeholder 'runFocusScope' seeds for 'WrapStop' -- see there)
-- means there's no real predecessor to hand off to in this scope; that
-- case leaves the key un-consumed instead of swallowing it, so whatever
-- encloses this scope gets a chance to react to it once this scope
-- closes.
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
-- @ccFocusPolicy@ makes it a focus target) is a direct 'UiEffect', read
-- off its own 'ciMouseDown' -- so focus moves on press, before any drag or
-- release decides whether the press itself counts as a click.
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
      renderStyled m s (ccContent cc (noInteraction s))
      pure (noInteraction s)

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
      -- A 'FocusScope' control owns no Tab\/Shift-Tab reaction of its own
      -- here -- see 'runFocusScope'. Everything else reacts exactly as it
      -- always has.
      case ccFocusPolicy cc of
        FocusScope {} -> pure ()
        _             -> applyNavigationKeys wasFocused
      nowFocused <- isFocused eid
      fireFocusChangeDirect cc (focusTransition wasFocused nowFocused)
      (m, styles) <- getStyleSet styleKey
      hitBounds   <- marginInsetBounds m
      raw         <- withBounds hitBounds (watchInteraction eid disabled (styleBase styles))
      when (ciMouseDown raw && isFocusable (ccFocusPolicy cc)) (emitUi (Focus currentScope eid))
      let active = intrinsicStates disabled raw `Set.union` ccActiveStates cc
          s      = resolveStyle styles active
      let final = raw { ciStyle = s }
      renderStyled m s (applyFocusPolicy eid (ccFocusPolicy cc) (ccContent cc final))
      when (isFocusable (ccFocusPolicy cc) && not disabled) (setPreviousTabStop eid)
      pure final

    -- The raw hover\/mouse-button\/keyboard\/focus watching every
    -- identified control does, regardless of its focus-management or
    -- chrome -- assembles each watcher's own report into one
    -- 'ControlInteraction' (@placeholderStyle@ standing in for 'ciStyle'
    -- until the caller resolves and overwrites it, since style resolution
    -- itself depends on these flags), fires @cc@'s own handlers, and
    -- reports the full picture.
    watchInteraction eid disabled placeholderStyle = do
      -- Read before anything else this frame (in particular, before
      -- 'watchHover' can freshly 'acquireCapture') so it reflects capture
      -- as of the *start* of the frame -- see 'ciWasDragging'.
      wasDragging <- isDragging eid
      hit         <- isRegionHit
      let eligible = not disabled && hit
      hoverI <- watchHover eid eligible
      mouseI <- watchMouseButton eid (ccMouseActivation cc) eligible
      focusI <- watchFocus eid disabled
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
            , ciWasDragging  = wasDragging
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
-- itself when clicked -- e.g. 'Blink.View.Controls.Label.label' redirecting
-- onto its 'Blink.View.Controls.Label.target'.
focusTargetOnClick :: Maybe e -> e -> ControlInteraction e msg -> View e msg ()
focusTargetOnClick scope target ci = when (ciClicked ci) (emitUi (Focus scope target))
