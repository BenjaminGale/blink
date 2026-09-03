{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The lowest of the two building-block layers every widget in
-- "Blink.Controls" is built on -- see "Blink.Controls.Control" for the one
-- above it. Re-exports "Blink.Attribute"'s 'Attribute'\/'resolve' mechanism, which
-- every layer here (and "Blink.Layout.Box") is configured through.
--
-- = The attribute mechanism
--
-- Each layer's own config type gets a one-method typeclass
-- (@HasElementConfig@, 'Blink.Controls.Control.HasControlConfig') so an
-- attribute defined once against that layer's config (e.g. 'onClicked'
-- against 'ElementConfig') can be applied to any config that nests one,
-- however deep, via the class's @over...@ method. A config that itself
-- /is/ the target type delegates with 'id'; a config that nests one
-- delegates by rewriting just that field. 'over...' composes through
-- arbitrary nesting depth, so an attribute's reach is decided by where its
-- field sits, not by which instances a widget declines to declare.
--
-- = Element
--
-- An element ('elementBase') watches whether the pointer is over it,
-- whether a mouse button is pressed or released on it, what keys are typed
-- while it holds focus, and whether focus moves onto or off of it, and
-- returns all of that as an 'ElementInteraction' -- firing the matching
-- handler from its 'ElementConfig' for each, once, in one place, after
-- every flag has been computed.
module Blink.Controls.Element
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

    -- ** Element attributes
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  , mouseActivation

    -- * Handler plumbing
  , runHandlers
  , post
  , postWith

    -- * Shared with Control
  , isMouseFreeFor
  ) where

import Control.Monad (when)

import Blink.Attribute (Attribute (..), resolve)
import Blink.Input (ButtonState (..), InputState (..), KeyEvent (..), Mouse (..), captureOf)
import Blink.UI

-- * Element

-- | A handler for an element event with no data of its own.
type EventHandler e msg = () -> [Out e msg]

-- | A handler for 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Out e msg]

-- | How an element's own mouse-button activity translates into
-- 'eiClicked' -- and, transitively, into "Blink.Controls.Control"'s
-- click-to-focus. A control declares this once, up front, as part of its
-- own interaction model; it is never inferred from what a particular drag
-- happened to do.
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
    -- 'Blink.Controls.Slider.slider'), the value has already changed by
    -- the time the button comes up, so the release should count regardless
    -- of where the pointer ends up -- plain capture without this would
    -- just be a drag with no activation at all.
  deriving (Eq, Show)

-- | One element's handlers, grouped by event, plus its 'MouseActivation'.
data ElementConfig e msg = ElementConfig
  { ecOnMouseEntered  :: [EventHandler e msg]
  , ecOnMouseExited   :: [EventHandler e msg]
  , ecOnMouseDown     :: [EventHandler e msg]
  , ecOnMouseUp       :: [EventHandler e msg]
  , ecOnClicked       :: [EventHandler e msg]
  , ecOnKeyPressed    :: [KeyEventHandler e msg]
  , ecOnFocusGained   :: [EventHandler e msg]
  , ecOnFocusLost     :: [EventHandler e msg]
  , ecMouseActivation :: MouseActivation
  }

-- | Every handler field empty, 'ClickActivated' for 'ecMouseActivation'.
defaultElementConfig :: ElementConfig e msg
defaultElementConfig = ElementConfig
  { ecOnMouseEntered  = []
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
-- 'Blink.Controls.Button.onActivated' for the event that also fires on
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
runHandlers :: [a -> [Out e msg]] -> a -> UI e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | Builds a reaction (an 'EventHandler'\/'Blink.Controls.Toggle.onSelectedChanged'-shaped
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
-- Shared with "Blink.Controls.Control", whose own auto-claim logic needs
-- the same check.
isMouseFreeFor :: Eq e => e -> UI e msg Bool
isMouseFreeFor eid = do
  capturedByMe <- isDragging eid
  (|| capturedByMe) <$> isMouseFree

-- | Watches this element's hover, mouse-button, keyboard, and focus
-- activity for the current frame -- within whatever bounds and scope its
-- caller has established via 'withBounds' -- and calls the matching
-- handler in @ec@ for each. Returns the full picture as an
-- 'ElementInteraction' so a caller built on top (e.g.
-- 'Blink.Controls.Control.controlBase') can read the same flags back
-- without re-querying the context itself. @ec@'s own 'ecMouseActivation'
-- decides what counts as a click -- see 'MouseActivation'.
elementBase :: Ord e => e -> ElementConfig e msg -> UI e msg ElementInteraction
elementBase eid ec = do
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
watchHover :: Ord e => e -> Bool -> UI e msg ElementInteraction
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
watchMouseButton :: Eq e => e -> MouseActivation -> Bool -> UI e msg ElementInteraction
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
watchFocus :: Eq e => e -> Bool -> UI e msg ElementInteraction
watchFocus eid disabled = do
  focused <- isFocused eid
  input   <- getInput
  let keysPressed = if not disabled && focused then inputKeyEvents input else []

  change <- getFocusChange
  let focusGained = maybe False (\fc -> focusChangeTo fc == Just eid) change
      focusLost   = maybe False (\fc -> focusChangeFrom fc == Just eid) change

  pure mempty
    { eiFocused     = focused
    , eiFocusGained = focusGained
    , eiFocusLost   = focusLost
    , eiKeysPressed = keysPressed
    }

-- | Fires the handler matching each flag set on @ei@ -- the one place
-- 'elementBase' dispatches anything, run once every flag has been
-- computed.
fireElementEvents :: ElementConfig e msg -> ElementInteraction -> UI e msg ()
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
