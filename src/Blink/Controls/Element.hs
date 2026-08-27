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
  overElement :: Attribute (ElementConfig e msg) -> Attribute cfg

instance HasElementConfig e msg (ElementConfig e msg) where
  overElement = id

-- | Reacts when the pointer starts being over the element this frame.
onMouseEntered :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseEntered f = overElement (Attribute (\ec -> ec { ecOnMouseEntered = ecOnMouseEntered ec ++ [f] }))

-- | Reacts when the pointer stops being over the element this frame.
onMouseExited :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseExited f = overElement (Attribute (\ec -> ec { ecOnMouseExited = ecOnMouseExited ec ++ [f] }))

-- | Reacts when the mouse button goes down while the element is hit.
onMouseDown :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseDown f = overElement (Attribute (\ec -> ec { ecOnMouseDown = ecOnMouseDown ec ++ [f] }))

-- | Reacts when the mouse button comes up while the element is hit, even
-- if the press started elsewhere. See 'onClicked' for the click-only
-- version.
onMouseUp :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onMouseUp f = overElement (Attribute (\ec -> ec { ecOnMouseUp = ecOnMouseUp ec ++ [f] }))

-- | Reacts when the element is clicked: the mouse button pressed and
-- released on it without leaving. Mouse-only -- see
-- 'Blink.Controls.Button.onActivated' for the event that also fires on
-- Enter while a button-like control holds focus.
onClicked :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onClicked f = overElement (Attribute (\ec -> ec { ecOnClicked = ecOnClicked ec ++ [f] }))

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasElementConfig e msg cfg => KeyEventHandler e msg -> Attribute cfg
onKeyPressed f = overElement (Attribute (\ec -> ec { ecOnKeyPressed = ecOnKeyPressed ec ++ [f] }))

-- | Reacts when the element is named the winner of a focus transfer.
onFocusGained :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onFocusGained f = overElement (Attribute (\ec -> ec { ecOnFocusGained = ecOnFocusGained ec ++ [f] }))

-- | Reacts when the element loses focus, whether to a transfer or a clear.
onFocusLost :: HasElementConfig e msg cfg => EventHandler e msg -> Attribute cfg
onFocusLost f = overElement (Attribute (\ec -> ec { ecOnFocusLost = ecOnFocusLost ec ++ [f] }))

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
-- without re-querying the context itself.
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
