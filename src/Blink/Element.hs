{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | An element is anything on screen that responds to the mouse or
-- keyboard: a button, a checkbox, a slider, a menu item. Every widget in
-- "Blink.Control" is built out of one or more elements.
--
-- Given an id and a list of handlers, 'element' watches whether the
-- pointer is over that element, whether a mouse button is pressed or
-- released on it, whether a full click happens on it, what keys are typed
-- while it holds focus, and whether focus moves onto or off of it -- and
-- calls the matching handler in @attrs@ for each.
module Blink.Element
  ( -- * Elements
    element

    -- * Events
    -- $events
  , EventHandler
  , KeyEventHandler
  , ElementEvents
  , HasElementEvents (..)
  , ElementAttrs

    -- * Event handlers
    -- ** Mouse
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
    -- ** Keyboard
  , onKeyPressed
    -- ** Focus
  , onFocusGained
  , onFocusLost

    -- * Focus transitions
    -- $focusTransitions
  , FocusTransition (..)
  , focusTransition

    -- * Firing events directly
    -- $fireDirectly
  , fireClick
  , fireFocusChange
  ) where

import Control.Monad (when)
import Data.List (foldl')

import Blink.Input (KeyEvent (..), InputState (..))
import Blink.Mouse (ButtonState (..), Mouse (..), captureOf)
import Blink.UI

-- * Elements

-- | Watches this element's hover, mouse-button, and key activity for the
-- current frame, and calls the matching handler in @attrs@ for each.
element :: Ord e => e -> [ElementAttrs e msg] -> UI e msg ()
element eid attrs = do
  let ec = resolveElementConfig attrs
  hoverStep ec eid
  buttonStep ec eid
  keyStep ec eid
  focusStep ec eid

-- | Fires 'onMouseEntered' when the pointer starts being over this
-- element this frame, and 'onMouseExited' when it stops. A disabled
-- element is never treated as hovered.
hoverStep :: Ord e => ElementConfig e msg -> e -> UI e msg ()
hoverStep ec eid = do
  disabled <- isDisabled
  hit      <- isRegionHit
  let isOver = not disabled && hit
  when isOver $ do
    registerMouseOver eid
    acquireCapture eid
  wasOver <- wasMouseOverLastFrame eid
  when (not wasOver && isOver) $ runHandlers (ecOnMouseEntered ec) ()
  when (wasOver && not isOver) $ runHandlers (ecOnMouseExited ec) ()

-- | Fires 'onMouseDown' when a mouse button is pressed on this element,
-- and 'onMouseUp' when one is released on it, even if the press started
-- elsewhere. Also fires 'onClicked' alongside 'onMouseUp', but only when
-- the press started on this element too.
buttonStep :: Ord e => ElementConfig e msg -> e -> UI e msg ()
buttonStep ec eid = do
  disabled <- isDisabled
  hit      <- isRegionHit
  mouse    <- getMouse
  let eligible     = not disabled && hit
      capturedByMe = captureOf (mouseButton mouse) == MouseCapturedBy eid
  when eligible $ case mouseButton mouse of
    ButtonDown _ -> runHandlers (ecOnMouseDown ec) ()
    ButtonReleased _ -> do
      runHandlers (ecOnMouseUp ec) ()
      when capturedByMe $ runHandlers (ecOnClicked ec) ()
    _ -> pure ()

-- | Fires one 'onKeyPressed' per key event this frame while the element
-- holds focus and is enabled.
keyStep :: Ord e => ElementConfig e msg -> e -> UI e msg ()
keyStep ec eid = do
  disabled <- isDisabled
  focused  <- isFocused eid
  input    <- getInput
  when (not disabled && focused) $
    mapM_ (runHandlers (ecOnKeyPressed ec)) (inputKeyEvents input)

-- | Fires 'onFocusGained' or 'onFocusLost' when this element is the
-- source or target of the most recent focus change. Fires even when the
-- element is disabled.
focusStep :: Eq e => ElementConfig e msg -> e -> UI e msg ()
focusStep ec eid = do
  change <- getFocusChange
  case change of
    Just fc
      | focusChangeTo   fc == Just eid -> runHandlers (ecOnFocusGained ec) ()
      | focusChangeFrom fc == Just eid -> runHandlers (ecOnFocusLost ec) ()
    _ -> pure ()

-- * Events

-- $events
-- Each @on...@ function below builds one of these values. A widget's own
-- attrs type collects them alongside its other capabilities via
-- 'HasElementEvents'.

-- | A handler for an element event with no data of its own.
type EventHandler e msg = () -> [Out e msg]

-- | A handler for 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Out e msg]

-- | One of the events an element can raise: a click, a mouse action, a
-- key press, or a focus change.
data ElementEvents e msg
  = ElementOnClicked (EventHandler e msg)
  | ElementOnFocusGained (EventHandler e msg)
  | ElementOnFocusLost (EventHandler e msg)
  | ElementOnMouseEntered (EventHandler e msg)
  | ElementOnMouseExited (EventHandler e msg)
  | ElementOnMouseDown (EventHandler e msg)
  | ElementOnMouseUp (EventHandler e msg)
  | ElementOnKeyPressed (KeyEventHandler e msg)

-- | Implemented by any attrs type that carries an element's mouse,
-- keyboard, and focus event handlers.
class HasElementEvents e msg cfg | cfg -> e msg where
  configureElementEvent :: ElementEvents e msg -> cfg
  extractElementEvent :: cfg -> Maybe (ElementEvents e msg)

-- | The plain attrs type for an element: nothing but its event handlers.
-- Widgets in "Blink.Control" build their own richer attrs types on
-- 'HasElementEvents' instead.
type ElementAttrs e msg = ElementEvents e msg

instance HasElementEvents e msg (ElementEvents e msg) where
  configureElementEvent = id
  extractElementEvent = Just

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

defaultElementConfig :: ElementConfig e msg
defaultElementConfig = ElementConfig [] [] [] [] [] [] [] []

-- | Resolves a @['ElementEvents' e msg]@ into its per-event handler lists;
-- a handler given multiple times for the same event fires every time.
resolveElementConfig :: [ElementEvents e msg] -> ElementConfig e msg
resolveElementConfig = foldl' apply defaultElementConfig
  where
    apply ec (ElementOnClicked f)      = ec { ecOnClicked = ecOnClicked ec ++ [f] }
    apply ec (ElementOnFocusGained f)  = ec { ecOnFocusGained = ecOnFocusGained ec ++ [f] }
    apply ec (ElementOnFocusLost f)    = ec { ecOnFocusLost = ecOnFocusLost ec ++ [f] }
    apply ec (ElementOnMouseEntered f) = ec { ecOnMouseEntered = ecOnMouseEntered ec ++ [f] }
    apply ec (ElementOnMouseExited f)  = ec { ecOnMouseExited = ecOnMouseExited ec ++ [f] }
    apply ec (ElementOnMouseDown f)    = ec { ecOnMouseDown = ecOnMouseDown ec ++ [f] }
    apply ec (ElementOnMouseUp f)      = ec { ecOnMouseUp = ecOnMouseUp ec ++ [f] }
    apply ec (ElementOnKeyPressed f)   = ec { ecOnKeyPressed = ecOnKeyPressed ec ++ [f] }

-- | Runs every handler in @hs@ on @a@.
runHandlers :: [a -> [Out e msg]] -> a -> UI e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- * Event handlers

-- ** Mouse

-- | Reacts when the mouse starts being over the element this frame.
onMouseEntered :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseEntered = configureElementEvent . ElementOnMouseEntered

-- | Reacts when the mouse stops being over the element this frame.
onMouseExited :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseExited = configureElementEvent . ElementOnMouseExited

-- | Reacts when the mouse button goes down while the element is hit.
onMouseDown :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseDown = configureElementEvent . ElementOnMouseDown

-- | Reacts when the mouse button comes up while the element is hit, even
-- if the press started elsewhere. See 'onClicked' for the click-only
-- version.
onMouseUp :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseUp = configureElementEvent . ElementOnMouseUp

-- | Reacts when the element is clicked: the mouse button pressed and
-- released on it without leaving. Fires alongside 'onMouseUp', but only
-- when the press also started on this element.
onClicked :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onClicked = configureElementEvent . ElementOnClicked

-- ** Keyboard

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasElementEvents e msg cfg => KeyEventHandler e msg -> cfg
onKeyPressed = configureElementEvent . ElementOnKeyPressed

-- ** Focus

-- | Reacts when the element is named the winner of a focus transfer.
onFocusGained :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onFocusGained = configureElementEvent . ElementOnFocusGained

-- | Reacts when the element loses focus, whether to a transfer or a clear.
onFocusLost :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onFocusLost = configureElementEvent . ElementOnFocusLost

-- * Focus transitions

-- $focusTransitions
-- For a caller tracking focus itself across frames: compare last frame's
-- holder to this frame's with 'focusTransition', then fire the result
-- with 'fireFocusChange'.

-- | Which way, if any, focus just moved -- see 'focusTransition'.
data FocusTransition
  = FocusUnchanged
  | GainedFocus
  | LostFocus

-- | Classifies a was\/now pair of focus states into the 'FocusTransition'
-- it represents, for 'fireFocusChange'.
focusTransition :: Bool -> Bool -> FocusTransition
focusTransition was now
  | not was && now = GainedFocus
  | was && not now  = LostFocus
  | otherwise       = FocusUnchanged

-- * Firing events directly

-- $fireDirectly
-- Extension points for widgets built on 'element' to fire these events
-- themselves -- for example, firing a click when Enter is pressed on a
-- focused button.

-- | Fires every 'onClicked' handler in @attrs@, the same way a real mouse
-- click would.
fireClick :: [ElementAttrs e msg] -> UI e msg ()
fireClick attrs = runHandlers (ecOnClicked (resolveElementConfig attrs)) ()

-- | Fires 'onFocusGained'\/'onFocusLost' directly from a given
-- 'FocusTransition', immediately rather than on the next frame.
fireFocusChange :: [ElementAttrs e msg] -> FocusTransition -> UI e msg ()
fireFocusChange attrs transition = do
  let ec = resolveElementConfig attrs
  case transition of
    GainedFocus    -> runHandlers (ecOnFocusGained ec) ()
    LostFocus      -> runHandlers (ecOnFocusLost ec) ()
    FocusUnchanged -> pure ()
