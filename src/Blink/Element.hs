{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The low-level interaction primitive beneath
-- "Blink.Control".'Blink.Control.control': 'element' observes and reports
-- what happened to one element this frame -- hover, mouse button, and key
-- events -- by firing whichever of @attrs@'s reactions match. No focus
-- claiming, no tab navigation, no activation\/click semantics: those are
-- built on top of the events this raises, by a higher-level combinator such
-- as 'Blink.Control.control'.
module Blink.Element
  ( EventHandler
  , KeyEventHandler
  , HasElementEvents (..)
  , ElementAttrs
  , element
  , fireFocusChange
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Control.Monad (when)
import Data.List (foldl')

import Blink.Input (KeyEvent (..), InputState (..))
import Blink.Mouse (ButtonState (..), Mouse (..), captureOf)
import Blink.UI

-- | A reaction to one of the eight events every element can raise --
-- @()@-shaped, since every one of them but 'onKeyPressed' carries no
-- payload of its own.
type EventHandler e msg = () -> [Out e msg]

-- | A reaction to 'onKeyPressed', with the triggering 'KeyEvent'.
type KeyEventHandler e msg = KeyEvent -> [Out e msg]

-- | Implemented by any attrs type that carries the raw mouse\/keyboard\/
-- focus reactions every element can raise -- built from @mk@\/@match@ pairs
-- rather than plain accessors, so a value can be both constructed /and/
-- generically inspected. 'Blink.Control.HasControlConfig' and friends use
-- the same shape; 'Blink.Control.ControlAttrs' and every ready-made
-- widget's own attrs type all implement this alongside their own
-- capabilities, which is what lets 'element' and 'Blink.Control.control'
-- both dispatch against whichever concrete attrs type a caller built.
class HasElementEvents e msg cfg | cfg -> e msg where
  mkOnClicked :: EventHandler e msg -> cfg
  matchOnClicked :: cfg -> Maybe (EventHandler e msg)
  mkOnFocusGained :: EventHandler e msg -> cfg
  matchOnFocusGained :: cfg -> Maybe (EventHandler e msg)
  mkOnFocusLost :: EventHandler e msg -> cfg
  matchOnFocusLost :: cfg -> Maybe (EventHandler e msg)
  mkOnMouseEntered :: EventHandler e msg -> cfg
  matchOnMouseEntered :: cfg -> Maybe (EventHandler e msg)
  mkOnMouseExited :: EventHandler e msg -> cfg
  matchOnMouseExited :: cfg -> Maybe (EventHandler e msg)
  mkOnMouseDown :: EventHandler e msg -> cfg
  matchOnMouseDown :: cfg -> Maybe (EventHandler e msg)
  mkOnMouseUp :: EventHandler e msg -> cfg
  matchOnMouseUp :: cfg -> Maybe (EventHandler e msg)
  mkOnKeyPressed :: KeyEventHandler e msg -> cfg
  matchOnKeyPressed :: cfg -> Maybe (KeyEventHandler e msg)

-- | Reacts when the press-and-release cycle completes on this element --
-- the mouse button coming up while the element is hit, /and/ this element
-- is the one that captured the original press. Fires alongside
-- 'onMouseUp' for the element the press-and-release cycle actually
-- completed on, but not for one that merely happened to be hit when a drag
-- begun elsewhere was released. A higher layer still decides whether this
-- (or some other activation key) counts as "activated" -- this is the
-- mouse-specific half of that fact, already checked against capture so the
-- caller doesn't have to.
onClicked :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onClicked = mkOnClicked

-- | Reacts when the element is named the winner of a focus transfer.
-- 'element' only observes this -- it never claims focus itself;
-- auto-claim\/self-clear are a different, simpler case handled entirely by
-- whichever code performs them directly (see 'Blink.UI.getFocusChange').
onFocusGained :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onFocusGained = mkOnFocusGained

-- | Reacts when the element loses focus, whether to a transfer or a clear.
onFocusLost :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onFocusLost = mkOnFocusLost

-- | Reacts when the mouse starts being over the element this frame.
onMouseEntered :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseEntered = mkOnMouseEntered

-- | Reacts when the mouse stops being over the element this frame.
onMouseExited :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseExited = mkOnMouseExited

-- | Reacts when the mouse button goes down while the element is hit.
onMouseDown :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseDown = mkOnMouseDown

-- | Reacts when the mouse button comes up while the element is hit --
-- fires regardless of which element (if any) holds capture, so a drag
-- begun on a different element and released here still reports this for
-- this element; a higher layer decides whether that counts as a click
-- (see 'onClicked').
onMouseUp :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseUp = mkOnMouseUp

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasElementEvents e msg cfg => KeyEventHandler e msg -> cfg
onKeyPressed = mkOnKeyPressed

-- | 'Blink.Element'\'s own closed attrs type -- one constructor per raw
-- event, and nothing else. For anything that wants bare raw-event
-- reporting with no control chrome\/focus\/tab behaviour on top; every
-- ready-made widget instead builds its own richer attrs type on
-- 'HasElementEvents' (see "Blink.Control").
data ElementAttrs e msg
  = ElementOnClicked (EventHandler e msg)
  | ElementOnFocusGained (EventHandler e msg)
  | ElementOnFocusLost (EventHandler e msg)
  | ElementOnMouseEntered (EventHandler e msg)
  | ElementOnMouseExited (EventHandler e msg)
  | ElementOnMouseDown (EventHandler e msg)
  | ElementOnMouseUp (EventHandler e msg)
  | ElementOnKeyPressed (KeyEventHandler e msg)

instance HasElementEvents e msg (ElementAttrs e msg) where
  mkOnClicked = ElementOnClicked
  matchOnClicked (ElementOnClicked f) = Just f
  matchOnClicked _ = Nothing
  mkOnFocusGained = ElementOnFocusGained
  matchOnFocusGained (ElementOnFocusGained f) = Just f
  matchOnFocusGained _ = Nothing
  mkOnFocusLost = ElementOnFocusLost
  matchOnFocusLost (ElementOnFocusLost f) = Just f
  matchOnFocusLost _ = Nothing
  mkOnMouseEntered = ElementOnMouseEntered
  matchOnMouseEntered (ElementOnMouseEntered f) = Just f
  matchOnMouseEntered _ = Nothing
  mkOnMouseExited = ElementOnMouseExited
  matchOnMouseExited (ElementOnMouseExited f) = Just f
  matchOnMouseExited _ = Nothing
  mkOnMouseDown = ElementOnMouseDown
  matchOnMouseDown (ElementOnMouseDown f) = Just f
  matchOnMouseDown _ = Nothing
  mkOnMouseUp = ElementOnMouseUp
  matchOnMouseUp (ElementOnMouseUp f) = Just f
  matchOnMouseUp _ = Nothing
  mkOnKeyPressed = ElementOnKeyPressed
  matchOnKeyPressed (ElementOnKeyPressed f) = Just f
  matchOnKeyPressed _ = Nothing

-- | Every raw event's resolved handler list, folded from an attrs list by
-- 'resolveElementConfig'.
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

-- | Resolves a @['ElementAttrs' e msg]@ by folding every entry over
-- 'defaultElementConfig' left to right -- each constructor appends to its
-- own event's handler list, so every handler given for the same event
-- still fires.
resolveElementConfig :: [ElementAttrs e msg] -> ElementConfig e msg
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

-- | Dispatches every handler in @hs@ against @a@.
runHandlers :: [a -> [Out e msg]] -> a -> UI e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | Fires 'onFocusGained'\/'onFocusLost' directly from a was\/now focus
-- transition, without waiting for 'element's own frame observation to
-- catch up -- for a caller (e.g. 'Blink.Control.control') that needs to
-- report a focus change it just caused itself this same frame, rather than
-- via 'Blink.UI.getFocusChange' (which only reflects it starting next
-- frame).
fireFocusChange :: [ElementAttrs e msg] -> Bool -> Bool -> UI e msg ()
fireFocusChange attrs was now = do
  let ec = resolveElementConfig attrs
  when (not was && now) $ runHandlers (ecOnFocusGained ec) ()
  when (was && not now) $ runHandlers (ecOnFocusLost ec) ()

-- | Observes and reports this frame's raw interaction events for the given
-- element, firing each against @attrs@'s resolved reactions. A
-- higher-level combinator (e.g. 'Blink.Control.control') that needs to
-- make its own decisions (e.g. "was this element just clicked") queries
-- the same underlying "Blink.UI" primitives directly, independent of this
-- call, rather than consuming anything back from it.
element :: Ord e => e -> [ElementAttrs e msg] -> UI e msg ()
element eid attrs = do
  let ec = resolveElementConfig attrs
  hoverStep ec eid
  buttonStep ec eid
  keyStep ec eid
  focusStep ec eid

-- | Advances this element's hover state for the current frame (still doing
-- the 'registerMouseOver'\/'acquireCapture' bookkeeping other elements'
-- contest checks depend on) and fires the hover edge, if any. Disabled
-- elements never register as hovered, matching
-- 'Blink.Control.styledElement'.
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

-- | Fires the button edge for this element: hit and enabled only, with no
-- capture-contest check on a release itself -- a drag started on a
-- different element still fires 'onMouseUp' here if the button comes up
-- while this element is hit. 'onClicked' is the one exception: it only
-- fires alongside 'onMouseUp' when this element also holds capture (i.e.
-- the press started here too), which is exactly why @eid@ -- unused by the
-- rest of this step -- is needed here.
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

-- | Fires 'onFocusGained'\/'onFocusLost' from the most recent
-- 'Blink.UI.getFocusChange' still visible to this element's scope, if this
-- element is either side of it. Not disabled-gated, unlike the other
-- steps -- a disabled element still observing its own focus loss is
-- consistent with 'element' reporting facts truthfully, and the case that
-- would otherwise need suppressing (the element removed from the tree
-- entirely) already has no attrs list to fire against regardless.
focusStep :: Eq e => ElementConfig e msg -> e -> UI e msg ()
focusStep ec eid = do
  change <- getFocusChange
  case change of
    Just fc
      | focusChangeTo   fc == Just eid -> runHandlers (ecOnFocusGained ec) ()
      | focusChangeFrom fc == Just eid -> runHandlers (ecOnFocusLost ec) ()
    _ -> pure ()
