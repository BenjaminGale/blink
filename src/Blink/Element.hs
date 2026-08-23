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
  , ElementEvents
  , HasElementEvents (..)
  , ElementAttrs
  , FocusTransition (..)
  , focusTransition
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
  , fireClick
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

-- | One of the eight raw reactions every element can raise -- the single
-- opaque payload every attrs type carries one of via 'HasElementEvents',
-- instead of each attrs type repeating its own parallel set of eight
-- constructors. Fully opaque: nothing outside this module ever needs to
-- inspect one directly -- 'fireClick' is how a caller (e.g.
-- 'Blink.Button.buttonBase', re-firing 'onClicked' on Enter) reaches the
-- handlers inside without needing accessors of its own.
data ElementEvents e msg
  = ElementOnClicked (EventHandler e msg)
  | ElementOnFocusGained (EventHandler e msg)
  | ElementOnFocusLost (EventHandler e msg)
  | ElementOnMouseEntered (EventHandler e msg)
  | ElementOnMouseExited (EventHandler e msg)
  | ElementOnMouseDown (EventHandler e msg)
  | ElementOnMouseUp (EventHandler e msg)
  | ElementOnKeyPressed (KeyEventHandler e msg)

-- | Implemented by any attrs type that carries the raw mouse\/keyboard\/
-- focus reactions every element can raise -- one @configure@\/@extract@
-- pair over the shared 'ElementEvents', rather than one pair per
-- individual event, so a value can be both constructed /and/ generically
-- inspected. 'Blink.Control.ControlAttrs' and every ready-made widget's own
-- attrs type all implement this alongside their own capabilities, which is
-- what lets 'element' and 'Blink.Control.control' both dispatch against
-- whichever concrete attrs type a caller built.
class HasElementEvents e msg cfg | cfg -> e msg where
  configureElementEvent :: ElementEvents e msg -> cfg
  extractElementEvent :: cfg -> Maybe (ElementEvents e msg)

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
onClicked = configureElementEvent . ElementOnClicked

-- | Reacts when the element is named the winner of a focus transfer.
-- 'element' only observes this -- it never claims focus itself;
-- auto-claim\/self-clear are a different, simpler case handled entirely by
-- whichever code performs them directly (see 'Blink.UI.getFocusChange').
onFocusGained :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onFocusGained = configureElementEvent . ElementOnFocusGained

-- | Reacts when the element loses focus, whether to a transfer or a clear.
onFocusLost :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onFocusLost = configureElementEvent . ElementOnFocusLost

-- | Reacts when the mouse starts being over the element this frame.
onMouseEntered :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseEntered = configureElementEvent . ElementOnMouseEntered

-- | Reacts when the mouse stops being over the element this frame.
onMouseExited :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseExited = configureElementEvent . ElementOnMouseExited

-- | Reacts when the mouse button goes down while the element is hit.
onMouseDown :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseDown = configureElementEvent . ElementOnMouseDown

-- | Reacts when the mouse button comes up while the element is hit --
-- fires regardless of which element (if any) holds capture, so a drag
-- begun on a different element and released here still reports this for
-- this element; a higher layer decides whether that counts as a click
-- (see 'onClicked').
onMouseUp :: HasElementEvents e msg cfg => EventHandler e msg -> cfg
onMouseUp = configureElementEvent . ElementOnMouseUp

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'.
onKeyPressed :: HasElementEvents e msg cfg => KeyEventHandler e msg -> cfg
onKeyPressed = configureElementEvent . ElementOnKeyPressed

-- | 'Blink.Element'\'s own closed attrs type -- literally 'ElementEvents'
-- itself, and nothing else. For anything that wants bare raw-event
-- reporting with no control chrome\/focus\/tab behaviour on top; every
-- ready-made widget instead builds its own richer attrs type on
-- 'HasElementEvents' (see "Blink.Control").
type ElementAttrs e msg = ElementEvents e msg

instance HasElementEvents e msg (ElementEvents e msg) where
  configureElementEvent = id
  extractElementEvent = Just

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

-- | Resolves a @['ElementEvents' e msg]@ by folding every entry over
-- 'defaultElementConfig' left to right -- each constructor appends to its
-- own event's handler list, so every handler given for the same event
-- still fires.
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

-- | Dispatches every handler in @hs@ against @a@.
runHandlers :: [a -> [Out e msg]] -> a -> UI e msg ()
runHandlers hs a = mapM_ dispatch (concatMap ($ a) hs)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

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

-- | Fires 'onFocusGained'\/'onFocusLost' directly from a 'FocusTransition',
-- without waiting for 'element's own frame observation to catch up -- for a
-- caller (e.g. 'Blink.Control.control') that needs to report a focus
-- change it just caused itself this same frame, rather than via
-- 'Blink.UI.getFocusChange' (which only reflects it starting next frame).
fireFocusChange :: [ElementAttrs e msg] -> FocusTransition -> UI e msg ()
fireFocusChange attrs transition = do
  let ec = resolveElementConfig attrs
  case transition of
    GainedFocus    -> runHandlers (ecOnFocusGained ec) ()
    LostFocus      -> runHandlers (ecOnFocusLost ec) ()
    FocusUnchanged -> pure ()

-- | Fires every 'onClicked' handler in @attrs@ directly, the same way a
-- real mouse click would -- for a caller (e.g. 'Blink.Button.buttonBase')
-- that needs to re-fire the same reactions from a different trigger (Enter
-- while focused), without a raw mouse click having actually happened.
fireClick :: [ElementAttrs e msg] -> UI e msg ()
fireClick attrs = runHandlers (ecOnClicked (resolveElementConfig attrs)) ()

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
