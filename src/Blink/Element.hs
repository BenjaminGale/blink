{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The low-level interaction primitive beneath
-- "Blink.Control".'Blink.Control.control': 'element' observes and reports
-- what happened to one element this frame -- hover, mouse button, and key
-- events -- as plain 'ElementEvent's, firing whichever of @attrs@'s
-- reactions match. No focus claiming, no tab navigation, no
-- activation\/click semantics: those are built on top of the events this
-- returns, by a higher-level combinator such as 'Blink.Control.control'.
module Blink.Element
  ( ElementEvent (..)
  , HasElementEvents (..)
  , ElementAttrs
  , element
  , fire
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

-- | A low-level interaction event 'element' fires: a purely observed fact
-- about mouse, keyboard, or focus state relative to one element, with no
-- interpretation of what it means (no click\/activation semantics, no focus
-- claiming). Higher-level combinators (e.g. 'Blink.Control.control') build
-- meaningful behaviour on top of these.
data ElementEvent
  = MouseEntered
    -- ^ The mouse just started being over the element this frame.
  | MouseExited
    -- ^ The mouse just stopped being over the element this frame.
  | MouseDown
    -- ^ The mouse button just went down while the element was hit.
  | MouseUp
    -- ^ The mouse button just came up while the element was hit -- fires
    -- regardless of which element (if any) holds capture, so a drag begun
    -- on a different element and released here still reports this for this
    -- element; a higher layer decides whether that counts as a click.
  | Clicked
    -- ^ The mouse button just came up while the element was hit, /and/ this
    -- element is the one that captured the original press -- fires
    -- alongside 'MouseUp' for the element the press-and-release cycle
    -- actually completed on, but not for one that merely happened to be
    -- hit when a drag begun elsewhere was released. A higher layer still
    -- decides whether this (or some other activation key) counts as
    -- "activated" -- this is the mouse-specific half of that fact, already
    -- checked against capture so the caller doesn't have to.
  | KeyPressed KeyEvent
    -- ^ A key event this frame, while the element holds focus.
  | FocusGained
    -- ^ This element was just named the winner of a 'Blink.UI.Focus'
    -- request. 'element' only observes this -- it never claims focus
    -- itself; auto-claim\/self-clear are a different, simpler case handled
    -- entirely by whichever code performs them directly (see
    -- 'Blink.UI.getFocusChange').
  | FocusLost
    -- ^ This element was just displaced by a 'Blink.UI.Focus' request, or
    -- cleared by a 'Blink.UI.ClearFocus' request.
  deriving (Eq, Show)

-- | Implemented by any attrs type that carries the raw mouse\/keyboard\/
-- focus reactions every element can raise -- built from @mk@\/@match@ pairs
-- rather than plain accessors, so a value can be both constructed /and/
-- generically inspected. 'Blink.Control.HasControlConfig' and friends use
-- the same shape; 'Blink.Control.ControlAttrs' and every ready-made
-- widget's own attrs type all implement this alongside their own
-- capabilities, which is what lets 'element' (via 'fire') and
-- 'Blink.Control.control' both dispatch against whichever concrete attrs
-- type a caller built.
class HasElementEvents e msg cfg | cfg -> e msg where
  mkOnClicked :: (() -> [Out e msg]) -> cfg
  matchOnClicked :: cfg -> Maybe (() -> [Out e msg])
  mkOnFocusGained :: (() -> [Out e msg]) -> cfg
  matchOnFocusGained :: cfg -> Maybe (() -> [Out e msg])
  mkOnFocusLost :: (() -> [Out e msg]) -> cfg
  matchOnFocusLost :: cfg -> Maybe (() -> [Out e msg])
  mkOnMouseEntered :: (() -> [Out e msg]) -> cfg
  matchOnMouseEntered :: cfg -> Maybe (() -> [Out e msg])
  mkOnMouseExited :: (() -> [Out e msg]) -> cfg
  matchOnMouseExited :: cfg -> Maybe (() -> [Out e msg])
  mkOnMouseDown :: (() -> [Out e msg]) -> cfg
  matchOnMouseDown :: cfg -> Maybe (() -> [Out e msg])
  mkOnMouseUp :: (() -> [Out e msg]) -> cfg
  matchOnMouseUp :: cfg -> Maybe (() -> [Out e msg])
  mkOnKeyPressed :: (KeyEvent -> [Out e msg]) -> cfg
  matchOnKeyPressed :: cfg -> Maybe (KeyEvent -> [Out e msg])

-- | Reacts when the press-and-release cycle completes on this element. See
-- 'Clicked'.
onClicked :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onClicked = mkOnClicked

-- | Reacts when the element is named the winner of a focus transfer. See
-- 'FocusGained'.
onFocusGained :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onFocusGained = mkOnFocusGained

-- | Reacts when the element loses focus, whether to a transfer or a clear.
-- See 'FocusLost'.
onFocusLost :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onFocusLost = mkOnFocusLost

-- | Reacts when the mouse starts being over the element. See 'MouseEntered'.
onMouseEntered :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onMouseEntered = mkOnMouseEntered

-- | Reacts when the mouse stops being over the element. See 'MouseExited'.
onMouseExited :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onMouseExited = mkOnMouseExited

-- | Reacts when the mouse button goes down while the element is hit. See
-- 'MouseDown'.
onMouseDown :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onMouseDown = mkOnMouseDown

-- | Reacts when the mouse button comes up while the element is hit. See
-- 'MouseUp'.
onMouseUp :: HasElementEvents e msg cfg => (() -> [Out e msg]) -> cfg
onMouseUp = mkOnMouseUp

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'. See 'KeyPressed'.
onKeyPressed :: HasElementEvents e msg cfg => (KeyEvent -> [Out e msg]) -> cfg
onKeyPressed = mkOnKeyPressed

-- | 'Blink.Element'\'s own closed attrs type -- one constructor per raw
-- event, and nothing else. For anything that wants bare raw-event
-- reporting with no control chrome\/focus\/tab behaviour on top; every
-- ready-made widget instead builds its own richer attrs type on
-- 'HasElementEvents' (see "Blink.Control").
data ElementAttrs e msg
  = ElementOnClicked (() -> [Out e msg])
  | ElementOnFocusGained (() -> [Out e msg])
  | ElementOnFocusLost (() -> [Out e msg])
  | ElementOnMouseEntered (() -> [Out e msg])
  | ElementOnMouseExited (() -> [Out e msg])
  | ElementOnMouseDown (() -> [Out e msg])
  | ElementOnMouseUp (() -> [Out e msg])
  | ElementOnKeyPressed (KeyEvent -> [Out e msg])

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
  { ecOnClicked      :: [() -> [Out e msg]]
  , ecOnFocusGained  :: [() -> [Out e msg]]
  , ecOnFocusLost    :: [() -> [Out e msg]]
  , ecOnMouseEntered :: [() -> [Out e msg]]
  , ecOnMouseExited  :: [() -> [Out e msg]]
  , ecOnMouseDown    :: [() -> [Out e msg]]
  , ecOnMouseUp      :: [() -> [Out e msg]]
  , ecOnKeyPressed   :: [KeyEvent -> [Out e msg]]
  }

defaultElementConfig :: ElementConfig e msg
defaultElementConfig = ElementConfig [] [] [] [] [] [] [] []

-- | Resolves an attrs list of /any/ type implementing 'HasElementEvents' by
-- folding left to right -- each entry matches exactly one of the eight
-- capabilities (every other @match@ call returns 'Nothing' for it), so this
-- works generically without knowing the attrs type's concrete shape.
resolveElementConfig :: HasElementEvents e msg cfg => [cfg] -> ElementConfig e msg
resolveElementConfig = foldl' apply defaultElementConfig
  where
    apply ec c = ElementConfig
      { ecOnClicked      = extend ecOnClicked      matchOnClicked
      , ecOnFocusGained  = extend ecOnFocusGained  matchOnFocusGained
      , ecOnFocusLost    = extend ecOnFocusLost    matchOnFocusLost
      , ecOnMouseEntered = extend ecOnMouseEntered matchOnMouseEntered
      , ecOnMouseExited  = extend ecOnMouseExited  matchOnMouseExited
      , ecOnMouseDown    = extend ecOnMouseDown    matchOnMouseDown
      , ecOnMouseUp      = extend ecOnMouseUp      matchOnMouseUp
      , ecOnKeyPressed   = extend ecOnKeyPressed   matchOnKeyPressed
      }
      where extend field match = maybe (field ec) (\f -> field ec ++ [f]) (match c)

-- | Dispatches every handler @ev@'s own resolved list accumulated, against
-- @ev@'s payload (@()@ for every event but 'KeyPressed').
fireEvent :: ElementConfig e msg -> ElementEvent -> UI e msg ()
fireEvent ec ev = case ev of
  Clicked      -> run ecOnClicked      ()
  FocusGained  -> run ecOnFocusGained  ()
  FocusLost    -> run ecOnFocusLost    ()
  MouseEntered -> run ecOnMouseEntered ()
  MouseExited  -> run ecOnMouseExited  ()
  MouseDown    -> run ecOnMouseDown    ()
  MouseUp      -> run ecOnMouseUp      ()
  KeyPressed k -> run ecOnKeyPressed   k
  where
    run field a = mapM_ dispatch (concatMap ($ a) (field ec))
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | Resolves @attrs@ once, then fires each of @evs@ against it in turn --
-- for a caller (e.g. 'Blink.Control.control') that needs to raise specific
-- 'ElementEvent's of its own choosing, on top of what 'element' reports for
-- the current frame.
fire :: HasElementEvents e msg cfg => [cfg] -> [ElementEvent] -> UI e msg ()
fire attrs evs = do
  let ec = resolveElementConfig attrs
  mapM_ (fireEvent ec) evs

-- | Observes and reports this frame's raw interaction events for the given
-- element (see 'ElementEvent'), firing each against @attrs@'s resolved
-- reactions. A higher-level combinator (e.g. 'Blink.Control.control') that
-- needs to make its own decisions (e.g. "was this element just clicked")
-- queries the same underlying "Blink.UI" primitives directly, independent
-- of this call, rather than consuming anything back from it.
element :: (Ord e, HasElementEvents e msg cfg) => e -> [cfg] -> UI e msg ()
element eid attrs = do
  hoverEvs  <- hoverStep eid
  buttonEvs <- buttonStep eid
  keyEvs    <- keyStep eid
  focusEvs  <- focusStep eid
  fire attrs (hoverEvs ++ buttonEvs ++ keyEvs ++ focusEvs)

-- | Advances this element's hover state for the current frame (still doing
-- the 'registerMouseOver'\/'acquireCapture' bookkeeping other elements'
-- contest checks depend on) and reports the hover edge, if any. Disabled
-- elements never register as hovered, matching
-- 'Blink.Control.styledElement'.
hoverStep :: Ord e => e -> UI e msg [ElementEvent]
hoverStep eid = do
  disabled <- isDisabled
  hit      <- isRegionHit
  let isOver = not disabled && hit
  when isOver $ do
    registerMouseOver eid
    acquireCapture eid
  wasOver <- wasMouseOverLastFrame eid
  pure $ concat
    [ [MouseEntered | not wasOver && isOver]
    , [MouseExited  | wasOver && not isOver]
    ]

-- | Reports the button edge for this element: hit and enabled only, with no
-- capture-contest check on 'MouseUp' itself -- a drag started on a
-- different element still reports 'MouseUp' here if the button comes up
-- while this element is hit. 'Clicked' is the one exception: it only fires
-- alongside 'MouseUp' when this element also holds capture (i.e. the press
-- started here too), which is exactly why @eid@ -- unused by the rest of
-- this step -- is needed here.
buttonStep :: Ord e => e -> UI e msg [ElementEvent]
buttonStep eid = do
  disabled <- isDisabled
  hit      <- isRegionHit
  mouse    <- getMouse
  let eligible     = not disabled && hit
      capturedByMe = captureOf (mouseButton mouse) == MouseCapturedBy eid
  pure $ if not eligible
    then []
    else case mouseButton mouse of
      ButtonDown     _ -> [MouseDown]
      ButtonReleased _ | capturedByMe -> [MouseUp, Clicked]
                       | otherwise    -> [MouseUp]
      _                -> []

-- | Reports one 'KeyPressed' per key event this frame while the element
-- holds focus and is enabled.
keyStep :: Ord e => e -> UI e msg [ElementEvent]
keyStep eid = do
  disabled <- isDisabled
  focused  <- isFocused eid
  input    <- getInput
  pure $ if not disabled && focused
    then map KeyPressed (inputKeyEvents input)
    else []

-- | Reports 'FocusGained'\/'FocusLost' from the most recent
-- 'Blink.UI.getFocusChange' still visible to this element's scope, if this
-- element is either side of it. Not disabled-gated, unlike the other
-- steps -- a disabled element still observing its own focus loss is
-- consistent with 'element' reporting facts truthfully, and the case that
-- would otherwise need suppressing (the element removed from the tree
-- entirely) already has no attrs list to fire against regardless.
focusStep :: Eq e => e -> UI e msg [ElementEvent]
focusStep eid = do
  change <- getFocusChange
  pure $ case change of
    Just fc
      | focusChangeTo   fc == Just eid -> [FocusGained]
      | focusChangeFrom fc == Just eid -> [FocusLost]
    _ -> []
