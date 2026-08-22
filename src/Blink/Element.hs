-- | The low-level interaction primitive beneath
-- "Blink.Controls".'Blink.Controls.control': 'element' observes and reports
-- what happened to one element this frame -- hover, mouse button, and key
-- events -- as plain 'ElementEvent's, and does nothing else. No focus
-- claiming, no tab navigation, no activation\/click semantics: those are
-- built on top of the events this returns, by a higher-level combinator such
-- as 'Blink.Controls.control'.
module Blink.Element
  ( ElementEvent (..)
  , HasElementEvent (..)
  , element
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

import Blink.Attributes (Attr, fire, onEvent)
import Blink.Input (KeyEvent (..), InputState (..))
import Blink.Mouse (ButtonState (..), Mouse (..), captureOf)
import Blink.UI

-- | A low-level interaction event 'element' fires: a purely observed fact
-- about mouse, keyboard, or focus state relative to one element, with no
-- interpretation of what it means (no click\/activation semantics, no focus
-- claiming). Higher-level combinators (e.g. 'Blink.Controls.control') build
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

-- | Lets a control's own event type carry 'ElementEvent's alongside its own
-- domain-specific ones (e.g. a toggle control's own selected-changed event),
-- so a single attrs list can react to both with 'onClicked' and friends as
-- well as the control's own reactions -- the same pattern
-- 'Blink.Attributes.HasControlConfig' uses for carrying a shared
-- 'Blink.Attributes.ControlConfig'. 'ElementEvent' itself trivially has an
-- instance, so a control with no events of its own can use 'ElementEvent'
-- directly as its attrs list's event type.
class HasElementEvent ev where
  liftElementEvent  :: ElementEvent -> ev
  matchElementEvent :: ev -> Maybe ElementEvent

instance HasElementEvent ElementEvent where
  liftElementEvent  = id
  matchElementEvent = Just

-- | Observes and reports this frame's raw interaction events for the given
-- element (see 'ElementEvent'), firing each against the attrs list via
-- 'fire'. A higher-level combinator (e.g. 'Blink.Control.control') that
-- needs to make its own decisions (e.g. "was this element just clicked")
-- queries the same underlying "Blink.UI" primitives directly, independent
-- of this call, rather than consuming anything back from it.
element :: (Ord e, HasElementEvent ev) => e -> [Attr e ev msg cfg] -> UI e msg ()
element eid attrs = do
  hoverEvs  <- hoverStep eid
  buttonEvs <- buttonStep eid
  keyEvs    <- keyStep eid
  focusEvs  <- focusStep eid
  fire attrs (map liftElementEvent (hoverEvs ++ buttonEvs ++ keyEvs ++ focusEvs))

-- | Advances this element's hover state for the current frame (still doing
-- the 'registerMouseOver'\/'acquireCapture' bookkeeping other elements'
-- contest checks depend on) and reports the hover edge, if any. Disabled
-- elements never register as hovered, matching
-- 'Blink.Controls.applyMouseOver'.
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

-- | Reacts when the mouse starts being over the element. See 'MouseEntered'.
onMouseEntered :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onMouseEntered reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just MouseEntered -> reaction ()
  _                 -> []

-- | Reacts when the mouse stops being over the element. See 'MouseExited'.
onMouseExited :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onMouseExited reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just MouseExited -> reaction ()
  _                -> []

-- | Reacts when the mouse button goes down while the element is hit. See
-- 'MouseDown'.
onMouseDown :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onMouseDown reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just MouseDown -> reaction ()
  _              -> []

-- | Reacts when the mouse button comes up while the element is hit. See
-- 'MouseUp'.
onMouseUp :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onMouseUp reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just MouseUp -> reaction ()
  _            -> []

-- | Reacts when the press-and-release cycle completes on this element
-- (mouse up while hit, having also captured the press). See 'Clicked'.
onClicked :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onClicked reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just Clicked -> reaction ()
  _            -> []

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'. See 'KeyPressed'.
onKeyPressed :: HasElementEvent ev => (KeyEvent -> [Out e msg]) -> Attr e ev msg cfg
onKeyPressed reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just (KeyPressed k) -> reaction k
  _                   -> []

-- | Reacts when the element is named the winner of a focus transfer. See
-- 'FocusGained'.
onFocusGained :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onFocusGained reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just FocusGained -> reaction ()
  _                -> []

-- | Reacts when the element loses focus, whether to a transfer or a clear.
-- See 'FocusLost'.
onFocusLost :: HasElementEvent ev => (() -> [Out e msg]) -> Attr e ev msg cfg
onFocusLost reaction = onEvent $ \ev -> case matchElementEvent ev of
  Just FocusLost -> reaction ()
  _              -> []
