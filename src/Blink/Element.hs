-- | The low-level interaction primitive beneath
-- "Blink.Controls".'Blink.Controls.control': 'element' observes and reports
-- what happened to one element this frame -- hover, mouse button, and key
-- events -- as plain 'ElementEvent's, and does nothing else. No focus
-- claiming, no tab navigation, no activation\/click semantics: those are
-- built on top of the events this returns, by a higher-level combinator such
-- as 'Blink.Controls.control'.
module Blink.Element
  ( ElementEvent (..)
  , element
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Control.Monad (when)

import Blink.Attributes (Attr, fire, onEvent)
import Blink.Input (KeyEvent (..), InputState (..))
import Blink.Mouse (ButtonState (..), Mouse (..))
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

-- | Observes and reports this frame's raw interaction events for the given
-- element (see 'ElementEvent'), firing each against the attrs list via
-- 'fire'. A higher-level combinator (e.g. 'Blink.Control.control') that
-- needs to make its own decisions (e.g. "was this element just clicked")
-- queries the same underlying "Blink.UI" primitives directly, independent
-- of this call, rather than consuming anything back from it.
element :: Ord e => e -> [Attr e ElementEvent msg cfg] -> UI e msg ()
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
-- capture-contest check -- a drag started on a different element still
-- reports 'MouseUp' here if the button comes up while this element is hit.
-- @eid@ isn't used -- deliberately: unlike 'hoverStep'\/'keyStep', the
-- button edge is reported for whichever element is hit, with no reference
-- to which element (if any) holds capture. Kept as a parameter for a
-- uniform call shape alongside the other steps.
buttonStep :: Ord e => e -> UI e msg [ElementEvent]
buttonStep _eid = do
  disabled <- isDisabled
  hit      <- isRegionHit
  mouse    <- getMouse
  let eligible = not disabled && hit
  pure $ if not eligible
    then []
    else case mouseButton mouse of
      ButtonDown     _ -> [MouseDown]
      ButtonReleased _ -> [MouseUp]
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
onMouseEntered :: (() -> [Out e msg]) -> Attr e ElementEvent msg cfg
onMouseEntered reaction = onEvent $ \ev -> case ev of
  MouseEntered -> reaction ()
  _            -> []

-- | Reacts when the mouse stops being over the element. See 'MouseExited'.
onMouseExited :: (() -> [Out e msg]) -> Attr e ElementEvent msg cfg
onMouseExited reaction = onEvent $ \ev -> case ev of
  MouseExited -> reaction ()
  _           -> []

-- | Reacts when the mouse button goes down while the element is hit. See
-- 'MouseDown'.
onMouseDown :: (() -> [Out e msg]) -> Attr e ElementEvent msg cfg
onMouseDown reaction = onEvent $ \ev -> case ev of
  MouseDown -> reaction ()
  _         -> []

-- | Reacts when the mouse button comes up while the element is hit. See
-- 'MouseUp'.
onMouseUp :: (() -> [Out e msg]) -> Attr e ElementEvent msg cfg
onMouseUp reaction = onEvent $ \ev -> case ev of
  MouseUp -> reaction ()
  _       -> []

-- | Reacts to a key event while the element holds focus, with the
-- triggering 'KeyEvent'. See 'KeyPressed'.
onKeyPressed :: (KeyEvent -> [Out e msg]) -> Attr e ElementEvent msg cfg
onKeyPressed reaction = onEvent $ \ev -> case ev of
  KeyPressed k -> reaction k
  _            -> []

-- | Reacts when the element is named the winner of a focus transfer. See
-- 'FocusGained'.
onFocusGained :: (() -> [Out e msg]) -> Attr e ElementEvent msg cfg
onFocusGained reaction = onEvent $ \ev -> case ev of
  FocusGained -> reaction ()
  _           -> []

-- | Reacts when the element loses focus, whether to a transfer or a clear.
-- See 'FocusLost'.
onFocusLost :: (() -> [Out e msg]) -> Attr e ElementEvent msg cfg
onFocusLost reaction = onEvent $ \ev -> case ev of
  FocusLost -> reaction ()
  _         -> []
