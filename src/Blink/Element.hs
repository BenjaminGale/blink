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
  ) where

import Control.Monad (when)

import Blink.Attributes (Attr, fire)
import Blink.Input (KeyEvent (..), InputState (..))
import Blink.Mouse (ButtonState (..), Mouse (..))
import Blink.UI

-- | A low-level interaction event 'element' fires: a purely observed fact
-- about mouse or keyboard state relative to one element, with no
-- interpretation of what it means (no click\/activation semantics, no focus
-- claiming). Higher-level combinators (e.g. 'Blink.Controls.control') build
-- meaningful behaviour on top of these.
--
-- No focus-gained\/-lost event yet -- that needs a cross-frame focus
-- snapshot ('Blink.UI.FocusState' only tracks the current, possibly
-- mid-frame-mutated focus) which doesn't exist yet; deferred until that
-- lands.
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
  deriving (Eq, Show)

-- | Observes and reports this frame's raw interaction events for the given
-- element (see 'ElementEvent'), firing each against the attrs list via
-- 'fire'. The lower-level primitive 'Blink.Controls.control' will eventually
-- be rebuilt on top of; for now it's independent and does not affect
-- 'Blink.Controls' at all.
element :: Ord e => e -> [Attr e ElementEvent msg cfg] -> UI e msg ()
element eid attrs = do
  hoverEvs  <- hoverStep eid
  buttonEvs <- buttonStep eid
  keyEvs    <- keyStep eid
  fire attrs (hoverEvs ++ buttonEvs ++ keyEvs)

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
