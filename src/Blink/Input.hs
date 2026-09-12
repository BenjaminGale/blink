{- |
Module: Blink.Input

Raw input types assembled by the backend each frame and passed to
'Blink.App.stepFrame' inside a 'Blink.App.FrameInput'. The 'InputState'
record aggregates pointer position, primary mouse button state, and keyboard
events for a single frame.

Also home to pure mouse state built from that raw input: which element (if
any) holds the mouse button's capture, and each element's hover state,
together with the per-frame transition functions that advance them. No
dependency on the 'Blink.View' monad -- 'Blink.View' holds a 'Mouse' in its
context and exposes monadic accessors built on top of what's defined here.
-}
module Blink.Input
  ( -- * Keyboard
    Key (..)
  , Modifier (..)
  , KeyEvent (..)
    -- * Frame input
  , InputState (..)
    -- * Mouse
  , MouseCapture (..)
  , ButtonState (..)
  , captureOf
  , nextButtonState
  , HoverState (..)
  , wasHit
  , nextHoverState
  , Mouse (..)
  , emptyMouse
  , advanceHover
  , advanceButton
  , advanceMouse
    -- * Hit-rect registry
  , HitRect (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Blink.Geometry (Point, Rectangle)

-- | The subset of keys that Blink's controls respond to. Text entry is
-- handled via 'inputTypedText' in 'InputState'; 'Key' covers only
-- navigation and editing keys.
data Key
  = KeyTab
    -- ^ Tab key (focus forward).
  | KeyReturn
    -- ^ Return \/ Enter key.
  | KeyBackspace
    -- ^ Backspace key.
  | KeyDelete
    -- ^ Forward Delete key.
  | KeySpace
    -- ^ Space bar.
  | KeyA
    -- ^ The @A@ key. With 'Ctrl' held, selects all in
    -- "Blink.Controls.TextInput".
  | KeyLeft
    -- ^ Left arrow.
  | KeyRight
    -- ^ Right arrow.
  | KeyUp
    -- ^ Up arrow.
  | KeyDown
    -- ^ Down arrow.
  deriving (Eq, Show)

-- | Keyboard modifier keys. Carried alongside a 'Key' in 'KeyEvent'.
data Modifier
  = Shift -- ^ Shift key held during the key press.
  | Ctrl  -- ^ Ctrl key held during the key press.
  deriving (Eq, Show)

-- | A single keyboard event from the platform: a key press together with
-- any modifier keys held at the time.
data KeyEvent = KeyEvent
  { key :: Key
    -- ^ The key that was pressed.
  , modifiers :: [Modifier]
    -- ^ Modifier keys held at the time of the press.
  , keyRepeat :: Bool
    -- ^ 'True' when this event was synthesized by the platform's own
    -- keyboard auto-repeat (holding the key down), rather than the initial
    -- physical press. Most consumers of a raw @['KeyEvent']@ want these --
    -- holding Backspace\/an arrow key repeating is ordinary text-editing
    -- behaviour (see "Blink.Controls.TextInput"). A control that only
    -- wants to react once per physical press (e.g.
    -- "Blink.Controls.Button"'s Enter-while-focused activation) checks
    -- this and ignores the event when it's 'True'.
  } deriving (Eq, Show)

-- | All per-frame input assembled by the backend. Passed to the view tree
-- via the 'Blink.App.FrameInput' each frame.
data InputState = InputState
  { inputMousePosition   :: Point
    -- ^ Cursor position in window coordinates.
  , inputLeftButtonDown  :: Bool
    -- ^ 'True' while the primary (left) mouse button is physically held.
    -- Button transition state (pressed\/released this frame) is derived by
    -- 'Blink.View' from this value compared against the previous frame.
  , inputKeyEvents       :: [KeyEvent]
    -- ^ Key-press events for this frame.
  , inputTypedText     :: [Text]
    -- ^ Unicode text input events for this frame, in order received.
    -- Distinct from 'inputKeyEvents': text input handles IME, key repeat,
    -- and composed characters; use this for text entry fields.
  , inputWheelDelta    :: Double
    -- ^ Vertical mouse wheel movement for this frame, in notches (or the
    -- platform's fractional equivalent for a trackpad/high-resolution
    -- wheel). Positive scrolls down/forward (the content should move up,
    -- revealing what's below); negative scrolls up/back. Zero when the
    -- wheel didn't move this frame. Already corrects for the platform's
    -- "natural"/flipped scrolling setting, so a consumer never has to.
  } deriving (Eq, Show)

-- | Which element, if any, holds mouse capture during a drag. A control
-- acquires capture on press so that it keeps receiving drag input even once
-- the cursor leaves its bounds.
data MouseCapture e
  = MouseNotCaptured
  | MouseCapturedBy e
  deriving (Eq, Show)

-- | The left mouse button's state for the current frame, together with
-- which element (if any) holds mouse capture. 'ButtonDown' and
-- 'ButtonReleased' each last for exactly one frame -- the frame the button
-- transitions -- so a control can tell "the button just went down/came up"
-- apart from "the button has been held/up for a while" ('ButtonHeld'/
-- 'ButtonUp').
--
-- Capture can only be held while the button is down, or through the single
-- frame it's released on -- never while 'ButtonUp' -- which is why it lives
-- inside 'ButtonDown', 'ButtonHeld', and 'ButtonReleased' rather than as a
-- separate field: a captured element while the button has been up for a
-- while is not a state that should be representable.
data ButtonState e
  = ButtonUp
    -- ^ Not held, and didn't just come up this frame. Never carries capture.
  | ButtonDown (MouseCapture e)
    -- ^ Just went down this frame -- the down edge.
  | ButtonHeld (MouseCapture e)
    -- ^ Held, past the first frame of the press.
  | ButtonReleased (MouseCapture e)
    -- ^ Came up this frame, having been held the frame before -- the up
    -- edge. Still carries whatever captured the press, so a control can
    -- distinguish a drag-release from a plain click.
  deriving (Eq, Show)

-- | The capture carried by a 'ButtonState', or 'MouseNotCaptured' when the
-- button is up.
captureOf :: ButtonState e -> MouseCapture e
captureOf ButtonUp             = MouseNotCaptured
captureOf (ButtonDown cap)     = cap
captureOf (ButtonHeld cap)     = cap
captureOf (ButtonReleased cap) = cap

-- | Advances a 'ButtonState' by one frame given whether the button was held
-- last frame and is held this frame. Capture only carries forward from
-- @existingCapture@ while the button stays held across the transition (or on
-- its release frame); it's dropped the moment the button was not down last
-- frame -- both when it's fully up and on a fresh press -- so a new press
-- never inherits a stale capture left over from a previous drag\/click
-- cycle. Acquisition -- setting capture in the first place -- happens
-- elsewhere (see 'Blink.View.acquireCapture').
nextButtonState :: Bool -> Bool -> MouseCapture e -> ButtonState e
nextButtonState prevDown currDown existingCapture
  | currDown && not prevDown = ButtonDown carriedCapture
  | currDown                 = ButtonHeld carriedCapture
  | prevDown                 = ButtonReleased carriedCapture
  | otherwise                = ButtonUp
  where
    carriedCapture
      | prevDown  = existingCapture
      | otherwise = MouseNotCaptured

-- | An element's hover state for the current frame. 'Entered' and 'Exited'
-- each last for exactly one frame -- the frame the geometric hit test
-- result changes -- so a control can tell "the mouse just entered/left" ('
-- Entered'/'Exited') apart from "the mouse has been over/away for a while"
-- ('Over'/'NotOver').
data HoverState
  = NotOver
    -- ^ Not hit this frame, wasn't last frame either.
  | Entered
    -- ^ Hit this frame, wasn't last frame -- the enter edge.
  | Over
    -- ^ Hit this frame, was also hit last frame.
  | Exited
    -- ^ Not hit this frame, was hit last frame -- the exit edge.
  deriving (Eq, Show)

-- | 'True' for the states that count as "currently hit" -- 'Entered' and
-- 'Over'.
wasHit :: HoverState -> Bool
wasHit Entered = True
wasHit Over    = True
wasHit _       = False

-- | Advances an element's 'HoverState' by one frame given whether it's hit
-- this frame.
nextHoverState :: HoverState -> Bool -> HoverState
nextHoverState prev isOverNow = case (wasHit prev, isOverNow) of
  (False, True)  -> Entered
  (True,  True)  -> Over
  (True,  False) -> Exited
  (False, False) -> NotOver

-- | One identified control's hit-tested bounds for a frame, together with a
-- per-frame registration index -- see 'mouseHitRectsNext'.
data HitRect = HitRect
  { hitRectBounds :: Rectangle
  , hitRectIndex  :: Int
  } deriving (Eq, Show)

-- | All mouse state for the current frame: the button/capture state, plus
-- each element's hover state and hit-tested bounds. Both are tracked the
-- same way: a read-only snapshot of last frame's results ('mouseHoverPrev'/
-- 'mouseHitRectsPrev') and a map being built up as elements are visited this
-- frame ('mouseHoverNext'/'mouseHitRectsNext') -- an element not visited
-- this frame simply has no entry in either "next" map, so it starts fresh
-- the next time it -- or a different element reusing its id -- is visited,
-- rather than resuming from a stale state.
--
-- 'mouseHitRectsNext' only ever gains an entry for a control that was
-- actually hit (and enabled) this frame -- see
-- 'Blink.Controls.Control.watchInteraction' -- so its size tracks the
-- depth of whatever's currently under the pointer, not the size of the
-- whole view. The registration index records visiting order (root before
-- the children it renders), so comparing indices lets a later query tell
-- "was something nested inside me, or drawn after me, also hit here" --
-- see 'Blink.View.isOccludedFor'.
data Mouse e = Mouse
  { mouseButton        :: ButtonState e
  , mouseHoverPrev     :: Map.Map e HoverState
  , mouseHoverNext     :: Map.Map e HoverState
  , mouseHitRectsPrev  :: Map.Map e HitRect
  , mouseHitRectsNext  :: Map.Map e HitRect
  }

-- | No button held, nothing hovered -- the starting state for a fresh
-- context.
emptyMouse :: Mouse e
emptyMouse = Mouse
  { mouseButton       = ButtonUp
  , mouseHoverPrev    = Map.empty
  , mouseHoverNext    = Map.empty
  , mouseHitRectsPrev = Map.empty
  , mouseHitRectsNext = Map.empty
  }

-- | Rolls 'mouseHoverNext'\/'mouseHitRectsNext' (this completed frame's
-- results) into 'mouseHoverPrev'\/'mouseHitRectsPrev' for the next frame to
-- read, starting fresh empty "next" maps. Leaves 'mouseButton' untouched --
-- used on its own when re-rendering the current frame rather than advancing
-- to a new one (see 'Blink.View.rerenderContext'), where the button reading
-- shouldn't be re-derived a second time against itself.
advanceHover :: Mouse e -> Mouse e
advanceHover mouse = mouse
  { mouseHoverPrev    = mouseHoverNext mouse
  , mouseHoverNext    = Map.empty
  , mouseHitRectsPrev = mouseHitRectsNext mouse
  , mouseHitRectsNext = Map.empty
  }

-- | Advances 'mouseButton' via 'nextButtonState', given whether the button
-- was held last frame and is held this frame. Leaves the hover maps
-- untouched.
advanceButton :: Bool -> Bool -> Mouse e -> Mouse e
advanceButton prevDown currDown mouse =
  mouse { mouseButton = nextButtonState prevDown currDown (captureOf (mouseButton mouse)) }

-- | Advances a 'Mouse' to the next frame given whether the button was held
-- last frame and is held this frame: 'advanceHover' followed by
-- 'advanceButton'.
advanceMouse :: Bool -> Bool -> Mouse e -> Mouse e
advanceMouse prevDown currDown = advanceButton prevDown currDown . advanceHover
