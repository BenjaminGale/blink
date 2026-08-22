-- | Pure mouse state: which element (if any) holds the mouse button's
-- capture, and each element's hover state, together with the per-frame
-- transition functions that advance them. No dependency on the 'Blink.UI'
-- monad -- 'Blink.UI' holds a 'Mouse' in its context and exposes monadic
-- accessors built on top of what's defined here.
module Blink.Mouse
  ( MouseCapture (..)
  , ButtonState (..)
  , captureOf
  , nextButtonState
  , HoverState (..)
  , wasHit
  , nextHoverState
  , Mouse (..)
  , emptyMouse
  , advanceMouse
  ) where

import qualified Data.Map.Strict as Map

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
-- last frame and is held this frame, carrying forward whatever the previous
-- state's capture was.
nextButtonState :: Bool -> Bool -> MouseCapture e -> ButtonState e
nextButtonState prevDown currDown carriedCapture
  | currDown && not prevDown = ButtonDown carriedCapture
  | currDown                 = ButtonHeld carriedCapture
  | prevDown                 = ButtonReleased carriedCapture
  | otherwise                = ButtonUp

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

-- | All mouse state for the current frame: the button/capture state, plus
-- each element's hover state. Hover is tracked as a read-only snapshot of
-- last frame's results ('mouseHoverPrev') and a map being built up as
-- elements are visited this frame ('mouseHoverNext') -- an element not
-- visited this frame simply has no entry in 'mouseHoverNext', so it starts
-- fresh (from 'NotOver') the next time it -- or a different element reusing
-- its id -- is visited, rather than resuming from a stale hover state.
data Mouse e = Mouse
  { mouseButton    :: ButtonState e
  , mouseHoverPrev :: Map.Map e HoverState
  , mouseHoverNext :: Map.Map e HoverState
  }

-- | No button held, nothing hovered -- the starting state for a fresh
-- context.
emptyMouse :: Mouse e
emptyMouse = Mouse
  { mouseButton    = ButtonUp
  , mouseHoverPrev = Map.empty
  , mouseHoverNext = Map.empty
  }

-- | Advances a 'Mouse' to the next frame given whether the button was held
-- last frame and is held this frame: advances 'mouseButton' via
-- 'nextButtonState', and rolls 'mouseHoverNext' (this completed frame's
-- results) into 'mouseHoverPrev' for the next frame to read, starting a
-- fresh empty 'mouseHoverNext'.
advanceMouse :: Bool -> Bool -> Mouse e -> Mouse e
advanceMouse prevDown currDown mouse = mouse
  { mouseButton    = nextButtonState prevDown currDown (captureOf (mouseButton mouse))
  , mouseHoverPrev = mouseHoverNext mouse
  , mouseHoverNext = Map.empty
  }
