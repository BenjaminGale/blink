{- |
Raw input types assembled by the backend each frame and passed to
'Blink.App.stepFrame' inside a 'Blink.App.FrameInput'. The 'InputState'
record aggregates pointer position, primary mouse button state, and keyboard
events for a single frame.
-}
module Blink.Input
  ( -- * Keyboard
    Key (..)
  , Modifier (..)
  , KeyEvent (..)
    -- * Frame input
  , InputState (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Point)

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
  | KeySpace
    -- ^ Space bar.
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
  deriving (Eq, Show)

-- | A single keyboard event from the platform: a key press together with
-- any modifier keys held at the time.
data KeyEvent = KeyEvent
  { key :: Key
    -- ^ The key that was pressed.
  , modifiers :: [Modifier]
    -- ^ Modifier keys held at the time of the press.
  } deriving (Eq, Show)

-- | All per-frame input assembled by the backend. Passed to the UI tree
-- via the 'Blink.App.FrameInput' each frame.
data InputState = InputState
  { inputMousePosition   :: Point
    -- ^ Cursor position in window coordinates.
  , inputLeftButtonDown  :: Bool
    -- ^ 'True' while the primary (left) mouse button is physically held.
    -- Button transition state (pressed\/released this frame) is derived by
    -- 'Blink.UI' from this value compared against the previous frame.
  , inputKeyEvents       :: [KeyEvent]
    -- ^ Key-press events for this frame.
  , inputTypedText     :: [Text]
    -- ^ Unicode text input events for this frame, in order received.
    -- Distinct from 'inputKeyEvents': text input handles IME, key repeat,
    -- and composed characters; use this for text entry fields.
  } deriving (Eq, Show)
