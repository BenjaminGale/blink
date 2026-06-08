module Blink.Input
  ( ButtonState (..)
  , Key (..)
  , Modifier (..)
  , KeyEvent (..)
  , InputState (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Point)

data ButtonState
  = ButtonUp
  | ButtonDown
  | ButtonReleased
  deriving (Eq, Show)

data Key = KeyTab | KeyReturn | KeyBackspace
  deriving (Eq, Show)

data Modifier = Shift
  deriving (Eq, Show)

data KeyEvent = KeyEvent
  { key :: Key
  , modifiers :: [Modifier]
  } deriving (Eq, Show)

data InputState = InputState
  { mousePosition :: Point
  , leftButton    :: ButtonState
  , keyEvents     :: [KeyEvent]
  , typedText     :: [Text]
  } deriving (Eq, Show)
