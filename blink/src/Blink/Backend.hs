{- |
Module: Blink.Backend

Everything a backend needs to run a Blink application: configure a
'BlinkHandle', feed it one 'FrameInput' per frame, and render the
'DrawCommand's and 'CursorShape' it returns. See "Blink.App" for how the
frame loop behaves in each driving mode.
-}
module Blink.Backend
  ( -- * Running an application
    App
  , configureContinuous
  , configureEventDriven
  , BlinkHandle (..)
  , MsgQueue (..)
    -- * Input
  , FrameInput (..)
  , emptyFrameInput
  , KeyEvent (..)
  , Key (..)
  , Modifier (..)
    -- * Output
  , FrameResult (..)
  , DrawCommand (..)
  , ImagePath
  , TextAlign (..)
  , Font (..)
  , FontWeight (..)
  , defaultFont
  , CursorShape (..)
    -- * Measurement
  , Measurers (..)
  , TextMeasurer (..)
  , ImageMeasurer (..)
  , noOpMeasurers
  , noOpTextMeasurer
  , noOpImageMeasurer
    -- * Geometry used by draw commands
  , Point (..)
  , Size (..)
  , Rectangle (..)
  , Colour (..)
  , CornerRadii (..)
  , Border
  , BorderLayer (..)
  , EdgeVisibility (..)
  ) where

import Blink.App
  ( App, BlinkHandle (..), FrameInput (..), FrameResult (..), MsgQueue (..)
  , configureContinuous, configureEventDriven, emptyFrameInput
  )
import Blink.Geometry
  ( Border, BorderLayer (..), Colour (..), CornerRadii (..), EdgeVisibility (..), Point (..), Rectangle (..), Size (..) )
import Blink.Input (Key (..), KeyEvent (..), Modifier (..))
import Blink.Rendering
  ( CursorShape (..), DrawCommand (..), Font (..), FontWeight (..), ImageMeasurer (..), ImagePath, Measurers (..), TextAlign (..)
  , TextMeasurer (..), defaultFont, noOpImageMeasurer, noOpMeasurers, noOpTextMeasurer
  )
