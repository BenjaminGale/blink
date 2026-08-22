{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Buttons and button-like controls: activated by a click or by pressing
-- Enter while focused.
module Blink.Button
  ( ButtonConfig
  , button
  , buttonBase
  , text
  , isTabStop
  , focusOnClick
  , FocusOnClick (..)
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
import Data.Text (Text)

import Blink.Attributes
  ( Attr, ControlConfig, FocusOnClick (..), HasControlConfig (..), HasTextConfig (..)
  , defaultControlConfig, configure, fire, focusOnClick, isTabStop, text
  )
import Blink.Control (control, getStyle)
import Blink.Element
  ( ElementEvent (..)
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Input (Key (KeyReturn), KeyEvent (..), InputState (..))
import Blink.Style (Style (..))
import Blink.UI (UI, drawText, getInput, isDisabled, isFocused)

-- | Runs @content@ as a normal interactive control (see 'control'), and
-- additionally fires 'Blink.Element.Clicked' -- alongside a real mouse
-- click -- when Enter is pressed while it holds focus and it isn't
-- disabled. The shape every button-like control (a plain 'button', or a
-- checkbox\/radio button activated the same way) is built from.
buttonBase :: (Ord e, HasControlConfig e cfg) => e -> cfg -> [Attr e ElementEvent msg cfg] -> UI e msg () -> UI e msg ()
buttonBase eid cfg attrs content = do
  control eid cfg attrs content
  focused  <- isFocused eid
  disabled <- isDisabled
  input    <- getInput
  let pressedReturn = any (\ev -> key ev == KeyReturn) (inputKeyEvents input)
  when (not disabled && focused && pressedReturn) $ fire attrs [Clicked]

-- | Configuration for 'button', set via 'text'. Defaults to @\"\"@.
data ButtonConfig e = ButtonConfig
  { buttonConfigControl :: ControlConfig e
  , buttonConfigText    :: Text
  }

defaultButtonConfig :: ButtonConfig e
defaultButtonConfig = ButtonConfig { buttonConfigControl = defaultControlConfig, buttonConfigText = "" }

instance HasControlConfig e (ButtonConfig e) where
  controlConfig    = buttonConfigControl
  setControlConfig cc cfg = cfg { buttonConfigControl = cc }

instance HasTextConfig (ButtonConfig e) where
  setText t cfg = cfg { buttonConfigText = t }

-- | A clickable button labelled via 'text'. Fires 'Blink.Element.Clicked'
-- -- handled with 'Blink.Element.onClicked' -- when activated by a
-- left-click or by pressing Enter while focused.
button :: Ord e => e -> [Attr e ElementEvent msg (ButtonConfig e)] -> UI e msg ()
button eid attrs = buttonBase eid cfg attrs $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) (buttonConfigText cfg)
  where
    cfg = configure defaultButtonConfig attrs
