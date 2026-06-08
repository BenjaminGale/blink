{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.Controls
  ( button
  ) where

import Data.Text (Text)
import Blink.Input (ButtonState (..), Key (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..))
import Blink.UI

button :: (Eq e, Ord e) => e -> Text -> UI e c Bool
button eid label = do
  control eid $ do
    style <- getStyle eid
    drawText (textColour style) (textAlign style) label
  isHit <- (== Just eid) <$> getHovered
  hasFocus <- isFocused eid
  btn <- getLeftButton
  input <- getInput
  let wasClicked = isHit && btn == ButtonReleased
      activated = hasFocus && any (\e -> key e == KeyReturn) (keyEvents input)
  return (wasClicked || activated)
