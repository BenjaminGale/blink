{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls
  ( button
  , textInput
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
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

textInput :: (Eq e, Ord e) => e -> Text -> (Text -> c) -> UI e c ()
textInput eid value mkCmd = do
  control eid $ do
    style    <- getStyle eid
    hasFocus <- isFocused eid
    let displayed = if hasFocus then value <> "|" else value
    drawText (textColour style) (textAlign style) displayed
  hasFocus <- isFocused eid
  when hasFocus $ do
    input <- getInput
    let withTyped    = foldl (<>) value (typedText input)
        hasBackspace = any (\e -> key e == KeyBackspace) (keyEvents input)
        result       = if hasBackspace && not (T.null withTyped)
                       then T.init withTyped
                       else withTyped
    when (result /= value) $ dispatch (mkCmd result)
