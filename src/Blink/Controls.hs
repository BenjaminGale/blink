{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls
  ( button
  , checkbox
  , label
  , progressBar
  , textInput
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Blink.Geometry (Alignment (..), Rectangle (..))
import Blink.Input (ButtonState (..), Key (..), KeyEvent (..), InputState (..))
import Blink.Layout (RectConstraint (..), Constraint (..), BoxConfig (..), hBox, defaultBoxConfig)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI

-- | Read-only text display.
-- TODO: truncate with ellipsis when text overflows the control bounds
-- TODO: clicking a label could transfer focus to an associated control (htmlFor-style)
-- TODO: hover could show a tooltip for truncated text
label :: (Eq e, Ord e) => e -> Text -> UI e c ()
label eid text = renderWithStyle eid $ do
  style <- getStyle eid
  drawText (textColour style) (textAlign style) text

-- | A read-only progress indicator. Value is clamped to [0, 1].
progressBar :: (Eq e, Ord e) => e -> Double -> UI e c ()
progressBar eid value = renderWithStyle eid $ do
  style <- getStyle eid
  r     <- getRect
  let clamped  = max 0 (min 1 value)
      fillRect' = r { rectWidth = rectWidth r * clamped }
  withBounds fillRect' $ fillRect (textColour style)

-- | A togglable checkbox with an adjacent label.
-- TODO: box size should derive from the font/line-height rather than being fixed
-- TODO: clicking the label should also toggle the checkbox (see label's focus-association TODO)
-- TODO: Space key should activate when KeySpace is added to the Key type
checkbox :: (Eq e, Ord e) => e -> e -> Text -> Bool -> (Bool -> c) -> UI e c ()
checkbox boxId labelId text checked mkCmd = do
  let boxControl = control boxId $ do
        style <- getStyle boxId
        when checked $ drawText (textColour style) AlignCenter "✓"
  hBox (defaultBoxConfig { boxSpacing = 4, boxFillCross = False })
    [ (RectConstraint (Exactly 20) (Exactly 20) MiddleLeft, boxControl)
    , (RectConstraint Fill         Fill         MiddleLeft, label labelId text)
    ]
  hasFocus <- isFocused boxId
  when hasFocus $ do
    styleSet <- getStyleSet boxId
    let s = focused styleSet
    case borderColour s of
      Just c  -> strokeRect c (borderWidth s)
      Nothing -> pure ()
  isHit    <- isHovered boxId
  btn      <- getLeftButton
  input    <- getInput
  isDisabl <- isDisabled
  let wasClicked = isHit && btn == ButtonReleased
      activated  = hasFocus && any (\e -> key e == KeyReturn) (keyEvents input)
  when ((wasClicked || activated) && not isDisabl) $ dispatch (mkCmd (not checked))

button :: (Eq e, Ord e) => e -> Text -> UI e c Bool
button eid txt = do
  control eid $ do
    style <- getStyle eid
    drawText (textColour style) (textAlign style) txt
  isHit    <- isHovered eid
  hasFocus <- isFocused eid
  btn      <- getLeftButton
  input    <- getInput
  isDisabl <- isDisabled
  let wasClicked = isHit && btn == ButtonReleased
      activated  = hasFocus && any (\e -> key e == KeyReturn) (keyEvents input)
  return ((wasClicked || activated) && not isDisabl)

textInput :: (Eq e, Ord e) => e -> Text -> (Text -> c) -> UI e c ()
textInput eid value mkCmd = do
  control eid $ do
    style    <- getStyle eid
    hasFocus <- isFocused eid
    isDisabl <- isDisabled
    let displayed = if hasFocus && not isDisabl then value <> "|" else value
    drawText (textColour style) (textAlign style) displayed
  hasFocus <- isFocused eid
  isDisabl <- isDisabled
  when (hasFocus && not isDisabl) $ do
    input <- getInput
    let withTyped    = foldl (<>) value (typedText input)
        hasBackspace = any (\e -> key e == KeyBackspace) (keyEvents input)
        result       = if hasBackspace && not (T.null withTyped)
                       then T.init withTyped
                       else withTyped
    when (result /= value) $ dispatch (mkCmd result)
