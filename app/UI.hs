{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when)
import Data.Text (Text)

data AppState = AppState
  { lastClicked :: Maybe Int
  , inputText :: Text
  , isChecked1 :: Bool
  , isChecked2 :: Bool
  , isChecked3 :: Bool
  , progressValue :: Double
  }

demoApp :: App Element (StandardControls Element) AppState
demoApp = App
  { startUp = pure (AppState Nothing "" False False False 0.5)
  , initialUIState = emptyStandardControls
  , theme = \s -> if isChecked2 s then darkTheme else lightTheme
  , view = demoView
  }

vScrollBar :: Orientation -> Double -> UI Element (StandardControls Element) AppState ()
vScrollBar = scrollBar ScrollBar1

hScrollBar :: Orientation -> Double -> UI Element (StandardControls Element) AppState ()
hScrollBar = scrollBar ScrollBar2

btn :: Int -> Text -> UI Element (StandardControls Element) AppState ()
btn i txt = do
  clicked <- button (Btn i) txt
  when clicked $ dispatch (\s -> s { lastClicked = Just i })

rowBg :: Bool -> Colour -> Colour -> Colour
rowBg dark d l = if dark then d else l

-- Row 1: fill behaviour — fixed | fill | fill | fixed (two fills share surplus evenly)
row1 :: Bool -> UI Element (StandardControls Element) AppState ()
row1 dark = fillRect (rowBg dark (RGBA 0.176 0.133 0.141 1) (RGBA 0.95 0.87 0.87 1)) >>
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (RectConstraint (Exactly 80) Fill TopLeft, btn 1 "Left")
    , (RectConstraint Fill         Fill Center,  btn 2 "<fill 1>")
    , (RectConstraint Fill         Fill Center,  btn 3 "<fill 2>")
    , (RectConstraint (Exactly 80) Fill TopLeft, btn 4 "Right")
    ]

-- Row 2: fillCross = False, children top/centre/bottom aligned; scrollbar fills the height
row2 :: Bool -> UI Element (StandardControls Element) AppState ()
row2 dark = fillRect (rowBg dark (RGBA 0.133 0.176 0.141 1) (RGBA 0.87 0.95 0.87 1)) >>
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center, boxFillCross = False })
    [ (RectConstraint (Exactly 100) (Exactly 30) TopLeft,    btn 5 "Top")
    , (RectConstraint (Exactly 100) (Exactly 50) MiddleLeft, btn 6 "Mid")
    , (RectConstraint (Exactly 100) (Exactly 40) BottomLeft, btn 7 "Bot")
    , (RectConstraint (Exactly 20)  Fill         TopLeft,    vScrollBar Vertical 0.3)
    ]

-- Row 3: same constraints as row 2, fillCross = True, content aligned to the right
row3 :: Bool -> UI Element (StandardControls Element) AppState ()
row3 dark = fillRect (rowBg dark (RGBA 0.133 0.141 0.176 1) (RGBA 0.87 0.87 0.95 1)) >>
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = MiddleRight })
    [ (RectConstraint (Exactly 100) (Exactly 30) TopLeft,    btn 8 "Top")
    , (RectConstraint (Exactly 100) (Exactly 50) MiddleLeft, btn 9 "Mid")
    , (RectConstraint (Exactly 100) (Exactly 40) BottomLeft, btn 10 "Bot")
    ]

-- Row 4: text input
row4 :: Bool -> AppState -> UI Element (StandardControls Element) AppState ()
row4 dark s = fillRect (rowBg dark (RGBA 0.176 0.165 0.118 1) (RGBA 0.95 0.95 0.87 1)) >>
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4 })
    [ (RectConstraint Fill (Exactly 30) MiddleLeft,
         textInput TextInput1 (inputText s) (\t st -> st { inputText = t })) ]

-- Row 5: checkboxes — the first checkbox enables/disables the rest
row5 :: Bool -> AppState -> UI Element (StandardControls Element) AppState ()
row5 dark s = fillRect (rowBg dark (RGBA 0.165 0.133 0.176 1) (RGBA 0.95 0.87 0.95 1)) >>
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (RectConstraint (Exactly 160) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox1 "Enable editing"  (isChecked1 s) (\v st -> st { isChecked1 = v }))
    , (RectConstraint (Exactly 160) (Exactly 30) MiddleLeft,
         disableWhen (not (isChecked1 s)) $
           checkbox CheckboxBox2 "Dark mode"     (isChecked2 s) (\v st -> st { isChecked2 = v }))
    , (RectConstraint (Exactly 160) (Exactly 30) MiddleLeft,
         disableWhen (not (isChecked1 s)) $
           checkbox CheckboxBox3 "Notifications" (isChecked3 s) (\v st -> st { isChecked3 = v }))
    ]

-- Row 6: progress bar with +/- buttons
row6 :: Bool -> AppState -> UI Element (StandardControls Element) AppState ()
row6 dark s = fillRect (rowBg dark (RGBA 0.118 0.165 0.176 1) (RGBA 0.87 0.95 0.95 1)) >>
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4 })
    [ (RectConstraint (Exactly 30) (Exactly 30) MiddleLeft, do
         clicked <- button (Btn 11) "-"
         when clicked $ dispatch (\st -> st { progressValue = max 0 (progressValue st - 0.1) }))
    , (RectConstraint Fill         (Exactly 20) MiddleLeft, progressBar ProgressBar1 (progressValue s))
    , (RectConstraint (Exactly 30) (Exactly 30) MiddleLeft, do
         clicked <- button (Btn 12) "+"
         when clicked $ dispatch (\st -> st { progressValue = min 1 (progressValue st + 0.1) }))
    ]

demoView :: UI Element (StandardControls Element) AppState ()
demoView = do
  s <- getAppState
  let dark = isChecked2 s
  when dark $ fillRect (RGBA 0.082 0.102 0.129 1)
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (RectConstraint Fill (Exactly 50) TopLeft, row1 dark)
    , (RectConstraint Fill Fill         TopLeft, row2 dark)
    , (RectConstraint Fill (Exactly 80) TopLeft, row3 dark)
    , (RectConstraint Fill (Exactly 50) TopLeft, disableWhen (not (isChecked1 s)) $ row4 dark s)
    , (RectConstraint Fill (Exactly 50) TopLeft, row5 dark s)
    , (RectConstraint Fill (Exactly 50) TopLeft, disableWhen (not (isChecked1 s)) $ row6 dark s)
    , (RectConstraint Fill (Exactly 20) TopLeft, hScrollBar Horizontal 0.3)
    ]
