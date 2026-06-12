{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T

data AppState = AppState
  { clickCount :: Int
  , inputText  :: Text
  , isChecked1 :: Bool
  , isChecked2 :: Bool
  , animating  :: Bool
  , sliderValue     :: Double
  , radioSelection  :: Int
  , radioSelection2 :: Int
  }

demoApp :: App Element (StandardControls Element) AppState
demoApp = App
  { startUp        = pure (AppState 0 "" False False False 0.5 0 0)
  , initialUIState = emptyStandardControls
  , theme          = \s -> if isChecked2 s then darkTheme else lightTheme
  , view           = demoView
  }

type DemoUI = UI Element (StandardControls Element) AppState

btn :: Int -> Text -> DemoUI ()
btn i txt = do
  clicked <- button (Btn i) txt
  when clicked $ dispatch (\s -> s { clickCount = min 50 (clickCount s + i) })

resetBtn :: DemoUI ()
resetBtn = do
  clicked <- button (Btn 0) "Reset"
  when clicked $ dispatch (\s -> s { clickCount = 0 })

rowButtons :: AppState -> DemoUI ()
rowButtons s =
  vBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4 })
    [ (Layout Fill Fill         TopLeft,
         hBox (defaultBoxConfig { boxSpacing = 4, boxAlignment = Center })
           [ (Layout (Exactly 80) Fill TopLeft,    btn 1 "One")
           , (Layout (Exactly 80) Fill TopLeft,    btn 2 "Two")
           , (Layout (Exactly 80) Fill TopLeft,    btn 3 "Three")
           , (Layout Fill         Fill MiddleLeft, label Label ("Clicks: " <> T.pack (show (clickCount s))))
           , (Layout (Exactly 80) Fill TopLeft,    resetBtn)
           ])
    , (Layout Fill (Exactly 20) TopLeft, progressBar ProgressBar1 (fromIntegral (clickCount s) / 50))
    ]

rowInput :: AppState -> DemoUI ()
rowInput s =
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout (Exactly 200) Fill         MiddleLeft, label Label "Text input")
    , (Layout Fill          (Exactly 30) TopLeft,    textInput TextInput1 (inputText s) (\t st -> st { inputText = t }))
    ]

rowCheckboxes :: AppState -> DemoUI ()
rowCheckboxes s =
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (Layout (Exactly 160) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox1 "Enable editing" (isChecked1 s) (\v st -> st { isChecked1 = v }))
    , (Layout (Exactly 120) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox2 "Dark mode" (isChecked2 s) (\v st -> st { isChecked2 = v }))
    , (Layout (Exactly 100) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox3 "Animate" (animating s) (\v st -> st { animating = v }))
    ]

rowSlider :: AppState -> DemoUI ()
rowSlider s =
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout (Exactly 200) Fill         MiddleLeft, label Label "Slider")
    , (Layout Fill          (Exactly 30) TopLeft,    slider Slider1 Horizontal (sliderValue s) (\v st -> st { sliderValue = v }))
    , (Layout (Exactly 60)  Fill         MiddleLeft, label Label (T.pack (show (round (sliderValue s * 100) :: Int) <> "%")))
    ]

rowRadio :: AppState -> DemoUI ()
rowRadio s =
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (Layout Fill Fill TopLeft,
         vBox defaultBoxConfig
           [ (Layout Fill (Exactly 26) TopLeft, label Label "Size")
           , (Layout Fill Fill TopLeft,
                radioGroup RadioOpt
                  [(0, "Small"), (1, "Medium"), (2, "Large")]
                  (radioSelection s)
                  (\v st -> st { radioSelection = v }))
           ])
    , (Layout Fill Fill TopLeft,
         vBox defaultBoxConfig
           [ (Layout Fill (Exactly 26) TopLeft, label Label "Priority")
           , (Layout Fill Fill TopLeft,
                radioGroup RadioOpt2
                  [(0, "Low"), (1, "Medium"), (2, "High"), (3, "Critical")]
                  (radioSelection2 s)
                  (\v st -> st { radioSelection2 = v }))
           ])
    ]

rowProgress :: AppState -> DemoUI ()
rowProgress s =
  vBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4 })
    [ (Layout Fill Fill         TopLeft,
         hBox (defaultBoxConfig { boxSpacing = 4 })
           [ (Layout Fill         Fill TopLeft,
                if animating s
                  then indeterminateProgressBar ProgressBar2
                  else progressBar ProgressBar2 0)
           , (Layout (Exactly 20) Fill TopLeft, scrollBar ScrollBar2 Vertical 0.3)
           ])
    , (Layout Fill (Exactly 20) TopLeft, scrollBar ScrollBar1 Horizontal 0.2)
    ]

demoView :: DemoUI ()
demoView = do
  s <- getAppState
  when (isChecked2 s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill (Exactly 50) TopLeft, rowCheckboxes s)
    , (Layout Fill (Exactly 70) TopLeft, disableWhen (not (isChecked1 s)) $ rowButtons s)
    , (Layout Fill (Exactly 50) TopLeft, disableWhen (not (isChecked1 s)) $ rowInput s)
    , (Layout Fill (Exactly 38) TopLeft, disableWhen (not (isChecked1 s)) $ rowSlider s)
    , (Layout Fill (Exactly 130) TopLeft, disableWhen (not (isChecked1 s)) $ rowRadio s)
    , (Layout Fill Fill         TopLeft, rowProgress s)
    ]
