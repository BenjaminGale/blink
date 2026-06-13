{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when, forM_)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

data AppState = AppState
  { clickCount :: Int
  , inputText  :: Text
  , isChecked1 :: Bool
  , isChecked2 :: Bool
  , animating  :: Bool
  , sliderValue       :: Double
  , radioSelection    :: Int
  , radioSelection2   :: Int
  , lastInput         :: Text
  , lastInputCount    :: Int
  , isHovering        :: Bool
  , lastClickedStatic  :: Maybe Int
  , lastClickedDynamic :: Maybe Int
  }

demoApp :: App Element AppState
demoApp = App
  { startUp = pure (AppState 0 "" False False False 0.5 0 0 "" 0 False Nothing Nothing)
  , theme   = \s -> if isChecked2 s then darkTheme else lightTheme
  , view    = demoView
  }

type DemoUI = UI Element AppState

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
    , (Layout Fill (Exactly 20) TopLeft, progressBar ProgressBar1 (Progress (fromIntegral (clickCount s) / 50)))
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
    [ (Layout Fill Fill TopLeft,
         if animating s
           then progressBar ProgressBar2 Indeterminate
           else progressBar ProgressBar2 (Progress 0))
    ]

rowScrollRegions :: AppState -> DemoUI ()
rowScrollRegions s =
  vBox (defaultBoxConfig { boxMargin = 4, boxSpacing = 4 })
    [ (Layout Fill (Exactly 26) TopLeft,
         hBox defaultBoxConfig
           [ (Layout Fill Fill MiddleLeft, label Label "Known size (scrollableRegion)")
           , (Layout Fill Fill MiddleLeft, label Label "Dynamic (scrollableDynamic, 100 items)")
           ])
    , (Layout Fill Fill TopLeft,
         hBox (defaultBoxConfig { boxSpacing = 8 })
           [ (Layout Fill Fill TopLeft, staticScrollList s)
           , (Layout Fill Fill TopLeft, dynamicScrollList s)
           ])
    ]

staticScrollList :: AppState -> DemoUI ()
staticScrollList s =
  scrollableRegion ScrollRegion1 (Size 400 (20 * 32)) $
    vBox defaultBoxConfig
      [ (Layout Fill (Exactly 32) TopLeft, item i)
      | i <- [1 .. 20 :: Int]
      ]
  where
    item i = do
      let isSelected = lastClickedStatic s == Just i
          txt = (if isSelected then "✓ " else "") <> "Item " <> T.pack (show i)
      clicked <- button (ScrollItem1 i) txt
      when clicked $ dispatch (\st -> st { lastClickedStatic = Just i })

dynamicScrollList :: AppState -> DemoUI ()
dynamicScrollList s = do
  bounds <- getBounds
  let itemH      = 32 :: Double
      totalItems = 100 :: Int
      contentH   = fromIntegral totalItems * itemH
      vRatio     = max 0 (min 1 (rectHeight bounds / contentH))
  scrollableDynamic ScrollRegion2 Nothing (Just vRatio) $ \_ vFrac -> do
    vp <- getBounds
    let vpH      = rectHeight vp
        offset   = vFrac * max 0 (contentH - vpH)
        firstIdx = floor (offset / itemH) :: Int
        subOff   = offset - fromIntegral firstIdx * itemH
        visibleN = ceiling ((vpH + subOff) / itemH) :: Int
    forM_ [0 .. visibleN - 1] $ \j ->
      let i     = firstIdx + j
          itemR = Rectangle (rectX vp) (rectY vp + fromIntegral j * itemH - subOff) (rectWidth vp) itemH
      in when (i < totalItems) $ withBounds itemR $ do
           let isSelected = lastClickedDynamic s == Just i
               txt = (if isSelected then "✓ " else "") <> "Item " <> T.pack (show (i + 1))
           clicked <- button (ScrollItem2 i) txt
           when clicked $ dispatch (\st -> st { lastClickedDynamic = Just i })

rowDebugInfo :: AppState -> (Int, Int) -> DemoUI ()
rowDebugInfo s (winW, winH) = do
  pos   <- getMousePos
  input <- getInput
  let winText    = "Window: " <> T.pack (show winW) <> " x " <> T.pack (show winH)
      mx         = T.pack (show (round (pointX pos) :: Int))
      my         = T.pack (show (round (pointY pos) :: Int))
      mouseText  = "Mouse: " <> mx <> ", " <> my
      buttonText = "Button: " <> T.pack (show (inputLeftButtonDown input))
      hoverText  = "Hover: " <> if isHovering s then "Yes" else "No"
      countSuffix = if lastInputCount s > 1 then " (" <> T.pack (show (lastInputCount s)) <> ")" else ""
      keyText     = "Last Key Press: " <> if T.null (lastInput s) then "none" else lastInput s <> countSuffix
  hBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 4, boxAlignment = Center })
    [ (Layout (Exactly 160) Fill MiddleLeft, label Label winText)
    , (Layout (Exactly 130) Fill MiddleLeft, label Label mouseText)
    , (Layout (Exactly 140) Fill MiddleLeft, label Label buttonText)
    , (Layout (Exactly 80)  Fill MiddleLeft, label Label hoverText)
    , (Layout Fill          Fill MiddleLeft, label Label keyText)
    ]

demoView :: DemoUI ()
demoView = do
  s     <- getAppState
  -- Snapshot input before controls consume key events (e.g. Tab navigation)
  input <- getInput
  win   <- getBounds
  let winSize = (round (rectWidth win) :: Int, round (rectHeight win) :: Int)
  when (isChecked2 s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill (Exactly 40) TopLeft, rowDebugInfo s winSize)
    , (Layout Fill (Exactly 50) TopLeft, rowCheckboxes s)
    , (Layout Fill (Exactly 70) TopLeft, disableWhen (not (isChecked1 s)) $ rowButtons s)
    , (Layout Fill (Exactly 50) TopLeft, disableWhen (not (isChecked1 s)) $ rowInput s)
    , (Layout Fill (Exactly 38) TopLeft, disableWhen (not (isChecked1 s)) $ rowSlider s)
    , (Layout Fill (Exactly 130) TopLeft, disableWhen (not (isChecked1 s)) $ rowRadio s)
    , (Layout Fill (Exactly 280) TopLeft, rowScrollRegions s)
    , (Layout Fill Fill          TopLeft, rowProgress s)
    ]
  -- Capture hover state at end of frame (after all controls have registered hover)
  mHov  <- UI $ \ctx -> pure (ixnHovered (ctxInteraction ctx), ctx)
  let typed   = T.concat (inputTypedText input)
      keyName = case inputKeyEvents input of
                  []      -> ""
                  (e : _) -> T.pack (show (key e))
      newInput = if not (T.null typed)
                   then if typed == " " then "Space" else "Character " <> typed
                   else keyName
  dispatch $ \s' -> s'
    { isHovering     = isJust mHov
    , lastInput      = if T.null newInput then lastInput s' else newInput
    , lastInputCount = if T.null newInput then lastInputCount s'
                       else if newInput == lastInput s' then lastInputCount s' + 1
                       else 1
    }
