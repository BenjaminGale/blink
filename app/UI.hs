{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when, forM_)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

-- Per-page state

data BasicControlsState = BasicControlsState
  { clickCount      :: Int
  , inputText       :: Text
  , editingEnabled  :: Bool
  , sliderValue     :: Double
  , radioSelection  :: Int
  , radioSelection2 :: Int
  }

initialBasicControls :: BasicControlsState
initialBasicControls = BasicControlsState
  { clickCount      = 0
  , inputText       = ""
  , editingEnabled  = False
  , sliderValue     = 0.5
  , radioSelection  = 0
  , radioSelection2 = 0
  }

data ScrollPageState = ScrollPageState
  { lastClickedStatic  :: Maybe Int
  , lastClickedDynamic :: Maybe Int
  }

initialScrollState :: ScrollPageState
initialScrollState = ScrollPageState Nothing Nothing

data ProgressState = ProgressState
  { animating :: Bool
  }

initialProgressState :: ProgressState
initialProgressState = ProgressState False

data LayoutPageState = LayoutPageState
  { hboxFillCross :: Bool
  , vboxFillCross :: Bool
  }

initialLayoutState :: LayoutPageState
initialLayoutState = LayoutPageState
  { hboxFillCross = True
  , vboxFillCross = True
  }

data Page
  = BasicControlsPage BasicControlsState
  | ScrollPage ScrollPageState
  | ProgressPage ProgressState
  | LayoutPage LayoutPageState

-- Shared state

data AppState = AppState
  { currentPage    :: Page
  , darkMode       :: Bool
  , isHovering     :: Bool
  , lastInput      :: Text
  , lastInputCount :: Int
  }

demoApp :: App Element AppState
demoApp = App
  { startUp = pure AppState
      { currentPage    = BasicControlsPage initialBasicControls
      , darkMode       = False
      , isHovering     = False
      , lastInput      = ""
      , lastInputCount = 0
      }
  , theme   = \s -> if darkMode s then darkTheme else lightTheme
  , view    = demoView
  }

type DemoUI = UI Element AppState

-- Shell

sidebar :: AppState -> DemoUI ()
sidebar s = do
  (_, btnChromH) <- measureChrome (NavBtn 0)
  Size _ th      <- measureText "Basic Controls"
  let btnH = addLength (Exactly th) btnChromH
  Size _ cbTh <- measureText "Dark mode"
  let cbH = Exactly (max 20 cbTh)
  vBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 8 })
    [ (Layout Fill btnH TopLeft, navBtn 0 "Basic Controls")
    , (Layout Fill btnH TopLeft, navBtn 1 "Scroll")
    , (Layout Fill btnH TopLeft, navBtn 2 "Progress")
    , (Layout Fill btnH TopLeft, navBtn 3 "Layout")
    , (Layout Fill Fill  TopLeft, pure ())
    , (Layout Fill cbH  TopLeft,
         checkbox CheckboxBox2 "Dark mode" (darkMode s) (\v st -> st { darkMode = v }))
    ]
  where
    navBtn i lbl = do
      clicked <- button (NavBtn i) lbl
      when clicked $ dispatch $ \st -> st { currentPage = pageForIndex i }

pageForIndex :: Int -> Page
pageForIndex 0 = BasicControlsPage initialBasicControls
pageForIndex 1 = ScrollPage initialScrollState
pageForIndex 2 = ProgressPage initialProgressState
pageForIndex _ = LayoutPage initialLayoutState

footer :: AppState -> (Int, Int) -> DemoUI ()
footer s (winW, winH) = do
  pos   <- getMousePos
  input <- getInput
  let winText    = "Window: " <> T.pack (show winW) <> " x " <> T.pack (show winH)
      mx         = T.pack (show (round (pointX pos) :: Int))
      my         = T.pack (show (round (pointY pos) :: Int))
      mouseText  = "Mouse: " <> mx <> ", " <> my
      buttonText = "Button: " <> T.pack (show (inputLeftButtonDown input))
      hoverText  = "Hover: " <> if isHovering s then "Yes" else "No"
      countSuffix = if lastInputCount s > 1
                      then " (" <> T.pack (show (lastInputCount s)) <> ")"
                      else ""
      keyText    = "Last Key Press: "
                <> if T.null (lastInput s) then "none" else lastInput s <> countSuffix
  (chromW, _) <- measureChrome Label
  let labelWidth t = flip addLength chromW . Exactly . sizeWidth <$> measureText t
  winW'    <- labelWidth winText
  mouseW'  <- labelWidth mouseText
  buttonW' <- labelWidth buttonText
  hoverW'  <- labelWidth hoverText
  hBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 4, boxAlignment = Center })
    [ (Layout winW'    Fill MiddleLeft, label Label winText)
    , (Layout mouseW'  Fill MiddleLeft, label Label mouseText)
    , (Layout buttonW' Fill MiddleLeft, label Label buttonText)
    , (Layout hoverW'  Fill MiddleLeft, label Label hoverText)
    , (Layout Fill               Fill MiddleLeft, label Label keyText)
    ]

centrePane :: AppState -> DemoUI ()
centrePane s = case currentPage s of
  BasicControlsPage ps -> basicControlsView ps
  ScrollPage ps        -> scrollView ps
  ProgressPage ps      -> progressView ps
  LayoutPage ps        -> layoutView ps

-- Basic Controls page

basicControlsView :: BasicControlsState -> DemoUI ()
basicControlsView ps =
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill (Exactly 50)  TopLeft, rowCheckboxes ps)
    , (Layout Fill (Exactly 70)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowButtons ps)
    , (Layout Fill (Exactly 50)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowInput ps)
    , (Layout Fill (Exactly 38)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowSlider ps)
    , (Layout Fill Fill          TopLeft, disableWhen (not (editingEnabled ps)) $ rowRadio ps)
    ]

rowCheckboxes :: BasicControlsState -> DemoUI ()
rowCheckboxes ps = do
  Size tw th <- measureText "Enable editing"
  let w = Exactly (tw + 24)  -- 20px mark + 4px spacing
      h = Exactly (max 20 th)
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (Layout w h MiddleLeft,
         checkbox CheckboxBox1 "Enable editing" (editingEnabled ps) (\v s ->
           updateBasicControls s (\p -> p { editingEnabled = v })))
    ]

rowButtons :: BasicControlsState -> DemoUI ()
rowButtons ps = do
  (chromW, _) <- measureChrome (Btn 1)
  sizes       <- mapM (fmap (Exactly . sizeWidth) . measureText) ["One", "Two", "Three", "Reset"]
  let btnW = addLength (maxLength sizes) chromW
  vBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4 })
    [ (Layout Fill Fill         TopLeft,
         hBox (defaultBoxConfig { boxSpacing = 4, boxAlignment = Center })
           [ (Layout btnW Fill TopLeft,    btn 1 "One")
           , (Layout btnW Fill TopLeft,    btn 2 "Two")
           , (Layout btnW Fill TopLeft,    btn 3 "Three")
           , (Layout Fill Fill MiddleLeft, label Label ("Clicks: " <> T.pack (show (clickCount ps))))
           , (Layout btnW Fill TopLeft,    resetBtn)
           ])
    , (Layout Fill (Exactly 20) TopLeft,
         progressBar ProgressBar1 (Progress (fromIntegral (clickCount ps) / 50)))
    ]
  where
    btn i txt = do
      clicked <- button (Btn i) txt
      when clicked $ dispatch $
        \s -> updateBasicControls s (\p -> p { clickCount = min 50 (clickCount p + i) })
    resetBtn = do
      clicked <- button (Btn 0) "Reset"
      when clicked $ dispatch $
        \s -> updateBasicControls s (\p -> p { clickCount = 0 })

rowInput :: BasicControlsState -> DemoUI ()
rowInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Text input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, label Label "Text input")
    , (Layout Fill   (Exactly 30) TopLeft,
         textInput TextInput1 (inputText ps) (\t s ->
           updateBasicControls s (\p -> p { inputText = t })))
    ]

rowSlider :: BasicControlsState -> DemoUI ()
rowSlider ps = do
  (chromW, _) <- measureChrome Label
  Size lw _   <- measureText "Slider"
  Size vw _   <- measureText "100%"
  let labelW = addLength (Exactly lw) chromW
      valueW = addLength (Exactly vw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, label Label "Slider")
    , (Layout Fill   (Exactly 30) TopLeft,
         slider Slider1 Horizontal (sliderValue ps) (\v s ->
           updateBasicControls s (\p -> p { sliderValue = v })))
    , (Layout valueW Fill         MiddleLeft,
         label Label (T.pack (show (round (sliderValue ps * 100) :: Int)) <> "%"))
    ]

rowRadio :: BasicControlsState -> DemoUI ()
rowRadio ps = do
  (_, chromH) <- measureChrome Label
  Size _ th   <- measureText "Size"
  let labelH = addLength (Exactly th) chromH
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (Layout Fill Fill TopLeft,
         vBox defaultBoxConfig
           [ (Layout Fill labelH TopLeft, label Label "Size")
           , (Layout Fill Fill    TopLeft,
                radioGroup RadioOpt
                  [(0, "Small"), (1, "Medium"), (2, "Large")]
                  (radioSelection ps)
                  (\v s -> updateBasicControls s (\p -> p { radioSelection = v })))
           ])
    , (Layout Fill Fill TopLeft,
         vBox defaultBoxConfig
           [ (Layout Fill labelH TopLeft, label Label "Priority")
           , (Layout Fill Fill    TopLeft,
                radioGroup RadioOpt2
                  [(0, "Low"), (1, "Medium"), (2, "High"), (3, "Critical")]
                  (radioSelection2 ps)
                  (\v s -> updateBasicControls s (\p -> p { radioSelection2 = v })))
           ])
    ]

updateBasicControls :: AppState -> (BasicControlsState -> BasicControlsState) -> AppState
updateBasicControls s f = case currentPage s of
  BasicControlsPage p -> s { currentPage = BasicControlsPage (f p) }
  _                   -> s

-- Scroll page

scrollView :: ScrollPageState -> DemoUI ()
scrollView ps = do
  (_, chromH) <- measureChrome Label
  Size _ th   <- measureText "Known size (scrollableRegion)"
  let headerH = addLength (Exactly th) chromH
  vBox (defaultBoxConfig { boxMargin = 8, boxSpacing = 4 })
    [ (Layout Fill headerH TopLeft,
         hBox defaultBoxConfig
           [ (Layout Fill Fill MiddleLeft, label Label "Known size (scrollableRegion)")
           , (Layout Fill Fill MiddleLeft, label Label "Dynamic (scrollableDynamic, 100 items)")
           ])
    , (Layout Fill Fill TopLeft,
         hBox (defaultBoxConfig { boxSpacing = 8 })
           [ (Layout Fill Fill TopLeft, staticScrollList ps)
           , (Layout Fill Fill TopLeft, dynamicScrollList ps)
           ])
    ]

staticScrollList :: ScrollPageState -> DemoUI ()
staticScrollList ps =
  scrollableRegion ScrollRegion1 (Size 400 (20 * 32)) $
    vBox defaultBoxConfig
      [ (Layout Fill (Exactly 32) TopLeft, item i)
      | i <- [1 .. 20 :: Int]
      ]
  where
    item i = do
      let isSelected = lastClickedStatic ps == Just i
          txt = (if isSelected then "✓ " else "") <> "Item " <> T.pack (show i)
      clicked <- button (ScrollItem1 i) txt
      when clicked $ dispatch $ \s -> updateScrollState s (\p -> p { lastClickedStatic = Just i })

dynamicScrollList :: ScrollPageState -> DemoUI ()
dynamicScrollList ps = do
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
           let isSelected = lastClickedDynamic ps == Just i
               txt = (if isSelected then "✓ " else "") <> "Item " <> T.pack (show (i + 1))
           clicked <- button (ScrollItem2 i) txt
           when clicked $ dispatch $ \s ->
             updateScrollState s (\p -> p { lastClickedDynamic = Just i })

updateScrollState :: AppState -> (ScrollPageState -> ScrollPageState) -> AppState
updateScrollState s f = case currentPage s of
  ScrollPage p -> s { currentPage = ScrollPage (f p) }
  _            -> s

-- Progress page

progressView :: ProgressState -> DemoUI ()
progressView ps = do
  Size _ th <- measureText "Animate"
  let cbH = Exactly (max 20 th)
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill cbH TopLeft,
         checkbox CheckboxBox3 "Animate" (animating ps) (\v s ->
           updateProgressState s (\p -> p { animating = v })))
    , (Layout Fill Fill TopLeft,
         if animating ps
           then progressBar ProgressBar2 Indeterminate
           else progressBar ProgressBar2 (Progress 0))
    ]

updateProgressState :: AppState -> (ProgressState -> ProgressState) -> AppState
updateProgressState s f = case currentPage s of
  ProgressPage p -> s { currentPage = ProgressPage (f p) }
  _              -> s

-- Layout page

layoutView :: LayoutPageState -> DemoUI ()
layoutView ps = do
  (_, labelChromH) <- measureChrome Label
  Size _ labelTh   <- measureText "Border Layout"
  Size _ cbTh      <- measureText "Fill cross axis"
  let labelH  = addLength (Exactly labelTh) labelChromH
      headerH = maxLength [labelH, Exactly (max 20 cbTh)]
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill headerH       TopLeft, sectionHeader "Border Layout" (pure ()))
    , (Layout Fill (Exactly 200) TopLeft, borderLayoutDemo)
    , (Layout Fill headerH       TopLeft, sectionHeader "hBox" $
         checkbox CheckboxBox4 "Fill cross axis" (hboxFillCross ps)
           (\v s -> updateLayoutState s (\p -> p { hboxFillCross = v })))
    , (Layout Fill (Exactly 100) TopLeft, hboxDemo (hboxFillCross ps))
    , (Layout Fill headerH       TopLeft, sectionHeader "vBox" $
         checkbox CheckboxBox5 "Fill cross axis" (vboxFillCross ps)
           (\v s -> updateLayoutState s (\p -> p { vboxFillCross = v })))
    , (Layout Fill Fill          TopLeft, vboxDemo (vboxFillCross ps))
    ]

sectionHeader :: Text -> DemoUI () -> DemoUI ()
sectionHeader title extra =
  hBox defaultBoxConfig
    [ (Layout Fill          Fill TopLeft, label Label title)
    , (Layout (Exactly 180) Fill TopLeft, extra)
    ]

colorPane :: Colour -> Text -> DemoUI ()
colorPane col lbl = do
  fillRect col
  label Label lbl

borderLayoutDemo :: DemoUI ()
borderLayoutDemo =
  borderLayout emptyBorderContent
    { topPanel    = Just (40,  colorPane (RGBA 0.95 0.72 0.25 1) "top")
    , bottomPanel = Just (30,  colorPane (RGBA 0.60 0.40 0.85 1) "bottom")
    , leftPanel   = Just (80,  colorPane (RGBA 0.18 0.56 0.90 1) "left")
    , rightPanel  = Just (80,  colorPane (RGBA 0.25 0.70 0.48 1) "right")
    , centrePanel = Just      (colorPane (RGBA 0.96 0.92 0.78 1) "centre")
    }

-- Four children with different width constraints; vertical alignment is visible
-- when boxFillCross is off (children are Exactly 48px tall in an Exactly 100px box).
hboxDemo :: Bool -> DemoUI ()
hboxDemo fillCross =
  hBox (defaultBoxConfig { boxFillCross = fillCross })
    [ (Layout (Exactly 80)     (Exactly 48) TopLeft,    colorPane (RGBA 0.95 0.72 0.25 1) "Exactly 80")
    , (Layout Fill             (Exactly 48) MiddleLeft, colorPane (RGBA 0.18 0.56 0.90 1) "Fill")
    , (Layout (AtLeast 60)     (Exactly 48) BottomLeft, colorPane (RGBA 0.25 0.70 0.48 1) "\x2265 60")
    , (Layout (Between 40 120) (Exactly 48) MiddleLeft, colorPane (RGBA 0.60 0.40 0.85 1) "40\x2013\&120")
    ]

-- Three children with different width constraints; horizontal alignment is visible
-- when boxFillCross is off (children are narrower than the full panel width).
vboxDemo :: Bool -> DemoUI ()
vboxDemo fillCross =
  vBox (defaultBoxConfig { boxFillCross = fillCross })
    [ (Layout (Exactly 180) (Exactly 48) TopLeft,    colorPane (RGBA 0.95 0.72 0.25 1) "Exactly 180 / TopLeft")
    , (Layout (Exactly 120) Fill         Center,      colorPane (RGBA 0.18 0.56 0.90 1) "Exactly 120 / Center")
    , (Layout (AtLeast 80)  (Exactly 48) TopRight,   colorPane (RGBA 0.25 0.70 0.48 1) "\x2265 80 / TopRight")
    ]

updateLayoutState :: AppState -> (LayoutPageState -> LayoutPageState) -> AppState
updateLayoutState s f = case currentPage s of
  LayoutPage p -> s { currentPage = LayoutPage (f p) }
  _            -> s

-- Top-level view

demoView :: DemoUI ()
demoView = do
  s     <- getAppState
  input <- getInput
  win   <- getBounds
  let winSize = (round (rectWidth win) :: Int, round (rectHeight win) :: Int)
  (_, chromH) <- measureChrome Label
  Size _ th   <- measureText "Window: 1920 x 1080"
  let footerH = preferredSize (addLength (Exactly th) (addLength chromH (Exactly 8))) 0
  when (darkMode s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  borderLayout emptyBorderContent
    { leftPanel   = Just (180, sidebar s)
    , centrePanel = Just (centrePane s)
    , bottomPanel = Just (footerH, footer s winSize)
    }
  mHov  <- getHoveredElement
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
