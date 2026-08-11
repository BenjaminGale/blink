{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

-- Per-page state

data BasicControlsState = BasicControlsState
  { clickCount      :: Int
  , inputText       :: Text
  , numberText      :: Text
  , passwordText    :: Text
  , editingEnabled  :: Bool
  , sliderValue     :: Double
  , radioSelection  :: Int
  , radioSelection2 :: Int
  }

initialBasicControls :: BasicControlsState
initialBasicControls = BasicControlsState
  { clickCount      = 0
  , inputText       = ""
  , numberText      = ""
  , passwordText    = ""
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

-- Messages

data BasicControlsMsg
  = ToggleEditing Bool
  | AddClicks Int
  | ResetClicks
  | SetInputText Text
  | SetNumberText Text
  | SetPasswordText Text
  | SetSliderValue Double
  | SetRadioSelection Int
  | SetRadioSelection2 Int

data ScrollMsg
  = ClickedStaticItem Int
  | SelectedDynamicItem Int

newtype ProgressMsg = ToggleAnimating Bool

data LayoutMsg
  = ToggleHboxFillCross Bool
  | ToggleVboxFillCross Bool

data Msg
  = Navigate Int
  | SetDarkMode Bool
  | BasicControlsMsg BasicControlsMsg
  | ScrollMsg ScrollMsg
  | ProgressMsg ProgressMsg
  | LayoutMsg LayoutMsg
  | FrameObserved Bool Text  -- ^ mouse-is-hovering, this frame's raw key/typed-text label

demoApp :: App Element Msg AppState
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
  , update  = updateApp
  }

updateApp :: Msg -> Update AppState ()
updateApp msg = case msg of
  Navigate i           -> modify $ \s -> s { currentPage = pageForIndex i }
  SetDarkMode v        -> modify $ \s -> s { darkMode = v }
  BasicControlsMsg m   -> modify $ \s -> updateBasicControls s (applyBasicControlsMsg m)
  ScrollMsg m          -> modify $ \s -> updateScrollState s (applyScrollMsg m)
  ProgressMsg m        -> modify $ \s -> updateProgressState s (applyProgressMsg m)
  LayoutMsg m          -> modify $ \s -> updateLayoutState s (applyLayoutMsg m)
  FrameObserved hov keyLabel -> modify $ \s -> s
    { isHovering     = hov
    , lastInput      = if T.null keyLabel then lastInput s else keyLabel
    , lastInputCount = if T.null keyLabel then lastInputCount s
                       else if keyLabel == lastInput s then lastInputCount s + 1
                       else 1
    }

applyBasicControlsMsg :: BasicControlsMsg -> BasicControlsState -> BasicControlsState
applyBasicControlsMsg msg p = case msg of
  ToggleEditing v      -> p { editingEnabled = v }
  AddClicks i          -> p { clickCount = min 50 (clickCount p + i) }
  ResetClicks          -> p { clickCount = 0 }
  SetInputText t       -> p { inputText = t }
  SetNumberText t      -> p { numberText = t }
  SetPasswordText t    -> p { passwordText = t }
  SetSliderValue v     -> p { sliderValue = v }
  SetRadioSelection v  -> p { radioSelection = v }
  SetRadioSelection2 v -> p { radioSelection2 = v }

applyScrollMsg :: ScrollMsg -> ScrollPageState -> ScrollPageState
applyScrollMsg msg p = case msg of
  ClickedStaticItem i    -> p { lastClickedStatic = Just i }
  SelectedDynamicItem i  -> p { lastClickedDynamic = Just i }

applyProgressMsg :: ProgressMsg -> ProgressState -> ProgressState
applyProgressMsg (ToggleAnimating v) p = p { animating = v }

applyLayoutMsg :: LayoutMsg -> LayoutPageState -> LayoutPageState
applyLayoutMsg msg p = case msg of
  ToggleHboxFillCross v -> p { hboxFillCross = v }
  ToggleVboxFillCross v -> p { vboxFillCross = v }

type DemoUI = UI Element Msg

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
         checkbox CheckboxBox2 "Dark mode" (darkMode s) [onToggle SetDarkMode])
    ]
  where
    navBtn i lbl = button (NavBtn i) lbl [onClick (Navigate i)]

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
  renderChrome StatusBar $
    hBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 4, boxAlignment = Center })
      [ (Layout winW'    Fill MiddleLeft, label Label winText [])
      , (Layout mouseW'  Fill MiddleLeft, label Label mouseText [])
      , (Layout buttonW' Fill MiddleLeft, label Label buttonText [])
      , (Layout hoverW'  Fill MiddleLeft, label Label hoverText [])
      , (Layout Fill               Fill MiddleLeft, label Label keyText [])
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
    , (Layout Fill (Exactly 50)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowNumberInput ps)
    , (Layout Fill (Exactly 50)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowPasswordInput ps)
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
         checkbox CheckboxBox1 "Enable editing" (editingEnabled ps)
           [onToggle (BasicControlsMsg . ToggleEditing)])
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
           , (Layout Fill Fill MiddleLeft, label Label ("Clicks: " <> T.pack (show (clickCount ps))) [])
           , (Layout btnW Fill TopLeft,    resetBtn)
           ])
    , (Layout Fill (Exactly 20) TopLeft,
         progressBar ProgressBar1 (Progress (fromIntegral (clickCount ps) / 50)) [])
    ]
  where
    btn i txt = button (Btn i) txt [onClick (BasicControlsMsg (AddClicks i))]
    resetBtn  = button (Btn 0) "Reset" [onClick (BasicControlsMsg ResetClicks)]

rowInput :: BasicControlsState -> DemoUI ()
rowInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Text input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, label Label "Text input" [])
    , (Layout Fill   (Exactly 30) TopLeft,
         textInputControl TextInput1 (inputText ps)
           [onInput (BasicControlsMsg . SetInputText)])
    ]

rowNumberInput :: BasicControlsState -> DemoUI ()
rowNumberInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Number input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, label Label "Number input" [])
    , (Layout Fill   (Exactly 30) TopLeft,
         textInputControl NumberInput1 (numberText ps)
           [inputFilter (T.filter isDigit), onInput (BasicControlsMsg . SetNumberText)])
    ]

rowPasswordInput :: BasicControlsState -> DemoUI ()
rowPasswordInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Password input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, label Label "Password input" [])
    , (Layout Fill   (Exactly 30) TopLeft,
         textInputControl PasswordInput1 (passwordText ps)
           [displayFilter (T.map (const '•')), onInput (BasicControlsMsg . SetPasswordText)])
    ]

rowSlider :: BasicControlsState -> DemoUI ()
rowSlider ps = do
  (chromW, _) <- measureChrome Label
  Size lw _   <- measureText "Slider"
  Size vw _   <- measureText "100%"
  let labelW = addLength (Exactly lw) chromW
      valueW = addLength (Exactly vw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, label Label "Slider" [])
    , (Layout Fill   (Exactly 30) TopLeft,
         slider Slider1 Horizontal (sliderValue ps)
           [onChange (BasicControlsMsg . SetSliderValue)])
    , (Layout valueW Fill         MiddleLeft,
         label Label (T.pack (show (round (sliderValue ps * 100) :: Int)) <> "%") [])
    ]

rowRadio :: BasicControlsState -> DemoUI ()
rowRadio ps = do
  (_, chromH) <- measureChrome Label
  Size _ th   <- measureText "Size"
  let labelH = addLength (Exactly th) chromH
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (Layout Fill Fill TopLeft,
         vBox defaultBoxConfig
           [ (Layout Fill labelH TopLeft, label Label "Size" [])
           , (Layout Fill Fill    TopLeft,
                radioGroup RadioOpt
                  [(0, "Small"), (1, "Medium"), (2, "Large")]
                  (radioSelection ps)
                  [onSelect (BasicControlsMsg . SetRadioSelection)])
           ])
    , (Layout Fill Fill TopLeft,
         vBox defaultBoxConfig
           [ (Layout Fill labelH TopLeft, label Label "Priority" [])
           , (Layout Fill Fill    TopLeft,
                radioGroup RadioOpt2
                  [(0, "Low"), (1, "Medium"), (2, "High"), (3, "Critical")]
                  (radioSelection2 ps)
                  [onSelect (BasicControlsMsg . SetRadioSelection2)])
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
  Size _ th   <- measureText "Known size (viewport)"
  let headerH = addLength (Exactly th) chromH
  vBox (defaultBoxConfig { boxMargin = 8, boxSpacing = 4 })
    [ (Layout Fill headerH TopLeft,
         hBox defaultBoxConfig
           [ (Layout Fill Fill MiddleLeft, label Label "Known size (viewport)" [])
           , (Layout Fill Fill MiddleLeft, label Label "Virtualized (listBox, 100 items)" [])
           ])
    , (Layout Fill Fill TopLeft,
         hBox (defaultBoxConfig { boxSpacing = 8 })
           [ (Layout Fill Fill TopLeft, staticScrollList ps)
           , (Layout Fill Fill TopLeft, dynamicScrollList ps)
           ])
    ]

staticScrollList :: ScrollPageState -> DemoUI ()
staticScrollList ps =
  viewport ScrollRegion1 (Size 400 (20 * 32)) [] $
    vBox defaultBoxConfig
      [ (Layout Fill (Exactly 32) TopLeft, item i)
      | i <- [1 .. 20 :: Int]
      ]
  where
    item i = do
      let isSelected = lastClickedStatic ps == Just i
          txt = (if isSelected then "✓ " else "") <> "Item " <> T.pack (show i)
      button (ScrollItem1 i) txt
        [onClick (ScrollMsg (ClickedStaticItem i))]

dynamicScrollList :: ScrollPageState -> DemoUI ()
dynamicScrollList ps =
  listBox ScrollList2 itemH items selected [onSelect (ScrollMsg . SelectedDynamicItem)] renderRow
  where
    itemH  = 32 :: Double
    items  = [ (i, "Item " <> T.pack (show (i + 1))) | i <- [0 .. 99 :: Int] ]
    selected = maybe (-1) id (lastClickedDynamic ps)

    renderRow eid isSelected (_, txt) = do
      style <- getStyle eid
      let rowText = (if isSelected then "✓ " else "") <> txt
      drawText (styleTextColour style) (styleTextAlign style) rowText

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
         checkbox CheckboxBox3 "Animate" (animating ps)
           [onToggle (ProgressMsg . ToggleAnimating)])
    , (Layout Fill Fill TopLeft,
         if animating ps
           then progressBar ProgressBar2 Indeterminate []
           else progressBar ProgressBar2 (Progress 0) [])
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
           [onToggle (LayoutMsg . ToggleHboxFillCross)])
    , (Layout Fill (Exactly 100) TopLeft, hboxDemo (hboxFillCross ps))
    , (Layout Fill headerH       TopLeft, sectionHeader "vBox" $
         checkbox CheckboxBox5 "Fill cross axis" (vboxFillCross ps)
           [onToggle (LayoutMsg . ToggleVboxFillCross)])
    , (Layout Fill Fill          TopLeft, vboxDemo (vboxFillCross ps))
    ]

sectionHeader :: Text -> DemoUI () -> DemoUI ()
sectionHeader title extra =
  hBox defaultBoxConfig
    [ (Layout Fill          Fill TopLeft, label Label title [])
    , (Layout (Exactly 180) Fill TopLeft, extra)
    ]

colorPane :: Colour -> Text -> DemoUI ()
colorPane col lbl = do
  fillRect col
  label Label lbl []

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

demoView :: AppState -> DemoUI ()
demoView s = do
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
  emit (FrameObserved (isJust mHov) newInput)
