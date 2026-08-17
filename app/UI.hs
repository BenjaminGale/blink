{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink.App
import Blink.Controls
import Blink.Geometry
import Blink.Input
import Blink.Layout
import Blink.Rendering
import Blink.UI
import Blink.Update
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when)
import Data.Char (isDigit)
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
  , radioSize       :: Maybe Text
  , radioSizeNoTab  :: Maybe Text
  }

initialBasicControls :: BasicControlsState
initialBasicControls = BasicControlsState
  { clickCount      = 0
  , inputText       = ""
  , numberText      = ""
  , passwordText    = ""
  , editingEnabled  = False
  , sliderValue     = 0.5
  , radioSize       = Nothing
  , radioSizeNoTab  = Nothing
  }

newtype ScrollPageState = ScrollPageState
  { lastClickedStatic  :: Maybe Int
  }

initialScrollState :: ScrollPageState
initialScrollState = ScrollPageState Nothing

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
  | SetRadioSize Text
  | SetRadioSizeNoTab Text

newtype ScrollMsg
  = ClickedStaticItem Int

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
  SetRadioSize t       -> p { radioSize = Just t }
  SetRadioSizeNoTab t  -> p { radioSizeNoTab = Just t }

applyScrollMsg :: ScrollMsg -> ScrollPageState -> ScrollPageState
applyScrollMsg (ClickedStaticItem i) p = p { lastClickedStatic = Just i }

applyProgressMsg :: ProgressMsg -> ProgressState -> ProgressState
applyProgressMsg (ToggleAnimating v) p = p { animating = v }

applyLayoutMsg :: LayoutMsg -> LayoutPageState -> LayoutPageState
applyLayoutMsg msg p = case msg of
  ToggleHboxFillCross v -> p { hboxFillCross = v }
  ToggleVboxFillCross v -> p { vboxFillCross = v }

type DemoUI = UI Element Msg

-- Shell

-- | Plain, non-interactive text under the shared 'Label' element ID.
caption :: Text -> DemoUI ()
caption t = label Label [text t]

sidebar :: AppState -> DemoUI ()
sidebar s = do
  (_, btnChromH) <- measureChrome (NavBtn 0)
  Size _ th      <- measureText "Basic Controls"
  let btnH = addLength (Exactly th) btnChromH
  Size _ cbTh      <- measureText "Dark mode"
  (_, cbChromH)    <- measureChrome (CheckboxN 2 CheckboxBox)
  let cbH = addLength (Exactly (max 20 cbTh)) cbChromH
  vBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 8 })
    [ (Layout Fill btnH TopLeft, navBtn 0 "Basic Controls")
    , (Layout Fill btnH TopLeft, navBtn 1 "Scroll")
    , (Layout Fill btnH TopLeft, navBtn 2 "Progress")
    , (Layout Fill btnH TopLeft, navBtn 3 "Layout")
    , (Layout Fill Fill  TopLeft, pure ())
    , (Layout Fill cbH  TopLeft,
         checkbox (CheckboxN 2) [text "Dark mode", checked (darkMode s), onToggle (postWith SetDarkMode)])
    ]
  where
    navBtn i lbl = button (NavBtn i) [text lbl, onClick (post (Navigate i))]

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
      [ (Layout winW'    Fill MiddleLeft, caption winText)
      , (Layout mouseW'  Fill MiddleLeft, caption mouseText)
      , (Layout buttonW' Fill MiddleLeft, caption buttonText)
      , (Layout hoverW'  Fill MiddleLeft, caption hoverText)
      , (Layout Fill               Fill MiddleLeft, caption keyText)
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
    , (Layout Fill (Exactly 30)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowRadioGroup ps)
    , (Layout Fill (Exactly 30)  TopLeft, disableWhen (not (editingEnabled ps)) $ rowRadioGroupNoTab ps)
    ]

rowCheckboxes :: BasicControlsState -> DemoUI ()
rowCheckboxes ps = do
  Size tw th        <- measureText "Enable editing"
  (chromW, chromH)  <- measureChrome (CheckboxN 1 CheckboxBox)
  let w = addLength (Exactly (tw + 24)) chromW  -- 20px mark + 4px spacing
      h = addLength (Exactly (max 20 th)) chromH
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (Layout w h MiddleLeft,
         checkbox (CheckboxN 1)
           [text "Enable editing", checked (editingEnabled ps)
           , onToggle (postWith (BasicControlsMsg . ToggleEditing))])
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
           , (Layout Fill Fill MiddleLeft, caption ("Clicks: " <> T.pack (show (clickCount ps))))
           , (Layout btnW Fill TopLeft,    resetBtn)
           ])
    , (Layout Fill (Exactly 20) TopLeft,
         progressBar ProgressBar1 [progress (Progress (fromIntegral (clickCount ps) / 50))])
    ]
  where
    btn i txt = button (Btn i) [text txt, onClick (post (BasicControlsMsg (AddClicks i)))]
    resetBtn  = button (Btn 0) [text "Reset", onClick (post (BasicControlsMsg ResetClicks))]

rowInput :: BasicControlsState -> DemoUI ()
rowInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Text input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, caption "Text input")
    , (Layout Fill   (Exactly 30) TopLeft,
         textInputControl TextInput1
           [text (inputText ps), onInput (postWith (BasicControlsMsg . SetInputText))])
    ]

rowNumberInput :: BasicControlsState -> DemoUI ()
rowNumberInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Number input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, caption "Number input")
    , (Layout Fill   (Exactly 30) TopLeft,
         textInputControl NumberInput1
           [text (numberText ps), inputFilter (T.filter isDigit), onInput (postWith (BasicControlsMsg . SetNumberText))])
    ]

rowPasswordInput :: BasicControlsState -> DemoUI ()
rowPasswordInput ps = do
  (chromW, _) <- measureChrome Label
  Size tw _   <- measureText "Password input"
  let labelW = addLength (Exactly tw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, caption "Password input")
    , (Layout Fill   (Exactly 30) TopLeft,
         textInputControl PasswordInput1
           [text (passwordText ps), displayFilter (T.map (const '•')), onInput (postWith (BasicControlsMsg . SetPasswordText))])
    ]

rowSlider :: BasicControlsState -> DemoUI ()
rowSlider ps = do
  (chromW, _) <- measureChrome Label
  Size lw _   <- measureText "Slider"
  Size vw _   <- measureText "100%"
  let labelW = addLength (Exactly lw) chromW
      valueW = addLength (Exactly vw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill         MiddleLeft, caption "Slider")
    , (Layout Fill   (Exactly 30) TopLeft,
         slider Slider1
           [orientation Horizontal, value (sliderValue ps)
           , onChange (postWith (BasicControlsMsg . SetSliderValue))])
    , (Layout valueW Fill         MiddleLeft,
         caption (T.pack (show (round (sliderValue ps * 100) :: Int)) <> "%"))
    ]

radioSizeOptions :: [Text]
radioSizeOptions = ["Small", "Medium", "Large"]

rowRadioGroup :: BasicControlsState -> DemoUI ()
rowRadioGroup ps = do
  (chromW, _) <- measureChrome Label
  Size lw _   <- measureText "Size"
  let labelW = addLength (Exactly lw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill MiddleLeft, caption "Size")
    , (Layout Fill   Fill TopLeft,
         radioGroup RadioSize $
           [ items radioSizeOptions
           , orientation Horizontal
           , itemLabel id
           , onSelect (\_ v -> [OutMsg (BasicControlsMsg (SetRadioSize v))])
           ] ++ [selected v | Just v <- [radioSize ps]])
    ]

-- | Same as 'rowRadioGroup', but with the group's own 'tabStop' off: the
-- group itself is never a Tab stop, and each Small\/Medium\/Large item
-- becomes individually Tab-reachable instead.
rowRadioGroupNoTab :: BasicControlsState -> DemoUI ()
rowRadioGroupNoTab ps = do
  (chromW, _) <- measureChrome Label
  Size lw _   <- measureText "Size (no group tab stop)"
  let labelW = addLength (Exactly lw) chromW
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (Layout labelW Fill MiddleLeft, caption "Size (no group tab stop)")
    , (Layout Fill   Fill TopLeft,
         radioGroup RadioSizeNoTab $
           [ items radioSizeOptions
           , orientation Horizontal
           , itemLabel id
           , tabStop False
           , onSelect (\_ v -> [OutMsg (BasicControlsMsg (SetRadioSizeNoTab v))])
           ] ++ [selected v | Just v <- [radioSizeNoTab ps]])
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
    [ (Layout Fill headerH TopLeft, caption "Known size (viewport)")
    , (Layout Fill Fill    TopLeft, staticScrollList ps)
    ]

staticScrollList :: ScrollPageState -> DemoUI ()
staticScrollList ps =
  viewport ScrollRegion1 [contentSize (Size 400 (20 * 32))] $
    vBox defaultBoxConfig
      [ (Layout Fill (Exactly 32) TopLeft, item i)
      | i <- [1 .. 20 :: Int]
      ]
  where
    item i = do
      let isSelected = lastClickedStatic ps == Just i
          txt = (if isSelected then "✓ " else "") <> "Item " <> T.pack (show i)
      button (ScrollItem1 i)
        [text txt, onClick (post (ScrollMsg (ClickedStaticItem i)))]

updateScrollState :: AppState -> (ScrollPageState -> ScrollPageState) -> AppState
updateScrollState s f = case currentPage s of
  ScrollPage p -> s { currentPage = ScrollPage (f p) }
  _            -> s

-- Progress page

progressView :: ProgressState -> DemoUI ()
progressView ps = do
  Size _ th     <- measureText "Animate"
  (_, cbChromH) <- measureChrome (CheckboxN 3 CheckboxBox)
  let cbH = addLength (Exactly (max 20 th)) cbChromH
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill cbH TopLeft,
         checkbox (CheckboxN 3)
           [text "Animate", checked (animating ps), onToggle (postWith (ProgressMsg . ToggleAnimating))])
    , (Layout Fill Fill TopLeft,
         if animating ps
           then progressBar ProgressBar2 [progress Indeterminate]
           else progressBar ProgressBar2 [progress (Progress 0)])
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
  (_, cbChromH)    <- measureChrome (CheckboxN 4 CheckboxBox)
  let labelH  = addLength (Exactly labelTh) labelChromH
      headerH = maxLength [labelH, addLength (Exactly (max 20 cbTh)) cbChromH]
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
    [ (Layout Fill headerH       TopLeft, sectionHeader "Border Layout" (pure ()))
    , (Layout Fill (Exactly 200) TopLeft, borderLayoutDemo)
    , (Layout Fill headerH       TopLeft, sectionHeader "hBox" $
         checkbox (CheckboxN 4)
           [text "Fill cross axis", checked (hboxFillCross ps), onToggle (postWith (LayoutMsg . ToggleHboxFillCross))])
    , (Layout Fill (Exactly 100) TopLeft, hboxDemo (hboxFillCross ps))
    , (Layout Fill headerH       TopLeft, sectionHeader "vBox" $
         checkbox (CheckboxN 5)
           [text "Fill cross axis", checked (vboxFillCross ps), onToggle (postWith (LayoutMsg . ToggleVboxFillCross))])
    , (Layout Fill Fill          TopLeft, vboxDemo (vboxFillCross ps))
    ]

sectionHeader :: Text -> DemoUI () -> DemoUI ()
sectionHeader title extra =
  hBox defaultBoxConfig
    [ (Layout Fill          Fill TopLeft, caption title)
    , (Layout (Exactly 180) Fill TopLeft, extra)
    ]

colorPane :: Colour -> Text -> DemoUI ()
colorPane col lbl = do
  fillRect col
  caption lbl

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
  anyHov <- isAnyMouseOver
  let typed   = T.concat (inputTypedText input)
      keyName = case inputKeyEvents input of
                  []      -> ""
                  (e : _) -> T.pack (show (key e))
      newInput = if not (T.null typed)
                   then if typed == " " then "Space" else "Character " <> typed
                   else keyName
  emit (FrameObserved anyHov newInput)
