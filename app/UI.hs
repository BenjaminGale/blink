{-# LANGUAGE OverloadedStrings #-}
module UI (ControlId, AppState (..), demoApp) where

import Blink.App
import Blink.Attribute (Attribute)
import Blink.Controls
import Blink.Controls.Button (onActivated)
import Blink.Controls.Control (isEnabled)
import Blink.Controls.Divider (orientation)
import Blink.Controls.Element (post, postWith)
import Blink.Controls.Label (LabelConfig, target, text)
import Blink.Controls.ProgressBar (ProgressValue (..), progress)
import Blink.Controls.RepeatButton
  (firedCount, onFiredCountChanged, onPressEnded, onPressStarted, pressStartedAt)
import Blink.Controls.Slider (onValueChanged)
import qualified Blink.Controls.Slider as Slider (value)
import Blink.Controls.TextInput (displayFilter, onInput, value)
import Blink.Controls.Toggle (isSelected, onSelectedChanged)
import Blink.Geometry
import Blink.Input
import Blink.Layout
import Blink.Rendering
import Blink.UI
import Blink.UI.Element (Element, elementWithLayout, runElement)
import Blink.Update
import Theme (ControlId (..), lightTheme, darkTheme)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T

-- Application state

data AppState = AppState
  { darkMode       :: Bool
  , editingEnabled :: Bool
  , clickCount     :: Int
  , toggleOn       :: Bool
  , radioChoice    :: Maybe Text
  , inputText      :: Text
  , passwordText   :: Text
  , animating      :: Bool
  , holdPressedAt  :: Maybe Double
  , holdFiredCount :: Int
  , sliderValue    :: Double
  , isHovering     :: Bool
  , lastInput      :: Text
  , lastInputCount :: Int
  }

data Msg
  = SetDarkMode Bool
  | SetEditingEnabled Bool
  | AddClick
  | ResetClicks
  | SetToggle Bool
  | PickRadio Text
  | SetInputText Text
  | SetPasswordText Text
  | SetAnimating Bool
  | HoldPressStarted Double
  | HoldFiredCountChanged Int
  | HoldPressEnded
  | SetSlider Double
  | FrameObserved Bool Text  -- ^ mouse-is-hovering, this frame's raw key/typed-text label

demoApp :: App ControlId Msg AppState
demoApp = App
  { startUp = pure AppState
      { darkMode       = False
      , editingEnabled = True
      , clickCount     = 0
      , toggleOn       = False
      , radioChoice    = Nothing
      , inputText      = ""
      , passwordText   = ""
      , animating      = False
      , holdPressedAt  = Nothing
      , holdFiredCount = 0
      , sliderValue    = 0.5
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
  SetDarkMode v       -> modify $ \s -> s { darkMode = v }
  SetEditingEnabled v -> modify $ \s -> s { editingEnabled = v }
  AddClick             -> modify $ \s -> s { clickCount = min 50 (clickCount s + 1) }
  ResetClicks           -> modify $ \s -> s { clickCount = 0 }
  SetToggle v          -> modify $ \s -> s { toggleOn = v }
  PickRadio v          -> modify $ \s -> s { radioChoice = Just v }
  SetInputText t       -> modify $ \s -> s { inputText = t }
  SetPasswordText t    -> modify $ \s -> s { passwordText = t }
  SetAnimating v       -> modify $ \s -> s { animating = v }
  HoldPressStarted t      -> modify $ \s -> s { holdPressedAt = Just t, holdFiredCount = 0 }
  HoldFiredCountChanged n -> modify $ \s -> s { holdFiredCount = n }
  HoldPressEnded          -> modify $ \s -> s { holdPressedAt = Nothing, holdFiredCount = 0 }
  SetSlider v          -> modify $ \s -> s { sliderValue = v }
  FrameObserved hov keyLabel -> modify $ \s -> s
    { isHovering     = hov
    , lastInput      = if T.null keyLabel then lastInput s else keyLabel
    , lastInputCount = if T.null keyLabel then lastInputCount s
                       else if keyLabel == lastInput s then lastInputCount s + 1
                       else 1
    }

type DemoUI = UI ControlId Msg

-- Shell

-- | Plain, non-interactive text under the shared 'Label' element ID.
caption :: Text -> [Attribute (LabelConfig ControlId Msg)] -> Element ControlId Msg
caption t attrs = label Label (text t : attrs)

-- | A row pairing a caption with the control it describes: fixed-width
-- label on the left (redirecting clicks to @targetId@ via 'target'),
-- control filling the rest. Takes 'rowLayout' (or another layout) so the
-- row's own width\/height fit alongside its siblings.
field :: [Attribute (BoxConfig ControlId Msg)] -> ControlId -> Text -> Element ControlId Msg -> Element ControlId Msg
field layoutAttrs targetId labelText control =
  hBox
    ( layoutAttrs ++
      [ spacing 8, alignment Center
      , children
          [ caption labelText [target targetId, width (exactly 120), height fill, align MiddleLeft]
          , control
          ]
      ]
    )

rowHeight :: Length
rowHeight = exactly 40

-- | Layout attributes shared by every direct child of 'mainList': the full
-- row width, the shared row height, top-left within that slot.
rowLayout :: HasLayoutConfig cfg => [Attribute cfg]
rowLayout = [width fill, height rowHeight, align TopLeft]

-- Control rows, top to bottom

rowDarkMode :: AppState -> Element ControlId Msg
rowDarkMode s =
  checkbox DarkModeCheckbox (rowLayout ++ [text "Dark mode", isSelected (darkMode s), onSelectedChanged (postWith SetDarkMode)])

rowEditing :: AppState -> Element ControlId Msg
rowEditing s =
  checkbox EditingCheckbox
    (rowLayout ++ [text "Enable editing", isSelected (editingEnabled s), onSelectedChanged (postWith SetEditingEnabled)])

-- | A plain full-width separator between the settings checkboxes above and
-- the interactive controls below -- 'divider's own default orientation and
-- thickness, and (since nothing here reacts to it) no id either.
rowDivider :: Element ControlId Msg
rowDivider = divider []

rowButtons :: AppState -> Element ControlId Msg
rowButtons s =
  hBox
    ( rowLayout ++
      [ spacing 8
      , children
          [ button ClickButton [text "Click me", onActivated (post AddClick), isEnabled (editingEnabled s), width (exactly 100), height fill]
          , repeatButton HoldButton
              [ text "Hold me", onActivated (post AddClick), isEnabled (editingEnabled s), width (exactly 100), height fill
              , pressStartedAt (holdPressedAt s), onPressStarted (postWith HoldPressStarted)
              , firedCount (holdFiredCount s), onFiredCountChanged (postWith HoldFiredCountChanged)
              , onPressEnded [OutMsg HoldPressEnded]
              ]
          , button ResetButton [text "Reset", onActivated (post ResetClicks), isEnabled (editingEnabled s), width (exactly 100), height fill]
          , divider [orientation Vertical, height fill]
          , caption ("Clicks: " <> T.pack (show (clickCount s))) [width fill, height fill, align MiddleLeft]
          ]
      ]
    )

rowToggle :: AppState -> Element ControlId Msg
rowToggle s =
  hBox
    ( rowLayout ++
      [ spacing 8
      , children
          [ toggleButton ToggleCtl
              [ text "Toggle me", isSelected (toggleOn s), onSelectedChanged (postWith SetToggle)
              , isEnabled (editingEnabled s), width (exactly 160), height fill
              ]
          , caption (if toggleOn s then "On" else "Off") [width fill, height fill, align MiddleLeft]
          ]
      ]
    )

radioOptions :: [Text]
radioOptions = ["Small", "Medium", "Large"]

rowRadio :: AppState -> Element ControlId Msg
rowRadio s =
  hBox
    ( rowLayout ++
      [ spacing 16
      , children
          [ radioOption i opt
          | (i, opt) <- zip [0 ..] radioOptions
          ]
      ]
    )
  where
    radioOption i opt =
      radioButton (RadioCtl i)
        [ text opt, isSelected (radioChoice s == Just opt), onSelectedChanged (\_ -> [OutMsg (PickRadio opt)])
        , isEnabled (editingEnabled s), width (exactly 100), height fill, align MiddleLeft
        ]

rowTextInput :: AppState -> Element ControlId Msg
rowTextInput s =
  field rowLayout TextInputCtl "Text input"
    (textInput TextInputCtl [value (inputText s), onInput (postWith SetInputText), isEnabled (editingEnabled s), height fill])

rowPasswordInput :: AppState -> Element ControlId Msg
rowPasswordInput s =
  field rowLayout PasswordInputCtl "Password input"
    (textInput PasswordInputCtl
        [ value (passwordText s), displayFilter (T.map (const '\8226')), onInput (postWith SetPasswordText)
        , isEnabled (editingEnabled s), height fill
        ])

rowAnimate :: AppState -> Element ControlId Msg
rowAnimate s =
  checkbox AnimateCheckbox
    (rowLayout ++ [text "Animate progress bar", isSelected (animating s), onSelectedChanged (postWith SetAnimating), isEnabled (editingEnabled s)])

rowProgress :: AppState -> Element ControlId Msg
rowProgress s =
  if animating s
    then progressBar (rowLayout ++ [progress Indeterminate])
    else progressBar (rowLayout ++ [progress (Progress (fromIntegral (clickCount s) / 50))])

rowSlider :: AppState -> Element ControlId Msg
rowSlider s =
  field rowLayout SliderCtl "Slider"
    ( hBox
        [ spacing 8, alignment Center
        , children
            [ slider SliderCtl [Slider.value (sliderValue s), onValueChanged (postWith SetSlider), isEnabled (editingEnabled s), height fill]
            , caption (T.pack (show (round (sliderValue s * 100) :: Int)) <> "%") [width (exactly 60), height fill, align MiddleLeft]
            ]
        ]
    )

-- Footer

footer :: AppState -> DemoUI ()
footer s = do
  pos    <- getMousePos
  input  <- getInput
  win    <- getWindowSize
  let winW      = round (rectWidth win) :: Int
      winH      = round (rectHeight win) :: Int
      winText    = "Window: " <> T.pack (show winW) <> " x " <> T.pack (show winH)
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
  runElement $ hBox
    [ spacing 24, margin 4, alignment Center
    , children
        [ caption winText    [width (exactly 160), height fill, align MiddleLeft]
        , caption mouseText  [width (exactly 160), height fill, align MiddleLeft]
        , caption buttonText [width (exactly 160), height fill, align MiddleLeft]
        , caption hoverText  [width (exactly 100), height fill, align MiddleLeft]
        , caption keyText    [width fill,          height fill, align MiddleLeft]
        ]
    ]

-- Top-level view

demoView :: AppState -> Element ControlId Msg
demoView s = elementWithLayout (Layout fill fill TopLeft) $ do
  input <- getInput
  when (darkMode s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  borderLayout [centre (mainList s), bottom 36 (footer s)]
  anyHov <- isAnyMouseOver
  let typed   = T.concat (inputTypedText input)
      keyName = case inputKeyEvents input of
                  []      -> ""
                  (e : _) -> T.pack (show (key e))
      newInput = if not (T.null typed)
                   then if typed == " " then "Space" else "Character " <> typed
                   else keyName
  emit (FrameObserved anyHov newInput)

mainList :: AppState -> DemoUI ()
mainList s =
  runElement $ vBox
    [ spacing 8, margin 12
    , children
        [ caption "Blink controls demo" [width fill, height (exactly 24), align TopLeft]
        , rowDarkMode s
        , rowEditing s
        , rowDivider
        , rowButtons s
        , rowToggle s
        , rowRadio s
        , rowTextInput s
        , rowPasswordInput s
        , rowAnimate s
        , rowProgress s
        , rowSlider s
        ]
    ]
