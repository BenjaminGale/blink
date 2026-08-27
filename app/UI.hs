{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink.App
import Blink.Attribute (Attribute)
import Blink.Controls
import Blink.Controls.Button (onActivated)
import Blink.Controls.Control (isEnabled)
import Blink.Controls.Element (post, postWith)
import Blink.Controls.Label (LabelConfig, text)
import Blink.Controls.ProgressBar (ProgressValue (..), progress)
import Blink.Controls.TextInput (displayFilter, onInput, value)
import Blink.Controls.Toggle (isSelected, onSelectedChanged)
import Blink.Geometry
import Blink.Input
import Blink.Layout
import Blink.Rendering
import Blink.UI
import Blink.UI.Element (runElement)
import qualified Blink.UI.Element as UIElement (Element)
import Blink.Update
import Theme (Element (..), lightTheme, darkTheme)
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
  | FrameObserved Bool Text  -- ^ mouse-is-hovering, this frame's raw key/typed-text label

demoApp :: App Element Msg AppState
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
  FrameObserved hov keyLabel -> modify $ \s -> s
    { isHovering     = hov
    , lastInput      = if T.null keyLabel then lastInput s else keyLabel
    , lastInputCount = if T.null keyLabel then lastInputCount s
                       else if keyLabel == lastInput s then lastInputCount s + 1
                       else 1
    }

type DemoUI = UI Element Msg

-- Shell

-- | Plain, non-interactive text under the shared 'Label' element ID.
caption :: Text -> [Attribute (LabelConfig Element Msg)] -> UIElement.Element Element Msg
caption t attrs = label Label (text t : attrs)

rowHeight :: Length
rowHeight = Exactly 40

-- | Layout attributes shared by every direct child of 'mainList': the full
-- row width, the shared row height, top-left within that slot.
rowLayout :: HasLayoutConfig cfg => [Attribute cfg]
rowLayout = [width Fill, height rowHeight, align TopLeft]

-- Control rows, top to bottom

rowDarkMode :: AppState -> UIElement.Element Element Msg
rowDarkMode s =
  checkbox DarkModeCheckbox (rowLayout ++ [text "Dark mode", isSelected (darkMode s), onSelectedChanged (postWith SetDarkMode)])

rowEditing :: AppState -> UIElement.Element Element Msg
rowEditing s =
  checkbox EditingCheckbox
    (rowLayout ++ [text "Enable editing", isSelected (editingEnabled s), onSelectedChanged (postWith SetEditingEnabled)])

rowButtons :: AppState -> UIElement.Element Element Msg
rowButtons s =
  hBox
    ( rowLayout ++
      [ spacing 8
      , children
          [ button ClickButton [text "Click me", onActivated (post AddClick), isEnabled (editingEnabled s), width (Exactly 100), height Fill]
          , button ResetButton [text "Reset", onActivated (post ResetClicks), isEnabled (editingEnabled s), width (Exactly 100), height Fill]
          , caption ("Clicks: " <> T.pack (show (clickCount s))) [width Fill, height Fill, align MiddleLeft]
          ]
      ]
    )

rowToggle :: AppState -> UIElement.Element Element Msg
rowToggle s =
  hBox
    ( rowLayout ++
      [ spacing 8
      , children
          [ toggleButton ToggleCtl
              [ text "Toggle me", isSelected (toggleOn s), onSelectedChanged (postWith SetToggle)
              , isEnabled (editingEnabled s), width (Exactly 160), height Fill
              ]
          , caption (if toggleOn s then "On" else "Off") [width Fill, height Fill, align MiddleLeft]
          ]
      ]
    )

radioOptions :: [Text]
radioOptions = ["Small", "Medium", "Large"]

rowRadio :: AppState -> UIElement.Element Element Msg
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
        , isEnabled (editingEnabled s), width (Exactly 100), height Fill, align MiddleLeft
        ]

rowTextInput :: AppState -> UIElement.Element Element Msg
rowTextInput s =
  hBox
    ( rowLayout ++
      [ spacing 8, alignment Center
      , children
          [ caption "Text input" [width (Exactly 120), height Fill, align MiddleLeft]
          , textInput TextInputCtl [value (inputText s), onInput (postWith SetInputText), isEnabled (editingEnabled s), height Fill]
          ]
      ]
    )

rowPasswordInput :: AppState -> UIElement.Element Element Msg
rowPasswordInput s =
  hBox
    ( rowLayout ++
      [ spacing 8, alignment Center
      , children
          [ caption "Password input" [width (Exactly 120), height Fill, align MiddleLeft]
          , textInput PasswordInputCtl
              [ value (passwordText s), displayFilter (T.map (const '\8226')), onInput (postWith SetPasswordText)
              , isEnabled (editingEnabled s), height Fill
              ]
          ]
      ]
    )

rowAnimate :: AppState -> UIElement.Element Element Msg
rowAnimate s =
  checkbox AnimateCheckbox
    (rowLayout ++ [text "Animate progress bar", isSelected (animating s), onSelectedChanged (postWith SetAnimating), isEnabled (editingEnabled s)])

rowProgress :: AppState -> UIElement.Element Element Msg
rowProgress s =
  if animating s
    then progressBar ProgressCtl (rowLayout ++ [progress Indeterminate])
    else progressBar ProgressCtl (rowLayout ++ [progress (Progress (fromIntegral (clickCount s) / 50))])

-- Footer

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
  runElement $ hBox
    [ spacing 24, margin 4, alignment Center
    , children
        [ caption winText    [width (Exactly 160), height Fill, align MiddleLeft]
        , caption mouseText  [width (Exactly 160), height Fill, align MiddleLeft]
        , caption buttonText [width (Exactly 160), height Fill, align MiddleLeft]
        , caption hoverText  [width (Exactly 100), height Fill, align MiddleLeft]
        , caption keyText    [width Fill,          height Fill, align MiddleLeft]
        ]
    ]

-- Top-level view

demoView :: AppState -> DemoUI ()
demoView s = do
  input <- getInput
  win   <- getBounds
  let winSize = (round (rectWidth win) :: Int, round (rectHeight win) :: Int)
  when (darkMode s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  borderLayout [centre (mainList s), bottom 36 (footer s winSize)]
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
        [ caption "Blink controls demo" [width Fill, height (Exactly 24), align TopLeft]
        , rowDarkMode s
        , rowEditing s
        , rowButtons s
        , rowToggle s
        , rowRadio s
        , rowTextInput s
        , rowPasswordInput s
        , rowAnimate s
        , rowProgress s
        ]
    ]
