{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink.App
import Blink.Controls
import Blink.Controls.Button (isSelected, onActivated, onSelectedChanged)
import Blink.Controls.Label (text)
import Blink.Controls.ProgressBar (ProgressValue (..), progress)
import Blink.Controls.TextInput (displayFilter, onInput, value)
import Blink.Geometry
import Blink.Input
import Blink.Layout
import Blink.Rendering
import Blink.UI
import Blink.UI.Element (elLayout, elementWithLayout, runElement)
import Blink.Update
import Theme (Element (..), lightTheme, darkTheme)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T

-- | Emits @msg@, ignoring whatever data the triggering event carried.
post :: msg -> a -> [Out e msg]
post msg = const [OutMsg msg]

-- | Emits @f a@ -- uses the triggering event's own data to build the message.
postWith :: (a -> msg) -> a -> [Out e msg]
postWith f a = [OutMsg (f a)]

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
caption :: Text -> DemoUI ()
caption t = runElement (label Label [text t])

rowHeight :: Length
rowHeight = Exactly 40

-- Control rows, top to bottom

rowDarkMode :: AppState -> DemoUI ()
rowDarkMode s =
  runElement $ checkbox DarkModeCheckbox [text "Dark mode", isSelected (darkMode s), onSelectedChanged (postWith SetDarkMode)]

rowEditing :: AppState -> DemoUI ()
rowEditing s =
  runElement $
    checkbox EditingCheckbox [text "Enable editing", isSelected (editingEnabled s), onSelectedChanged (postWith SetEditingEnabled)]

rowButtons :: AppState -> DemoUI ()
rowButtons s =
  runElement $ hBox
    [ spacing 8
    , children
        [ (button ClickButton [text "Click me", onActivated (post AddClick)]) { elLayout = Layout (Exactly 100) Fill TopLeft }
        , (button ResetButton [text "Reset", onActivated (post ResetClicks)]) { elLayout = Layout (Exactly 100) Fill TopLeft }
        , elementWithLayout (Layout Fill Fill MiddleLeft) (caption ("Clicks: " <> T.pack (show (clickCount s))))
        ]
    ]

rowToggle :: AppState -> DemoUI ()
rowToggle s =
  runElement $ hBox
    [ spacing 8
    , children
        [ (toggleButton ToggleCtl [text "Toggle me", isSelected (toggleOn s), onSelectedChanged (postWith SetToggle)])
             { elLayout = Layout (Exactly 160) Fill TopLeft }
        , elementWithLayout (Layout Fill Fill MiddleLeft) (caption (if toggleOn s then "On" else "Off"))
        ]
    ]

radioOptions :: [Text]
radioOptions = ["Small", "Medium", "Large"]

rowRadio :: AppState -> DemoUI ()
rowRadio s =
  runElement $ hBox
    [ spacing 16
    , children
        [ (radioOption i opt) { elLayout = Layout (Exactly 100) Fill MiddleLeft }
        | (i, opt) <- zip [0 ..] radioOptions
        ]
    ]
  where
    radioOption i opt =
      radioButton (RadioCtl i)
        [text opt, isSelected (radioChoice s == Just opt), onSelectedChanged (\_ -> [OutMsg (PickRadio opt)])]

rowTextInput :: AppState -> DemoUI ()
rowTextInput s =
  runElement $ hBox
    [ spacing 8, alignment Center
    , children
        [ elementWithLayout (Layout (Exactly 120) Fill MiddleLeft) (caption "Text input")
        , (textInput TextInputCtl [value (inputText s), onInput (postWith SetInputText)])
             { elLayout = Layout Fill Fill TopLeft }
        ]
    ]

rowPasswordInput :: AppState -> DemoUI ()
rowPasswordInput s =
  runElement $ hBox
    [ spacing 8, alignment Center
    , children
        [ elementWithLayout (Layout (Exactly 120) Fill MiddleLeft) (caption "Password input")
        , (textInput PasswordInputCtl
               [value (passwordText s), displayFilter (T.map (const '\8226')), onInput (postWith SetPasswordText)])
             { elLayout = Layout Fill Fill TopLeft }
        ]
    ]

rowAnimate :: AppState -> DemoUI ()
rowAnimate s =
  runElement $
    checkbox AnimateCheckbox [text "Animate progress bar", isSelected (animating s), onSelectedChanged (postWith SetAnimating)]

rowProgress :: AppState -> DemoUI ()
rowProgress s =
  runElement $
    if animating s
      then progressBar ProgressCtl [progress Indeterminate]
      else progressBar ProgressCtl [progress (Progress (fromIntegral (clickCount s) / 50))]

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
        [ elementWithLayout (Layout (Exactly 160) Fill MiddleLeft) (caption winText)
        , elementWithLayout (Layout (Exactly 160) Fill MiddleLeft) (caption mouseText)
        , elementWithLayout (Layout (Exactly 160) Fill MiddleLeft) (caption buttonText)
        , elementWithLayout (Layout (Exactly 100) Fill MiddleLeft) (caption hoverText)
        , elementWithLayout (Layout Fill          Fill MiddleLeft) (caption keyText)
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
        [ elementWithLayout (Layout Fill (Exactly 24) TopLeft) (caption "Blink controls demo")
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (rowDarkMode s)
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (rowEditing s)
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (disableWhen (not (editingEnabled s)) (rowButtons s))
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (disableWhen (not (editingEnabled s)) (rowToggle s))
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (disableWhen (not (editingEnabled s)) (rowRadio s))
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (disableWhen (not (editingEnabled s)) (rowTextInput s))
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (disableWhen (not (editingEnabled s)) (rowPasswordInput s))
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (disableWhen (not (editingEnabled s)) (rowAnimate s))
        , elementWithLayout (Layout Fill rowHeight    TopLeft) (rowProgress s)
        ]
    ]
