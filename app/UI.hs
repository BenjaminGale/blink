{-# LANGUAGE OverloadedStrings #-}
module UI (Element, AppState (..), demoApp) where

import Blink.App
import Blink.Controls.Button
import Blink.Controls.Checkbox
import Blink.Controls.RadioButton
import Blink.Controls.TextInput
import Blink.Controls.ProgressBar
import Blink.Controls.Label (label)
import qualified Blink.Controls.Label as Lbl
import Blink.Geometry
import Blink.Input
import Blink.Layout
import Blink.Rendering
import Blink.UI
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
caption t = label Label [Lbl.text t]

rowHeight :: Length
rowHeight = Exactly 40

-- Control rows, top to bottom

rowDarkMode :: AppState -> DemoUI ()
rowDarkMode s =
  checkbox DarkModeCheckbox [text "Dark mode", isSelected (darkMode s), onSelectedChanged (postWith SetDarkMode)]

rowEditing :: AppState -> DemoUI ()
rowEditing s =
  checkbox EditingCheckbox [text "Enable editing", isSelected (editingEnabled s), onSelectedChanged (postWith SetEditingEnabled)]

rowButtons :: AppState -> DemoUI ()
rowButtons s =
  hBox (defaultBoxConfig { boxSpacing = 8 })
    [ (Layout (Exactly 100) Fill TopLeft, button ClickButton [text "Click me", onClicked (post AddClick)])
    , (Layout (Exactly 100) Fill TopLeft, button ResetButton [text "Reset", onClicked (post ResetClicks)])
    , (Layout Fill Fill MiddleLeft, caption ("Clicks: " <> T.pack (show (clickCount s))))
    ]

rowToggle :: AppState -> DemoUI ()
rowToggle s =
  hBox (defaultBoxConfig { boxSpacing = 8 })
    [ (Layout (Exactly 160) Fill TopLeft,
         toggleButton ToggleCtl [text "Toggle me", isSelected (toggleOn s), onSelectedChanged (postWith SetToggle)])
    , (Layout Fill Fill MiddleLeft, caption (if toggleOn s then "On" else "Off"))
    ]

radioOptions :: [Text]
radioOptions = ["Small", "Medium", "Large"]

rowRadio :: AppState -> DemoUI ()
rowRadio s =
  hBox (defaultBoxConfig { boxSpacing = 16 })
    [ (Layout (Exactly 100) Fill MiddleLeft, radioOption i opt)
    | (i, opt) <- zip [0 ..] radioOptions
    ]
  where
    radioOption i opt =
      radioButton (RadioCtl i)
        [text opt, isSelected (radioChoice s == Just opt), onSelectedChanged (\_ -> [OutMsg (PickRadio opt)])]

rowTextInput :: AppState -> DemoUI ()
rowTextInput s =
  hBox (defaultBoxConfig { boxSpacing = 8, boxAlignment = Center })
    [ (Layout (Exactly 120) Fill MiddleLeft, caption "Text input")
    , (Layout Fill Fill TopLeft,
         textInput TextInputCtl [text (inputText s), onInput (postWith SetInputText)])
    ]

rowPasswordInput :: AppState -> DemoUI ()
rowPasswordInput s =
  hBox (defaultBoxConfig { boxSpacing = 8, boxAlignment = Center })
    [ (Layout (Exactly 120) Fill MiddleLeft, caption "Password input")
    , (Layout Fill Fill TopLeft,
         textInput PasswordInputCtl
           [text (passwordText s), displayFilter (T.map (const '\8226')), onInput (postWith SetPasswordText)])
    ]

rowAnimate :: AppState -> DemoUI ()
rowAnimate s =
  checkbox AnimateCheckbox [text "Animate progress bar", isSelected (animating s), onSelectedChanged (postWith SetAnimating)]

rowProgress :: AppState -> DemoUI ()
rowProgress s =
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
  hBox (defaultBoxConfig { boxSpacing = 24, boxMargin = 4, boxAlignment = Center })
    [ (Layout (Exactly 160) Fill MiddleLeft, caption winText)
    , (Layout (Exactly 160) Fill MiddleLeft, caption mouseText)
    , (Layout (Exactly 160) Fill MiddleLeft, caption buttonText)
    , (Layout (Exactly 100) Fill MiddleLeft, caption hoverText)
    , (Layout Fill          Fill MiddleLeft, caption keyText)
    ]

-- Top-level view

demoView :: AppState -> DemoUI ()
demoView s = do
  input <- getInput
  win   <- getBounds
  let winSize = (round (rectWidth win) :: Int, round (rectHeight win) :: Int)
  when (darkMode s) $ fillRect (RGBA 0.082 0.102 0.129 1)
  borderLayout emptyBorderContent
    { centrePanel = Just (mainList s)
    , bottomPanel = Just (36, footer s winSize)
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

mainList :: AppState -> DemoUI ()
mainList s =
  vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 12 })
    [ (Layout Fill (Exactly 24) TopLeft, caption "Blink controls demo")
    , (Layout Fill rowHeight    TopLeft, rowDarkMode s)
    , (Layout Fill rowHeight    TopLeft, rowEditing s)
    , (Layout Fill rowHeight    TopLeft, disableWhen (not (editingEnabled s)) (rowButtons s))
    , (Layout Fill rowHeight    TopLeft, disableWhen (not (editingEnabled s)) (rowToggle s))
    , (Layout Fill rowHeight    TopLeft, disableWhen (not (editingEnabled s)) (rowRadio s))
    , (Layout Fill rowHeight    TopLeft, disableWhen (not (editingEnabled s)) (rowTextInput s))
    , (Layout Fill rowHeight    TopLeft, disableWhen (not (editingEnabled s)) (rowPasswordInput s))
    , (Layout Fill rowHeight    TopLeft, disableWhen (not (editingEnabled s)) (rowAnimate s))
    , (Layout Fill rowHeight    TopLeft, rowProgress s)
    ]
