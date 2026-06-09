{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Text (Text)
import Blink.Controls (button, checkbox, textInput)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, uniform)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = TestControl | LabelControl | OtherControl
  deriving (Eq, Ord, Show)

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { background   = testColour
  , textColour   = testColour
  , textAlign    = AlignCenter
  , margin       = uniform 10
  , padding      = uniform 5
  , borderColour = Nothing
  , borderWidth  = 0
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet
  { normal   = testStyle
  , hovered  = testStyle
  , pressed  = testStyle
  , focused  = testStyle
  , disabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme
  { elementStyles = Map.fromList [(TestControl, testStyleSet), (OtherControl, testStyleSet)]
  , defaultStyle  = testStyleSet
  }

controlRect :: Rectangle
controlRect = Rectangle 0 0 100 100

bgRect :: Rectangle
bgRect = insetRect (uniform 10) controlRect

contentRect :: Rectangle
contentRect = insetRect (uniform 5) bgRect

mkCtx :: InputState -> UIContext TestElement c
mkCtx input = emptyUIContext controlRect input testTheme

withFocus :: Maybe TestElement -> UIContext TestElement c -> UIContext TestElement c
withFocus e ctx = ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = e } }

getFocused :: UIContext TestElement c -> Maybe TestElement
getFocused = focusedElement . ctxFocusState

noInput :: InputState
noInput = InputState
  { mousePosition = Point 200 200
  , leftButton    = ButtonUp
  , keyEvents     = []
  , typedText     = []
  }

mouseAt :: Point -> ButtonState -> [KeyEvent] -> InputState
mouseAt pos btn keys = InputState
  { mousePosition = pos
  , leftButton    = btn
  , keyEvents     = keys
  , typedText     = []
  }

insidePoints :: [(String, Point)]
insidePoints =
  [ ("at the center",           Point 50 50)
  , ("at the top-left corner",  Point 10 10)
  , ("at the bottom-right corner", Point 90 90)
  ]

outsidePoints :: [(String, Point)]
outsidePoints =
  [ ("in the margin area",  Point 5 5)
  , ("outside the control", Point 200 200)
  ]

testBorderColour :: Colour
testBorderColour = RGBA 1 0 0 1

testStyleWithBorder :: Style
testStyleWithBorder = testStyle { borderColour = Just testBorderColour, borderWidth = 1 }

testStyleSetWithBorder :: StyleSet
testStyleSetWithBorder = StyleSet
  { normal   = testStyleWithBorder
  , hovered  = testStyleWithBorder
  , pressed  = testStyleWithBorder
  , focused  = testStyleWithBorder
  , disabled = testStyleWithBorder
  }

testThemeWithBorder :: Theme TestElement
testThemeWithBorder = Theme
  { elementStyles = Map.fromList [(TestControl, testStyleSetWithBorder), (OtherControl, testStyleSetWithBorder)]
  , defaultStyle  = testStyleSetWithBorder
  }

-- Zero-margin theme for checkbox: the box occupies a 20×20 slot at Rectangle 0 40 20 20
-- (MiddleLeft, Exactly 20×20 in a 100×100 rect). Without this, margin=10 collapses the
-- bgRect to zero and hover detection never fires.
zeroMarginStyle :: Style
zeroMarginStyle = testStyle { margin = uniform 0, padding = uniform 0 }

zeroMarginStyleSet :: StyleSet
zeroMarginStyleSet = StyleSet
  { normal   = zeroMarginStyle
  , hovered  = zeroMarginStyle
  , pressed  = zeroMarginStyle
  , focused  = zeroMarginStyle
  , disabled = zeroMarginStyle
  }

checkboxTheme :: Theme TestElement
checkboxTheme = testTheme
  { elementStyles = Map.fromList [(TestControl, zeroMarginStyleSet), (OtherControl, testStyleSet)] }

focusBorderStyleSet :: StyleSet
focusBorderStyleSet = testStyleSet { focused = testStyleWithBorder }

focusBorderTheme :: Theme TestElement
focusBorderTheme = testTheme
  { elementStyles = Map.fromList [(TestControl, focusBorderStyleSet)] }

isStrokeRect :: DrawCommand -> Bool
isStrokeRect (StrokeRect {}) = True
isStrokeRect _               = False

type WidgetRunner c = UIContext TestElement c -> UIContext TestElement c

-- | Shared focus, tab, and hover tests for any widget whose primary interactive
--   element is TestControl. Pass a point inside the control's hittable area.
controlBehaviourSpec :: WidgetRunner c -> Point -> Spec
controlBehaviourSpec run hitPoint = do
  describe "focus" $ do
    it "receives focus when nothing else is focused" $
      getFocused (run (mkCtx noInput))
        `shouldBe` Just TestControl

    it "does not take focus from another element" $
      getFocused (run (withFocus (Just OtherControl) (mkCtx noInput)))
        `shouldBe` Just OtherControl

    it "receives focus when clicked" $
      getFocused (run (withFocus (Just OtherControl) (mkCtx (mouseAt hitPoint ButtonReleased []))))
        `shouldBe` Just TestControl

  describe "tab navigation" $ do
    it "passes focus to the next control when Tab is pressed" $
      getFocused (run (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyTab []] })))
        `shouldBe` Nothing

    it "passes focus to the previous control when Shift+Tab is pressed" $
      getFocused (run (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyTab [Shift]] }) { ctxPreviousControl = Just OtherControl }))
        `shouldBe` Just OtherControl

  describe "hover detection" $ do
    it "is hovered when the mouse is inside" $
      ctxHoveredElement (run (mkCtx (mouseAt hitPoint ButtonUp [])))
        `shouldBe` Just TestControl

    it "is not hovered when the mouse is outside" $
      ctxHoveredElement (run (mkCtx (mouseAt (Point 200 200) ButtonUp [])))
        `shouldBe` Nothing

-- | Background and border rendering tests. Only applicable to single controls
--   that fill controlRect directly (not composite widgets).
backgroundAndBorderSpec :: WidgetRunner c -> Spec
backgroundAndBorderSpec run = do
  let runWithBorder ctx = run (ctx { ctxTheme = testThemeWithBorder })
  it "does not draw a background in the margin area" $
    ctxDrawCommands (run (mkCtx noInput))
      `shouldNotContain` [FillRect controlRect testColour]

  it "fills its background area" $
    ctxDrawCommands (run (mkCtx noInput))
      `shouldContain` [FillRect bgRect testColour]

  it "clips content to its padding area" $
    ctxDrawCommands (run (mkCtx noInput))
      `shouldContain` [PushClip contentRect]

  it "does not draw a border when borderColour is Nothing" $
    filter isStrokeRect (ctxDrawCommands (run (mkCtx noInput)))
      `shouldBe` []

  it "draws a border when borderColour is set" $
    ctxDrawCommands (runWithBorder (mkCtx noInput))
      `shouldContain` [StrokeRect bgRect testBorderColour 1]

runButton :: WidgetRunner ()
runButton ctx = snd $ runUI (button TestControl "label") ctx

runTextInputControl :: WidgetRunner Text
runTextInputControl ctx = snd $ runUI (textInput TestControl "" id) ctx

runTextInput :: Text -> UIContext TestElement Text -> UIContext TestElement Text
runTextInput value ctx = snd $ runUI (textInput TestControl value id) ctx

-- Forces checkboxTheme so the 20×20 box slot is hittable regardless of mkCtx's theme.
runCheckboxControl :: WidgetRunner Bool
runCheckboxControl ctx = snd $ runUI (checkbox TestControl LabelControl "test label" False id) (ctx { ctxTheme = checkboxTheme })

runCheckbox :: Bool -> UIContext TestElement Bool -> UIContext TestElement Bool
runCheckbox checked ctx = snd $ runUI (checkbox TestControl LabelControl "test label" checked id) ctx

mkCheckboxCtx :: InputState -> UIContext TestElement Bool
mkCheckboxCtx input = emptyUIContext controlRect input checkboxTheme

-- Center of the box bgRect (Rectangle 0 40 20 20) with zero-margin theme
boxPoint :: Point
boxPoint = Point 10 50

drawnTexts :: UIContext e c -> [Text]
drawnTexts ctx = [t | DrawText _ t _ _ <- getDrawCommands ctx]

spec :: Spec
spec = do
  describe "textInput" $ do
    controlBehaviourSpec runTextInputControl (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runTextInputControl

    describe "rendering" $ do
      it "displays the value without a cursor when unfocused" $
        drawnTexts (runTextInput "hello" (withFocus (Just OtherControl) (mkCtx noInput)))
          `shouldContain` ["hello"]

      it "displays the value with a cursor when focused" $
        drawnTexts (runTextInput "hello" (withFocus (Just TestControl) (mkCtx noInput)))
          `shouldContain` ["hello|"]

    describe "text editing" $ do
      it "appends typed characters to the value" $
        getCommands (runTextInput "hello" (withFocus (Just TestControl) (mkCtx noInput { typedText = ["!"] })))
          `shouldBe` ["hello!"]

      it "removes the last character on backspace" $
        getCommands (runTextInput "hello" (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyBackspace []] })))
          `shouldBe` ["hell"]

      it "does not dispatch when backspace is pressed on an empty value" $
        getCommands (runTextInput "" (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyBackspace []] })))
          `shouldBe` []

      it "does not dispatch when there is no input" $
        getCommands (runTextInput "hello" (withFocus (Just TestControl) (mkCtx noInput)))
          `shouldBe` []

      it "does not process input when unfocused" $
        getCommands (runTextInput "hello" (withFocus (Just OtherControl) (mkCtx noInput { typedText = ["!"], keyEvents = [KeyEvent KeyBackspace []] })))
          `shouldBe` []

  describe "checkbox" $ do
    controlBehaviourSpec runCheckboxControl boxPoint

    describe "toggle behaviour" $ do
      it "dispatches True when the box is clicked while unchecked" $
        getCommands (runCheckbox False (mkCheckboxCtx (mouseAt boxPoint ButtonReleased [])))
          `shouldBe` [True]

      it "dispatches False when the box is clicked while checked" $
        getCommands (runCheckbox True (mkCheckboxCtx (mouseAt boxPoint ButtonReleased [])))
          `shouldBe` [False]

      it "dispatches toggle when Enter is pressed while focused" $
        getCommands (runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput { keyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` [True]

      it "does not dispatch when clicked outside the box" $
        getCommands (runCheckbox False (mkCheckboxCtx (mouseAt (Point 50 50) ButtonReleased [])))
          `shouldBe` []

      it "does not dispatch when Enter is pressed while unfocused" $
        getCommands (runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput { keyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` []

    describe "rendering" $ do
      it "draws the checkmark when checked" $
        drawnTexts (runCheckbox True (mkCheckboxCtx noInput))
          `shouldContain` ["✓"]

      it "does not draw the checkmark when unchecked" $
        drawnTexts (runCheckbox False (mkCheckboxCtx noInput))
          `shouldNotContain` ["✓"]

      it "draws the label text" $
        drawnTexts (runCheckbox False (mkCheckboxCtx noInput))
          `shouldContain` ["test label"]

    describe "focus ring" $ do
      it "draws a focus ring around the full control when focused" $
        ctxDrawCommands (runCheckbox False (withFocus (Just TestControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme }))
          `shouldContain` [StrokeRect controlRect testBorderColour 1]

      it "does not draw a focus ring when unfocused" $
        ctxDrawCommands (runCheckbox False (withFocus (Just OtherControl) (mkCheckboxCtx noInput) { ctxTheme = focusBorderTheme }))
          `shouldNotContain` [StrokeRect controlRect testBorderColour 1]

  describe "button" $ do
    controlBehaviourSpec runButton (Point 50 50)
    describe "background and border" $ backgroundAndBorderSpec runButton

    describe "rendering" $ do
      it "draws the label" $
        drawnTexts (runButton (mkCtx noInput))
          `shouldContain` ["label"]

    describe "click behaviour" $ do
      forM_ insidePoints $ \(desc, pt) ->
        it ("is clicked when the mouse is released " <> desc) $
          fst (runUI (button TestControl "label") (mkCtx (mouseAt pt ButtonReleased [])))
            `shouldBe` True

      forM_ outsidePoints $ \(desc, pt) ->
        it ("is not clicked when the mouse is released " <> desc) $
          fst (runUI (button TestControl "label") (mkCtx (mouseAt pt ButtonReleased [])))
            `shouldBe` False

      it "is clicked when Enter is pressed and the button has focus" $
        fst (runUI (button TestControl "label") (mkCtx noInput { keyEvents = [KeyEvent KeyReturn []] }))
          `shouldBe` True

      it "is not clicked when Enter is pressed and the button does not have focus" $
        fst (runUI (button TestControl "label") (withFocus (Just OtherControl) (mkCtx noInput { keyEvents = [KeyEvent KeyReturn []] })))
          `shouldBe` False

      it "is not clicked when Tab and Enter are pressed simultaneously" $
        fst (runUI (button TestControl "label")
          (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyTab [], KeyEvent KeyReturn []] })))
          `shouldBe` False
