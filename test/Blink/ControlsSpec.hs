{-# LANGUAGE OverloadedStrings #-}
module Blink.ControlsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Text (Text)
import Blink.Controls (button, textInput)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, uniform)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = TestControl | OtherControl
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
  { normal = testStyle
  , hovered = testStyle
  , pressed = testStyle
  , focused = testStyle
  , disabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme
  { elementStyles = Map.fromList [(TestControl, testStyleSet), (OtherControl, testStyleSet)]
  , defaultStyle = testStyleSet
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
  [ ("at the center", Point 50 50)
  , ("at the top-left corner", Point 10 10)
  , ("at the bottom-right corner", Point 90 90)
  ]

outsidePoints :: [(String, Point)]
outsidePoints =
  [ ("in the margin area", Point 5 5)
  , ("outside the control", Point 200 200)
  ]

testBorderColour :: Colour
testBorderColour = RGBA 1 0 0 1

testStyleWithBorder :: Style
testStyleWithBorder = testStyle { borderColour = Just testBorderColour, borderWidth = 1 }

testStyleSetWithBorder :: StyleSet
testStyleSetWithBorder = StyleSet
  { normal = testStyleWithBorder
  , hovered = testStyleWithBorder
  , pressed = testStyleWithBorder
  , focused = testStyleWithBorder
  , disabled = testStyleWithBorder
  }

testThemeWithBorder :: Theme TestElement
testThemeWithBorder = Theme
  { elementStyles = Map.fromList [(TestControl, testStyleSetWithBorder), (OtherControl, testStyleSetWithBorder)]
  , defaultStyle = testStyleSetWithBorder
  }

isStrokeRect :: DrawCommand -> Bool
isStrokeRect (StrokeRect {}) = True
isStrokeRect _               = False

type WidgetRunner c = UIContext TestElement c -> UIContext TestElement c

controlBehaviourSpec :: WidgetRunner c -> Spec
controlBehaviourSpec run = do
  let runWithBorder ctx = run (ctx { ctxTheme = testThemeWithBorder })
  describe "focus" $ do
    it "receives focus when nothing else is focused" $
      getFocused (run (mkCtx noInput))
        `shouldBe` Just TestControl

    it "does not take focus from another element" $
      getFocused (run (withFocus (Just OtherControl) (mkCtx noInput)))
        `shouldBe` Just OtherControl

    it "receives focus when clicked" $
      getFocused (run (withFocus (Just OtherControl) (mkCtx (mouseAt (Point 50 50) ButtonReleased []))))
        `shouldBe` Just TestControl

  describe "tab navigation" $ do
    it "passes focus to the next control when Tab is pressed" $
      getFocused (run (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyTab []] })))
        `shouldBe` Nothing

    it "passes focus to the previous control when Shift+Tab is pressed" $
      getFocused (run (withFocus (Just TestControl) (mkCtx noInput { keyEvents = [KeyEvent KeyTab [Shift]] }) { ctxPreviousControl = Just OtherControl }))
        `shouldBe` Just OtherControl

  describe "hover detection" $ do
    forM_ insidePoints $ \(desc, pt) ->
      it ("is hovered when the mouse is " <> desc) $
        ctxHoveredElement (run (mkCtx (mouseAt pt ButtonUp [])))
          `shouldBe` Just TestControl

    forM_ outsidePoints $ \(desc, pt) ->
      it ("is not hovered when the mouse is " <> desc) $
        ctxHoveredElement (run (mkCtx (mouseAt pt ButtonUp [])))
          `shouldBe` Nothing

  describe "background and border" $ do
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

drawnTexts :: UIContext e c -> [Text]
drawnTexts ctx = [t | DrawText _ t _ _ <- getDrawCommands ctx]

spec :: Spec
spec = do
  describe "textInput" $ do
    controlBehaviourSpec runTextInputControl

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

  describe "button" $ do
    controlBehaviourSpec runButton

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
