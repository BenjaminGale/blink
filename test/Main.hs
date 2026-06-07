{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.DrawCall (Colour (..), TextAlign (..), DrawCall (..))
import Blink.Geometry (Point (..), Rectangle (..), insetRect, uniform)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = TestControl | OtherControl
  deriving (Eq, Ord, Show)

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { background = testColour
  , textColour = testColour
  , textAlign = AlignCenter
  , margin = uniform 10
  , padding = uniform 5
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

mkCtx :: InputState -> UIContext TestElement
mkCtx input = UIContext
  { ctxBounds = controlRect
  , ctxInput = input
  , ctxTheme = testTheme
  }

noInput :: InputState
noInput = InputState
  { mousePosition = Point 200 200
  , leftButton = ButtonUp
  , keyEvents = []
  }

mouseAt :: Point -> ButtonState -> [KeyEvent] -> InputState
mouseAt pos btn keys = InputState
  { mousePosition = pos
  , leftButton = btn
  , keyEvents = keys
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

type WidgetRunner = UIContext TestElement -> UIState TestElement () -> UIState TestElement ()

runButton :: WidgetRunner
runButton ctx st = snd $ runUI (button TestControl "label") ctx st

controlBehaviourSpec :: WidgetRunner -> Spec
controlBehaviourSpec run = do
  describe "focus" $ do
    it "receives focus when nothing else is focused" $
      focusedElement (run (mkCtx noInput) emptyUIState)
        `shouldBe` Just TestControl

    it "does not take focus from another element" $
      focusedElement (run (mkCtx noInput) emptyUIState { focusedElement = Just OtherControl })
        `shouldBe` Just OtherControl

    it "receives focus when clicked" $
      focusedElement (run (mkCtx (mouseAt (Point 50 50) ButtonReleased [])) emptyUIState { focusedElement = Just OtherControl })
        `shouldBe` Just TestControl

  describe "tab navigation" $ do
    it "loses focus when Tab is pressed" $
      focusedElement (run (mkCtx noInput { keyEvents = [KeyEvent KeyTab []] }) emptyUIState { focusedElement = Just TestControl })
        `shouldBe` Nothing

    it "passes focus to the next control when Tab is pressed" $
      focusNext (run (mkCtx noInput { keyEvents = [KeyEvent KeyTab []] }) emptyUIState { focusedElement = Just TestControl })
        `shouldBe` True

    it "passes focus to the previous control when Shift+Tab is pressed" $
      focusedElement (run (mkCtx noInput { keyEvents = [KeyEvent KeyTab [Shift]] }) emptyUIState { focusedElement = Just TestControl, previousControl = Just OtherControl })
        `shouldBe` Just OtherControl

    it "ensures Tab only moves focus once per frame" $
      tabConsumed (run (mkCtx noInput { keyEvents = [KeyEvent KeyTab []] }) emptyUIState { focusedElement = Just TestControl })
        `shouldBe` True

  describe "hover detection" $ do
    forM_ insidePoints $ \(desc, pt) ->
      it ("is hovered when the mouse is " <> desc) $
        hoveredElement (run (mkCtx (mouseAt pt ButtonUp [])) emptyUIState)
          `shouldBe` Just TestControl

    forM_ outsidePoints $ \(desc, pt) ->
      it ("is not hovered when the mouse is " <> desc) $
        hoveredElement (run (mkCtx (mouseAt pt ButtonUp [])) emptyUIState)
          `shouldBe` Nothing

  describe "rendering" $ do
    it "does not draw a background in the margin area" $
      drawCalls (run (mkCtx noInput) emptyUIState)
        `shouldNotContain` [FillRect controlRect testColour]

    it "fills its background area" $
      drawCalls (run (mkCtx noInput) emptyUIState)
        `shouldContain` [FillRect bgRect testColour]

    it "clips content to its padding area" $
      drawCalls (run (mkCtx noInput) emptyUIState)
        `shouldContain` [PushClip contentRect]

main :: IO ()
main = hspec $ do
  describe "button" $ do
    controlBehaviourSpec runButton

    describe "click behaviour" $ do
      forM_ insidePoints $ \(desc, pt) ->
        it ("is clicked when the mouse is released " <> desc) $
          fst (runUI (button TestControl "label") (mkCtx (mouseAt pt ButtonReleased [])) emptyUIState)
            `shouldBe` True

      forM_ outsidePoints $ \(desc, pt) ->
        it ("is not clicked when the mouse is released " <> desc) $
          fst (runUI (button TestControl "label") (mkCtx (mouseAt pt ButtonReleased [])) emptyUIState)
            `shouldBe` False

      it "is clicked when Enter is pressed and the button has focus" $
        fst (runUI (button TestControl "label") (mkCtx noInput { keyEvents = [KeyEvent KeyReturn []] }) emptyUIState)
          `shouldBe` True

      it "is not clicked when Enter is pressed and the button does not have focus" $
        fst (runUI (button TestControl "label") (mkCtx noInput { keyEvents = [KeyEvent KeyReturn []] }) emptyUIState { focusedElement = Just OtherControl })
          `shouldBe` False
