{-# LANGUAGE OverloadedStrings #-}
module Blink.CheckboxSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes (Attr, text)
import Blink.Checkbox (CheckboxConfig, checkbox, isSelected, onSelectedChanged)
import Blink.Element (ElementEvent)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = Remember deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 10
  , stylePadding      = uniform 5
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet
  { styleSetNormal   = testStyle
  , styleSetHovered  = testStyle
  , styleSetPressed  = testStyle
  , styleSetFocused  = testStyle
  , styleSetDisabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = testStyleSet }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

down, releasedAt :: Point -> InputState
down p       = noInput { inputMousePosition = p, inputLeftButtonDown = True }
releasedAt p = noInput { inputMousePosition = p, inputLeftButtonDown = False }

onControl :: Point
onControl = Point 50 50

type Attr' = Attr TestElement ElementEvent String (CheckboxConfig TestElement String)

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (checkbox Remember attrs) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)

step :: [Attr'] -> InputState -> UIContext TestElement String -> IO (UIContext TestElement String)
step attrs input ctx = snd <$> runUI (checkbox Remember attrs) (nextFrameContext testBounds input testTheme (contextAnimation ctx) ctx)

spec :: Spec
spec = describe "Blink.Checkbox" $ do
  it "draws the unchecked glyph and its caption while not selected" $ do
    ctx <- start [text "Remember me"]
    getDrawCommands ctx `shouldContain`
      [ DrawText (Rectangle 15 15 20 70) "\9744" testColour AlignCenter
      , DrawText (Rectangle 35 15 50 70) "Remember me" testColour AlignCenter
      ]

  it "draws the checked glyph while selected" $ do
    ctx <- start [text "Remember me", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 20 70) "\9745" testColour AlignCenter]

  it "fires onSelectedChanged with the flipped value when clicked" $ do
    let attrs = [isSelected False, onSelectedChanged (\b -> [OutMsg (show b)])]
    ctx0 <- start attrs
    ctx1 <- step attrs (down onControl) ctx0
    ctx2 <- step attrs (releasedAt onControl) ctx1
    getMessages ctx2 `shouldBe` [show True]

  it "fires onSelectedChanged with False when clicked while selected" $ do
    let attrs = [isSelected True, onSelectedChanged (\b -> [OutMsg (show b)])]
    ctx0 <- start attrs
    ctx1 <- step attrs (down onControl) ctx0
    ctx2 <- step attrs (releasedAt onControl) ctx1
    getMessages ctx2 `shouldBe` [show False]

  it "fires onSelectedChanged when activated via Enter while focused" $ do
    let attrs = [isSelected False, onSelectedChanged (\b -> [OutMsg (show b)])]
    ctx0 <- start attrs
    ctx1 <- step attrs (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0
    getMessages ctx1 `shouldBe` [show True]
