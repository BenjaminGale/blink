{-# LANGUAGE OverloadedStrings #-}
module Blink.LabelSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes (Attr, FocusOnClick (..), focusOnClick, text)
import Blink.Element (ElementEvent)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Label (LabelConfig, label, target)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = Caption | Target deriving (Eq, Ord, Show)

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

onCaption :: Point
onCaption = Point 50 50

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (label Caption attrs) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)

type Attr' = Attr TestElement ElementEvent String (LabelConfig TestElement)

spec :: Spec
spec = describe "Blink.Label" $ do
  it "draws its text in the resolved style" $ do
    ctx <- start [text "Hello"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "Hello" testColour AlignCenter]

  it "never claims focus, even with nothing else focused" $ do
    ctx <- start []
    contextFocus ctx `shouldBe` Nothing

  it "does not take focus when clicked by default" $ do
    ctx0 <- start []
    ctx1 <- snd <$> runUI (label Caption []) (nextFrameContext testBounds (down onCaption) testTheme (contextAnimation ctx0) ctx0)
    ctx2 <- snd <$> runUI (label Caption []) (nextFrameContext testBounds (releasedAt onCaption) testTheme (contextAnimation ctx1) ctx1)
    contextFocus ctx2 `shouldBe` Nothing

  it "ignores an explicit focusOnClick -- clicking it never moves focus anywhere" $ do
    let attrs = [focusOnClick (FocusTarget Target)]
    ctx0 <- start attrs
    ctx1 <- snd <$> runUI (label Caption attrs) (nextFrameContext testBounds (down onCaption) testTheme (contextAnimation ctx0) ctx0)
    ctx2 <- snd <$> runUI (label Caption attrs) (nextFrameContext testBounds (releasedAt onCaption) testTheme (contextAnimation ctx1) ctx1)
    ctx3 <- snd <$> runUI (label Caption attrs) (nextFrameContext testBounds noInput testTheme (contextAnimation ctx2) ctx2)
    contextFocus ctx3 `shouldBe` Nothing

  it "redirects a click's focus onto the element named by target" $ do
    let attrs = [target Target]
    ctx0 <- start attrs
    ctx1 <- snd <$> runUI (label Caption attrs) (nextFrameContext testBounds (down onCaption) testTheme (contextAnimation ctx0) ctx0)
    ctx2 <- snd <$> runUI (label Caption attrs) (nextFrameContext testBounds (releasedAt onCaption) testTheme (contextAnimation ctx1) ctx1)
    contextFocus ctx2 `shouldBe` Nothing -- not yet -- deferred
    ctx3 <- snd <$> runUI (label Caption attrs) (nextFrameContext testBounds noInput testTheme (contextAnimation ctx2) ctx2)
    contextFocus ctx3 `shouldBe` Just Target
