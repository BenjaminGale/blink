{-# LANGUAGE OverloadedStrings #-}
module Blink.LabelledControlSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.ControlBehaviour (controlBehaviourSpec, defaultControlBehaviourConfig)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Label (DisplayMode (..), LabelledControlAttrs, content, displayMode, glyph, labelledControl, text)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = Widget deriving (Eq, Ord, Show)

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

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin the test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

-- | The margin-and-padding-inset content area 'labelledControl' draws its
-- label into.
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 70

type Attr' = LabelledControlAttrs TestElement String

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (labelledControl Widget attrs) seedCtx

spec :: Spec
spec = describe "Blink.Label.labelledControl" $ do
  controlBehaviourSpec defaultControlBehaviourConfig
    testBounds seedCtx Widget (Point 5 5) hitRect (Point 200 200) (labelledControl Widget)

  it "draws just its text, filling the content area, in TextOnly mode (the default)" $ do
    ctx <- start [text "Hello"]
    getDrawCommands ctx `shouldContain` [DrawText contentRect "Hello" testColour AlignCenter]

  it "draws just its glyph, filling the content area, in GlyphOnly mode" $ do
    ctx <- start [glyph "*", displayMode GlyphOnly]
    getDrawCommands ctx `shouldContain` [DrawText contentRect "*" testColour AlignCenter]

  it "draws its glyph in a fixed-width column followed by its text in TextAndGlyph mode" $ do
    ctx <- start [text "Hello", glyph "*", displayMode TextAndGlyph]
    getDrawCommands ctx `shouldContain`
      [ DrawText (Rectangle 15 15 20 70) "*" testColour AlignCenter
      , DrawText (Rectangle 35 15 50 70) "Hello" testColour AlignCenter
      ]

  it "places the rendered label wherever content's function runs it, not just over the whole content area" $ do
    let shiftDown lbl = withBounds (contentRect { rectY = rectY contentRect + 5 }) lbl
    ctx <- start [text "Hello", content shiftDown]
    getDrawCommands ctx `shouldContain` [DrawText (contentRect { rectY = rectY contentRect + 5 }) "Hello" testColour AlignCenter]
