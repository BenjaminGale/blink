{-# LANGUAGE OverloadedStrings #-}
module Blink.RadioButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import Blink.Attributes (Attr, text)
import Blink.Generators (genPointIn)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..), Key (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.RadioButton (RadioButtonConfig, ToggleEvent, isSelected, onSelectedChanged, radioButton)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = OptionA deriving (Eq, Ord, Show)

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
-- the 10px margin the test style here uses -- covers both the radio
-- button's glyph (x: 15-35) and caption (x: 35-85), so random points from
-- within it exercise both halves.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attr' = Attr TestElement ToggleEvent String (RadioButtonConfig TestElement)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (radioButton OptionA attrs) seedCtx

spec :: Spec
spec = describe "Blink.RadioButton" $ do
  it "draws the unselected glyph and its caption while not selected" $ do
    ctx <- start [text "Option A"]
    getDrawCommands ctx `shouldContain`
      [ DrawText (Rectangle 15 15 20 70) "\9675" testColour AlignCenter
      , DrawText (Rectangle 35 15 50 70) "Option A" testColour AlignCenter
      ]

  it "draws the selected glyph while selected" $ do
    ctx <- start [text "Option A", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 20 70) "\9673" testColour AlignCenter]

  it "fires onSelectedChanged with True when clicked while unselected" $ monadicIO $ do
    let attrs = [isSelected False, onSelectedChanged (\b -> [OutMsg (show b)])]
    p <- pick (genPointIn hitRect)
    result <- run (runInteractions testBounds seedCtx (radioButton OptionA attrs) [] [ClickAt p])
    assert (resultMessages result == [show True])

  it "does not fire onSelectedChanged when clicked while already selected" $ monadicIO $ do
    let attrs = [isSelected True, onSelectedChanged (\b -> [OutMsg (show b)])]
    p <- pick (genPointIn hitRect)
    result <- run (runInteractions testBounds seedCtx (radioButton OptionA attrs) [] [ClickAt p])
    assert (resultMessages result == [])

  it "fires onSelectedChanged when activated via Enter while unselected and focused" $ do
    let attrs = [isSelected False, onSelectedChanged (\b -> [OutMsg (show b)])]
    result <- runInteractions testBounds seedCtx (radioButton OptionA attrs) [Wait 1] [PressKey KeyReturn []]
    resultMessages result `shouldBe` [show True]
