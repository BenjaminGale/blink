{-# LANGUAGE OverloadedStrings #-}
module Blink.RadioButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Control (text)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.RadioButton (RadioButtonAttributes, isSelected, radioButton)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.ToggleBehaviour (toggleBehaviourSpec)
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

type Attr' = RadioButtonAttributes TestElement String

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (radioButton OptionA attrs) seedCtx

spec :: Spec
spec = describe "Blink.RadioButton" $ do
  -- Unlike a flipping toggle, a radio button only ever moves from
  -- unselected to selected -- activating it while already selected leaves
  -- it selected, so it reports nothing.
  toggleBehaviourSpec (const True) testBounds seedCtx OptionA (Point 5 5) hitRect (Point 200 200) (radioButton OptionA)

  it "draws the unselected glyph and its caption while not selected" $ do
    ctx <- start [text "Option A"]
    getDrawCommands ctx `shouldContain`
      [ DrawText (Rectangle 15 15 20 70) "\9675" testColour AlignCenter
      , DrawText (Rectangle 35 15 50 70) "Option A" testColour AlignCenter
      ]

  it "draws the selected glyph while selected" $ do
    ctx <- start [text "Option A", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 20 70) "\9673" testColour AlignCenter]
