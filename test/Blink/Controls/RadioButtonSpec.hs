{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.RadioButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Element (Attribute)
import Blink.Controls.Label (text)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.Toggle (ToggleConfig, isSelected)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Layout.Constraints (Layout (..), Length (Fill))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (elLayout, runElement)

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
  , styleBorderColour = Nothing
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 10
  , metricsPadding     = uniform 5
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = Map.empty }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (testMetrics, testStyleSet) }

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

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a radio button that fills its given bounds
-- entirely, as every control did before controls reported their own
-- 'Layout'. 'radioButton' now defaults to sizing itself to its own content,
-- so these tests ask for the old full-size behaviour explicitly, the same
-- way any other caller would.
fullSize :: [Attribute'] -> UI TestElement String ()
fullSize attrs = runElement (radioButton OptionA attrs) { elLayout = Layout Fill Fill TopLeft }

start :: [Attribute'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.RadioButton" $ do
  -- Unlike a flipping toggle, a radio button only ever moves from
  -- unselected to selected -- activating it while already selected leaves
  -- it selected, so it reports nothing.
  toggleBehaviourSpec (const True) testBounds seedCtx OptionA (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws the unselected glyph and its caption while not selected" $ do
    ctx <- start [text "Option A"]
    getDrawCommands ctx `shouldContain`
      [ DrawText (Rectangle 15 15 20 70) "\9675" testColour AlignCenter
      , DrawText (Rectangle 35 15 50 70) "Option A" testColour AlignCenter
      ]

  it "draws the selected glyph while selected" $ do
    ctx <- start [text "Option A", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 20 70) "\9679" testColour AlignCenter]
