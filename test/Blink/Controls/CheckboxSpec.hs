{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.CheckboxSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Element (Attribute)
import Blink.Controls.Label (text)
import Blink.Controls.Toggle (ToggleConfig, isSelected)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform, uniformBorder)
import Blink.Input (InputState (..))
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (elLayout, runElement)

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
-- the 10px margin every test style here uses -- covers both the checkbox's
-- glyph (x: 15-35) and caption (x: 41-85), so random points from within it
-- exercise both halves.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a checkbox that fills its given bounds entirely,
-- as every control did before controls reported their own 'Layout'.
-- 'checkbox' now defaults to sizing itself to its own content, so these
-- tests ask for the old full-size behaviour explicitly, the same way any
-- other caller would.
fullSize :: [Attribute'] -> UI TestElement String ()
fullSize attrs = runElement (checkbox Remember attrs) { elLayout = Layout fill fill TopLeft }

start :: [Attribute'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Checkbox" $ do
  toggleBehaviourSpec not testBounds seedCtx Remember (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws the box and its caption, with no tick, while not selected" $ do
    ctx <- start [text "Remember me"]
    let cmds = getDrawCommands ctx
    cmds `shouldContain`
      [ StrokeBorder (Rectangle 16 41 18 18) testColour (uniformBorder 1)
      , DrawText (Rectangle 41 15 44 70) "Remember me" testColour AlignCenter
      ]
    cmds `shouldNotContain` [DrawText (Rectangle 16 41 18 18) "\10003" testColour AlignCenter]

  it "draws a tick inside the box while selected" $ do
    ctx <- start [text "Remember me", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 16 41 18 18) "\10003" testColour AlignCenter]
