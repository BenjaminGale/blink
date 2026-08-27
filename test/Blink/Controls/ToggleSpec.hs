{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ToggleSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Element (Attribute)
import Blink.Controls.Toggle (ToggleConfig, isSelected, toggleButton, toggleChecked)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)

import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Layout.Constraints (Layout (..), Length (Fill))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (elLayout, runElement)

data TestElement = Ok deriving (Eq, Ord, Show)

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

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

toggleTestTheme :: Theme TestElement
toggleTestTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  =
      ( testMetrics
      , testStyleSet { styleOverrides = Map.singleton toggleChecked (\s -> s { styleTextColour = pressedColour }) }
      )
  }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput toggleTestTheme noOpTextMeasurer

-- | See 'Blink.Controls.ButtonSpec.fullSize' -- same reasoning, for
-- 'toggleButton'.
fullSize :: [Attribute'] -> UI TestElement String ()
fullSize attrs = runElement (toggleButton Ok attrs) { elLayout = Layout Fill Fill TopLeft }

start :: [Attribute'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Toggle" $ do
  describe "toggleButton" $ do
    toggleBehaviourSpec not testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSize

    it "draws in its normal style while not selected" $ do
      ctx <- start []
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" testColour AlignCenter]

    it "draws in its pressed style while selected, even without being physically pressed" $ do
      ctx <- start [isSelected True]
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" pressedColour AlignCenter]
