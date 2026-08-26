{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Button (ButtonConfig, ToggleConfig, button, isSelected, toggleButton, toggleChecked)
import Blink.Controls.ButtonBehaviour (buttonBehaviourSpec)
import Blink.Controls.Element (Attribute)
import Blink.Controls.Label (text)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)
import qualified Data.Text as T

import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Layout.Constraints (Layout (..), Length (Fill, FitContent))
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

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (testMetrics, testStyleSet) }

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

type Attribute' = Attribute (ButtonConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

-- | The behaviour contracts below (hover, click, focus, ...) are about
-- interaction, not sizing -- they're written against a button that fills
-- its given bounds entirely, as every control did before controls reported
-- their own 'Layout'. 'button' now defaults to sizing its height to its
-- own content, so these tests ask for the old full-size behaviour
-- explicitly, the same way any other caller would.
fullSize :: [Attribute'] -> UI TestElement String ()
fullSize attrs = runElement (button Ok attrs) { elLayout = Layout Fill Fill TopLeft }

start :: [Attribute'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Button" $ do
  buttonBehaviourSpec testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws its text in the resolved style" $ do
    ctx <- start [text "OK"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "OK" testColour AlignCenter]

  describe "FitContent sizing" $ do
    -- Spec scenario: a button with 'width FitContent' (here, both axes, via
    -- a direct 'elLayout' override -- there is no public 'width'/'height'
    -- attribute for controls yet) sizes itself to its own chrome-wrapped
    -- caption. Verified against a manually computed 'Exactly' from the same
    -- style, to the pixel, per invariant 5 (chrome insets defined once).
    it "sizes to its chrome-wrapped caption, matching a manual computation from the same style" $ do
      let caption  = "OK"
          fixedWidthMeasurer :: TextMeasurer
          fixedWidthMeasurer = noOpTextMeasurer
            { tmTextSize = \t -> pure (Size (fromIntegral (T.length t) * 10) 12) }
          contentSize  = Size (fromIntegral (T.length caption) * 10) 12
          chromeWidth  = 2 * (10 + 5)  -- margin + padding, both sides; no border
          chromeHeight = 2 * (10 + 5)
          expectedW    = sizeWidth contentSize + chromeWidth
          expectedH    = sizeHeight contentSize + chromeHeight
          -- The background rect 'renderStyled' fills is the outer bounds
          -- inset by margin (10px each side) -- not the outer bounds
          -- themselves.
          expectedBg   = Rectangle 10 10 (expectedW - 20) (expectedH - 20)
          fitContentEl attrs = runElement (button Ok attrs) { elLayout = Layout FitContent FitContent TopLeft }
          fitCtx = emptyUIContext (Rectangle 0 0 500 500) noInput testTheme fixedWidthMeasurer
      ctx <- snd <$> runUI (fitContentEl [text caption]) fitCtx
      getDrawCommands ctx `shouldContain` [FillRect expectedBg testColour]

  describe "toggleButton" $ do
    toggleBehaviourSpec not testBounds toggleSeedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSizeToggle

    it "draws in its normal style while not selected" $ do
      ctx <- startToggle []
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" testColour AlignCenter]

    it "draws in its pressed style while selected, even without being physically pressed" $ do
      ctx <- startToggle [isSelected True]
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" pressedColour AlignCenter]

type ToggleAttr' = Attribute (ToggleConfig TestElement String)

toggleSeedCtx :: UIContext TestElement String
toggleSeedCtx = emptyUIContext testBounds noInput toggleTestTheme noOpTextMeasurer

-- | See 'fullSize' -- same reasoning, for 'toggleButton'.
fullSizeToggle :: [ToggleAttr'] -> UI TestElement String ()
fullSizeToggle attrs = runElement (toggleButton Ok attrs) { elLayout = Layout Fill Fill TopLeft }

startToggle :: [ToggleAttr'] -> IO (UIContext TestElement String)
startToggle attrs = snd <$> runUI (fullSizeToggle attrs) toggleSeedCtx
