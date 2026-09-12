{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Button (ButtonActivation (..), ButtonConfig, activation, button, onActivated)
import Blink.Controls.ButtonBehaviour (buttonBehaviourSpec, defaultButtonBehaviourConfig)
import Blink.Controls.Control (Attribute, post)
import Blink.Controls.Label (text)
import qualified Data.Text as T

import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View
import Blink.Element (elLayout, runElement)

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

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  , inputWheelDelta     = 0
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (ButtonConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

-- | The behaviour contracts below (hover, click, focus, ...) are about
-- interaction, not sizing -- they're written against a button that fills
-- its given bounds entirely, as every control did before controls reported
-- their own 'Layout'. 'button' now defaults to sizing its height to its
-- own content, so these tests ask for the old full-size behaviour
-- explicitly, the same way any other caller would.
fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = runElement (button Ok attrs) { elLayout = Layout fill fill TopLeft }

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = snd <$> runView (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Button" $ do
  buttonBehaviourSpec defaultButtonBehaviourConfig testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws its text in the resolved style" $ do
    ctx <- start [text "OK"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "OK" testColour AlignCenter]

  describe "activation ActivateOnPress" $ do
    -- 'buttonBehaviourSpec' above only covers the default 'ActivateOnClick'
    -- activation -- these check that 'button' itself, not just
    -- 'Blink.Controls.RepeatButton.repeatButton', honours the
    -- 'ActivateOnPress' override 'activation' exposes.
    let insidePoint = Point 50 50
        taggedActivated = [activation ActivateOnPress, onActivated (post "Activated")]

    it "fires onActivated immediately on press, not on release" $ do
      result <- runInteractions testBounds seedCtx (fullSize taggedActivated) [] [MouseDown insidePoint]
      resultMessages result `shouldBe` ["Activated"]

    it "does not fire onActivated again on release" $ do
      result <- runInteractions testBounds seedCtx (fullSize taggedActivated) [MouseDown insidePoint] [MouseUp insidePoint]
      resultMessages result `shouldBe` []

  describe "FitContent sizing" $ do
    -- Spec scenario: a button with 'width fitContent' (here, both axes, via
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
          fitContentEl attrs = runElement (button Ok attrs) { elLayout = Layout fitContent fitContent TopLeft }
          fitCtx = withMeasurers (noOpMeasurers { msrText = fixedWidthMeasurer })
                     (emptyViewContext (Rectangle 0 0 500 500) noInput testTheme)
      ctx <- snd <$> runView (fitContentEl [text caption]) fitCtx
      getDrawCommands ctx `shouldContain` [FillRect expectedBg testColour]
