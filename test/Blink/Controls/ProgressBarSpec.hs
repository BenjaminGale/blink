{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ProgressBarSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Element (Attribute, elementId)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.ElementBehaviour (tagged)
import Blink.Controls.FixedFocusBehaviour (fixedNotFocusableSpec)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Controls.ProgressBar (ProgressBarConfig, ProgressValue (..), bandSpeed, progress, progressBar)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.UI
import Blink.UI.Element (runElement)

data TestElement = Bar deriving (Eq, Ord, Show)

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
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

-- | Content rect for 'testBounds': inset by margin (10) then padding (5).
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 70

type Attribute' = Attribute (ProgressBarConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

run :: [Attribute'] -> IO (UIContext TestElement String)
run attrs = snd <$> runUI (runElement (progressBar attrs)) seedCtx

-- | 'progressBar' with 'elementId' 'Bar' set -- for the shared behaviour
-- contracts below, which need a real identity to track hover\/click\/focus
-- against.
renderWithId :: [Attribute'] -> UI TestElement String ()
renderWithId attrs = runElement (progressBar (elementId Bar : attrs))

-- | A context whose animation clock reads one elapsed second -- 'runUI'
-- against this directly, rather than through 'runInteractions', since
-- advancing frames via simulated input never moves the animation clock on
-- its own (it's fed in explicitly via 'nextFrameContext').
elapsedCtx :: UIContext TestElement String
elapsedCtx = nextFrameContext testBounds noInput testTheme (mkAnimationState 0 1 False) seedCtx

spec :: Spec
spec = describe "Blink.Controls.ProgressBar" $ do
  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    testBounds seedCtx Bar (Point 5 5) hitRect (Point 200 200) renderWithId

  fixedNotFocusableSpec testBounds seedCtx renderWithId

  describe "no id" $ do
    -- No 'elementId' at all -- 'run' never adds one, unlike 'renderWithId'.
    it "raises no events at all, even with every handler attached and the cursor pressed and released over it" $ do
      result <- runInteractions testBounds seedCtx (runElement (progressBar tagged)) []
                  [MouseDown (Point 50 50), MouseUp (Point 50 50)]
      resultMessages result `shouldBe` []

    it "still renders (in its resting style), just like it does with an id" $ do
      ctx <- run [progress (Progress 0.5)]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 35 70) testColour]

  describe "Progress" $ do
    it "fills the correct proportion of the content area at 0.5" $ do
      ctx <- run [progress (Progress 0.5)]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 35 70) testColour]

    it "fills the full content area at 1.0" $ do
      ctx <- run [progress (Progress 1.0)]
      getDrawCommands ctx `shouldContain` [FillRect contentRect testColour]

    it "fills zero width at 0.0" $ do
      ctx <- run [progress (Progress 0.0)]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

    it "clamps values above 1.0 to full width" $ do
      ctx <- run [progress (Progress 1.5)]
      getDrawCommands ctx `shouldContain` [FillRect contentRect testColour]

    it "clamps values below 0.0 to zero width" $ do
      ctx <- run [progress (Progress (-0.5))]
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 15 15 0 70) testColour]

  describe "Indeterminate" $ do
    it "sweeps the band using the default band speed (0.5)" $ do
      ctx <- snd <$> runUI (runElement (progressBar [progress Indeterminate])) elapsedCtx
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 39.5 15 21 70) testColour]

    it "sweeps faster when a custom speed is given" $ do
      ctx <- snd <$> runUI (runElement (progressBar [progress Indeterminate, bandSpeed 1.0])) elapsedCtx
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle (-6) 15 21 70) testColour]

    it "keeps the animation ticker alive" $ do
      ctx <- snd <$> runUI (runElement (progressBar [progress Indeterminate])) elapsedCtx
      contextRequiresAnimation ctx `shouldBe` True

    it "does not request animation for a determinate bar" $ do
      ctx <- run [progress (Progress 0.5)]
      contextRequiresAnimation ctx `shouldBe` False
