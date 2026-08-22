{-# LANGUAGE OverloadedStrings #-}
module Blink.ProgressBarSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes (Attr)
import Blink.Element (ElementEvent)
import Blink.ElementBehaviour (elementBehaviourSpec)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.ProgressBar (ProgressBarConfig, ProgressValue (..), bandSpeed, progress, progressBar)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

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
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

-- | Content rect for 'testBounds': inset by margin (10) then padding (5).
contentRect :: Rectangle
contentRect = Rectangle 15 15 70 70

type Attr' = Attr TestElement ElementEvent String (ProgressBarConfig TestElement)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

run :: [Attr'] -> IO (UIContext TestElement String)
run attrs = snd <$> runUI (progressBar Bar attrs) seedCtx

-- | A context whose animation clock reads one elapsed second -- 'runUI'
-- against this directly, rather than through 'runInteractions', since
-- advancing frames via simulated input never moves the animation clock on
-- its own (it's fed in explicitly via 'nextFrameContext').
elapsedCtx :: UIContext TestElement String
elapsedCtx = nextFrameContext testBounds noInput testTheme (mkAnimationState 0 1 False) seedCtx

spec :: Spec
spec = describe "Blink.ProgressBar" $ do
  elementBehaviourSpec testBounds seedCtx Bar hitRect (Point 200 200) (progressBar Bar)

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
      ctx <- snd <$> runUI (progressBar Bar [progress Indeterminate]) elapsedCtx
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 39.5 15 21 70) testColour]

    it "sweeps faster when a custom speed is given" $ do
      ctx <- snd <$> runUI (progressBar Bar [progress Indeterminate, bandSpeed 1.0]) elapsedCtx
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle (-6) 15 21 70) testColour]

    it "keeps the animation ticker alive" $ do
      ctx <- snd <$> runUI (progressBar Bar [progress Indeterminate]) elapsedCtx
      contextRequiresAnimation ctx `shouldBe` True

    it "does not request animation for a determinate bar" $ do
      ctx <- run [progress (Progress 0.5)]
      contextRequiresAnimation ctx `shouldBe` False
