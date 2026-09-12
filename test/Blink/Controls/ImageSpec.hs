{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ImageSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Image (ImageConfig, fitHeight, fitWidth, image, preserveRatio, source)
import Blink.Geometry (Point (..), Rectangle (..), Size (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View
import Blink.Element (Attribute, runElement)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testStyle :: Style
testStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignLeft
  , styleBorderColour = Nothing
  }

-- | No margin\/padding\/border, so the drawn rectangle below reflects
-- 'image'\'s own computed size directly, with no chrome inset to account
-- for -- chrome itself is 'measureChrome', shared machinery already
-- covered elsewhere.
testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = Map.empty }

testTheme :: Theme ()
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (testMetrics, testStyleSet) }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  , inputWheelDelta     = 0
  }

-- | Reports a fixed 100x50 natural size for any path, so the fit/
-- preserve-ratio maths below have a known, non-square starting point.
stubMeasurers :: Measurers
stubMeasurers = noOpMeasurers
  { msrImage = ImageMeasurer { imNaturalSize = \_ -> pure (Size 100 50) }
  }

seedCtx :: ViewContext () String
seedCtx = withMeasurers stubMeasurers (emptyViewContext testBounds noInput testTheme)

run :: [Attribute (ImageConfig () String)] -> IO [DrawCommand]
run attrs = getDrawCommands . snd <$> runView (runElement (image (source "test.svg" : attrs))) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Image" $ do
  it "sizes to the image's natural size when no fit dimension is set" $ do
    draws <- run []
    draws `shouldContain` [DrawImage (Rectangle 0 0 100 50) "test.svg" (RGBA 1 1 1 1)]

  it "scales proportionally from fitWidth alone" $ do
    draws <- run [fitWidth 50]
    draws `shouldContain` [DrawImage (Rectangle 0 0 50 25) "test.svg" (RGBA 1 1 1 1)]

  it "scales proportionally from fitHeight alone" $ do
    draws <- run [fitHeight 100]
    draws `shouldContain` [DrawImage (Rectangle 0 0 200 100) "test.svg" (RGBA 1 1 1 1)]

  it "ignores aspect ratio when preserveRatio is False, using each fit dimension independently" $ do
    draws <- run [fitWidth 10, fitHeight 10, preserveRatio False]
    draws `shouldContain` [DrawImage (Rectangle 0 0 10 10) "test.svg" (RGBA 1 1 1 1)]
