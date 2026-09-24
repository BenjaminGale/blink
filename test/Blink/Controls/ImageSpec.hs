{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ImageSpec (spec) where

import Test.Hspec

import Blink.Controls.ControlBehaviour (styleAttributeSpec)
import Blink.Controls.Fixtures (hitRectFor, mkTestTheme, noInput, plainMetrics, plainStyle, plainStyleSet, standardMetrics, testColour)
import Blink.Controls.Image (ImageConfig, fitHeight, fitWidth, image, preserveRatio, source)
import Blink.Geometry (Rectangle (..), Size (..), uniform)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Theme, styleTextAlign)
import Blink.View
import Blink.Element (Attribute, runElement)

data TestElement = Pic deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

-- | No margin\/padding\/border, so the drawn rectangle below reflects
-- 'image'\'s own computed size directly, with no chrome inset to account
-- for -- chrome itself is 'measureChrome', shared machinery already
-- covered elsewhere.
testTheme :: Theme ()
testTheme = mkTestTheme
  (plainMetrics (uniform 0) (uniform 0))
  (plainStyleSet ((plainStyle testColour) { styleTextAlign = AlignLeft }))

-- | Reports a fixed 100x50 natural size for any path, so the fit/
-- preserve-ratio maths below have a known, non-square starting point.
stubMeasurers :: Measurers
stubMeasurers = noOpMeasurers
  { msrImage = ImageMeasurer { imNaturalSize = \_ -> pure (Size 100 50) }
  }

seedCtx :: ViewContext () String
seedCtx = withMeasurers stubMeasurers (emptyViewContext testBounds noInput testTheme)

-- | Reports a zero natural size, for testing the fallback that avoids
-- dividing by zero when scaling to preserve an undefined aspect ratio.
zeroSizeMeasurers :: Measurers
zeroSizeMeasurers = noOpMeasurers
  { msrImage = ImageMeasurer { imNaturalSize = \_ -> pure (Size 0 0) }
  }

run :: [Attribute (ImageConfig () String)] -> IO [DrawCommand]
run attrs = getDrawCommands . snd <$> runView (runElement (image (source "test.svg" : attrs))) seedCtx

-- | Real margin, unlike 'testTheme', for the hit-region contract below.
contractTheme :: Theme TestElement
contractTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

-- | Small enough, with chrome, to fit 'testBounds' -- unlike 'stubMeasurers'.
contractMeasurers :: Measurers
contractMeasurers = noOpMeasurers
  { msrImage = ImageMeasurer { imNaturalSize = \_ -> pure (Size 40 40) }
  }

contractCtx :: ViewContext TestElement String
contractCtx = withMeasurers contractMeasurers (emptyViewContext testBounds noInput contractTheme)

-- | The 70x70 rect 'renderContract' renders at, inset by 'standardMetrics'.
contractHitRect :: Rectangle
contractHitRect = hitRectFor (Rectangle 0 0 70 70)

-- | 'image' with a source, for the shared style contract below.
renderContract :: [Attribute (ImageConfig TestElement String)] -> View TestElement String ()
renderContract attrs = runElement (image (source "test.svg" : attrs))

spec :: Spec
spec = describe "Blink.Controls.Image" $ do
  styleAttributeSpec testBounds contractCtx contractHitRect renderContract

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

  it "scales from whichever of fitWidth/fitHeight fits tighter, when both are set" $ do
    -- Against the 100x50 stub, fitWidth 60 alone would scale by 0.6 (to
    -- 60x30) and fitHeight 40 alone would scale by 0.8 (to 80x40) -- the
    -- smaller scale factor is the tighter fit, so fitWidth's 0.6 should
    -- win over fitHeight's 0.8.
    draws <- run [fitWidth 60, fitHeight 40]
    draws `shouldContain` [DrawImage (Rectangle 0 0 60 30) "test.svg" (RGBA 1 1 1 1)]

  it "falls back to the fit dimensions directly when the natural size is zero" $ do
    draws <- getDrawCommands . snd <$> runView
      (runElement (image [source "test.svg", fitWidth 50, fitHeight 25]))
      (withMeasurers zeroSizeMeasurers seedCtx)
    draws `shouldContain` [DrawImage (Rectangle 0 0 50 25) "test.svg" (RGBA 1 1 1 1)]
