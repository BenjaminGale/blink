{-# LANGUAGE OverloadedStrings #-}
module Blink.View.DrawingSpec (spec) where

import Test.Hspec

import Blink.Geometry (Point (..), Rectangle (..), Size (..), uniformRadii)
import Blink.Input (InputState (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (soloBorder)
import Blink.View
import Blink.View.Drawing (drawImage, drawText, fillRect, strokeRect, withBackground, withBorder, withClip)
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Drawing" $ do
  describe "withClip" $ do
    -- In each test the region checked is testBounds (100×100), so the mouse
    -- is inside the element's own bounds; only the clip region should block
    -- the hit test.
    it "isRegionHit is False when the mouse is inside bounds but outside the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseOutsideClip = noInput { inputMousePosition = Point 50 75 }
      (hit, _) <- runWith mouseOutsideClip
        (withBounds clipRect $ withClip $ withBounds testBounds isRegionHit)
      hit `shouldBe` False

    it "isRegionHit is True when the mouse is inside both the bounds and the clip region" $ do
      let clipRect = Rectangle 0 0 100 50
          mouseInsideClip = noInput { inputMousePosition = Point 50 25 }
      (hit, _) <- runWith mouseInsideClip
        (withBounds clipRect $ withClip $ withBounds testBounds isRegionHit)
      hit `shouldBe` True

    it "wraps draw commands in PushClip / PopClip" $ do
      let clipRect = Rectangle 0 0 100 50
      (_, ctx) <- run0 (withBounds clipRect $ withClip $ fillRect (RGBA 1 0 0 1))
      getDrawCommands ctx `shouldBe`
        [PushClip clipRect, FillRect clipRect (RGBA 1 0 0 1), PopClip]

    it "isRegionHit is False when the mouse is outside the intersection of nested clip regions" $ do
      let outerClip = Rectangle 0 0 100 50
          innerClip = Rectangle 0 25 100 50
          -- intersection is y 25–50; mouse at (50, 10) is inside outerClip but outside intersection
          mouseOutside = noInput { inputMousePosition = Point 50 10 }
      (hit, _) <- runWith mouseOutside
        (withBounds outerClip $ withClip $
         withBounds innerClip $ withClip $
         withBounds testBounds isRegionHit)
      hit `shouldBe` False

  describe "drawing" $ do
    it "fillRect emits a FillRect command for the current bounds" $ do
      let colour = RGBA 1 0 0 1
      (_, ctx) <- run0 (fillRect colour)
      getDrawCommands ctx `shouldBe` [FillRect testBounds colour]

    it "strokeRect emits a StrokeBorder command for the current bounds" $ do
      let colour = RGBA 0 1 0 1
          border = soloBorder colour 2
      (_, ctx) <- run0 (strokeRect border)
      getDrawCommands ctx `shouldBe` [StrokeBorder testBounds border]

    it "drawText emits a DrawText command for the current bounds" $ do
      let colour = RGBA 0 0 1 1
      (_, ctx) <- run0 (drawText colour AlignCenter "hello")
      getDrawCommands ctx `shouldBe` [DrawText testBounds "hello" colour AlignCenter]

    it "drawImage emits a DrawImage command for the current bounds" $ do
      let colour = RGBA 1 0 0 1
      (_, ctx) <- run0 (drawImage colour "icons/check.svg")
      getDrawCommands ctx `shouldBe` [DrawImage testBounds "icons/check.svg" colour]

    it "measureImage returns the size the ImageMeasurer reports for the path" $ do
      let stubMeasurer = noOpImageMeasurer { imNaturalSize = \_ -> pure (Size 24 24) }
          ctx0 = withMeasurers (noOpMeasurers { msrImage = stubMeasurer })
                   (emptyViewContext testBounds noInput emptyTheme)
      (size, _) <- runView (measureImage "icons/check.svg") ctx0
      size `shouldBe` Size 24 24

    it "getDrawCommands returns commands in submission order" $ do
      let c1 = RGBA 1 0 0 1
          c2 = RGBA 0 1 0 1
      (_, ctx) <- run0 (fillRect c1 >> fillRect c2)
      getDrawCommands ctx `shouldBe` [FillRect testBounds c1, FillRect testBounds c2]

    it "nextFrameContext clears draw commands from the previous frame" $ do
      (_, ctx) <- run0 (fillRect (RGBA 1 0 0 1))
      let ctx' = advance noInput ctx
      getDrawCommands ctx' `shouldBe` []

    describe "withBackground" $ do
      it "emits a FillRect when the colour is opaque and the radii are all zero" $ do
        let colour = RGBA 1 0 0 1
        (_, ctx) <- run0 (withBackground (uniformRadii 0) colour (pure ()))
        getDrawCommands ctx `shouldBe` [FillRect testBounds colour]

      it "emits no fill when the colour is fully transparent" $ do
        (_, ctx) <- run0 (withBackground (uniformRadii 0) (RGBA 0 0 0 0) (pure ()))
        getDrawCommands ctx `shouldBe` []

      it "emits a FillRoundedRect, not a FillRect, when the radii aren't all zero" $ do
        let colour = RGBA 1 0 0 1
            radii  = uniformRadii 8
        (_, ctx) <- run0 (withBackground radii colour (pure ()))
        getDrawCommands ctx `shouldBe` [FillRoundedRect testBounds radii colour]

    describe "withBorder" $ do
      it "strokes the border after the content" $ do
        let bgColour = RGBA 1 0 0 1
            border   = soloBorder (RGBA 0 0 1 1) 1
        (_, ctx) <- run0 (withBorder border (fillRect bgColour))
        getDrawCommands ctx `shouldBe`
          [ FillRect testBounds bgColour
          , StrokeBorder testBounds border
          ]

      it "emits no StrokeBorder when every layer is fully transparent" $ do
        (_, ctx) <- run0 (withBorder (soloBorder (RGBA 0 0 0 0) 1) (pure ()))
        getDrawCommands ctx `shouldBe` []

      it "drops only the transparent layers from a mixed stack" $ do
        let visible     = soloBorder (RGBA 0 1 0 1) 1
            transparent = soloBorder (RGBA 0 0 0 0) 2
        (_, ctx) <- run0 (withBorder (transparent ++ visible) (pure ()))
        getDrawCommands ctx `shouldBe` [StrokeBorder testBounds visible]
