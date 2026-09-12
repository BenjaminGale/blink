module Blink.View.AnimationSpec (spec) where

import Test.Hspec

import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Animation" $ do
  describe "mkAnimationState" $ do
    it "leaves a delta within [0, 0.1] unchanged" $ do
      animDelta (mkAnimationState 0.016 0 False) `shouldBe` 0.016

    it "clamps a delta above 100ms down to 0.1" $ do
      animDelta (mkAnimationState 5 0 False) `shouldBe` 0.1

    it "clamps a negative delta up to 0" $ do
      animDelta (mkAnimationState (-1) 0 False) `shouldBe` 0

  describe "animation" $ do
    let animState isTick = mkAnimationState 0.016 1.5 isTick
        seedWith :: Bool -> ViewContext () Int
        seedWith isTick = nextFrameContext testBounds noInput emptyTheme (animState isTick)
                            (emptyViewContext testBounds noInput emptyTheme)
        tickCtx    = seedWith True
        nonTickCtx = seedWith False

    it "getAnimDelta returns the frame delta" $ do
      (d, _) <- runView getAnimDelta tickCtx
      d `shouldBe` 0.016

    it "getAnimElapsed returns the total elapsed time" $ do
      (e, _) <- runView getAnimElapsed tickCtx
      e `shouldBe` 1.5

    it "withAnimationFrame runs its body on tick frames" $ do
      (_, ctx) <- runView (withAnimationFrame (emit 1)) tickCtx
      getMessages ctx `shouldBe` [1]

    it "withAnimationFrame skips its body on non-tick frames" $ do
      (_, ctx) <- runView (withAnimationFrame (emit 1)) nonTickCtx
      getMessages ctx `shouldBe` []

    it "requiresAnimation sets the animation continuation flag" $ do
      (_, ctx) <- runView requiresAnimation tickCtx
      contextRequiresAnimation ctx `shouldBe` True
