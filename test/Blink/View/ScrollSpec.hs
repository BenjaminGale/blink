module Blink.View.ScrollSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Scroll" $ do
  describe "scroll state" $ do
    it "returns 0 when no position has been recorded" $ do
      (v, _) <- run0 (getScrollState ())
      v `shouldBe` 0

    it "requestScrollTo is deferred rather than applied immediately" $ do
      (v, _) <- run0 (requestScrollTo () 0.5 >> getScrollState ())
      v `shouldBe` 0

    it "a requested scroll position is visible via getScrollState once settled" $ do
      (_, ctx) <- run0 (requestScrollTo () 0.5)
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 0.5

    it "requestScrollBy composes with the current position, clamped to [0, 1]" $ do
      (_, ctx) <- run0 (requestScrollTo () 0.5 >> requestScrollBy () 0.7)
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 1.0

    it "requestScrollTo clamps an out-of-range value to [0, 1]" $ do
      (_, ctx) <- run0 (requestScrollTo () 1.5)
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 1.0

    it "keeps scroll positions separate per element" $ do
      (_, ctx) <- runTwoElem (requestScrollTo ElemA 0.3 >> requestScrollTo ElemB 0.7)
      (v, _) <- runView (getScrollState ElemA) (settleEffects ctx)
      v `shouldBe` 0.3

  describe "clampScrollPos" $ do
    it "clamps values below 0 to 0" $
      clampScrollPos (-0.5) `shouldBe` 0
    it "clamps values above 1 to 1" $
      clampScrollPos 1.5 `shouldBe` 1
    it "preserves values inside [0, 1]" $
      clampScrollPos 0.5 `shouldBe` 0.5
    it "preserves 0" $
      clampScrollPos 0 `shouldBe` 0
    it "preserves 1" $
      clampScrollPos 1 `shouldBe` 1

  describe "clampScrollPos properties" $ do
    prop "is idempotent" $ \x ->
      clampScrollPos (clampScrollPos x) == (clampScrollPos x :: Double)

    prop "result is always in [0, 1]" $ \x ->
      let v = clampScrollPos (x :: Double) in v >= 0 && v <= 1
