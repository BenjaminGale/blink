module Blink.View.ScrollSpec (spec) where

import Test.Hspec

import Blink.View
import Blink.Testing
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Scroll" $ do
  describe "scroll state" $ do
    it "returns 0 when no position has been recorded" $ do
      (v, _) <- run0 (getScrollState ())
      v `shouldBe` 0

    it "requestScrollBy is deferred rather than applied immediately" $ do
      (v, _) <- run0 (requestScrollBy () 30 >> getScrollState ())
      v `shouldBe` 0

    it "a requested scroll offset is visible via getScrollState once settled" $ do
      (_, ctx) <- run0 (requestScrollBy () 30)
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 30

    it "accumulates requestScrollBy calls in the same frame" $ do
      (_, ctx) <- run0 (requestScrollBy () 30 >> requestScrollBy () 20)
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 50

    it "never scrolls to a negative offset" $ do
      (_, ctx) <- run0 (requestScrollBy () 30 >> requestScrollBy () (-50))
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 0

    it "leaves the offset alone until the control draws when asked for a fraction" $ do
      (_, ctx) <- run0 (requestScrollBy () 30 >> requestScrollTo () 1)
      (v, _) <- runView (getScrollState ()) (settleEffects ctx)
      v `shouldBe` 30

    it "applies setScrollStateNow within the same frame" $ do
      (v, _) <- run0 (setScrollStateNow () 40 >> getScrollState ())
      v `shouldBe` 40

    it "keeps scroll offsets separate per element" $ do
      (_, ctx) <- runTwoElem (requestScrollBy ElemA 30 >> requestScrollBy ElemB 70)
      (v, _) <- runView (getScrollState ElemA) (settleEffects ctx)
      v `shouldBe` 30
