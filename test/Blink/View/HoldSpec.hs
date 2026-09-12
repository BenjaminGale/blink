module Blink.View.HoldSpec (spec) where

import Test.Hspec

import qualified Blink.View.Hold as Hold
import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Hold" $ do
  describe "resolveHoldRepeats" $ do
    it "returns 0 while not held" $ do
      (n, _) <- run0 (resolveHoldRepeats () False 0.4 0.08)
      n `shouldBe` 0

    it "returns 0 for a press that has not yet reached the initial delay" $ do
      (n, _) <- run0 (resolveHoldRepeats () True 0.4 0.08)
      n `shouldBe` 0

    it "fires once a held press crosses the initial delay" $ do
      ctx0 <- freshCtx
      (_, ctx1) <- runView (resolveHoldRepeats () True 0.4 0.08) ctx0
      let ctx2 = nextFrameContext testBounds noInput emptyTheme (mkAnimationState 0.4 0.4 True) ctx1
      (n, _) <- runView (resolveHoldRepeats () True 0.4 0.08) ctx2
      n `shouldBe` 1

    it "keeps separate elements' cadences independent" $ do
      ctx0 <- snd <$> runTwoElem (pure ())
      (_, ctx1) <- runView (resolveHoldRepeats ElemA True 0.4 0.08) ctx0
      let ctx2 = nextFrameContext testBounds noInput twoElemTheme (mkAnimationState 0.4 0.4 True) ctx1
      (nA, ctx3) <- runView (resolveHoldRepeats ElemA True 0.4 0.08) ctx2
      (nB, _)    <- runView (resolveHoldRepeats ElemB True 0.4 0.08) ctx3
      nA `shouldBe` 1
      nB `shouldBe` 0

  describe "repeatsDueBy" $ do
    it "fires no repeats before the initial delay" $
      Hold.repeatsDueBy 0.4 0.08 0.3 `shouldBe` 0

    it "fires the first repeat exactly at the initial delay" $
      Hold.repeatsDueBy 0.4 0.08 0.4 `shouldBe` 1

    it "fires again every interval thereafter" $ do
      Hold.repeatsDueBy 0.4 0.08 0.49 `shouldBe` 2
      Hold.repeatsDueBy 0.4 0.08 0.57 `shouldBe` 3

    it "catches up on multiple interval crossings spanned by one long duration" $
      Hold.repeatsDueBy 0.4 0.08 0.65 `shouldBe` 4
