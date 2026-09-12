module Blink.View.ExtentSpec (spec) where

import Test.Hspec

import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Extent" $ do
  it "returns 0 when nothing has adjusted the extent yet" $ do
    (v, _) <- run0 (getExtentState ())
    v `shouldBe` 0

  it "requestExtentBy is deferred rather than applied immediately" $ do
    (v, _) <- run0 (requestExtentBy () 5 >> getExtentState ())
    v `shouldBe` 0

  it "a requested adjustment is visible via getExtentState once settled" $ do
    (_, ctx) <- run0 (requestExtentBy () 5)
    (v, _) <- runView (getExtentState ()) (settleEffects ctx)
    v `shouldBe` 5

  it "multiple requestExtentBy calls in the same frame accumulate" $ do
    (_, ctx) <- run0 (requestExtentBy () 5 >> requestExtentBy () (-2))
    (v, _) <- runView (getExtentState ()) (settleEffects ctx)
    v `shouldBe` 3

  it "composes with the extent already accumulated from a prior frame" $ do
    (_, ctx0) <- run0 (requestExtentBy () 5)
    let ctx1 = advance noInput ctx0
    (_, ctx2) <- runView (requestExtentBy () 5) ctx1
    (v, _) <- runView (getExtentState ()) (settleEffects ctx2)
    v `shouldBe` 10

  it "keeps extent state separate per element" $ do
    (_, ctx) <- runTwoElem (requestExtentBy ElemA 5 >> requestExtentBy ElemB 8)
    let settled = settleEffects ctx
    contextExtentState ElemA settled `shouldBe` 5
    contextExtentState ElemB settled `shouldBe` 8
