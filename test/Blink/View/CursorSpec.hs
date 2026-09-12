module Blink.View.CursorSpec (spec) where

import Test.Hspec

import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.Cursor" $ do
  it "getCursorIndex returns Nothing when nothing has been recorded" $ do
    (i, _) <- run0 (getCursorIndex ())
    i `shouldBe` Nothing

  it "setCursorIndex is deferred rather than applied immediately" $ do
    (i, _) <- run0 (setCursorIndex () (Just 3) >> getCursorIndex ())
    i `shouldBe` Nothing

  it "a set index is visible via getCursorIndex once settled" $ do
    (_, ctx) <- run0 (setCursorIndex () (Just 3))
    (i, _) <- runView (getCursorIndex ()) (settleEffects ctx)
    i `shouldBe` Just 3

  it "setCursorIndex Nothing clears a previously set index once settled" $ do
    (_, ctx0) <- run0 (setCursorIndex () (Just 3))
    let ctx1 = advance noInput ctx0
    (_, ctx2) <- runView (setCursorIndex () Nothing) ctx1
    (i, _) <- runView (getCursorIndex ()) (settleEffects ctx2)
    i `shouldBe` Nothing

  it "keeps cursor indices separate per element" $ do
    (_, ctx) <- runTwoElem (setCursorIndex ElemA (Just 1) >> setCursorIndex ElemB (Just 2))
    let settled = settleEffects ctx
    contextCursorIndex ElemA settled `shouldBe` Just 1
    contextCursorIndex ElemB settled `shouldBe` Just 2
