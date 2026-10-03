module Blink.View.CursorShapeSpec (spec) where

import Test.Hspec

import Blink.View
import Blink.View.Fixtures

spec :: Spec
spec = describe "Blink.View.CursorShape" $ do
  it "defaults to the arrow when nothing requests a cursor" $ do
    (_, ctx) <- run0 (pure ())
    getCursorShape ctx `shouldBe` CursorArrow

  it "requestCursor sets the frame's cursor shape" $ do
    (_, ctx) <- run0 (requestCursor CursorResizeHorizontal)
    getCursorShape ctx `shouldBe` CursorResizeHorizontal

  it "the last requestCursor call in a frame wins" $ do
    (_, ctx) <- run0 (requestCursor CursorResizeHorizontal >> requestCursor CursorArrow)
    getCursorShape ctx `shouldBe` CursorArrow

  it "resets to the arrow on the next frame" $ do
    (_, ctx)  <- run0 (requestCursor CursorResizeHorizontal)
    let ctx' = advance noInput ctx
    getCursorShape ctx' `shouldBe` CursorArrow
