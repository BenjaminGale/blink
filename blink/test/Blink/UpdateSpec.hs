module Blink.UpdateSpec (spec) where

import Test.Hspec

import Blink.Update

spec :: Spec
spec = describe "Update" $ do
  describe "modify" $
    it "applies the function to the state" $
      runUpdate (modify (+1)) (1 :: Int) `shouldBe` 2

  describe "put" $
    it "replaces the state" $
      runUpdate (put 5) (1 :: Int) `shouldBe` 5

  describe "gets" $
    it "does not affect the state read back via runUpdate" $
      runUpdate (gets (+100) >>= \_ -> pure ()) (1 :: Int) `shouldBe` 1

  describe "sequencing" $ do
    it "threads state through multiple modify calls in order" $
      runUpdate (modify (+1) >> modify (*10)) (1 :: Int) `shouldBe` 20

    it "lets later steps read the state written by earlier steps" $
      runUpdate
        (do
          modify (+1)
          n <- get
          put (n * 10))
        (1 :: Int)
        `shouldBe` 20

    it "lets a value bound with gets flow into a later step" $
      runUpdate
        (do
          n <- gets (+1)
          put n)
        (1 :: Int)
        `shouldBe` 2

  describe "quit" $ do
    let quitRequested act = resultQuit (runUpdateEffects (act :: Update Int () () ()) 0)

    it "is requested by quit" $
      quitRequested quit `shouldBe` True

    it "is not requested by an update that never calls quit" $
      quitRequested (modify (+1)) `shouldBe` False

    it "stays requested when later steps follow it" $
      quitRequested (quit >> modify (+1)) `shouldBe` True
