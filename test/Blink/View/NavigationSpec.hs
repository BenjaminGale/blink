module Blink.View.NavigationSpec (spec) where

import Test.Hspec

import Blink.Input (Key (..))
import Blink.View
import Blink.View.Fixtures

customKeys :: NavigationKeys
customKeys = NavigationKeys
  { navAdvance = [(KeyRight, [])]
  , navRetreat = [(KeyLeft, [])]
  }

spec :: Spec
spec = describe "Blink.View.Navigation" $ do
  it "getNavigationKeys returns Tab/Shift-Tab by default" $ do
    (keys, _) <- run0 getNavigationKeys
    keys `shouldBe` defaultNavigationKeys

  it "withNavigationKeys replaces the ambient keys inside the sub-tree" $ do
    (keys, _) <- run0 (withNavigationKeys customKeys getNavigationKeys)
    keys `shouldBe` customKeys

  it "withNavigationKeys restores the outer keys after the sub-tree completes" $ do
    (keys, _) <- run0 (withNavigationKeys customKeys (pure ()) >> getNavigationKeys)
    keys `shouldBe` defaultNavigationKeys
