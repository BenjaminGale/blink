{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.TreeSpec (spec) where

import qualified Data.Set as Set
import Data.Tree (Tree (..))
import Test.Hspec

import Blink.Controls.Tree (flattenVisible)

-- | src
--   |- src/List.hs
--   |- src/Controls
--        |- src/Controls/Button.hs
--   test
forest :: [Tree String]
forest =
  [ Node "src"
      [ Node "src/List.hs" []
      , Node "src/Controls"
          [ Node "src/Controls/Button.hs" [] ]
      ]
  , Node "test" []
  ]

spec :: Spec
spec = describe "Blink.Controls.Tree" $
  describe "flattenVisible" $ do
    it "shows only the roots when nothing is expanded" $
      flattenVisible forest Set.empty `shouldBe`
        [ ("src", 0), ("test", 0) ]

    it "descends into an expanded node's children, but not a collapsed grandchild's" $
      flattenVisible forest (Set.singleton "src") `shouldBe`
        [ ("src", 0), ("src/List.hs", 1), ("src/Controls", 1), ("test", 0) ]

    it "descends further once a deeper node is also expanded" $
      flattenVisible forest (Set.fromList ["src", "src/Controls"]) `shouldBe`
        [ ("src", 0)
        , ("src/List.hs", 1)
        , ("src/Controls", 1)
        , ("src/Controls/Button.hs", 2)
        , ("test", 0)
        ]

    it "yields nothing for an empty forest" $
      flattenVisible ([] :: [Tree String]) (Set.singleton "src") `shouldBe` []
