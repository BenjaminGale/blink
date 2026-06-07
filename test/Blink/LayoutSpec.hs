module Blink.LayoutSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import Blink.Geometry (Rectangle (..))
import Blink.Layout (Constraint (..), hBoxLayout, resolveConstraint, vBoxLayout)

r :: Rectangle
r = Rectangle 0 0 100 50

spec :: Spec
spec = describe "layout" $ do
  describe "resolveConstraint" $ do
    it "Exactly ignores available space" $
      resolveConstraint (Exactly 50) 100 `shouldBe` 50

    it "Fill uses all available space" $
      resolveConstraint Fill 100 `shouldBe` 100

    describe "AtLeast" $ do
      it "enforces the minimum size when space is insufficient" $
        resolveConstraint (AtLeast 50) 30 `shouldBe` 50

      it "grows to fill space when it exceeds the minimum" $
        resolveConstraint (AtLeast 50) 80 `shouldBe` 80

    describe "AtMost" $ do
      it "grows to fill space when within its maximum" $
        resolveConstraint (AtMost 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        resolveConstraint (AtMost 80) 100 `shouldBe` 80

    describe "Between" $ do
      it "enforces the minimum size when space is insufficient" $
        resolveConstraint (Between 20 80) 10 `shouldBe` 20

      it "grows to fill space when within its range" $
        resolveConstraint (Between 20 80) 50 `shouldBe` 50

      it "limits to its maximum when space exceeds it" $
        resolveConstraint (Between 20 80) 100 `shouldBe` 80

  describe "hBoxLayout" $ do
    it "single cell occupies the full bounds" $
      hBoxLayout r 0 [Fill] `shouldBe` [Rectangle 0 0 100 50]

    it "two cells split the width equally with no spacing" $
      hBoxLayout r 0 [Fill, Fill] `shouldBe`
        [Rectangle 0 0 50 50, Rectangle 50 0 50 50]

    it "gaps between cells reduce the space available for cell widths" $
      hBoxLayout (Rectangle 0 0 110 50) 10 [Fill, Fill] `shouldBe`
        [Rectangle 0 0 50 50, Rectangle 60 0 50 50]

    it "each cell is offset by the widths of preceding cells and gaps" $
      hBoxLayout (Rectangle 0 0 160 50) 5 [Fill, Fill, Fill] `shouldBe`
        [Rectangle 0 0 50 50, Rectangle 55 0 50 50, Rectangle 110 0 50 50]

    it "all cells share the vertical position and height of the bounds" $
      hBoxLayout (Rectangle 20 10 100 50) 0 [Fill, Fill] `shouldBe`
        [Rectangle 20 10 50 50, Rectangle 70 10 50 50]

    describe "constraint resolution" $ do
      let cases =
            [ ( "fixed-size cells get their exact sizes"
              , [Exactly 30, Exactly 70], [30, 70] )
            , ( "fill cell takes all space not claimed by fixed-size cells"
              , [Exactly 40, Fill], [40, 60] )
            , ( "fill cells share remaining space equally"
              , [Exactly 40, Fill, Fill], [40, 30, 30] )
            , ( "fill cells divide all available space equally"
              , [Fill, Fill], [50, 50] )
            , ( "minimum-size cells grow equally to fill available space"
              , [AtLeast 20, Fill], [60, 40] )
            , ( "minimum-size cells share available space equally"
              , [AtLeast 20, AtLeast 20], [50, 50] )
            , ( "cells below their maximum grow to share available space"
              , [AtMost 60, Fill], [50, 50] )
            , ( "cells are not shrunk below their required size"
              , [Exactly 50, Exactly 80], [50, 80] )
            , ( "cells within their size range grow to fill available space"
              , [Between 10 90, Fill], [55, 45] )
            ]
      forM_ cases $ \(desc, constraints, expectedWidths) ->
        it desc $
          map rectWidth (hBoxLayout r 0 constraints) `shouldBe` expectedWidths

  describe "vBoxLayout" $ do
    it "single cell occupies the full bounds" $
      vBoxLayout r 0 [Fill] `shouldBe` [Rectangle 0 0 100 50]

    it "two cells split the height equally with no spacing" $
      vBoxLayout r 0 [Fill, Fill] `shouldBe`
        [Rectangle 0 0 100 25, Rectangle 0 25 100 25]

    it "gaps between cells reduce the space available for cell heights" $
      vBoxLayout (Rectangle 0 0 100 60) 10 [Fill, Fill] `shouldBe`
        [Rectangle 0 0 100 25, Rectangle 0 35 100 25]

    it "each cell is offset by the heights of preceding cells and gaps" $
      vBoxLayout (Rectangle 0 0 100 160) 5 [Fill, Fill, Fill] `shouldBe`
        [Rectangle 0 0 100 50, Rectangle 0 55 100 50, Rectangle 0 110 100 50]

    it "all cells share the horizontal position and width of the bounds" $
      vBoxLayout (Rectangle 20 10 100 50) 0 [Fill, Fill] `shouldBe`
        [Rectangle 20 10 100 25, Rectangle 20 35 100 25]

    describe "constraint resolution" $ do
      let cases =
            [ ( "fixed-size cells get their exact sizes"
              , [Exactly 10, Exactly 40], [10, 40] )
            , ( "fill cell takes all space not claimed by fixed-size cells"
              , [Exactly 20, Fill], [20, 30] )
            , ( "fill cells divide all available space equally"
              , [Fill, Fill], [25, 25] )
            , ( "minimum-size cells grow equally to fill available space"
              , [AtLeast 10, Fill], [30, 20] )
            , ( "cells are not shrunk below their required size"
              , [Exactly 30, Exactly 40], [30, 40] )
            ]
      forM_ cases $ \(desc, constraints, expectedHeights) ->
        it desc $
          map rectHeight (vBoxLayout r 0 constraints) `shouldBe` expectedHeights
