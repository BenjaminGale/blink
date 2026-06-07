module Blink.GeometrySpec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import Blink.Geometry
  ( Alignment (..)
  , Insets (..)
  , Point (..)
  , Rectangle (..)
  , Size (..)
  , alignRect
  , containsPoint
  , insetRect
  , rectCentredAt
  , rectOrigin
  , resizeRect
  , uniform
  )

spec :: Spec
spec = describe "geometry" $ do
  describe "uniform" $ do
    it "produces insets with all four sides equal to the given value" $
      uniform 10 `shouldBe` Insets 10 10 10 10

    it "produces all-zero insets for a zero input" $
      uniform 0 `shouldBe` Insets 0 0 0 0

  describe "insetRect" $ do
    it "shrinks all sides equally when given uniform insets" $
      insetRect (uniform 10) (Rectangle 0 0 100 50)
        `shouldBe` Rectangle 10 10 80 30

    it "applies each edge's inset independently" $
      insetRect (Insets 5 10 15 20) (Rectangle 0 0 100 60)
        `shouldBe` Rectangle 20 5 70 40

    it "clamps width to zero when horizontal insets exceed the rectangle width" $
      rectWidth (insetRect (uniform 60) (Rectangle 0 0 100 200)) `shouldBe` 0

    it "clamps height to zero when vertical insets exceed the rectangle height" $
      rectHeight (insetRect (uniform 60) (Rectangle 0 0 200 100)) `shouldBe` 0

  describe "rectOrigin" $ do
    it "is positioned at the origin with zero size" $
      rectOrigin `shouldBe` Rectangle 0 0 0 0

  describe "resizeRect" $ do
    it "sets the new dimensions while preserving the position" $
      resizeRect (Size 200 100) (Rectangle 10 20 50 50)
        `shouldBe` Rectangle 10 20 200 100

    it "preserves the position when resized to zero dimensions" $
      resizeRect (Size 0 0) (Rectangle 10 20 50 50)
        `shouldBe` Rectangle 10 20 0 0

  describe "rectCentredAt" $ do
    it "produces a rectangle centred at the given point with the given size" $
      rectCentredAt (Point 50 40) (Size 20 10)
        `shouldBe` Rectangle 40 35 20 10

    it "correctly centres when the resulting origin has negative coordinates" $
      rectCentredAt (Point 0 0) (Size 10 6)
        `shouldBe` Rectangle (-5) (-3) 10 6

  describe "containsPoint" $ do
    let testRect = Rectangle 10 20 80 60

    let interiorAndBoundaryPoints =
          [ ("a point strictly inside", Point 50 50)
          , ("a point on the left edge", Point 10 50)
          , ("a point on the right edge", Point 90 50)
          , ("a point on the top edge", Point 50 20)
          , ("a point on the bottom edge", Point 50 80)
          , ("the top-left corner", Point 10 20)
          , ("the bottom-right corner", Point 90 80)
          ]

    let exteriorPoints =
          [ ("a point just outside the left edge", Point 9 50)
          , ("a point just outside the right edge", Point 91 50)
          , ("a point just above the top edge", Point 50 19)
          , ("a point just below the bottom edge", Point 50 81)
          ]

    forM_ interiorAndBoundaryPoints $ \(desc, pt) ->
      it ("contains " <> desc) $
        containsPoint testRect pt `shouldBe` True

    forM_ exteriorPoints $ \(desc, pt) ->
      it ("does not contain " <> desc) $
        containsPoint testRect pt `shouldBe` False

  describe "alignRect" $ do
    let container = Rectangle 10 20 100 60
        size = Size 40 20

    let alignmentCases =
          [ ("TopLeft places the rectangle at the top-left of the container", TopLeft, Rectangle 10 20 40 20)
          , ("TopCenter centres the rectangle horizontally at the top", TopCenter, Rectangle 40 20 40 20)
          , ("TopRight places the rectangle at the top-right of the container", TopRight, Rectangle 70 20 40 20)
          , ("MiddleLeft places the rectangle at the left, centred vertically", MiddleLeft, Rectangle 10 40 40 20)
          , ("Center centres the rectangle horizontally and vertically", Center, Rectangle 40 40 40 20)
          , ("MiddleRight places the rectangle at the right, centred vertically", MiddleRight, Rectangle 70 40 40 20)
          , ("BottomLeft places the rectangle at the bottom-left of the container", BottomLeft, Rectangle 10 60 40 20)
          , ("BottomCenter centres the rectangle horizontally at the bottom", BottomCenter, Rectangle 40 60 40 20)
          , ("BottomRight places the rectangle at the bottom-right of the container", BottomRight, Rectangle 70 60 40 20)
          ]

    forM_ alignmentCases $ \(desc, alignment, expected) ->
      it desc $
        alignRect alignment container size `shouldBe` expected

    it "preserves the given size for all alignments" $
      let allAlignments = [minBound .. maxBound] :: [Alignment]
          results = map (\a -> alignRect a container size) allAlignments
      in all (\rect -> rectWidth rect == sizeWidth size && rectHeight rect == sizeHeight size) results
           `shouldBe` True
