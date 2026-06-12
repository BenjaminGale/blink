module Blink.GeometrySpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Blink.Generators ()
import Blink.Geometry
  ( Alignment (..)
  , Insets (..)
  , Point (..)
  , Rectangle (..)
  , Size (..)
  , alignRect
  , containsPoint
  , insetRect
  , intersectRect
  , leftInset
  , rectCentredAt
  , rectFromSize
  , resizeRect
  , topInset
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

    prop "offsets the origin by the left and top insets" $ \ins r ->
      rectX (insetRect ins r) == rectX r + leftInset ins
      && rectY (insetRect ins r) == rectY r + topInset ins

    it "clamps width to zero when horizontal insets exceed the rectangle width" $
      rectWidth (insetRect (uniform 60) (Rectangle 0 0 100 200)) `shouldBe` 0

    it "clamps height to zero when vertical insets exceed the rectangle height" $
      rectHeight (insetRect (uniform 60) (Rectangle 0 0 200 100)) `shouldBe` 0

  describe "rectFromSize" $ do
    it "creates a rectangle at the origin with the given dimensions" $
      rectFromSize (Size 200 100) `shouldBe` Rectangle 0 0 200 100

  describe "resizeRect" $ do
    it "sets the new dimensions while preserving the position" $
      resizeRect (Size 200 100) (Rectangle 10 20 50 50)
        `shouldBe` Rectangle 10 20 200 100

    it "preserves the position when resized to zero dimensions" $
      resizeRect (Size 0 0) (Rectangle 10 20 50 50)
        `shouldBe` Rectangle 10 20 0 0

  describe "rectCentredAt" $ do
    it "produces a rectangle centred at the given point with the given size" $
      rectCentredAt (Point 50 40) (Rectangle 0 0 20 10)
        `shouldBe` Rectangle 40 35 20 10

    it "correctly centres when the resulting origin has negative coordinates" $
      rectCentredAt (Point 0 0) (Rectangle 0 0 10 6)
        `shouldBe` Rectangle (-5) (-3) 10 6

  describe "containsPoint" $ do
    let testRect = Rectangle 10 20 80 60

    it "contains a point strictly inside" $
      containsPoint (Point 50 50) testRect `shouldBe` True

    it "contains a point on the boundary" $
      containsPoint (Point 10 50) testRect `shouldBe` True

    it "does not contain a point outside" $
      containsPoint (Point 9 50) testRect `shouldBe` False

    prop "contains a point if and only if it lies within the inclusive bounds" $ \p r ->
      containsPoint p r ==
        ( pointX p >= rectX r && pointX p <= rectX r + rectWidth r
       && pointY p >= rectY r && pointY p <= rectY r + rectHeight r
        )

    prop "a zero-size rectangle contains only its origin point" $ \p q ->
      containsPoint q (Rectangle (pointX p) (pointY p) 0 0) == (q == p)

  describe "intersectRect" $ do
    it "returns the overlapping region of two partially overlapping rectangles" $
      intersectRect (Rectangle 0 0 10 10) (Rectangle 5 5 10 10)
        `shouldBe` Rectangle 5 5 5 5

    prop "is commutative" $ \a b ->
      intersectRect a b == (intersectRect b a :: Rectangle)

    prop "is idempotent" $ \a ->
      intersectRect a a == (a :: Rectangle)

    prop "always produces non-negative dimensions" $ \a b ->
      let r = intersectRect a b
      in rectWidth r >= 0 && rectHeight r >= (0 :: Double)

    prop "result dimensions do not exceed either input's" $ \a b ->
      let r = intersectRect a b
      in rectWidth r <= rectWidth a && rectWidth r <= rectWidth b
      && rectHeight r <= rectHeight a && rectHeight r <= rectHeight b

  describe "alignRect" $ do
    let container = Rectangle 10 20 100 60
        size = Rectangle 0 0 40 20

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

    prop "preserves the given size regardless of container or alignment" $
      \alignment c r ->
        let result = alignRect alignment c r
        in rectWidth result == rectWidth r && rectHeight result == rectHeight r
