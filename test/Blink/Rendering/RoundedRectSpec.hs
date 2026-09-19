module Blink.Rendering.RoundedRectSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Test.Hspec

import Blink.Geometry (Rectangle (..), CornerRadii (..), EdgeVisibility (..), allEdgesVisible, uniformRadii)
import Blink.Rendering.RoundedRect (ringRects)

r :: Rectangle
r = Rectangle 0 0 20 20

-- | Only the top-left corner rounded -- isolates that corner's own arc
-- rows from the other three corners' (empty) and the straight edges,
-- which stay inset away from the rounded corner and so never land
-- inside its @radius x radius@ box.
topLeftOnly :: Double -> CornerRadii
topLeftOnly radius = (uniformRadii 0) { radiusTopLeft = radius }

-- | The pieces 'ringRects' produced for the top-left corner's own
-- @radius x radius@ box, ordered top to bottom.
topLeftArc :: Double -> Double -> [(Rectangle, Double)]
topLeftArc radius thickness =
  sortOn (rectY . fst) [ p | p@(rect, _) <- ringRects r (topLeftOnly radius) thickness allEdgesVisible, rectX rect < radius, rectY rect < radius ]

-- | The outer curve's continuous x-offset from the corner's own tip at
-- row @dy@, per the circle equation, sampling at the row's vertical
-- centre (@dy + 0.5@) the same way 'Blink.Rendering.RoundedRect' does --
-- the independent definition the implementation is checked against, not
-- a call into it.
expectedOuterX :: Double -> Int -> Double
expectedOuterX radius dy = radius - sqrt (radius * radius - fromCentre * fromCentre)
  where fromCentre = radius - (fromIntegral dy + 0.5)

-- | The inner curve's continuous x-offset at row @dy@, for a corner
-- whose thickness is thinner than its radius (so an inner circle of
-- @innerRadius@ leaves a hole). Same construction as 'expectedOuterX',
-- against the inner circle instead of the outer one.
expectedInnerX :: Double -> Double -> Int -> Double
expectedInnerX radius innerRadius dy = radius - sqrt (innerRadius * innerRadius - fromCentre * fromCentre)
  where fromCentre = radius - (fromIntegral dy + 0.5)

-- | Total coverage-weighted ring width at each row (summed across that
-- row's pieces, since an antialiased row can split into an outer fringe,
-- a core, and an inner fringe), ordered top to bottom. Weighting by
-- coverage matters here: a fringe pixel's physical width is always 1
-- regardless of how much of it the curve actually covers, so an
-- unweighted sum plateaus at the box edge instead of tracking the true
-- (still-growing) analytic span.
rowWidths :: Double -> Double -> [Double]
rowWidths radius thickness =
  map snd $ Map.toAscList $ Map.fromListWith (+) [ (rectY rect, rectWidth rect * coverage) | (rect, coverage) <- topLeftArc radius thickness ]

spec :: Spec
spec = describe "Blink.Rendering.RoundedRect" $ do
  describe "ringRects" $ do
    it "produces the four full-coverage straight edges and no corner pieces when every radius is 0" $
      ringRects r (uniformRadii 0) 2 allEdgesVisible `shouldMatchList`
        [ (Rectangle 0 0 20 2, 1)   -- top
        , (Rectangle 0 18 20 2, 1)  -- bottom
        , (Rectangle 0 0 2 20, 1)   -- left
        , (Rectangle 18 0 2 20, 1)  -- right
        ]

    it "omits a hidden edge, squared off, when its radius is 0" $
      ringRects r (uniformRadii 0) 2 (allEdgesVisible { edgeBottomVisible = False }) `shouldMatchList`
        [ (Rectangle 0 0 20 2, 1)
        , (Rectangle 0 0 2 20, 1)
        , (Rectangle 18 0 2 20, 1)
        ]

    describe "a rounded corner (radius 10, thickness 10 -- fully solid, no inner hole)" $ do
      it "places the outer fringe pixel's column and coverage per the circle equation, for every row" $ do
        let radius = 10
        mapM_
          (\dy ->
            let outerX = expectedOuterX radius dy
                col    = fromIntegral (floor outerX :: Int)
                covered = col + 1 - outerX
            in topLeftArc radius radius `shouldContain` [(Rectangle col (fromIntegral dy) 1 1, covered)]
          )
          [0 .. floor radius - 1 :: Int]

      it "widens row by row, moving away from the tip toward the straight edge" $
        let widths = rowWidths 10 10
        in zipWith (<) widths (tail widths) `shouldSatisfy` and

      it "never emits a third, inner-hole piece when the layer is thick enough to leave no hole" $
        -- 10 rows (radius 10), each just an outer fringe plus a core,
        -- no inner fringe: 20 pieces.
        length (topLeftArc 10 10) `shouldBe` 20

    describe "a corner thinner than its radius (radius 10, thickness 4 -- leaves an inner hole)" $ do
      let radius = 10
          innerRadius = 6 -- radius - thickness

      it "places the inner fringe pixel's column and coverage per the circle equation, once a row reaches the hole" $
        mapM_
          (\dy ->
            let innerX = expectedInnerX radius innerRadius dy
                col     = fromIntegral (floor innerX :: Int)
                covered = innerX - col
            in topLeftArc radius 4 `shouldContain` [(Rectangle col (fromIntegral dy) 1 1, covered)]
          )
          [4 .. floor radius - 1 :: Int]

      it "emits no inner-hole piece for the rows above the hole" $
        length [ () | (rect, _) <- topLeftArc radius 4, rectY rect < 4 ] `shouldBe` 2 * 4 -- outer fringe + core per row, no inner piece

    describe "antialiasing" $
      it "gives a boundary pixel partial coverage strictly between 0 and 1, not a hard edge" $ do
        let coverages = [ c | (rect, c) <- topLeftArc 10 10, rectWidth rect == 1 ]
        coverages `shouldSatisfy` any (\c -> c > 0 && c < 1)
