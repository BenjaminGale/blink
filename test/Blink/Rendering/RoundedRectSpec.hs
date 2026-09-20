module Blink.Rendering.RoundedRectSpec (spec) where

import Test.Hspec

import Blink.Geometry (Rectangle (..), CornerRadii (..), EdgeVisibility (..), allEdgesVisible, uniformRadii)
import Blink.Rendering.RoundedRect (RingVertex (..), ringMesh)

r :: Rectangle
r = Rectangle 0 0 20 20

-- | Only the top-left corner rounded, isolating that corner's own mesh
-- from the other three corners' (empty, at radius 0) and the straight
-- edges.
topLeftOnly :: Double -> CornerRadii
topLeftOnly radius = (uniformRadii 0) { radiusTopLeft = radius }

-- | No edges visible, isolating a corner's own mesh from the straight
-- edge bands 'ringMesh' would otherwise add alongside it (edge
-- visibility has no bearing on the corner meshes themselves).
noEdges :: EdgeVisibility
noEdges = EdgeVisibility False False False False

-- | The triangles a mesh's flat index list describes, resolved to their
-- three actual vertices, three indices at a time.
triangles :: ([RingVertex], [Int]) -> [(RingVertex, RingVertex, RingVertex)]
triangles (verts, idx) = go idx
  where
    at i = verts !! i
    go (a : b : c : rest) = (at a, at b, at c) : go rest
    go _                  = []

-- | A triangle's area via the shoelace formula, independent of the
-- mesh-building code under test.
triangleArea :: RingVertex -> RingVertex -> RingVertex -> Double
triangleArea a b c =
  abs ((ringVertexX b - ringVertexX a) * (ringVertexY c - ringVertexY a)
     - (ringVertexX c - ringVertexX a) * (ringVertexY b - ringVertexY a)) / 2

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))

-- | Fully covered ("coverage 1") vertices are the ones that actually
-- describe the ring's shape; feathered ("coverage 0") vertices exist
-- only so the rasterizer can interpolate a fade between the two.
onlyCoverage :: Double -> [RingVertex] -> [RingVertex]
onlyCoverage c = filter ((== c) . ringVertexCoverage)

-- | Total area of every triangle whose three vertices are all fully
-- covered -- the solid part of the mesh a viewer actually sees as
-- opaque, independent of how many feather bands surround it.
solidArea :: ([RingVertex], [Int]) -> Double
solidArea mesh = sum
  [ triangleArea a b c
  | (a, b, c) <- triangles mesh
  , all ((== 1) . ringVertexCoverage) [a, b, c]
  ]

spec :: Spec
spec = describe "Blink.Rendering.RoundedRect" $ do
  describe "ringMesh" $ do
    it "gives a straight edge (radius 0) a solid area matching (thickness - featherWidth) x length -- the feather straddles the true edge rather than padding outside its full thickness" $ do
      let mesh = ringMesh r (uniformRadii 0) 2 allEdgesVisible
          -- Four edges of length 20, each with a 1px-narrower-than-thickness solid core: 4 * 20 * (2 - 1).
          expected = 4 * 20 * 1
      abs (solidArea mesh - expected) / expected `shouldSatisfy` (< 1e-9)

    it "omits a hidden edge's contribution to the solid area" $ do
      let full   = solidArea (ringMesh r (uniformRadii 0) 2 allEdgesVisible)
          hidden = solidArea (ringMesh r (uniformRadii 0) 2 (allEdgesVisible { edgeBottomVisible = False }))
      abs (full - hidden - 20 * 1) / (20 * 1) `shouldSatisfy` (< 1e-9)

    it "feathers a straight edge symmetrically: a coverage-0 vertex sits exactly as far outside the true edge as a coverage-1 one sits inside it" $ do
      let (verts, _) = ringMesh r (uniformRadii 0) 2 (EdgeVisibility True False False False)
          topEdgeYs c = [ ringVertexY v | v <- verts, ringVertexCoverage v == c ]
      -- True outer edge is y=0: the coverage-1 sample should be at +0.5, the
      -- coverage-0 sample at -0.5 -- equidistant from the true boundary.
      minimum (topEdgeYs 0) `shouldBe` (-0.5)
      minimum (topEdgeYs 1) `shouldBe` 0.5

    it "produces no geometry at all when the layer has no thickness" $
      ringMesh r (uniformRadii 5) 0 allEdgesVisible `shouldBe` ([], [])

    describe "a rounded corner (radius 10, thickness 10, fully solid, no inner hole)" $ do
      let radius = 10
          mesh@(verts, _) = ringMesh r (topLeftOnly radius) radius noEdges
          center = (radius, radius)
          fullyCovered = onlyCoverage 1 verts
          feathered    = onlyCoverage 0 verts

      it "every fully-covered vertex sits at the centre or half a pixel inside the outer circle" $
        all (\v -> let d = distance center (ringVertexX v, ringVertexY v)
                   in d < 1e-9 || abs (d - (radius - 0.5)) < 1e-9) fullyCovered
          `shouldBe` True

      it "has feathered vertices, each half a pixel outside the outer circle" $ do
        feathered `shouldNotSatisfy` null
        all (\v -> abs (distance center (ringVertexX v, ringVertexY v) - (radius + 0.5)) < 1e-9) feathered
          `shouldBe` True

      it "meets the (equally feathered) straight top edge at exactly the same point, so there's no gap or overlap at the seam" $ do
        -- The corner's own outer-core ring reaches the tangent (local (radius, 0))
        -- at exactly the top edge's own outer-core offset (see 'bandOffsets'),
        -- since both are the same true boundary (y = rectY r) feathered the same way.
        let tangentPt = (rectX r + radius, rectY r + 0.5)
            cornerHasPoint = any (\v -> distance (ringVertexX v, ringVertexY v) tangentPt < 1e-9 && ringVertexCoverage v == 1) fullyCovered
        cornerHasPoint `shouldBe` True

      it "the solid, fully-covered triangles cover approximately a quarter circle's worth of area" $ do
        let expected = pi * (radius - 0.5) * (radius - 0.5) / 4
        abs (solidArea mesh - expected) / expected `shouldSatisfy` (< 0.02)

    describe "a corner thinner than its radius (radius 10, thickness 4, leaves an inner hole)" $ do
      let radius = 10
          thickness = 4
          innerRadius = radius - thickness
          mesh@(verts, _) = ringMesh r (topLeftOnly radius) thickness noEdges
          center = (radius, radius)
          fullyCovered = onlyCoverage 1 verts

      it "every fully-covered vertex sits half a pixel inside the outer circle or half a pixel outside the inner one" $
        all (\v -> let d = distance center (ringVertexX v, ringVertexY v)
                   in abs (d - (radius - 0.5)) < 1e-9 || abs (d - (innerRadius + 0.5)) < 1e-9) fullyCovered
          `shouldBe` True

      it "the ring's core covers approximately the analytic annulus area, shrunk by the feather on each side" $ do
        let outerCoreR = radius - 0.5
            innerCoreR = innerRadius + 0.5
            expected = pi * (outerCoreR * outerCoreR - innerCoreR * innerCoreR) / 4
        abs (solidArea mesh - expected) / expected `shouldSatisfy` (< 0.02)

    describe "mesh validity" $ do
      let mesh@(verts, idx) = ringMesh r (uniformRadii 8) 3 allEdgesVisible

      it "gives every vertex exactly 0 or 1 coverage, since antialiasing comes from interpolating across a triangle edge rather than a fractional vertex" $
        all (\v -> ringVertexCoverage v == 0 || ringVertexCoverage v == 1) verts `shouldBe` True

      it "gives an index list that is whole triangles, all referring to real vertices" $ do
        length idx `mod` 3 `shouldBe` 0
        all (\i -> i >= 0 && i < length verts) idx `shouldBe` True
        length (triangles mesh) `shouldBe` length idx `div` 3
