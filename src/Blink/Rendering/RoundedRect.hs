{- |
Module: Blink.Rendering.RoundedRect

Tessellates a border layer's ring (its stroke, at whatever 'CornerRadii'
and 'EdgeVisibility' it declares) into a triangle mesh a backend can
hand straight to a hardware/software rasterizer that interpolates
per-vertex colour, such as SDL2's @SDL_RenderGeometry@, rather than a
pile of axis-aligned rectangles. Every boundary -- a rounded corner's
arc and a straight edge's own two long sides alike -- is feathered by
@featherWidth@, centred on the boundary's true position (half the
feather inside it, half outside), fading to zero coverage; the
rasterizer's own interpolation between a coverage-0 and a coverage-1
vertex then antialiases the whole ring uniformly, rather than a rounded
corner alone. Every vertex carries a coverage fraction (1 inside the
ring, 0 at a feather edge) for the caller to fold into its colour's
alpha, and the whole layer collapses into one combined vertex\/index
buffer so a caller can draw it with a single draw call. Kept
independent of any particular graphics backend so it can be
unit-tested as plain geometry.
-}
module Blink.Rendering.RoundedRect
  ( RingVertex (..)
  , ringMesh
  ) where

import Blink.Geometry (Rectangle (..), CornerRadii (..), EdgeVisibility (..))

-- | A vertex of a border layer's ring mesh: a position in screen space
-- paired with a coverage fraction (1 = fully inside the ring, 0 = fully
-- outside), for the caller to fold into its colour's alpha channel so a
-- rasterizer interpolating between a coverage-0 and a coverage-1 vertex
-- antialiases that edge automatically.
data RingVertex = RingVertex
  { ringVertexX :: Double
  , ringVertexY :: Double
  , ringVertexCoverage :: Double
  } deriving (Eq, Show)

-- | Which of the rectangle's four corners: used only to select that
-- corner's own radius and to mirror its (canonically top-left) arc into
-- the right quadrant.
data Corner = TopLeft | TopRight | BottomRight | BottomLeft

corners :: [Corner]
corners = [TopLeft, TopRight, BottomRight, BottomLeft]

cornerRadius :: Corner -> CornerRadii -> Double
cornerRadius TopLeft     = radiusTopLeft
cornerRadius TopRight    = radiusTopRight
cornerRadius BottomRight = radiusBottomRight
cornerRadius BottomLeft  = radiusBottomLeft

-- | Total width, in pixels, of the antialiased transition at a
-- boundary -- split half inside it and half outside (see 'bandOffsets')
-- so a straight run's edge softens the same way a rounded corner's arc
-- does, rather than a rounded corner alone growing a fringe outside its
-- true curve while an adjoining straight edge stays a hard, un-feathered
-- line (which reads as the corner being measurably thicker).
featherWidth :: Double
featherWidth = 1

-- | The triangle mesh (vertices, plus a flat list of indices taken
-- three at a time as one triangle) that fills a border layer's ring:
-- one feathered wedge per corner (omitted where that corner's radius or
-- the layer's thickness is 0, since a square corner is already covered
-- by its adjoining straight edges) plus one feathered band per visible
-- straight edge, all in one combined buffer.
ringMesh :: Rectangle -> CornerRadii -> Double -> EdgeVisibility -> ([RingVertex], [Int])
ringMesh r radii thickness visible = appendMeshes (cornerMeshes ++ edgeMeshes)
  where
    cornerMeshes = [ cornerWedge c (cornerRadius c radii) r thickness | c <- corners ]
    edgeMeshes   = [ edgeMesh edge | edge <- edgeSpans r radii thickness visible ]

-- | A point in a corner's own local frame (as if it were top-left, tip
-- at the origin), paired with its coverage.
type LocalPt = (Double, Double, Double)

-- | Given a boundary's true perpendicular position (@outerTrue@,
-- @innerTrue@ -- the far side of the stroke from the hole, and the
-- near side) returns, in that same order, the four perpendicular
-- positions to sample coverage at: feathered fully outside (0), the
-- boundary's own inward half-feather (1), the inner boundary's own
-- outward half-feather (1), and feathered fully into the hole (0).
-- Clamps the two inner samples to the stroke's own midpoint if
-- @featherWidth@ would otherwise make them cross -- a stroke thinner
-- than the feather still tapers smoothly rather than inverting.
bandOffsets :: Double -> Double -> (Double, Double, Double, Double)
bandOffsets outerTrue innerTrue =
  (featherOut, clampOuterSide outerCore, clampInnerSide innerCore, featherIn)
  where
    s = signum (outerTrue - innerTrue)
    half = featherWidth / 2
    featherOut = outerTrue + s * half
    outerCore  = outerTrue - s * half
    innerCore  = innerTrue + s * half
    featherIn  = innerTrue - s * half
    mid = (outerTrue + innerTrue) / 2
    -- outerCore must stay between outerTrue and mid.
    clampOuterSide v
      | s >= 0    = if v < mid then mid else v
      | otherwise = if v > mid then mid else v
    -- innerCore must stay between innerTrue and mid -- the opposite side.
    clampInnerSide v
      | s >= 0    = if v > mid then mid else v
      | otherwise = if v < mid then mid else v

-- | One rounded corner's mesh, placed into @r@'s coordinate frame.
--
-- A corner's outer and inner boundaries are concentric circles of
-- radius @radius@ and @radius - thickness@, both centred at the corner
-- box's far corner (@(radius, radius)@ in the corner's own local
-- frame), so the arc sampled at radius @radius@ meets the two straight
-- edges exactly where 'edgeSpans' starts them, and a smaller
-- concentric circle traces the inner edge of a hollow ring. When the
-- layer is thick enough to leave no hole (@thickness >= radius@), the
-- shape degenerates to a simple fan from that centre point instead of
-- an annulus strip.
cornerWedge :: Corner -> Double -> Rectangle -> Double -> ([RingVertex], [Int])
cornerWedge _ radius _ thickness
  | radius <= 0 || thickness <= 0 = ([], [])
cornerWedge corner radius r thickness = placeMesh corner r localMesh
  where
    segs = arcSegments radius
    innerRadius = radius - thickness
    (featherOutR, outerCoreR, _, _) = bandOffsets radius innerRadius
    outerCoreRing  = arcRing radius outerCoreR 1 segs
    featherOutRing = arcRing radius featherOutR 0 segs
    localMesh
      | innerRadius <= 0 =
          combineLocalMeshes
            [ fanMesh (radius, radius, 1) outerCoreRing
            , stripMesh outerCoreRing featherOutRing
            ]
      | otherwise =
          let (_, _, innerCoreR, featherInR) = bandOffsets radius innerRadius
              innerCoreRing = arcRing radius innerCoreR 1 segs
              featherInRing = arcRing radius (max 0 featherInR) 0 segs
          in combineLocalMeshes
               [ stripMesh featherInRing innerCoreRing
               , stripMesh innerCoreRing outerCoreRing
               , stripMesh outerCoreRing featherOutRing
               ]

-- | Number of straight segments to sample a corner's quarter-circle arc
-- into -- roughly one per pixel of radius, bounded so a tiny corner
-- isn't over-tessellated and a huge one doesn't blow up the triangle
-- count.
arcSegments :: Double -> Int
arcSegments radius = max 6 (min 64 (ceiling radius))

-- | @segs + 1@ points sampling the quarter circle of radius @rho@
-- centred at @(outerR, outerR)@ (the corner's own far corner, in its
-- local frame) from where it meets the leading straight edge to where
-- it meets the trailing one, all at the given coverage.
arcRing :: Double -> Double -> Double -> Int -> [LocalPt]
arcRing outerR rho coverage segs =
  [ (outerR + rho * cos a, outerR + rho * sin a, coverage)
  | i <- [0 .. segs]
  , let a = pi + (pi / 2) * (fromIntegral i / fromIntegral segs)
  ]

-- | A triangle fan from @apex@ to every consecutive pair of @ring@'s
-- points.
fanMesh :: LocalPt -> [LocalPt] -> ([LocalPt], [Int])
fanMesh apex ring = (apex : ring, indices)
  where
    n = length ring
    indices = concat [ [0, i, i + 1] | i <- [1 .. n - 1] ]

-- | A triangle strip filling the band between two same-length rings,
-- @inner@ then @outer@ (by index, not necessarily by radius -- callers
-- pass whichever pair bounds the band).
stripMesh :: [LocalPt] -> [LocalPt] -> ([LocalPt], [Int])
stripMesh innerPts outerPts = (innerPts ++ outerPts, indices)
  where
    n = length innerPts
    indices = concat
      [ [i, i + 1, n + i + 1, i, n + i + 1, n + i]
      | i <- [0 .. n - 2]
      ]

-- | Combines local sub-meshes into one, offsetting each's indices past
-- the vertices already accumulated.
combineLocalMeshes :: [([LocalPt], [Int])] -> ([LocalPt], [Int])
combineLocalMeshes = foldr combine ([], [])
  where
    combine (vs, idx) (accV, accI) = (vs ++ accV, idx ++ map (+ length vs) accI)

-- | Places a mesh computed as if @corner@ were top-left into @r@'s
-- coordinate frame, mirroring horizontally and\/or vertically as
-- 'placePoint' does for a single point.
placeMesh :: Corner -> Rectangle -> ([LocalPt], [Int]) -> ([RingVertex], [Int])
placeMesh corner r (pts, idx) = (map toVertex pts, idx)
  where
    toVertex (x, y, c) = let (gx, gy) = placePoint corner r (x, y) in RingVertex gx gy c

-- | Maps a point computed as if @corner@ were top-left into @r@'s
-- coordinate frame.
placePoint :: Corner -> Rectangle -> (Double, Double) -> (Double, Double)
placePoint corner r (x, y) = (gx, gy)
  where
    gx = case corner of
      TopLeft     -> rectX r + x
      BottomLeft  -> rectX r + x
      TopRight    -> rectX r + rectWidth r - x
      BottomRight -> rectX r + rectWidth r - x
    gy = case corner of
      TopLeft     -> rectY r + y
      TopRight    -> rectY r + y
      BottomLeft  -> rectY r + rectHeight r - y
      BottomRight -> rectY r + rectHeight r - y

-- | One straight edge's span: the true outer and inner perpendicular
-- offsets (see 'bandOffsets'), which axis is perpendicular to the edge,
-- and the range along the edge's own length (unfeathered -- an edge's
-- two ends meet a corner or another edge exactly, needing no fade).
data EdgeSpan = EdgeSpan
  { edgeAxis        :: Axis
  , edgeOuterTrue   :: Double
  , edgeInnerTrue   :: Double
  , edgeSpanStart   :: Double
  , edgeSpanEnd     :: Double
  }

data Axis = PerpendicularX | PerpendicularY

-- | The feathered band mesh for one straight edge: 'bandOffsets'
-- applied perpendicular to the edge, each of the three resulting
-- (coverage-paired) bands spanning its full, unfeathered length.
edgeMesh :: EdgeSpan -> ([RingVertex], [Int])
edgeMesh edge = appendMeshes [ bandMesh a b, bandMesh b c, bandMesh c d ]
  where
    (featherOut, outerCore, innerCore, featherIn) = bandOffsets (edgeOuterTrue edge) (edgeInnerTrue edge)
    a = (featherOut, 0)
    b = (outerCore, 1)
    c = (innerCore, 1)
    d = (featherIn, 0)
    bandMesh (p1, cov1) (p2, cov2) = quadMesh (perpRect p1 cov1 p2 cov2)
    perpRect p1 cov1 p2 cov2 = case edgeAxis edge of
      PerpendicularY ->
        ( (edgeSpanStart edge, p1, cov1), (edgeSpanEnd edge, p1, cov1)
        , (edgeSpanEnd edge, p2, cov2), (edgeSpanStart edge, p2, cov2)
        )
      PerpendicularX ->
        ( (p1, edgeSpanStart edge, cov1), (p1, edgeSpanEnd edge, cov1)
        , (p2, edgeSpanEnd edge, cov2), (p2, edgeSpanStart edge, cov2)
        )

-- | A quad from four corner points, each paired with its own coverage
-- (so a band between a coverage-0 and a coverage-1 side fades across
-- it), wound consistently.
quadMesh :: (LocalPt, LocalPt, LocalPt, LocalPt) -> ([RingVertex], [Int])
quadMesh ((x0, y0, c0), (x1, y1, c1), (x2, y2, c2), (x3, y3, c3)) =
  ( [ RingVertex x0 y0 c0
    , RingVertex x1 y1 c1
    , RingVertex x2 y2 c2
    , RingVertex x3 y3 c3
    ]
  , [0, 1, 2, 0, 2, 3]
  )

-- | Combines already-placed sub-meshes into one, offsetting each's
-- indices past the vertices already accumulated -- used to merge every
-- corner's and edge's mesh into the one buffer 'ringMesh' returns.
appendMeshes :: [([RingVertex], [Int])] -> ([RingVertex], [Int])
appendMeshes = foldr combine ([], [])
  where
    combine (vs, idx) (accV, accI) = (vs ++ accV, idx ++ map (+ length vs) accI)

-- | The four straight edges between corners, each inset at both ends by
-- its adjoining corners' radii, and omitted where 'EdgeVisibility' hides
-- it or the layer has no thickness.
edgeSpans :: Rectangle -> CornerRadii -> Double -> EdgeVisibility -> [EdgeSpan]
edgeSpans r radii t visible =
  concatMap keep
    [ (edgeTopVisible visible,    EdgeSpan PerpendicularY (rectY r) (rectY r + t) (rectX r + tl) (rectX r + rectWidth r - tr))
    , (edgeBottomVisible visible, EdgeSpan PerpendicularY (rectY r + rectHeight r) (rectY r + rectHeight r - t) (rectX r + bl) (rectX r + rectWidth r - br))
    , (edgeLeftVisible visible,   EdgeSpan PerpendicularX (rectX r) (rectX r + t) (rectY r + tl) (rectY r + rectHeight r - bl))
    , (edgeRightVisible visible,  EdgeSpan PerpendicularX (rectX r + rectWidth r) (rectX r + rectWidth r - t) (rectY r + tr) (rectY r + rectHeight r - br))
    ]
  where
    tl = radiusTopLeft radii
    tr = radiusTopRight radii
    br = radiusBottomRight radii
    bl = radiusBottomLeft radii
    keep (visibleEdge, edge) =
      [ edge | visibleEdge && t > 0 && edgeSpanEnd edge > edgeSpanStart edge ]
