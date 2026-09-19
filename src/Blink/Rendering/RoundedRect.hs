{- |
Module: Blink.Rendering.RoundedRect

Rasterizes a border layer's ring (its stroke, at whatever 'CornerRadii'
and 'EdgeVisibility' it declares) into the axis-aligned rectangles a
backend with no rounded-rect primitive -- SDL2's 'SDL.fillRect'
included -- can fill directly. Each rectangle carries a coverage
fraction alongside it: 1 for an ordinary opaque piece, or a fraction for
a rounded corner's antialiased edge pixel, which the caller blends
rather than rounding to a hard, staircased edge. Kept independent of any
particular graphics backend so it can be unit-tested as plain geometry.
-}
module Blink.Rendering.RoundedRect
  ( ringRects
  ) where

import Blink.Geometry (Rectangle (..), CornerRadii (..), EdgeVisibility (..))

-- | Which of the rectangle's four corners: used only to select that
-- corner's own radius and to mirror a rasterized arc row into the right
-- quadrant.
data Corner = TopLeft | TopRight | BottomRight | BottomLeft

corners :: [Corner]
corners = [TopLeft, TopRight, BottomRight, BottomLeft]

cornerRadius :: Corner -> CornerRadii -> Double
cornerRadius TopLeft     = radiusTopLeft
cornerRadius TopRight    = radiusTopRight
cornerRadius BottomRight = radiusBottomRight
cornerRadius BottomLeft  = radiusBottomLeft

-- | The axis-aligned rectangles that fill a border layer's ring, each
-- paired with its coverage (see the module header): one fully-covered
-- rect per visible straight edge, inset at both ends by its two
-- adjoining corners' radii so it stops where that corner's own arc
-- begins, plus each corner's own rounded arc. A zero-radius corner
-- contributes no arc pieces -- its square corner is already covered by
-- the adjoining straight edges meeting there.
ringRects :: Rectangle -> CornerRadii -> Double -> EdgeVisibility -> [(Rectangle, Double)]
ringRects r radii thickness visible =
  concatMap (\c -> cornerArc c (cornerRadius c radii) r thickness) corners
    ++ [ (rect, 1) | rect <- edgeRects r radii thickness visible ]

-- | One corner's rounded arc: every row's pieces, placed in @r@'s own
-- coordinate frame.
cornerArc :: Corner -> Double -> Rectangle -> Double -> [(Rectangle, Double)]
cornerArc corner radius r thickness
  | radius <= 0 = []
  | otherwise =
      [ (placeRow corner r dy x0 w, coverage)
      | dy <- [0 .. ceiling radius - 1]
      , (x0, w, coverage) <- rowPieces radius thickness dy
      ]

-- | One row's pieces, measured as if the corner were top-left --
-- 'placeRow' mirrors these into the other three quadrants: a fully-
-- covered core span, plus a 1px antialiased fringe piece at the outer
-- curve and, if the layer is thin enough to leave a hole at this row's
-- height, another at the inner curve. Empty once the row falls entirely
-- inside that hole.
rowPieces :: Double -> Double -> Int -> [(Double, Double, Double)]
rowPieces radius thickness dy
  | spanWidth <= 0 = []
  | otherwise =
      [ (outerCol, 1, outerCoverage) | outerCoverage > 0 ]
        ++ [ (coreStart, coreWidth, 1) | coreWidth > 0 ]
        ++ [ (innerCol, 1, innerCoverage) | hasInnerEdge, innerCoverage > 0 ]
  where
    (xStart, spanWidth) = rowSpan radius thickness dy
    xEnd          = xStart + spanWidth
    outerCol      = fromIntegral (floor xStart :: Int)
    outerCoverage = outerCol + 1 - xStart
    coreStart     = outerCol + 1
    hasInnerEdge  = xEnd < radius - 1e-9
    innerCol      = fromIntegral (floor xEnd :: Int)
    innerCoverage = xEnd - innerCol
    coreEnd       = if hasInnerEdge then innerCol else xEnd
    coreWidth     = max 0 (coreEnd - coreStart)

-- | For a corner of the given outer radius and ring thickness, the span
-- (offset from the corner's own tip, width) of the row @dy@ integer
-- pixels in from the outer edge, measured as if the corner were
-- top-left. Samples the circle at the row's vertical centre
-- (@dy + 0.5@), not its top edge, so the curve isn't biased half a
-- pixel off from where it's actually drawn. Zero width once the row
-- falls entirely inside the hole a thickness smaller than the radius
-- leaves at the centre.
rowSpan :: Double -> Double -> Int -> (Double, Double)
rowSpan radius thickness dy = (xStart, max 0 (xEnd - xStart))
  where
    fromCentre  = radius - (fromIntegral dy + 0.5)
    outerReach  = chordHalfWidth radius fromCentre
    innerRadius = radius - thickness
    innerReach  = if innerRadius <= 0 then 0 else chordHalfWidth innerRadius fromCentre
    xStart      = radius - outerReach
    xEnd        = radius - innerReach

-- | Half the width of a circle of the given radius at distance @y@ from
-- its centre, or 0 once @y@ is outside the circle entirely.
chordHalfWidth :: Double -> Double -> Double
chordHalfWidth radius y
  | absY >= radius = 0
  | otherwise       = sqrt (radius * radius - absY * absY)
  where absY = abs y

-- | Places a piece computed as if @corner@ were top-left, at row @dy@ of
-- @corner@'s own arc, in @r@'s coordinate frame.
placeRow :: Corner -> Rectangle -> Int -> Double -> Double -> Rectangle
placeRow corner r dy x0 w = Rectangle gx gy w 1
  where
    gx = case corner of
      TopLeft     -> rectX r + x0
      BottomLeft  -> rectX r + x0
      TopRight    -> rectX r + rectWidth r - x0 - w
      BottomRight -> rectX r + rectWidth r - x0 - w
    gy = case corner of
      TopLeft     -> rectY r + fromIntegral dy
      TopRight    -> rectY r + fromIntegral dy
      BottomLeft  -> rectY r + rectHeight r - fromIntegral (dy + 1)
      BottomRight -> rectY r + rectHeight r - fromIntegral (dy + 1)

-- | The four straight edges between corners, each inset at both ends by
-- its adjoining corners' radii, and omitted where 'EdgeVisibility' hides
-- it or the layer has no thickness.
edgeRects :: Rectangle -> CornerRadii -> Double -> EdgeVisibility -> [Rectangle]
edgeRects r radii t visible =
  concatMap keep
    [ (edgeTopVisible visible,    Rectangle (rectX r + tl) (rectY r) (rectWidth r - tl - tr) t)
    , (edgeBottomVisible visible, Rectangle (rectX r + bl) (rectY r + rectHeight r - t) (rectWidth r - bl - br) t)
    , (edgeLeftVisible visible,   Rectangle (rectX r) (rectY r + tl) t (rectHeight r - tl - bl))
    , (edgeRightVisible visible,  Rectangle (rectX r + rectWidth r - t) (rectY r + tr) t (rectHeight r - tr - br))
    ]
  where
    tl = radiusTopLeft radii
    tr = radiusTopRight radii
    br = radiusBottomRight radii
    bl = radiusBottomLeft radii
    keep (visibleEdge, rect) =
      [ rect | visibleEdge && t > 0 && rectWidth rect > 0 && rectHeight rect > 0 ]
