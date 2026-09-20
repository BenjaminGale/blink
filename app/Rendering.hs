{-# LANGUAGE OverloadedStrings #-}
module Rendering
  ( TextureCache
  , newTextureCache
  , freeTextureCache
  , ImageCache
  , newImageCache
  , freeImageCache
  , submitDrawCommand
  , mkTextMeasurer
  , mkImageMeasurer
  ) where

import Blink
import SDL (($=))
import qualified SDL
import qualified SDL.Raw as Raw
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt)

-- | Entries a 'BoundedCache' holds before it evicts its
-- least-recently-used ones -- caches keyed by arbitrary rendered text
-- (glyph textures, glyph offsets) would otherwise grow without bound as
-- an app displays new strings (timestamps, counters, live data) over its
-- lifetime.
maxCacheEntries :: Int
maxCacheEntries = 512

-- | Entries evicted at once when a 'BoundedCache' exceeds
-- 'maxCacheEntries'. Evicting a batch rather than exactly one entry per
-- insert means the eviction sort below runs rarely rather than on every
-- insert once the cache is full.
evictBatchSize :: Int
evictBatchSize = maxCacheEntries `div` 4

-- | A size-bounded, least-recently-used cache: a plain 'Map' paired with
-- a monotonic tick, each entry stamped with the tick it was last looked
-- up or created at. Untouched below 'maxCacheEntries', so a hit stays a
-- map lookup plus a stamp update -- see 'cacheGetOrCreate'.
type BoundedCache k v = IORef (Map k (v, Word64), Word64)

newBoundedCache :: IO (BoundedCache k v)
newBoundedCache = newIORef (Map.empty, 0)

-- | Runs @onDestroy@ (e.g. 'SDL.destroyTexture') on every value still
-- held, for use at app shutdown.
freeBoundedCache :: BoundedCache k v -> (v -> IO ()) -> IO ()
freeBoundedCache cache onDestroy = do
  (m, _) <- readIORef cache
  mapM_ (onDestroy . fst) (Map.elems m)

-- | Returns the cached value at @key@, bumping its recency, or runs
-- @create@ to make one on a miss. Once the cache exceeds
-- 'maxCacheEntries', evicts the 'evictBatchSize' least-recently-used
-- entries (running @onEvict@ on each, e.g. to destroy its texture).
cacheGetOrCreate :: Ord k => BoundedCache k v -> (v -> IO ()) -> k -> IO v -> IO v
cacheGetOrCreate cache onEvict cacheKey create = do
  (m, tick) <- readIORef cache
  case Map.lookup cacheKey m of
    Just (v, _) -> do
      writeIORef cache (Map.insert cacheKey (v, tick) m, tick + 1)
      pure v
    Nothing -> do
      v <- create
      let m' = Map.insert cacheKey (v, tick) m
      m'' <- if Map.size m' > maxCacheEntries
        then evictLRU onEvict m'
        else pure m'
      writeIORef cache (m'', tick + 1)
      pure v

-- | Drops the 'evictBatchSize' entries with the oldest recency stamp,
-- running @onEvict@ on each of their values first.
evictLRU :: Ord k => (v -> IO ()) -> Map k (v, Word64) -> IO (Map k (v, Word64))
evictLRU onEvict m = do
  let (toEvict, toKeep) = splitAt evictBatchSize (sortOn (snd . snd) (Map.toList m))
  mapM_ (onEvict . fst . snd) toEvict
  pure (Map.fromList toKeep)

type TextureCache = BoundedCache (Text, SDL.V4 Word8) (SDL.Texture, CInt, CInt)

destroyTextureEntry :: (SDL.Texture, CInt, CInt) -> IO ()
destroyTextureEntry (t, _, _) = SDL.destroyTexture t

newTextureCache :: IO TextureCache
newTextureCache = newBoundedCache

freeTextureCache :: TextureCache -> IO ()
freeTextureCache cache = freeBoundedCache cache destroyTextureEntry

-- | 'NaturalImage' is keyed by path alone -- the image loaded at whatever
-- size its own source declares, used for 'mkImageMeasurer' and for
-- drawing any non-@.svg@ source (already a fixed-resolution raster, so
-- there's no different "size" to rasterize it at). 'SizedImage' is an
-- @.svg@ source re-rasterized at a specific pixel size (see
-- 'loadSizedImageTexture'), so it stays crisp when displayed above its
-- own natural size instead of stretching one small raster.
data ImageCacheKey
  = NaturalImage ImagePath
  | SizedImage ImagePath CInt CInt
  deriving (Eq, Ord)

type ImageCache = BoundedCache ImageCacheKey (SDL.Texture, CInt, CInt)

newImageCache :: IO ImageCache
newImageCache = newBoundedCache

freeImageCache :: ImageCache -> IO ()
freeImageCache cache = freeBoundedCache cache destroyTextureEntry

-- | Loads the image at @path@ into a texture at its own natural size and
-- caches it, or returns the cached texture from an earlier call -- used
-- by 'mkImageMeasurer' (which only wants the size) and as the draw-time
-- texture for any source 'loadSizedImageTexture' doesn't specialize.
loadImageTexture :: SDL.Renderer -> ImageCache -> ImagePath -> IO (SDL.Texture, CInt, CInt)
loadImageTexture renderer cache path =
  cacheGetOrCreate cache destroyTextureEntry (NaturalImage path) $ do
    tex <- Image.loadTexture renderer (T.unpack path)
    (SDL.TextureInfo _ _ w h) <- SDL.queryTexture tex
    pure (tex, w, h)

-- | 'True' for a path whose extension is @.svg@ (case-insensitive).
isSvgPath :: ImagePath -> Bool
isSvgPath path = T.toLower (T.takeWhileEnd (/= '.') path) == "svg"

-- | Removes any existing @attr="..."@ (or @attr='...'@) occurrence, so a
-- fresh value can be inserted without leaving a stale duplicate attribute
-- behind. Leaves @t@ unchanged if @attr=@ isn't followed by a quoted
-- value at all (a malformed document isn't this function's problem to
-- solve).
stripAttr :: Text -> Text -> Text
stripAttr attr t = case T.breakOn (attr <> "=") t of
  (_, rest) | T.null rest -> t
  (before, rest) ->
    let afterEq = T.drop (T.length attr + 1) rest
    in case T.uncons afterEq of
      Nothing -> t
      Just (quoteChar, afterOpen) ->
        case T.breakOn (T.singleton quoteChar) afterOpen of
          (_, closing) | T.null closing -> t
          (_, closing) -> before <> T.drop 1 closing

-- | Overrides an SVG document's declared width\/height so SDL2_image
-- rasterizes it at exactly @(w, h)@ pixels, rather than whatever the
-- source itself declares (most often its @viewBox@, which can be far
-- smaller than any size it's actually displayed at); also forces its
-- root @fill@ to white, so a source whose paths don't set their own
-- @fill@ (as Blink's own bundled icons don't) renders as a plain white
-- silhouette rather than the SVG-default black -- see 'renderImage' for
-- why that's what makes tinting via 'SDL.textureColorMod' possible at
-- all. A source with its own explicit per-path @fill@ (a multi-colour
-- illustration, say) is unaffected, since an explicit @fill@ always
-- wins over one merely inherited from the root.
sizedSvgSource :: CInt -> CInt -> Text -> Text
sizedSvgSource w h = insertAttrs . stripAttr "fill" . stripAttr "height" . stripAttr "width"
  where
    insertAttrs t = case T.breakOn "<svg" t of
      (before, rest) | not (T.null rest) ->
        before <> "<svg width=\"" <> T.pack (show w) <> "\" height=\"" <> T.pack (show h)
          <> "\" fill=\"white\"" <> T.drop 4 rest
      _ -> t

-- | Re-rasterizes the @.svg@ at @path@ at exactly @(w, h)@ pixels and
-- caches it, or returns the cached texture from an earlier call at the
-- same size. Falls back to 'loadImageTexture' (the source's own natural
-- size) for any non-@.svg@ path, since re-decoding a fixed-resolution
-- raster format at a different size wouldn't change its pixels.
loadSizedImageTexture :: SDL.Renderer -> ImageCache -> ImagePath -> (CInt, CInt) -> IO SDL.Texture
loadSizedImageTexture renderer cache path (w, h)
  | not (isSvgPath path) = (\(tex, _, _) -> tex) <$> loadImageTexture renderer cache path
  | otherwise = do
      (tex, _, _) <- cacheGetOrCreate cache destroyTextureEntry (SizedImage path w h) $ do
        src <- TIO.readFile (T.unpack path)
        tex <- Image.decodeTexture renderer (TE.encodeUtf8 (sizedSvgSource w h src))
        pure (tex, w, h)
      pure tex

toWord8 :: Double -> Word8
toWord8 c = round (c * 255)

-- | @colour@'s own alpha, scaled by @coverage@ -- lets a caller draw a
-- partially-covered pixel (e.g. a rounded border corner's antialiased
-- fringe) by blending rather than rounding it to a hard edge. Requires
-- the renderer's own blend mode to actually be alpha blending, set once
-- at startup.
toSDLColorWithCoverage :: Double -> Colour -> SDL.V4 Word8
toSDLColorWithCoverage coverage (RGBA r g b a) = SDL.V4 (toWord8 r) (toWord8 g) (toWord8 b) (toWord8 (a * coverage))

toSDLColor :: Colour -> SDL.V4 Word8
toSDLColor = toSDLColorWithCoverage 1

-- | Like 'toSDLColorWithCoverage', but as the 'Raw.Color' a 'SDL.Vertex'
-- carries rather than the 'SDL.V4' 'SDL.rendererDrawColor' wants.
toRawColorWithCoverage :: Double -> Colour -> Raw.Color
toRawColorWithCoverage coverage (RGBA r g b a) =
  Raw.Color (toWord8 r) (toWord8 g) (toWord8 b) (toWord8 (a * coverage))

toSDLColor3 :: Colour -> SDL.V3 Word8
toSDLColor3 (RGBA r g b _) = SDL.V3 (toWord8 r) (toWord8 g) (toWord8 b)

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect r =
  SDL.Rectangle
    (SDL.P (SDL.V2 (round (rectX r)) (round (rectY r))))
    (SDL.V2 (round (rectWidth r)) (round (rectHeight r)))

intersectSDLRect :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt
intersectSDLRect (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1))
                 (SDL.Rectangle (SDL.P (SDL.V2 x2 y2)) (SDL.V2 w2 h2)) =
  let x = max x1 x2
      y = max y1 y2
      r = min (x1 + w1) (x2 + w2)
      b = min (y1 + h1) (y2 + h2)
  in SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (max 0 (r - x)) (max 0 (b - y)))

alignedTextRect :: Rectangle -> TextAlign -> CInt -> CInt -> SDL.Rectangle CInt
alignedTextRect r textAlign tw th =
  let cy = round (rectY r + (rectHeight r - fromIntegral th) / 2)
      cx = case textAlign of
        AlignLeft   -> round (rectX r)
        AlignCenter -> round (rectX r + (rectWidth r - fromIntegral tw) / 2)
        AlignRight  -> round (rectX r + rectWidth r) - tw
  in SDL.Rectangle (SDL.P (SDL.V2 cx cy)) (SDL.V2 tw th)

renderFill :: SDL.Renderer -> Rectangle -> Colour -> IO ()
renderFill renderer r color = do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.fillRect renderer (Just (toSDLRect r))

-- | Draws a background fill clipped to the given corner radii, as the
-- single triangle mesh 'fillMesh' computes for it -- the rounded-fill
-- counterpart to 'renderFill', reached only for a genuinely rounded
-- background (see 'Blink.View.Drawing.fillRoundedRect').
renderRoundedFill :: SDL.Renderer -> Rectangle -> CornerRadii -> Colour -> IO ()
renderRoundedFill renderer r radii color = submitMesh renderer color (fillMesh r radii)

-- | Draws every layer in the stack, back-to-front, each expanded outward
-- from @r@ by its own 'layerOffset'.
renderBorder :: SDL.Renderer -> Rectangle -> Border -> IO ()
renderBorder renderer r = mapM_ (renderBorderLayer renderer r)

-- | Draws one layer's ring, its visible straight edges plus any rounded
-- corners its 'CornerRadii' declare, as the single triangle mesh
-- 'ringMesh' computes for it.
renderBorderLayer :: SDL.Renderer -> Rectangle -> BorderLayer -> IO ()
renderBorderLayer renderer r layer =
  submitMesh renderer (layerColour layer) (ringMesh outer (layerRadii layer) (layerWidth layer) (layerVisible layer))
  where
    outer = expandBy (layerOffset layer) r

-- | Submits a mesh ('ringMesh' or 'fillMesh') as a single
-- 'SDL.renderGeometry' call. Each vertex's coverage is folded into
-- @color@'s alpha, so SDL's own rasterizer interpolates a rounded
-- boundary's antialiased fringe between a feather vertex (coverage 0)
-- and its neighbouring boundary vertex (coverage 1) instead of
-- hard-rounding to a staircase.
submitMesh :: SDL.Renderer -> Colour -> ([MeshVertex], [Int]) -> IO ()
submitMesh renderer color (verts, idxs)
  | null verts = pure ()
  | otherwise  = SDL.renderGeometry renderer Nothing vertices indices
  where
    vertices = VS.fromList (map toVertex verts)
    indices  = VS.fromList (map fromIntegral idxs)
    toVertex v = SDL.Vertex
      (Raw.FPoint (realToFrac (meshVertexX v)) (realToFrac (meshVertexY v)))
      (toRawColorWithCoverage (meshVertexCoverage v) color)
      (Raw.FPoint 0 0)

-- | Expands a rectangle outward by @o@ pixels on every side, for
-- positioning a border layer at its 'layerOffset' from the control's own
-- bounds.
expandBy :: Double -> Rectangle -> Rectangle
expandBy o r = Rectangle (rectX r - o) (rectY r - o) (rectWidth r + 2 * o) (rectHeight r + 2 * o)

renderText :: SDL.Renderer -> Font.Font -> TextureCache -> Rectangle -> Text -> Colour -> TextAlign -> IO ()
renderText renderer font cache r txt color textAlign = do
  let sdlColor = toSDLColor color
  (texture, tw, th) <- cacheGetOrCreate cache destroyTextureEntry (txt, sdlColor) $ do
    surface <- Font.blended font sdlColor txt
    tex     <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    (SDL.TextureInfo _ _ w h) <- SDL.queryTexture tex
    pure (tex, w, h)
  SDL.copy renderer texture Nothing (Just (alignedTextRect r textAlign (fromIntegral tw) (fromIntegral th)))

-- | Tints the image at @path@ by @colour@ -- see 'DrawImage' for what
-- that does and does not affect, and 'sizedSvgSource' for how an
-- @.svg@'s own paths end up white (and so tintable) in the first place.
-- Sets the texture's colour\/alpha mod immediately before every draw
-- (rather than once at load time), since the same cached texture is
-- reused across draws that may want different tints -- e.g. a checkbox
-- icon drawn once at rest and once, elsewhere on screen, while hovered.
renderImage :: SDL.Renderer -> ImageCache -> Rectangle -> ImagePath -> Colour -> IO ()
renderImage renderer cache r path colour = do
  let sdlRect = toSDLRect r
      SDL.Rectangle _ (SDL.V2 w h) = sdlRect
      RGBA _ _ _ a = colour
  texture <- loadSizedImageTexture renderer cache path (w, h)
  SDL.textureColorMod texture $= toSDLColor3 colour
  SDL.textureAlphaMod texture $= toWord8 a
  SDL.copy renderer texture Nothing (Just sdlRect)

pushClip :: SDL.Renderer -> IORef [SDL.Rectangle CInt] -> Rectangle -> IO ()
pushClip renderer clipRef r = do
  stack <- readIORef clipRef
  let new     = toSDLRect r
      clipped = case stack of
        []            -> new
        (topClip : _) -> intersectSDLRect topClip new
  writeIORef clipRef (clipped : stack)
  SDL.rendererClipRect renderer $= Just clipped

popClip :: SDL.Renderer -> IORef [SDL.Rectangle CInt] -> IO ()
popClip renderer clipRef = do
  stack <- readIORef clipRef
  let rest = tail stack
  writeIORef clipRef rest
  case rest of
    []            -> SDL.rendererClipRect renderer $= Nothing
    (topClip : _) -> SDL.rendererClipRect renderer $= Just topClip

submitDrawCommand :: SDL.Renderer -> Font.Font -> TextureCache -> ImageCache -> IORef [SDL.Rectangle CInt] -> DrawCommand -> IO ()
submitDrawCommand renderer _ _ _ _          (FillRect r color)            = renderFill   renderer r color
submitDrawCommand renderer _ _ _ _          (FillRoundedRect r radii color) = renderRoundedFill renderer r radii color
submitDrawCommand renderer _ _ _ _          (StrokeBorder r border)      = renderBorder renderer r border
submitDrawCommand _ _ _ _ _                 (DrawText _ txt _ _) | T.null txt = pure ()
submitDrawCommand renderer font cache _ _   (DrawText r txt color textAlign) = renderText renderer font cache r txt color textAlign
submitDrawCommand renderer _ _ imgCache _   (DrawImage r path colour)     = renderImage  renderer imgCache r path colour
submitDrawCommand renderer _ _ _ clipRef    (PushClip r)                  = pushClip     renderer clipRef r
submitDrawCommand renderer _ _ _ clipRef     PopClip                      = popClip      renderer clipRef

mkTextMeasurer :: Font.Font -> IO TextMeasurer
mkTextMeasurer font = do
  offsetCache <- newBoundedCache :: IO (BoundedCache Text [Float])
  pure TextMeasurer
    { tmCharOffset   = \t i -> do
        offsets <- getOffsets offsetCache font t
        pure $ indexOr 0 i offsets

    , tmCharAtOffset = \t x -> do
        offsets <- getOffsets offsetCache font t
        pure $ findCharAt offsets x

    , tmTextSize     = \t ->
        if T.null t
          then do h <- Font.height font
                  pure (Size 0 (fromIntegral h))
          else do (w, h) <- Font.size font t
                  pure (Size (fromIntegral w) (fromIntegral h))
    }

mkImageMeasurer :: SDL.Renderer -> ImageCache -> ImageMeasurer
mkImageMeasurer renderer cache = ImageMeasurer
  { imNaturalSize = \path -> do
      (_, w, h) <- loadImageTexture renderer cache path
      pure (Size (fromIntegral w) (fromIntegral h))
  }

getOffsets :: BoundedCache Text [Float] -> Font.Font -> Text -> IO [Float]
getOffsets cache font t = cacheGetOrCreate cache (const (pure ())) t (buildOffsets font t)

buildOffsets :: Font.Font -> Text -> IO [Float]
buildOffsets font t = do
  advances <- mapM (glyphAdvance font) (T.unpack t)
  pure $ map fromIntegral (scanl (+) (0 :: Int) advances)

glyphAdvance :: Font.Font -> Char -> IO Int
glyphAdvance font ch = do
  mMetrics <- Font.glyphMetrics font ch
  pure $ case mMetrics of
    Nothing                    -> 0
    Just (_, _, _, _, advance) -> advance

indexOr :: a -> Int -> [a] -> a
indexOr def i xs
  | i < 0 || i >= length xs = def
  | otherwise                = xs !! i

findCharAt :: [Float] -> Float -> Int
findCharAt []      _ = 0
findCharAt offsets x =
  let n         = length offsets - 1
      midpoints = [ (offsets !! i + offsets !! (i+1)) / 2 | i <- [0 .. n-1] ]
      idx       = length (takeWhile (<= x) midpoints)
  in max 0 (min n idx)

-- Rounded-rect tessellation --------------------------------------------
--
-- Turns a border layer's stroke, or a background's fill, into a triangle
-- mesh that 'SDL.renderGeometry' can rasterize directly, rather than a
-- pile of axis-aligned rectangles. Lives here, in the SDL backend, rather
-- than in the library: it's tied to one specific rendering technique (a
-- feathered triangle mesh for a rasterizer that linearly interpolates
-- per-vertex colour) that only this backend uses -- a backend built on a
-- native 2D API (Skia, Cairo, Direct2D, a browser canvas) would just call
-- its own rounded-rect primitive and none of this would apply.
--
-- Every boundary -- a rounded corner's arc, and a straight edge's own
-- outer side -- is feathered by 'featherWidth', centred on the
-- boundary's true position (half the feather inside it, half outside),
-- fading to zero coverage; the rasterizer's own interpolation between a
-- coverage-0 and a coverage-1 vertex then antialiases the whole shape
-- uniformly rather than a rounded corner alone.

-- | A vertex of a mesh: a position in screen space paired with a
-- coverage fraction (1 = fully inside the shape, 0 = fully outside, at a
-- feather edge), for the caller to fold into its colour's alpha channel.
data MeshVertex = MeshVertex
  { meshVertexX :: Double
  , meshVertexY :: Double
  , meshVertexCoverage :: Double
  } deriving (Eq, Show)

-- | Which of the rectangle's four corners: used only to select that
-- corner's own radius and to mirror its (canonically top-left) arc into
-- the right quadrant.
data Corner = CornerTopLeft | CornerTopRight | CornerBottomRight | CornerBottomLeft

corners :: [Corner]
corners = [CornerTopLeft, CornerTopRight, CornerBottomRight, CornerBottomLeft]

cornerRadius :: Corner -> CornerRadii -> Double
cornerRadius CornerTopLeft     = radiusTopLeft
cornerRadius CornerTopRight    = radiusTopRight
cornerRadius CornerBottomRight = radiusBottomRight
cornerRadius CornerBottomLeft  = radiusBottomLeft

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
ringMesh :: Rectangle -> CornerRadii -> Double -> EdgeVisibility -> ([MeshVertex], [Int])
ringMesh r radii strokeWidth visible = appendMeshes (cornerMeshes ++ edgeMeshes)
  where
    cornerMeshes = [ cornerWedge c (cornerRadius c radii) r strokeWidth | c <- corners ]
    edgeMeshes   = [ edgeMesh edge | edge <- edgeSpans r radii strokeWidth visible ]

-- | The solid fill for a background of the given rectangle and corner
-- radii: each corner's quarter-disk (reusing 'cornerWedge's own
-- no-inner-hole branch, since a fill has no hole either), each straight
-- edge's own antialiased outer fringe (reusing 'edgeSpans'\/'bandOffsets',
-- but only the true-edge-to-core band -- a fill has no inner edge to
-- feather), and the flat, un-feathered interior filling the rest: a
-- standard three-rectangle cross decomposition (a full-height middle
-- strip plus left\/right strips, each inset by that side's own corner
-- radii) that handles independent per-corner radii without gaps or
-- overlaps. The interior rectangles run flush to the true edges, so they
-- overlap the edge\/corner pieces' own coverage-1 core very slightly --
-- harmless, since that's the same opaque colour drawn twice, not two
-- different alphas blending.
fillMesh :: Rectangle -> CornerRadii -> ([MeshVertex], [Int])
fillMesh r radii = appendMeshes (cornerMeshes ++ edgeMeshes ++ interiorMeshes)
  where
    cornerMeshes  = [ cornerWedge c radius r radius | c <- corners, let radius = cornerRadius c radii, radius > 0 ]
    edgeMeshes    = [ edgeOuterFeather edge | edge <- edgeSpans r radii featherWidth allEdgesVisible ]
    interiorMeshes = interiorRects r radii

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
cornerWedge :: Corner -> Double -> Rectangle -> Double -> ([MeshVertex], [Int])
cornerWedge _ radius _ strokeWidth
  | radius <= 0 || strokeWidth <= 0 = ([], [])
cornerWedge corner radius r strokeWidth = placeMesh corner r localMesh
  where
    segs = arcSegments radius
    innerRadius = radius - strokeWidth
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
placeMesh :: Corner -> Rectangle -> ([LocalPt], [Int]) -> ([MeshVertex], [Int])
placeMesh corner r (pts, idx) = (map toVertex pts, idx)
  where
    toVertex (x, y, c) = let (gx, gy) = placePoint corner r (x, y) in MeshVertex gx gy c

-- | Maps a point computed as if @corner@ were top-left into @r@'s
-- coordinate frame.
placePoint :: Corner -> Rectangle -> (Double, Double) -> (Double, Double)
placePoint corner r (x, y) = (gx, gy)
  where
    gx = case corner of
      CornerTopLeft     -> rectX r + x
      CornerBottomLeft  -> rectX r + x
      CornerTopRight    -> rectX r + rectWidth r - x
      CornerBottomRight -> rectX r + rectWidth r - x
    gy = case corner of
      CornerTopLeft     -> rectY r + y
      CornerTopRight    -> rectY r + y
      CornerBottomLeft  -> rectY r + rectHeight r - y
      CornerBottomRight -> rectY r + rectHeight r - y

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
edgeMesh :: EdgeSpan -> ([MeshVertex], [Int])
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

-- | Just the outer, antialiased fringe of a straight edge -- 'edgeMesh's
-- first band (true edge to its half-feather-inset core), with no inner
-- band, since a fill has no inner edge to feather; 'interiorRects'
-- covers everything past the core.
edgeOuterFeather :: EdgeSpan -> ([MeshVertex], [Int])
edgeOuterFeather edge = bandMesh a b
  where
    (featherOut, outerCore, _, _) = bandOffsets (edgeOuterTrue edge) (edgeInnerTrue edge)
    a = (featherOut, 0)
    b = (outerCore, 1)
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

-- | The flat, fully-covered interior of a fill: the rectangle minus its
-- four corner boxes (each corner's own radius-by-radius square, whose
-- rounding is instead handled by 'cornerMeshes' above), decomposed into
-- plain rectangles that need no feathering of their own since every
-- point in them sits outside every corner's box.
--
-- A safe "cross" (a full-width middle row plus a top\/bottom bar and a
-- full-height middle column plus a left\/right bar, all sized off
-- @maxLeft@\/@maxRight@\/@maxTop@\/@maxBottom@ -- the /larger/ of each
-- side's two corner radii) covers everything except the four small
-- corners of that cross, where the smaller of a side's two radii would
-- otherwise leave a sliver unfilled (e.g. a point just past 'tl' but
-- still short of @max tl bl@, with @bl > tl@) -- those are filled by up
-- to two more rectangles per corner, splitting that corner's own
-- (up to @maxLeft@-by-@maxTop@) box around the part its own radius
-- actually excludes. In the uniform-radius and top-only-rounded cases
-- this library actually constructs, every one of those extra pieces
-- collapses to zero width or height and 'solidRect' drops it, leaving
-- exactly the plain cross either way would produce; the extra pieces
-- only matter for four genuinely different corner radii at once.
interiorRects :: Rectangle -> CornerRadii -> [([MeshVertex], [Int])]
interiorRects r radii =
  [ -- The safe cross: a full-height middle column plus a left/right bar
    -- over its safe middle rows -- together, everything except the four
    -- corner cells (the middle column's own full height already covers
    -- the top/bottom margins of its own columns, so no separate top/
    -- bottom bar is needed).
    solidRect (x0 + maxLeft) y0 (w - maxLeft - maxRight) h
  , solidRect x0 (y0 + maxTop) maxLeft (h - maxTop - maxBottom)
  , solidRect (x0 + w - maxRight) (y0 + maxTop) maxRight (h - maxTop - maxBottom)
    -- Each corner's own maxLeft/maxRight-by-maxTop/maxBottom cell, minus
    -- its own radius-by-radius box (covered separately by its quarter
    -- disk), split into the two rectangles either side of that box.
  , solidRect (x0 + tl) y0 (maxLeft - tl) maxTop
  , solidRect x0 (y0 + tl) tl (maxTop - tl)
  , solidRect (x0 + w - maxRight) y0 (maxRight - tr) maxTop
  , solidRect (x0 + w - tr) (y0 + tr) tr (maxTop - tr)
  , solidRect (x0 + w - maxRight) (y0 + h - maxBottom) (maxRight - br) maxBottom
  , solidRect (x0 + w - br) (y0 + h - maxBottom) br (maxBottom - br)
  , solidRect (x0 + bl) (y0 + h - maxBottom) (maxLeft - bl) maxBottom
  , solidRect x0 (y0 + h - maxBottom) bl (maxBottom - bl)
  ]
  where
    x0 = rectX r
    y0 = rectY r
    w  = rectWidth r
    h  = rectHeight r
    tl = radiusTopLeft radii
    tr = radiusTopRight radii
    br = radiusBottomRight radii
    bl = radiusBottomLeft radii
    maxLeft   = max tl bl
    maxRight  = max tr br
    maxTop    = max tl tr
    maxBottom = max bl br
    solidRect x y rw rh
      | rw <= 0 || rh <= 0 = ([], [])
      | otherwise          = quadMesh
          ( (x, y, 1), (x + rw, y, 1)
          , (x + rw, y + rh, 1), (x, y + rh, 1)
          )

-- | A quad from four corner points, each paired with its own coverage
-- (so a band between a coverage-0 and a coverage-1 side fades across
-- it), wound consistently.
quadMesh :: (LocalPt, LocalPt, LocalPt, LocalPt) -> ([MeshVertex], [Int])
quadMesh ((x0, y0, c0), (x1, y1, c1), (x2, y2, c2), (x3, y3, c3)) =
  ( [ MeshVertex x0 y0 c0
    , MeshVertex x1 y1 c1
    , MeshVertex x2 y2 c2
    , MeshVertex x3 y3 c3
    ]
  , [0, 1, 2, 0, 2, 3]
  )

-- | Combines already-placed sub-meshes into one, offsetting each's
-- indices past the vertices already accumulated -- used to merge every
-- corner's and edge's mesh into the one buffer 'ringMesh'\/'fillMesh'
-- returns.
appendMeshes :: [([MeshVertex], [Int])] -> ([MeshVertex], [Int])
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
