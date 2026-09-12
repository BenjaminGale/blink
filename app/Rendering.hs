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
import Control.Monad (when)
import SDL (($=))
import qualified SDL
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

toSDLColor :: Colour -> SDL.V4 Word8
toSDLColor (RGBA r g b _) = SDL.V4 (toWord8 r) (toWord8 g) (toWord8 b) 255

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

-- | Draws each edge as its own filled rectangle, in the outer rect's own
-- rounded integer coordinate frame throughout — rounding @r@ just once and
-- deriving every edge from those integers, rather than rounding each edge
-- rectangle independently. Independent rounding let adjacent edges land on
-- different pixels for the same corner when @r@'s bounds were fractional
-- (routine after layout centring/flex math), leaving a 1px gap or overlap
-- at the corner; sharing one integer frame makes the four edges tile
-- exactly.
renderBorder :: SDL.Renderer -> Rectangle -> Colour -> BorderEdges -> IO ()
renderBorder renderer r color edges = do
  SDL.rendererDrawColor renderer $= toSDLColor color
  let SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h) = toSDLRect r
      t  = round (edgeTop edges)
      ri = round (edgeRight edges)
      b  = round (edgeBottom edges)
      l  = round (edgeLeft edges)
      mkRect rx ry rw rh = SDL.Rectangle (SDL.P (SDL.V2 rx ry)) (SDL.V2 rw rh)
  when (t > 0)  $ SDL.fillRect renderer (Just (mkRect x y w t))
  when (b > 0)  $ SDL.fillRect renderer (Just (mkRect x (y + h - b) w b))
  when (l > 0)  $ SDL.fillRect renderer (Just (mkRect x (y + t) l (h - t - b)))
  when (ri > 0) $ SDL.fillRect renderer (Just (mkRect (x + w - ri) (y + t) ri (h - t - b)))

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
submitDrawCommand renderer _ _ _ _          (StrokeBorder r color edges)  = renderBorder renderer r color edges
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
