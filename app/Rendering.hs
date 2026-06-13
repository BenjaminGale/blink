module Rendering
  ( TextureCache
  , newTextureCache
  , freeTextureCache
  , submitDrawCommand
  , mkTextMeasurer
  ) where

import Blink
import Blink.Rendering (TextMeasurer (..))
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)

type TextureCache = IORef (Map (Text, SDL.V4 Word8) (SDL.Texture, CInt, CInt))

newTextureCache :: IO TextureCache
newTextureCache = newIORef Map.empty

freeTextureCache :: TextureCache -> IO ()
freeTextureCache cache =
  readIORef cache >>= mapM_ (\(t, _, _) -> SDL.destroyTexture t) . Map.elems

toWord8 :: Double -> Word8
toWord8 c = round (c * 255)

toSDLColor :: Colour -> SDL.V4 Word8
toSDLColor (RGBA r g b _) = SDL.V4 (toWord8 r) (toWord8 g) (toWord8 b) 255

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
alignedTextRect r align tw th =
  let cy = round (rectY r + rectHeight r / 2) - th `div` 2
      cx = case align of
        AlignLeft   -> round (rectX r)
        AlignCenter -> round (rectX r + rectWidth r / 2) - tw `div` 2
        AlignRight  -> round (rectX r + rectWidth r) - tw
  in SDL.Rectangle (SDL.P (SDL.V2 cx cy)) (SDL.V2 tw th)

renderFill :: SDL.Renderer -> Rectangle -> Colour -> IO ()
renderFill renderer r color = do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.fillRect renderer (Just (toSDLRect r))

renderStroke :: SDL.Renderer -> Rectangle -> Colour -> IO ()
renderStroke renderer r color = do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.drawRect renderer (Just (toSDLRect r))

renderText :: SDL.Renderer -> Font.Font -> TextureCache -> Rectangle -> Text -> Colour -> TextAlign -> IO ()
renderText renderer font cache r text color align = do
  let sdlColor = toSDLColor color
      cacheKey = (text, sdlColor)
  m <- readIORef cache
  (texture, tw, th) <- case Map.lookup cacheKey m of
    Just hit -> pure hit
    Nothing  -> do
      surface <- Font.blended font sdlColor text
      tex     <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      (SDL.TextureInfo _ _ w h) <- SDL.queryTexture tex
      writeIORef cache (Map.insert cacheKey (tex, w, h) m)
      pure (tex, w, h)
  SDL.copy renderer texture Nothing (Just (alignedTextRect r align (fromIntegral tw) (fromIntegral th)))

pushClip :: SDL.Renderer -> IORef [SDL.Rectangle CInt] -> Rectangle -> IO ()
pushClip renderer clipRef r = do
  stack <- readIORef clipRef
  let new     = toSDLRect r
      clipped = case stack of
        []        -> new
        (top : _) -> intersectSDLRect top new
  writeIORef clipRef (clipped : stack)
  SDL.rendererClipRect renderer $= Just clipped

popClip :: SDL.Renderer -> IORef [SDL.Rectangle CInt] -> IO ()
popClip renderer clipRef = do
  stack <- readIORef clipRef
  let rest = tail stack
  writeIORef clipRef rest
  case rest of
    []        -> SDL.rendererClipRect renderer $= Nothing
    (top : _) -> SDL.rendererClipRect renderer $= Just top

submitDrawCommand :: SDL.Renderer -> Font.Font -> TextureCache -> IORef [SDL.Rectangle CInt] -> DrawCommand -> IO ()
submitDrawCommand renderer _ _ _        (FillRect r color)            = renderFill   renderer r color
submitDrawCommand renderer _ _ _        (StrokeRect r color _)        = renderStroke renderer r color
submitDrawCommand _ _ _ _               (DrawText _ text _ _) | T.null text = pure ()
submitDrawCommand renderer font cache _ (DrawText r text color align) = renderText   renderer font cache r text color align
submitDrawCommand renderer _ _ clipRef  (PushClip r)                  = pushClip     renderer clipRef r
submitDrawCommand renderer _ _ clipRef   PopClip                      = popClip      renderer clipRef

mkTextMeasurer :: Font.Font -> IO TextMeasurer
mkTextMeasurer font = do
  offsetCache <- newIORef (Map.empty :: Map Text [Float])
  pure TextMeasurer
    { tmCharOffset   = \t i -> do
        offsets <- getOffsets offsetCache font t
        pure $ indexOr 0 i offsets

    , tmCharAtOffset = \t x -> do
        offsets <- getOffsets offsetCache font t
        pure $ findCharAt offsets x
    }

getOffsets :: IORef (Map Text [Float]) -> Font.Font -> Text -> IO [Float]
getOffsets cacheRef font t = do
  cache <- readIORef cacheRef
  case Map.lookup t cache of
    Just offsets -> pure offsets
    Nothing -> do
      offsets <- buildOffsets font t
      writeIORef cacheRef (Map.insert t offsets cache)
      pure offsets

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
