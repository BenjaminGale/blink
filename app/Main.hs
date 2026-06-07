{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import SDL (($=))
import qualified SDL
import Data.Word (Word8)
import Foreign.C.Types (CInt)

cursorRect :: UI ()
cursorRect = do
  mouse <- getMousePos
  let r = rectCentredAt mouse (Size 40 40)
  layout r $ fillRect (RGBA 1 0.4 0.2 1)

main :: IO ()
main = do
  SDL.initializeAll
  window   <- SDL.createWindow "blink" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  loop renderer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Renderer -> IO ()
loop renderer = do
  shouldQuit <- fmap (any isQuit) SDL.pollEvents
  if shouldQuit
    then return ()
    else do
      mousePos <- SDL.getAbsoluteMouseLocation
      let input   = InputState { mousePosition = sdlPoint mousePos }
          winRect = Rectangle 0 0 800 600
          ctx     = UIContext { drawRect = winRect, inputState = input }
          (_, st) = runUI cursorRect ctx emptyUIState

      SDL.rendererDrawColor renderer $= SDL.V4 30 30 30 255
      SDL.clear renderer
      mapM_ (submitDrawCall renderer) (drawCalls st)
      SDL.present renderer

      loop renderer
  where
    isQuit e = SDL.eventPayload e == SDL.QuitEvent

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)

submitDrawCall :: SDL.Renderer -> DrawCall -> IO ()
submitDrawCall renderer (FillRect r (RGBA red grn blu _)) = do
  let toWord8 c = round (c * 255) :: Word8
  SDL.rendererDrawColor renderer $= SDL.V4 (toWord8 red) (toWord8 grn) (toWord8 blu) 255
  SDL.fillRect renderer (Just (toSDLRect r))

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect r =
  SDL.Rectangle
    (SDL.P (SDL.V2 (round (rectX r)) (round (rectY r))))
    (SDL.V2 (round (rectWidth r)) (round (rectHeight r)))
