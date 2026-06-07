{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import UI (demoView)
import SDL (($=))
import qualified SDL
import Data.Word (Word8)
import Foreign.C.Types (CInt)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "blink" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  loop renderer ButtonUp
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Renderer -> ButtonState -> IO ()
loop renderer buttonState = do
  events <- SDL.pollEvents
  let shouldQuit = any (== SDL.QuitEvent) (map SDL.eventPayload events)
      buttonState' = foldl updateButton buttonState events
  if shouldQuit
    then return ()
    else do
      mousePos <- SDL.getAbsoluteMouseLocation
      let input = InputState
            { mousePosition = sdlPoint mousePos
            , leftButton = buttonState'
            }
          winRect = Rectangle 0 0 800 600
          ctx = UIContext { drawRect = winRect, inputState = input }
          (_, st) = runUI demoView ctx emptyUIState

      SDL.rendererDrawColor renderer $= SDL.V4 30 30 30 255
      SDL.clear renderer
      mapM_ (submitDrawCall renderer) (drawCalls st)
      SDL.present renderer

      loop renderer buttonState'

updateButton :: ButtonState -> SDL.Event -> ButtonState
updateButton current e = case SDL.eventPayload e of
  SDL.MouseButtonEvent d
    | SDL.mouseButtonEventButton d == SDL.ButtonLeft ->
        case SDL.mouseButtonEventMotion d of
          SDL.Pressed -> ButtonDown
          SDL.Released -> ButtonUp
  _ -> current

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)

submitDrawCall :: SDL.Renderer -> DrawCall -> IO ()
submitDrawCall renderer (FillRect r (RGBA red green blue _)) = do
  let toWord8 c = round (c * 255) :: Word8
  SDL.rendererDrawColor renderer $= SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255
  SDL.fillRect renderer (Just (toSDLRect r))

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect r =
  SDL.Rectangle
    (SDL.P (SDL.V2 (round (rectX r)) (round (rectY r))))
    (SDL.V2 (round (rectWidth r)) (round (rectHeight r)))
