{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import UI (AppState, demoApp)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import Data.Word (Word8)
import Foreign.C.Types (CInt)

fontPath :: FilePath
fontPath = "assets/fonts/Inter-Regular.ttf"

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  window <- SDL.createWindow "blink" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- Font.load fontPath 14
  initialState <- startUp demoApp
  loop renderer font initialState ButtonUp
  Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Font.quit
  SDL.quit

loop :: SDL.Renderer -> Font.Font -> AppState -> ButtonState -> IO ()
loop renderer font state buttonState = do
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
          (_, uiSt) = runUI (view demoApp state) ctx emptyUIState
          state' = execCommands (update demoApp) (pendingCommands uiSt) state

      SDL.rendererDrawColor renderer $= SDL.V4 30 30 30 255
      SDL.clear renderer
      mapM_ (submitDrawCall renderer font) (drawCalls uiSt)
      SDL.present renderer

      loop renderer font state' (nextFrameButton buttonState')

execCommands :: (c -> s -> Update s c ()) -> [c] -> s -> s
execCommands updateFn cmds initialState = foldl step initialState cmds
  where
    step s cmd =
      let Update f = updateFn cmd s
          ((), s', _) = f s
      in s'

updateButton :: ButtonState -> SDL.Event -> ButtonState
updateButton current e = case SDL.eventPayload e of
  SDL.MouseButtonEvent d
    | SDL.mouseButtonEventButton d == SDL.ButtonLeft ->
        case SDL.mouseButtonEventMotion d of
          SDL.Pressed -> ButtonDown
          SDL.Released -> ButtonReleased
  _ -> current

nextFrameButton :: ButtonState -> ButtonState
nextFrameButton ButtonReleased = ButtonUp
nextFrameButton s = s

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)

submitDrawCall :: SDL.Renderer -> Font.Font -> DrawCall -> IO ()
submitDrawCall renderer _ (FillRect r (RGBA red green blue _)) = do
  let toWord8 c = round (c * 255) :: Word8
  SDL.rendererDrawColor renderer $= SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255
  SDL.fillRect renderer (Just (toSDLRect r))
submitDrawCall renderer font (DrawText r text) = do
  surface <- Font.blended font (SDL.V4 255 255 255 255) text
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  (SDL.TextureInfo _ _ tw th) <- SDL.queryTexture texture
  let dstRect = centredRect r (fromIntegral tw) (fromIntegral th)
  SDL.copy renderer texture Nothing (Just dstRect)
  SDL.destroyTexture texture

centredRect :: Rectangle -> CInt -> CInt -> SDL.Rectangle CInt
centredRect r tw th =
  let cx = round (rectX r + rectWidth r / 2) - tw `div` 2
      cy = round (rectY r + rectHeight r / 2) - th `div` 2
  in SDL.Rectangle (SDL.P (SDL.V2 cx cy)) (SDL.V2 tw th)

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect r =
  SDL.Rectangle
    (SDL.P (SDL.V2 (round (rectX r)) (round (rectY r))))
    (SDL.V2 (round (rectWidth r)) (round (rectHeight r)))
