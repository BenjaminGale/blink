{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import UI (demoApp)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import Data.IORef
import Data.Word (Word8)
import Foreign.C.Types (CInt)

fontPath :: FilePath
fontPath = "assets/fonts/Inter-Regular.ttf"

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  window <- SDL.createWindow "blink" SDL.defaultWindow { SDL.windowResizable = True }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- Font.load fontPath 14

  eventsRef <- newIORef ([] :: [SDL.Event])
  buttonRef <- newIORef ButtonUp

  let backend = Backend
        { collectEvents = do
            first <- SDL.waitEvent
            rest  <- SDL.pollEvents
            writeIORef eventsRef (first : rest)
        , shouldClose = do
            events <- readIORef eventsRef
            return $ any (== SDL.QuitEvent) (map SDL.eventPayload events)
        , pollInput = do
            events <- readIORef eventsRef
            btn <- readIORef buttonRef
            let btn' = foldl updateButton btn events
                keys = concatMap toKeyEvents events
            writeIORef buttonRef (nextFrameButton btn')
            mousePos <- SDL.getAbsoluteMouseLocation
            return InputState
              { mousePosition = sdlPoint mousePos
              , leftButton = btn'
              , keyEvents = keys
              }
        , windowSize = do
            SDL.V2 w h <- SDL.get (SDL.windowSize window)
            return (Size (fromIntegral w) (fromIntegral h))
        , frameMode = EventDriven
        , render = \calls -> do
            SDL.rendererDrawColor renderer $= SDL.V4 229 229 234 255
            SDL.clear renderer
            clipRef <- newIORef ([] :: [SDL.Rectangle CInt])
            mapM_ (submitDrawCall renderer font clipRef) calls
            SDL.present renderer
        }

  runApp backend demoApp

  Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Font.quit
  SDL.quit

updateButton :: ButtonState -> SDL.Event -> ButtonState
updateButton current e = case SDL.eventPayload e of
  SDL.MouseButtonEvent d
    | SDL.mouseButtonEventButton d == SDL.ButtonLeft ->
        case SDL.mouseButtonEventMotion d of
          SDL.Pressed  -> ButtonDown
          SDL.Released -> ButtonReleased
  _ -> current

nextFrameButton :: ButtonState -> ButtonState
nextFrameButton ButtonReleased = ButtonUp
nextFrameButton s = s

toKeyEvents :: SDL.Event -> [KeyEvent]
toKeyEvents e = case SDL.eventPayload e of
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    , not (SDL.keyboardEventRepeat d)
    -> case SDL.keysymKeycode (SDL.keyboardEventKeysym d) of
         SDL.KeycodeTab ->
           let mods = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyTab, modifiers = if shifted then [Shift] else [] }]
         SDL.KeycodeReturn -> [KeyEvent { key = KeyReturn, modifiers = [] }]
         _ -> []
  _ -> []

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)

submitDrawCall :: SDL.Renderer -> Font.Font -> IORef [SDL.Rectangle CInt] -> DrawCall -> IO ()
submitDrawCall renderer _ _ (FillRect r (RGBA red green blue _)) = do
  let toWord8 c = round (c * 255) :: Word8
  SDL.rendererDrawColor renderer $= SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255
  SDL.fillRect renderer (Just (toSDLRect r))
submitDrawCall renderer font _ (DrawText r text (RGBA red green blue _) align) = do
  let toWord8 c = round (c * 255) :: Word8
  surface <- Font.blended font (SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255) text
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  (SDL.TextureInfo _ _ tw th) <- SDL.queryTexture texture
  let dstRect = alignedTextRect r align (fromIntegral tw) (fromIntegral th)
  SDL.copy renderer texture Nothing (Just dstRect)
  SDL.destroyTexture texture
submitDrawCall renderer _ clipRef (PushClip r) = do
  stack <- readIORef clipRef
  let new = toSDLRect r
      clipped = case stack of
        []        -> new
        (top : _) -> intersectRect top new
  writeIORef clipRef (clipped : stack)
  SDL.rendererClipRect renderer $= Just clipped
submitDrawCall renderer _ clipRef PopClip = do
  stack <- readIORef clipRef
  let rest = drop 1 stack
  writeIORef clipRef rest
  case rest of
    []        -> SDL.rendererClipRect renderer $= Nothing
    (top : _) -> SDL.rendererClipRect renderer $= Just top

intersectRect :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt
intersectRect (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1))
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

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect r =
  SDL.Rectangle
    (SDL.P (SDL.V2 (round (rectX r)) (round (rectY r))))
    (SDL.V2 (round (rectWidth r)) (round (rectHeight r)))
