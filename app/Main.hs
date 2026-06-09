{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import UI (demoApp)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Raw
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)

demoFontPath :: FilePath
demoFontPath = "assets/fonts/Inter-Regular.ttf"

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  window   <- SDL.createWindow "blink" SDL.defaultWindow { SDL.windowResizable = True }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font     <- Font.load demoFontPath 14
  SDL.Raw.startTextInput

  buttonRef <- newIORef ButtonUp

  -- demoApp uses no async effects so the notify callback is never invoked;
  -- a real event-driven backend would call SDL.pushEvent here to unblock waitEvent.
  let notify   = pure () :: IO ()
      measurer = noOpMeasurer

      renderFrame calls = do
        SDL.rendererDrawColor renderer $= SDL.V4 229 229 234 255
        SDL.clear renderer
        clipRef <- newIORef ([] :: [SDL.Rectangle CInt])
        mapM_ (submitDrawCommand renderer font clipRef) calls
        SDL.present renderer

  handle <- configureEventDriven demoApp notify measurer
  state0  <- initState handle

  loop handle buttonRef renderFrame window state0

  Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Font.quit
  SDL.quit

loop
  :: BlinkHandle s
  -> IORef ButtonState
  -> ([DrawCommand] -> IO ())
  -> SDL.Window
  -> s
  -> IO ()
loop handle buttonRef renderFrame window state = do
  first <- SDL.waitEvent
  rest  <- SDL.pollEvents
  let events = first : rest

  btn <- readIORef buttonRef
  let btn'   = foldl updateButton btn events
      keys   = concatMap toKeyEvents events
      chars  = concatMap toTypedText events
      isQuit = SDL.QuitEvent `elem` map SDL.eventPayload events
  writeIORef buttonRef (nextFrameButton btn')

  mousePos         <- SDL.getAbsoluteMouseLocation
  SDL.V2 winW winH <- SDL.get (SDL.windowSize window)

  -- Use positional constructor to avoid ambiguity with InputState field names
  let fi = FrameInput
             (sdlPoint mousePos)
             btn'
             keys
             chars
             (Size (fromIntegral winW) (fromIntegral winH))
             isQuit

  result <- stepFrame handle fi state
  case result of
    Continue draws state' -> renderFrame draws >> loop handle buttonRef renderFrame window state'
    Quit     draws _      -> renderFrame draws

-- | A no-op 'TextMeasurer' for backends that do not yet use text measurement.
noOpMeasurer :: TextMeasurer
noOpMeasurer = TextMeasurer
  { measureFont  = \_ -> pure (FontMetrics 0 0 0)
  , measureText  = \_ _ -> pure (Size 0 0)
  , charOffset   = \_ _ _ -> pure 0
  , charAtOffset = \_ _ _ -> pure 0
  }

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
nextFrameButton s              = s

toKeyEvents :: SDL.Event -> [KeyEvent]
toKeyEvents e = case SDL.eventPayload e of
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    -> case SDL.keysymKeycode (SDL.keyboardEventKeysym d) of
         SDL.KeycodeTab ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyTab, modifiers = [Shift | shifted] }]
         SDL.KeycodeReturn    -> [KeyEvent { key = KeyReturn,    modifiers = [] }]
         SDL.KeycodeBackspace -> [KeyEvent { key = KeyBackspace, modifiers = [] }]
         _ -> []
  _ -> []

toTypedText :: SDL.Event -> [Text]
toTypedText e = case SDL.eventPayload e of
  SDL.TextInputEvent d -> [SDL.textInputEventText d]
  _                    -> []

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)

submitDrawCommand :: SDL.Renderer -> Font.Font -> IORef [SDL.Rectangle CInt] -> DrawCommand -> IO ()
submitDrawCommand renderer _ _ (FillRect r (RGBA red green blue _)) = do
  let toWord8 c = round (c * 255) :: Word8
  SDL.rendererDrawColor renderer $= SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255
  SDL.fillRect renderer (Just (toSDLRect r))
submitDrawCommand renderer _ _ (StrokeRect r (RGBA red green blue _) _) = do
  let toWord8 c = round (c * 255) :: Word8
  SDL.rendererDrawColor renderer $= SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255
  SDL.drawRect renderer (Just (toSDLRect r))
submitDrawCommand _ _ _ (DrawText _ text _ _) | T.null text = pure ()
submitDrawCommand renderer font _ (DrawText r text (RGBA red green blue _) align) = do
  let toWord8 c = round (c * 255) :: Word8
  surface <- Font.blended font (SDL.V4 (toWord8 red) (toWord8 green) (toWord8 blue) 255) text
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  (SDL.TextureInfo _ _ tw th) <- SDL.queryTexture texture
  let dstRect = alignedTextRect r align (fromIntegral tw) (fromIntegral th)
  SDL.copy renderer texture Nothing (Just dstRect)
  SDL.destroyTexture texture
submitDrawCommand renderer _ clipRef (PushClip r) = do
  stack <- readIORef clipRef
  let new     = toSDLRect r
      clipped = case stack of
        []        -> new
        (top : _) -> intersectRect top new
  writeIORef clipRef (clipped : stack)
  SDL.rendererClipRect renderer $= Just clipped
submitDrawCommand renderer _ clipRef PopClip = do
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
