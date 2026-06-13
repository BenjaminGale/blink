{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import UI (demoApp)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Raw
import Control.Monad (void)
import Data.IORef
import Data.Maybe (isJust)
import Foreign.Ptr (nullPtr)
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

  buttonRef  <- newIORef ButtonUp
  mAnimEvent <- SDL.registerEvent
                  (\_ _ -> pure (Just ()))
                  (\_ -> pure (SDL.RegisteredEventData Nothing 0 nullPtr nullPtr))

  let notify   = case mAnimEvent of
                   Just et -> void $ SDL.pushRegisteredEvent et ()
                   Nothing -> pure ()
      measurer = noOpMeasurer

      renderFrame calls = do
        SDL.rendererDrawColor renderer $= SDL.V4 229 229 234 255
        SDL.clear renderer
        clipRef <- newIORef ([] :: [SDL.Rectangle CInt])
        mapM_ (submitDrawCommand renderer font clipRef) calls
        SDL.present renderer

  handle <- configureEventDriven demoApp notify measurer

  loop handle buttonRef renderFrame window mAnimEvent

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
  -> Maybe (SDL.RegisteredEventType ())
  -> IO ()
loop handle buttonRef renderFrame window mAnimEvent = do
  first <- SDL.waitEvent
  rest  <- SDL.pollEvents
  let events = first : rest

  btn <- readIORef buttonRef
  let btn'  = foldl updateButton btn events
      keys  = concatMap toKeyEvents events
      chars = concatMap toTypedText events
      isQuit = SDL.QuitEvent `elem` map SDL.eventPayload events
  isAnimTick <- maybe (pure False) (\et -> fmap or $ mapM (isAnimationEvent et) events) mAnimEvent
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
             isAnimTick

  result <- stepFrame handle fi
  case result of
    Continue draws _ -> renderFrame draws >> loop handle buttonRef renderFrame window mAnimEvent
    Quit     draws _ -> renderFrame draws

isAnimationEvent :: SDL.RegisteredEventType () -> SDL.Event -> IO Bool
isAnimationEvent et e = isJust <$> SDL.getRegisteredEvent et e

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
          SDL.Released -> ButtonReleased
          SDL.Pressed  -> case current of
            ButtonReleased -> ButtonReleased  -- preserve a release seen earlier this frame
            _              -> ButtonDown
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
         SDL.KeycodeSpace     -> [KeyEvent { key = KeySpace,     modifiers = [] }]
         SDL.KeycodeLeft      -> [KeyEvent { key = KeyLeft,      modifiers = [] }]
         SDL.KeycodeRight     -> [KeyEvent { key = KeyRight,     modifiers = [] }]
         SDL.KeycodeUp        -> [KeyEvent { key = KeyUp,        modifiers = [] }]
         SDL.KeycodeDown      -> [KeyEvent { key = KeyDown,      modifiers = [] }]
         _ -> []
  _ -> []

toTypedText :: SDL.Event -> [Text]
toTypedText e = case SDL.eventPayload e of
  SDL.TextInputEvent d -> [SDL.textInputEventText d]
  _                    -> []

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)

toWord8 :: Double -> Word8
toWord8 c = round (c * 255)

toSDLColor :: Colour -> SDL.V4 Word8
toSDLColor (RGBA r g b _) = SDL.V4 (toWord8 r) (toWord8 g) (toWord8 b) 255

renderFill :: SDL.Renderer -> Rectangle -> Colour -> IO ()
renderFill renderer r color = do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.fillRect renderer (Just (toSDLRect r))

renderStroke :: SDL.Renderer -> Rectangle -> Colour -> IO ()
renderStroke renderer r color = do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.drawRect renderer (Just (toSDLRect r))

renderText :: SDL.Renderer -> Font.Font -> Rectangle -> Text -> Colour -> TextAlign -> IO ()
renderText renderer font r text color align = do
  surface <- Font.blended font (toSDLColor color) text
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  (SDL.TextureInfo _ _ tw th) <- SDL.queryTexture texture
  SDL.copy renderer texture Nothing (Just (alignedTextRect r align (fromIntegral tw) (fromIntegral th)))
  SDL.destroyTexture texture

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

submitDrawCommand :: SDL.Renderer -> Font.Font -> IORef [SDL.Rectangle CInt] -> DrawCommand -> IO ()
submitDrawCommand renderer _ _       (FillRect r color)              = renderFill   renderer r color
submitDrawCommand renderer _ _       (StrokeRect r color _)          = renderStroke renderer r color
submitDrawCommand _ _ _              (DrawText _ text _ _) | T.null text = pure ()
submitDrawCommand renderer font _    (DrawText r text color align)   = renderText   renderer font r text color align
submitDrawCommand renderer _ clipRef (PushClip r)                    = pushClip     renderer clipRef r
submitDrawCommand renderer _ clipRef  PopClip                        = popClip      renderer clipRef

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

toSDLRect :: Rectangle -> SDL.Rectangle CInt
toSDLRect r =
  SDL.Rectangle
    (SDL.P (SDL.V2 (round (rectX r)) (round (rectY r))))
    (SDL.V2 (round (rectWidth r)) (round (rectHeight r)))
