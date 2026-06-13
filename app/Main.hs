{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import Rendering
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
  texCache  <- newTextureCache
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
        mapM_ (submitDrawCommand renderer font texCache clipRef) calls
        SDL.present renderer

  handle <- configureEventDriven demoApp notify measurer

  loop handle buttonRef renderFrame window mAnimEvent

  freeTextureCache texCache
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

  let fi = FrameInput
             { mousePosition   = sdlPoint mousePos
             , mouseButton     = btn'
             , keyEvents       = keys
             , typedText       = chars
             , windowSize      = Size (fromIntegral winW) (fromIntegral winH)
             , quitRequested   = isQuit
             , isAnimationTick = isAnimTick
             }

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
