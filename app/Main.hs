{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import Rendering
import UI (demoApp)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Raw
import Control.Monad (foldM, void)
import Data.IORef (newIORef)
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

  texCache  <- newTextureCache
  mAnimEvent <- SDL.registerEvent
                  (\_ _ -> pure (Just ()))
                  (\_ -> pure (SDL.RegisteredEventData Nothing 0 nullPtr nullPtr))

  let notify        = case mAnimEvent of
                       Just et -> void $ SDL.pushRegisteredEvent et ()
                       Nothing -> pure ()
      checkAnimTick = case mAnimEvent of
                       Nothing -> \_ -> pure False
                       Just et -> \evs -> or <$> mapM (fmap isJust . SDL.getRegisteredEvent et) evs
  measurer <- mkTextMeasurer font

  let renderFrame calls = do
        SDL.rendererDrawColor renderer $= SDL.V4 229 229 234 255
        SDL.clear renderer
        clipRef <- newIORef ([] :: [SDL.Rectangle CInt])
        mapM_ (submitDrawCommand renderer font texCache clipRef) calls
        SDL.present renderer

  handle <- configureEventDriven demoApp notify measurer

  loop handle False renderFrame window checkAnimTick

  freeTextureCache texCache
  Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Font.quit
  SDL.quit

loop
  :: BlinkHandle s
  -> Bool
  -> ([DrawCommand] -> IO ())
  -> SDL.Window
  -> ([SDL.Event] -> IO Bool)
  -> IO ()
loop handle btnDown renderFrame window checkAnimTick = do
  first <- SDL.waitEvent
  rest  <- SDL.pollEvents
  mousePos         <- SDL.getAbsoluteMouseLocation
  SDL.V2 winW winH <- SDL.get (SDL.windowSize window)
  let pos     = sdlPoint mousePos
      winSize = Size (fromIntegral winW) (fromIntegral winH)
  (btnDown', result) <- foldM (stepEvent pos winSize) (btnDown, Nothing) (first : rest)
  case result of
    Just (Continue draws _) -> renderFrame draws >> loop handle btnDown' renderFrame window checkAnimTick
    Just (Quit     draws _) -> renderFrame draws
    Nothing                 -> loop handle btnDown' renderFrame window checkAnimTick
  where
    stepEvent pos winSize (btn, _) event = do
      isAnimTick <- checkAnimTick [event]
      let btn' = updateButton btn event
          fi   = FrameInput
                   { mousePosition   = pos
                   , mouseButtonDown = btn'
                   , keyEvents       = toKeyEvents event
                   , typedText       = toTypedText event
                   , windowSize      = winSize
                   , quitRequested   = SDL.eventPayload event == SDL.QuitEvent
                   , isAnimationTick = isAnimTick
                   }
      result <- stepFrame handle fi
      pure (btn', Just result)

updateButton :: Bool -> SDL.Event -> Bool
updateButton current e = case SDL.eventPayload e of
  SDL.MouseButtonEvent d
    | SDL.mouseButtonEventButton d == SDL.ButtonLeft ->
        case SDL.mouseButtonEventMotion d of
          SDL.Released -> False
          SDL.Pressed  -> True
  _ -> current

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
         SDL.KeycodeLeft      ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyLeft,  modifiers = [Shift | shifted] }]
         SDL.KeycodeRight     ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyRight, modifiers = [Shift | shifted] }]
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
