{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blink
import Rendering
import UI (demoApp)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Raw
import Control.Concurrent.STM (atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Monad (foldM, unless, void)
import Data.Char (chr, toUpper)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Foreign.Ptr (nullPtr)
import Data.Text (Text)
import Foreign.C.Types (CInt)

-- | A bounded, STM-backed 'MsgQueue' -- Blink only defines the interface
-- ('MsgQueue', 'Cmd'); the backend owns the actual data structure and its
-- backpressure policy. 'writeTBQueue' blocks the completing 'Cmd''s own
-- thread once @capacity@ results are already waiting to be drained, rather
-- than dropping any; 'flushTBQueue' drains everything currently queued
-- without blocking, which is exactly what 'stepFrame' needs each frame.
newBoundedMsgQueue :: Int -> IO (MsgQueue msg)
newBoundedMsgQueue capacity = do
  queue <- newTBQueueIO (fromIntegral capacity)
  pure MsgQueue
    { enqueueMsg = atomically . writeTBQueue queue
    , drainMsgs  = atomically (flushTBQueue queue)
    }

demoFontPath :: FilePath
demoFontPath = "assets/fonts/Inter-Regular.ttf"

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  -- Without this, SDL defaults to nearest-neighbor sampling, so any
  -- stretched texture (an image scaled above its natural size, in
  -- particular) comes out blocky rather than smooth.
  _ <- SDL.setHintWithPriority SDL.OverridePriority SDL.HintRenderScaleQuality SDL.ScaleLinear
  window   <- SDL.createWindow "blink" SDL.defaultWindow { SDL.windowResizable = True }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  -- Without this, every fill is forced fully opaque regardless of its
  -- colour's own alpha -- needed for a rounded border corner's
  -- anti-aliased fringe pixels to actually blend instead of being drawn
  -- solid.
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  font     <- Font.load demoFontPath 14
  SDL.Raw.startTextInput

  texCache  <- newTextureCache
  imgCache  <- newImageCache
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
  let imageMeasurer = mkImageMeasurer renderer imgCache
  msgQueue <- newBoundedMsgQueue 256

  let renderFrame calls = do
        SDL.rendererDrawColor renderer $= SDL.V4 229 229 234 255
        SDL.clear renderer
        clipRef <- newIORef ([] :: [SDL.Rectangle CInt])
        mapM_ (submitDrawCommand renderer font texCache imgCache clipRef) calls
        SDL.present renderer

  handle <- configureEventDriven demoApp msgQueue notify
              (Measurers { msrText = measurer, msrImage = imageMeasurer })

  arrowCursor    <- SDL.createSystemCursor SDL.SystemCursorArrow
  resizeCursor   <- SDL.createSystemCursor SDL.SystemCursorSizeWE
  lastCursorRef  <- newIORef CursorArrow
  let applyCursor = setActiveCursor lastCursorRef arrowCursor resizeCursor

  loop handle False applyCursor renderFrame window checkAnimTick

  SDL.freeCursor arrowCursor
  SDL.freeCursor resizeCursor
  freeTextureCache texCache
  freeImageCache imgCache
  Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Font.quit
  SDL.quit

-- | Applies the frame's requested 'CursorShape' to the platform pointer,
-- skipping the call to SDL when it's unchanged from last frame -- setting
-- the system cursor every single frame regardless is wasted work, since
-- the pointer shape only ever needs to change on a hover/drag transition.
setActiveCursor :: IORef CursorShape -> SDL.Cursor -> SDL.Cursor -> CursorShape -> IO ()
setActiveCursor lastCursorRef arrowCursor resizeCursor shape = do
  prev <- readIORef lastCursorRef
  unless (prev == shape) $ do
    SDL.activeCursor SDL.$= case shape of
      CursorArrow             -> arrowCursor
      CursorResizeHorizontal  -> resizeCursor
    writeIORef lastCursorRef shape

loop
  :: BlinkHandle s
  -> Bool
  -> (CursorShape -> IO ())
  -> ([DrawCommand] -> IO ())
  -> SDL.Window
  -> ([SDL.Event] -> IO Bool)
  -> IO ()
loop handle btnDown applyCursor renderFrame window checkAnimTick = do
  first <- SDL.waitEvent
  rest  <- SDL.pollEvents
  mousePos         <- SDL.getAbsoluteMouseLocation
  SDL.V2 winW winH <- SDL.get (SDL.windowSize window)
  let pos     = sdlPoint mousePos
      winSize = Size (fromIntegral winW) (fromIntegral winH)
  (btnDown', result) <- foldM (stepEvent pos winSize) (btnDown, Nothing) (first : rest)
  case result of
    Just (Continue draws cursor _) -> do
      renderFrame draws
      applyCursor cursor
      loop handle btnDown' applyCursor renderFrame window checkAnimTick
    Just (Quit draws cursor _) -> renderFrame draws >> applyCursor cursor
    Nothing                    -> loop handle btnDown' applyCursor renderFrame window checkAnimTick
  where
    stepEvent pos winSize (btn, _) event = do
      isAnimTick <- checkAnimTick [event]
      mods       <- SDL.getModState
      let btn' = updateButton btn event
          fi   = FrameInput
                   { mousePosition   = pos
                   , mouseButtonDown = btn'
                   , keyEvents       = toKeyEvents event
                   , typedText       = toTypedText event
                   , wheelDelta      = toWheelDelta event
                   , altHeld         = SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods
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

-- | The uppercase letter a letter keycode ('SDL.KeycodeA' through
-- 'SDL.KeycodeZ', whose underlying codes are the ASCII lowercase range)
-- represents, or 'Nothing' for any other keycode.
letterKeycode :: SDL.Keycode -> Maybe Char
letterKeycode kc
  | code >= SDL.unwrapKeycode SDL.KeycodeA && code <= SDL.unwrapKeycode SDL.KeycodeZ
  = Just (toUpper (chr (fromIntegral code)))
  | otherwise = Nothing
  where code = SDL.unwrapKeycode kc

toKeyEvents :: SDL.Event -> [KeyEvent]
toKeyEvents e = case SDL.eventPayload e of
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    , let keysym = SDL.keyboardEventKeysym d
    , let mods   = SDL.keysymModifier keysym
    , let alt    = SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods
    , Just c <- letterKeycode (SDL.keysymKeycode keysym)
    , alt
    -> [KeyEvent { key = KeyChar c, modifiers = [Alt], keyRepeat = SDL.keyboardEventRepeat d }]
  SDL.KeyboardEvent d
    | SDL.keyboardEventKeyMotion d == SDL.Pressed
    -> let rep = SDL.keyboardEventRepeat d
       in case SDL.keysymKeycode (SDL.keyboardEventKeysym d) of
         SDL.KeycodeTab ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyTab, modifiers = [Shift | shifted], keyRepeat = rep }]
         SDL.KeycodeReturn    -> [KeyEvent { key = KeyReturn,    modifiers = [], keyRepeat = rep }]
         SDL.KeycodeBackspace -> [KeyEvent { key = KeyBackspace, modifiers = [], keyRepeat = rep }]
         SDL.KeycodeDelete    -> [KeyEvent { key = KeyDelete,    modifiers = [], keyRepeat = rep }]
         SDL.KeycodeSpace     -> [KeyEvent { key = KeySpace,     modifiers = [], keyRepeat = rep }]
         SDL.KeycodeA         ->
           let mods  = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               ctrld = SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods
           in [KeyEvent { key = KeyA, modifiers = [Ctrl | ctrld], keyRepeat = rep }]
         SDL.KeycodeLeft      ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyLeft,  modifiers = [Shift | shifted], keyRepeat = rep }]
         SDL.KeycodeRight     ->
           let mods    = SDL.keysymModifier (SDL.keyboardEventKeysym d)
               shifted = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
           in [KeyEvent { key = KeyRight, modifiers = [Shift | shifted], keyRepeat = rep }]
         SDL.KeycodeUp        -> [KeyEvent { key = KeyUp,        modifiers = [], keyRepeat = rep }]
         SDL.KeycodeDown      -> [KeyEvent { key = KeyDown,      modifiers = [], keyRepeat = rep }]
         SDL.KeycodeEscape    -> [KeyEvent { key = KeyEscape,    modifiers = [], keyRepeat = rep }]
         _ -> []
  _ -> []

toTypedText :: SDL.Event -> [Text]
toTypedText e = case SDL.eventPayload e of
  SDL.TextInputEvent d -> [SDL.textInputEventText d]
  _                    -> []

-- | Vertical wheel movement for this event, or 0 if it isn't a wheel event
-- -- see 'Blink.Input.inputWheelDelta' for the sign convention. SDL reports
-- @y@ positive "away from the user" (scrolling up/back) unless the
-- platform's natural-scrolling setting flips it, so both cases are negated
-- to land on "positive scrolls down/forward".
toWheelDelta :: SDL.Event -> Double
toWheelDelta e = case SDL.eventPayload e of
  SDL.MouseWheelEvent d ->
    let SDL.V2 _ y = SDL.mouseWheelEventPos d
        flipSign = case SDL.mouseWheelEventDirection d of
          SDL.ScrollNormal  -> 1
          SDL.ScrollFlipped -> -1
    in negate (fromIntegral y * flipSign)
  _ -> 0

sdlPoint :: SDL.Point SDL.V2 CInt -> Point
sdlPoint (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)
