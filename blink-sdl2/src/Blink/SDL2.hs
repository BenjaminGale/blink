{- |
Module: Blink.SDL2

Runs a Blink application in an SDL2 window:

@
main :: IO ()
main = runApp Config
  { windowTitle = \"My app\"
  , fontFiles   = FontFile \"Inter\" Regular \"assets\/fonts\/Inter-Regular.ttf\"
               :| [FontFile \"Inter\" Bold \"assets\/fonts\/Inter-Bold.ttf\"]
  } myApp
@

The frame loop is event-driven: the window redraws when input arrives,
when an animation is running, or when a background command finishes.

Build the executable with @ghc-options: -threaded@. The loop blocks in
SDL's event wait, and without the threaded runtime that wait also stops
the threads running background commands and the animation ticker.
-}
module Blink.SDL2
  ( Config (..)
  , FontFile (..)
  , runApp
  ) where

import Blink.Backend
import Blink.SDL2.Input (sdlPoint, toKeyEvents, toModifiers, toTypedText, toWheelDelta, updateButton)
import Blink.SDL2.Fonts (FontFile (..), checkFontFiles, freeFontCache, newFontCache)
import Blink.SDL2.Rendering
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Raw
import Control.Exception (onException)
import Control.Concurrent.STM (atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Monad (foldM, unless, void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)

-- | How 'runApp' sets up the window.
data Config = Config
  { windowTitle :: Text
    -- ^ Shown in the window's title bar.
  , fontFiles   :: NonEmpty FontFile
    -- ^ The TrueType files to draw text with, one per family and weight.
    -- The first file's family is used for any 'Font' whose family is
    -- 'Nothing' or names a family with no file. A weight with no file of
    -- its own is drawn from another file of the same family, made bold by
    -- SDL_ttf when bold was asked for.
  }

-- | Opens a window, runs @app@ until the window is closed, then releases
-- every SDL resource it acquired. Checks every file in 'fontFiles' before
-- opening the window, and fails with an 'IOError' naming the first one
-- that can't be loaded.
runApp :: Ord e => Config -> App e msg s -> IO ()
runApp config app = do
  SDL.initializeAll
  Font.initialize
  checkFontFiles (fontFiles config) `onException` (Font.quit >> SDL.quit)
  -- Without this, SDL defaults to nearest-neighbor sampling, so any
  -- stretched texture (an image scaled above its natural size, in
  -- particular) comes out blocky rather than smooth.
  _ <- SDL.setHintWithPriority SDL.OverridePriority SDL.HintRenderScaleQuality SDL.ScaleLinear
  window   <- SDL.createWindow (windowTitle config) SDL.defaultWindow { SDL.windowResizable = True }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  -- Without this, every fill is forced fully opaque regardless of its
  -- colour's own alpha -- needed for a rounded border corner's
  -- anti-aliased fringe pixels to actually blend instead of being drawn
  -- solid.
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  fonts    <- newFontCache (fontFiles config)
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
  measurer <- mkTextMeasurer fonts
  let imageMeasurer = mkImageMeasurer renderer imgCache
  msgQueue <- newBoundedMsgQueue 256

  let renderFrame calls = do
        SDL.rendererDrawColor renderer $= SDL.V4 229 229 234 255
        SDL.clear renderer
        clipRef <- newIORef ([] :: [SDL.Rectangle CInt])
        mapM_ (submitDrawCommand renderer fonts texCache imgCache clipRef) calls
        SDL.present renderer

  handle <- configureEventDriven app msgQueue notify
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
  freeFontCache fonts
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Font.quit
  SDL.quit

-- | A bounded, STM-backed 'MsgQueue' -- Blink only defines the interface
-- ('MsgQueue', 'Blink.Cmd.Cmd'); the backend owns the actual data structure and its
-- backpressure policy. 'writeTBQueue' blocks the thread of a completing 'Blink.Cmd.Cmd'
-- once @capacity@ results are already waiting to be drained, rather
-- than dropping any; 'flushTBQueue' drains everything currently queued
-- without blocking, which is exactly what 'stepFrame' needs each frame.
newBoundedMsgQueue :: Int -> IO (MsgQueue msg)
newBoundedMsgQueue capacity = do
  queue <- newTBQueueIO (fromIntegral capacity)
  pure MsgQueue
    { enqueueMsg = atomically . writeTBQueue queue
    , drainMsgs  = atomically (flushTBQueue queue)
    }

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
                   , heldModifiers   = toModifiers mods
                   , windowSize      = winSize
                   , quitRequested   = SDL.eventPayload event == SDL.QuitEvent
                   , isAnimationTick = isAnimTick
                   , frameTime       = Nothing
                   }
      result <- stepFrame handle fi
      pure (btn', Just result)

