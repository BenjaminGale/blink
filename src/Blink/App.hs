{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.App
  ( Backend (..)
  , FrameMode (..)
  , App (..)
  , runApp
  ) where

import Blink.Rendering (DrawCommand)
import Blink.Geometry (Point (..), Rectangle (..), Size (..))
import Blink.Input (ButtonState (..), InputState (..))
import Blink.Style (Theme)
import Blink.UI (UI, UIContext (..), emptyUIContext, nextFrameContext, runUI)
import Blink.Update (Update, execCommands)
import Control.Monad (unless)

data FrameMode = EventDriven | Continuous

data Backend = Backend
  { collectEvents :: IO InputState
  , shouldClose :: IO Bool
  , windowSize :: IO Size
  , render :: [DrawCommand] -> IO ()
  , frameMode :: FrameMode
  }

data App e s c = App
  { startUp :: IO s
  , theme :: Theme e
  , view :: s -> UI e c ()
  , update :: c -> Update s c ()
  }

runApp :: Backend -> App e s c -> IO ()
runApp backend app = do
  state <- startUp app
  let initialCtx = emptyUIContext
        (Rectangle 0 0 0 0)
        (InputState (Point 0 0) ButtonUp [])
        (Blink.App.theme app)
  loop backend app state initialCtx

loop :: Backend -> App e s c -> s -> UIContext e c -> IO ()
loop backend app state ctx = do
  events <- collectEvents backend
  close <- shouldClose backend

  unless close $ do
      size <- windowSize backend
      let winRect = Rectangle 0 0 (sizeWidth size) (sizeHeight size)
          (_, ctx1) = runUI (view app state) (nextFrameContext winRect events ctx)
          nextFocus = if ctxFocusedRendered ctx1 then ctxFocusedElement ctx1 else Nothing
          state' = execCommands (update app) (ctxPendingCommands ctx1) state
          freshCtx2 = (nextFrameContext winRect (events { keyEvents = [] }) ctx1)
            { ctxFocusedElement = nextFocus
            , ctxFocusNext = False
            }
          (drawCalls', nextCtx) = case frameMode backend of
            EventDriven ->
              let (_, ctx2) = runUI (view app state') freshCtx2
              in (ctxDrawCommands ctx2, ctx2 { ctxFocusedElement = nextFocus, ctxFocusNext = ctxFocusNext ctx1 })
            Continuous -> (ctxDrawCommands ctx1, ctx1 { ctxFocusedElement = nextFocus })
      render backend drawCalls'
      loop backend app state' nextCtx
