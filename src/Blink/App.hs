{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.App
  ( Backend (..)
  , FrameMode (..)
  , App (..)
  , runApp
  ) where

import Blink.Rendering (DrawCommand)
import Blink.Geometry (Point (..), Size (..), rectOrigin, resizeRect)
import Blink.Input (ButtonState (..), InputState (..))
import Blink.Style (Theme)
import Blink.UI (FocusState (..), UI, UIContext (..), emptyUIContext, nextFrameContext, runUI, getDrawCommands, getCommands)
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
        rectOrigin
        (InputState (Point 0 0) ButtonUp [] [])
        (Blink.App.theme app)
  loop backend app state initialCtx

loop :: Backend -> App e s c -> s -> UIContext e c -> IO ()
loop backend app state ctx = do
  events <- collectEvents backend
  close <- shouldClose backend
  unless close $ do
      size <- windowSize backend
      let winRect = resizeRect size rectOrigin
          processedCtx = snd $ runUI (view app state) (nextFrameContext winRect events ctx)
          focusState = ctxFocusState processedCtx
          nextFocus = if focusedThisFrame focusState then focusedElement focusState else Nothing
          state' = execCommands (update app) (getCommands processedCtx) state
          (drawCalls', nextCtx) = case frameMode backend of
            EventDriven ->
              let freshCtx = (nextFrameContext winRect (events { keyEvents = [], typedText = [] }) processedCtx)
                    { ctxFocusState = focusState { focusedElement = nextFocus } }
                  renderedCtx = snd $ runUI (view app state') freshCtx
              in (getDrawCommands renderedCtx, renderedCtx { ctxFocusState = focusState { focusedElement = nextFocus } })
            Continuous -> (getDrawCommands processedCtx, processedCtx { ctxFocusState = focusState { focusedElement = nextFocus } })
      render backend drawCalls'
      loop backend app state' nextCtx
