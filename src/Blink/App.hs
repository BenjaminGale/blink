{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.App
  ( Backend (..)
  , FrameMode (..)
  , App (..)
  , runApp
  ) where

import Blink.Rendering (DrawCommand)
import Blink.Geometry (Rectangle (..), Size (..))
import Blink.Input (InputState (..))
import Blink.Style (Theme)
import Blink.UI (UI, UIContext (..), emptyUIContext, runUI)
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
  loop backend app state Nothing False Nothing

loop :: Backend -> App e s c -> s -> Maybe e -> Bool -> Maybe e -> IO ()
loop backend app state prevFocus prevFocusNext prevPrevCtrl = do
  events <- collectEvents backend
  close <- shouldClose backend

  unless close $ do
      size <- windowSize backend
      let winRect = Rectangle 0 0 (sizeWidth size) (sizeHeight size)
          appTheme = Blink.App.theme app
          freshCtx = (emptyUIContext winRect events appTheme)
            { ctxFocusedElement = prevFocus
            , ctxFocusNext = prevFocusNext
            , ctxPreviousControl = prevPrevCtrl
            }
          (_, ctx1) = runUI (view app state) freshCtx
          nextFocus = if ctxFocusedRendered ctx1 then ctxFocusedElement ctx1 else Nothing
          state' = execCommands (update app) (ctxPendingCommands ctx1) state
          freshCtx2 = (emptyUIContext winRect (events { keyEvents = [] }) appTheme)
            { ctxFocusedElement = nextFocus
            , ctxPreviousControl = ctxPreviousControl ctx1
            }
          (drawCalls', prevCtrl') = case frameMode backend of
            EventDriven ->
              let (_, ctx2) = runUI (view app state') freshCtx2
              in (ctxDrawCommands ctx2, ctxPreviousControl ctx2)
            Continuous -> (ctxDrawCommands ctx1, ctxPreviousControl ctx1)
      render backend drawCalls'
      loop backend app state' nextFocus (ctxFocusNext ctx1) prevCtrl'
