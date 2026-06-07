module Blink.App
  ( Backend (..)
  , App (..)
  , runApp
  ) where

import Blink.DrawCall (DrawCall)
import Blink.Geometry (Rectangle (..), Size (..))
import Blink.Input (InputState)
import Blink.UI (UI, UIContext (..), emptyUIState, runUI, drawCalls, pendingCommands)
import Blink.Update (Update, execCommands)

data Backend = Backend
  { shouldClose :: IO Bool
  , pollInput   :: IO InputState
  , windowSize  :: IO Size
  , render      :: [DrawCall] -> IO ()
  }

data App e s c = App
  { startUp :: IO s
  , view    :: s -> UI e c ()
  , update  :: c -> s -> Update s c ()
  }

runApp :: Backend -> App e s c -> IO ()
runApp backend app = do
  state <- startUp app
  loop backend app state

loop :: Backend -> App e s c -> s -> IO ()
loop backend app state = do
  close <- shouldClose backend
  if close
    then return ()
    else do
      size <- windowSize backend
      input <- pollInput backend
      let winRect = Rectangle 0 0 (sizeWidth size) (sizeHeight size)
          ctx = UIContext { drawRect = winRect, inputState = input }
          (_, uiSt) = runUI (view app state) ctx emptyUIState
          state' = execCommands (update app) (pendingCommands uiSt) state
      render backend (drawCalls uiSt)
      loop backend app state'
