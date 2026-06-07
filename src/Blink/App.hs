{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.App
  ( Backend (..)
  , FrameMode (..)
  , App (..)
  , runApp
  ) where

import Blink.DrawCall (DrawCall)
import Blink.Geometry (Rectangle (..), Size (..))
import Blink.Input (InputState (..))
import Blink.Style (Theme)
import Blink.UI (UI, UIContext (..), UIState (..), emptyUIState, runUI, drawCalls, pendingCommands, focusedElement, focusedRendered, focusNext, previousControl)
import Blink.Update (Update, execCommands)

data FrameMode = EventDriven | Continuous

data Backend = Backend
  { collectEvents :: IO InputState
  , shouldClose :: IO Bool
  , windowSize :: IO Size
  , render :: [DrawCall] -> IO ()
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
  input <- collectEvents backend
  close <- shouldClose backend
  if close
    then return ()
    else do
      size <- windowSize backend
      let winRect = Rectangle 0 0 (sizeWidth size) (sizeHeight size)
          appTheme = Blink.App.theme app
          ctx = UIContext { drawRect = winRect, inputState = input, uiTheme = appTheme }
          freshSt = emptyUIState
            { focusedElement = prevFocus
            , focusNext = prevFocusNext
            , previousControl = prevPrevCtrl
            }
          (_, uiSt1) = runUI (view app state) ctx freshSt
          nextFocus = if focusedRendered uiSt1 then focusedElement uiSt1 else Nothing
          state' = execCommands (update app) (pendingCommands uiSt1) state
          ctx2 = ctx { inputState = (inputState ctx) { keyEvents = [] } }
          freshSt2 = emptyUIState
            { focusedElement = nextFocus
            , previousControl = previousControl uiSt1
            }
          (drawCalls', prevCtrl') = case frameMode backend of
            EventDriven ->
              let (_, uiSt2) = runUI (view app state') ctx2 freshSt2
              in (drawCalls uiSt2, previousControl uiSt2)
            Continuous -> (drawCalls uiSt1, previousControl uiSt1)
      render backend drawCalls'
      loop backend app state' nextFocus (focusNext uiSt1) prevCtrl'
