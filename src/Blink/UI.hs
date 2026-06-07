module Blink.UI
  ( UIContext (..)
  , UIState (..)
  , UI (..)
  , ControlState (..)
  , emptyUIState
  , getRect
  , getMousePos
  , getLeftButton
  , layout
  , fillRect
  , regionHit
  , control
  , button
  ) where

import Blink.DrawCall (Colour (..), DrawCall (..))
import Blink.Geometry (Point, Rectangle, containsPoint)
import Blink.Input    (ButtonState (..), InputState (..))

data UIContext = UIContext
  { drawRect :: Rectangle
  , inputState :: InputState
  }

data UIState = UIState
  { drawCalls :: [DrawCall]
  }

newtype UI a = UI { runUI :: UIContext -> UIState -> (a, UIState) }

instance Functor UI where
  fmap f (UI g) = UI $ \ctx st ->
    let (a, st') = g ctx st
    in  (f a, st')

instance Applicative UI where
  pure a = UI $ \_ st -> (a, st)
  UI f <*> UI x = UI $ \ctx st ->
    let (g, st') = f ctx st
        (a, st'') = x ctx st'
    in  (g a, st'')

instance Monad UI where
  return = pure
  UI x >>= f = UI $ \ctx st ->
    let (a, st') = x ctx st
        UI g = f a
    in  g ctx st'

data ControlState = ControlState
  { isHovered :: Bool
  , isPressed :: Bool
  }

emptyUIState :: UIState
emptyUIState = UIState { drawCalls = [] }

getRect :: UI Rectangle
getRect = UI $ \ctx st -> (drawRect ctx, st)

getMousePos :: UI Point
getMousePos = UI $ \ctx st -> (mousePosition (inputState ctx), st)

getLeftButton :: UI ButtonState
getLeftButton = UI $ \ctx st -> (leftButton (inputState ctx), st)

layout :: Rectangle -> UI a -> UI a
layout r (UI f) = UI $ \ctx st ->
  let (a, st') = f (ctx { drawRect = r }) st
  in  (a, st')

fillRect :: Colour -> UI ()
fillRect colour = UI $ \ctx st ->
  let call = FillRect (drawRect ctx) colour
  in  ((), st { drawCalls = drawCalls st ++ [call] })

regionHit :: UI Bool
regionHit = do
  r <- getRect
  mouse <- getMousePos
  return (containsPoint r mouse)

control :: UI () -> UI ControlState
control content = do
  hit <- regionHit
  btn <- getLeftButton
  let cs = ControlState hit (hit && btn == ButtonDown)
  content
  return cs

button :: UI () -> UI Bool
button content = do
  cs <- control content
  let colour
        | isPressed cs = RGBA 0.7 0.2 0.1 1
        | isHovered cs = RGBA 1 0.4 0.2 1
        | otherwise = RGBA 0.4 0.4 0.4 1
  fillRect colour
  return (isPressed cs)
