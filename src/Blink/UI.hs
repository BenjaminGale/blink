module Blink.UI
  ( UIContext (..)
  , UIState (..)
  , UI (..)
  , emptyUIState
  , getRect
  , getMousePos
  , layout
  , fillRect
  ) where

import Blink.DrawCall (Colour, DrawCall (..))
import Blink.Geometry (Point, Rectangle)
import Blink.Input    (InputState (..))

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
    let (g,  st') = f ctx st
        (a,  st'') = x ctx st'
    in  (g a, st'')

instance Monad UI where
  return = pure
  UI x >>= f = UI $ \ctx st ->
    let (a,  st') = x ctx st
        UI g = f a
    in  g ctx st'

emptyUIState :: UIState
emptyUIState = UIState { drawCalls = [] }

getRect :: UI Rectangle
getRect = UI $ \ctx st -> (drawRect ctx, st)

getMousePos :: UI Point
getMousePos = UI $ \ctx st -> (mousePosition (inputState ctx), st)

layout :: Rectangle -> UI a -> UI a
layout r (UI f) = UI $ \ctx st ->
  let (a, st') = f (ctx { drawRect = r }) st
  in  (a, st')

fillRect :: Colour -> UI ()
fillRect colour = UI $ \ctx st ->
  let call = FillRect (drawRect ctx) colour
  in  ((), st { drawCalls = drawCalls st ++ [call] })
