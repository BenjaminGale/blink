module Blink.UI
  ( UIContext (..)
  , UIState (..)
  , UI (..)
  , emptyUIState
  , getRect
  , getMousePos
  , getLeftButton
  , getHovered
  , layout
  , fillRect
  , drawText
  , regionHit
  , control
  , button
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Blink.DrawCall (Colour (..), DrawCall (..))
import Blink.Geometry (Point, Rectangle, containsPoint)
import Blink.Input (ButtonState (..), InputState (..))

data UIContext e = UIContext
  { drawRect :: Rectangle
  , inputState :: InputState
  }

data UIState e = UIState
  { drawCalls :: [DrawCall]
  , hoveredElement :: Maybe e
  }

newtype UI e a = UI { runUI :: UIContext e -> UIState e -> (a, UIState e) }

instance Functor (UI e) where
  fmap f (UI g) = UI $ \ctx st ->
    let (a, st') = g ctx st
    in (f a, st')

instance Applicative (UI e) where
  pure a = UI $ \_ st -> (a, st)
  UI f <*> UI x = UI $ \ctx st ->
    let (g, st') = f ctx st
        (a, st'') = x ctx st'
    in (g a, st'')

instance Monad (UI e) where
  return = pure
  UI x >>= f = UI $ \ctx st ->
    let (a, st') = x ctx st
        UI g = f a
    in g ctx st'

emptyUIState :: UIState e
emptyUIState = UIState { drawCalls = [], hoveredElement = Nothing }

getRect :: UI e Rectangle
getRect = UI $ \ctx st -> (drawRect ctx, st)

getMousePos :: UI e Point
getMousePos = UI $ \ctx st -> (mousePosition (inputState ctx), st)

getLeftButton :: UI e ButtonState
getLeftButton = UI $ \ctx st -> (leftButton (inputState ctx), st)

getHovered :: UI e (Maybe e)
getHovered = UI $ \_ st -> (hoveredElement st, st)

layout :: Rectangle -> UI e a -> UI e a
layout r (UI f) = UI $ \ctx st ->
  let (a, st') = f (ctx { drawRect = r }) st
  in (a, st')

fillRect :: Colour -> UI e ()
fillRect colour = UI $ \ctx st ->
  let call = FillRect (drawRect ctx) colour
  in ((), st { drawCalls = drawCalls st ++ [call] })

drawText :: Text -> UI e ()
drawText text = UI $ \ctx st ->
  let call = DrawText (drawRect ctx) text
  in ((), st { drawCalls = drawCalls st ++ [call] })

regionHit :: UI e Bool
regionHit = do
  r <- getRect
  containsPoint r <$> getMousePos

control :: Eq e => e -> UI e ()
control eid = do
  hit <- regionHit
  when hit $ do
    UI $ \_ st -> ((), st { hoveredElement = Just eid })

button :: Eq e => e -> Text -> UI e Bool
button eid label = do
  control eid
  hovered <- (== Just eid) <$> getHovered
  btn <- getLeftButton
  let pressed = hovered && btn == ButtonDown
      colour
        | pressed = RGBA 0.7 0.2 0.1 1
        | hovered = RGBA 1 0.4 0.2 1
        | otherwise = RGBA 0.4 0.4 0.4 1
  fillRect colour
  drawText label
  return pressed
