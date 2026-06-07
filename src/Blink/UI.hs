{-# LANGUAGE OverloadedStrings #-}
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
  , emitCommand
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

data UIState e c = UIState
  { drawCalls :: [DrawCall]
  , hoveredElement :: Maybe e
  , pendingCommands :: [c]
  }

newtype UI e c a = UI { runUI :: UIContext e -> UIState e c -> (a, UIState e c) }

instance Functor (UI e c) where
  fmap f (UI g) = UI $ \ctx st ->
    let (a, st') = g ctx st
    in (f a, st')

instance Applicative (UI e c) where
  pure a = UI $ \_ st -> (a, st)
  UI f <*> UI x = UI $ \ctx st ->
    let (g, st') = f ctx st
        (a, st'') = x ctx st'
    in (g a, st'')

instance Monad (UI e c) where
  return = pure
  UI x >>= f = UI $ \ctx st ->
    let (a, st') = x ctx st
        UI g = f a
    in g ctx st'

emptyUIState :: UIState e c
emptyUIState = UIState
  { drawCalls = []
  , hoveredElement = Nothing
  , pendingCommands = []
  }

getRect :: UI e c Rectangle
getRect = UI $ \ctx st -> (drawRect ctx, st)

getMousePos :: UI e c Point
getMousePos = UI $ \ctx st -> (mousePosition (inputState ctx), st)

getLeftButton :: UI e c ButtonState
getLeftButton = UI $ \ctx st -> (leftButton (inputState ctx), st)

getHovered :: UI e c (Maybe e)
getHovered = UI $ \_ st -> (hoveredElement st, st)

layout :: Rectangle -> UI e c a -> UI e c a
layout r (UI f) = UI $ \ctx st ->
  let (a, st') = f (ctx { drawRect = r }) st
  in (a, st')

fillRect :: Colour -> UI e c ()
fillRect colour = UI $ \ctx st ->
  let call = FillRect (drawRect ctx) colour
  in ((), st { drawCalls = drawCalls st ++ [call] })

drawText :: Text -> UI e c ()
drawText text = UI $ \ctx st ->
  let call = DrawText (drawRect ctx) text
  in ((), st { drawCalls = drawCalls st ++ [call] })

emitCommand :: c -> UI e c ()
emitCommand cmd = UI $ \_ st ->
  ((), st { pendingCommands = pendingCommands st ++ [cmd] })

regionHit :: UI e c Bool
regionHit = do
  r <- getRect
  containsPoint r <$> getMousePos

control :: Eq e => e -> UI e c ()
control eid = do
  hit <- regionHit
  when hit $ UI $ \_ st -> ((), st { hoveredElement = Just eid })

button :: Eq e => e -> Text -> UI e c Bool
button eid label = do
  control eid
  hovered <- (== Just eid) <$> getHovered
  btn <- getLeftButton
  let pressed = hovered && btn == ButtonDown
      clicked = hovered && btn == ButtonReleased
      colour
        | pressed = RGBA 0.7 0.2 0.1 1
        | hovered = RGBA 1 0.4 0.2 1
        | otherwise = RGBA 0.4 0.4 0.4 1
  fillRect colour
  drawText label
  return clicked
