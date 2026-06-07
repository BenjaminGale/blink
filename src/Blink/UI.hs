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
  , getFocus
  , setFocus
  , clearFocus
  , getInput
  , getStyle
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
import qualified Data.Map.Strict as Map
import Blink.DrawCall (Colour (..), DrawCall (..))
import Blink.Geometry (Point, Rectangle, containsPoint)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))

data UIContext e = UIContext
  { drawRect :: Rectangle
  , inputState :: InputState
  , uiTheme :: Theme e
  }

data UIState e c = UIState
  { drawCalls :: [DrawCall]
  , hoveredElement :: Maybe e
  , focusedElement :: Maybe e
  , focusedRendered :: Bool
  , focusNext :: Bool
  , tabConsumed :: Bool
  , previousControl :: Maybe e
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
  , focusedElement = Nothing
  , focusedRendered = False
  , focusNext = False
  , tabConsumed = False
  , previousControl = Nothing
  , pendingCommands = []
  }

getRect :: UI e c Rectangle
getRect = UI $ \ctx st -> (drawRect ctx, st)

getMousePos :: UI e c Point
getMousePos = UI $ \ctx st -> (mousePosition (inputState ctx), st)

getLeftButton :: UI e c ButtonState
getLeftButton = UI $ \ctx st -> (leftButton (inputState ctx), st)

getInput :: UI e c InputState
getInput = UI $ \ctx st -> (inputState ctx, st)

getStyle :: Ord e => e -> UI e c Style
getStyle eid = do
  t <- UI $ \ctx st -> (uiTheme ctx, st)
  isHovered <- (== Just eid) <$> getHovered
  isFocused <- (== Just eid) <$> getFocus
  btn <- getLeftButton
  let ss = Map.findWithDefault (defaultStyle t) eid (elementStyles t)
      isPressed = isHovered && btn == ButtonDown
  return $ if isPressed     then pressed  ss
           else if isHovered then hovered  ss
           else if isFocused then focused  ss
           else                   normal   ss

getHovered :: UI e c (Maybe e)
getHovered = UI $ \_ st -> (hoveredElement st, st)

getFocus :: UI e c (Maybe e)
getFocus = UI $ \_ st -> (focusedElement st, st)

setFocus :: e -> UI e c ()
setFocus eid = UI $ \_ st -> ((), st { focusedElement = Just eid })

clearFocus :: UI e c ()
clearFocus = UI $ \_ st -> ((), st { focusedElement = Nothing })

layout :: Rectangle -> UI e c a -> UI e c a
layout r (UI f) = UI $ \ctx st ->
  let (a, st') = f (ctx { drawRect = r }) st
  in (a, st')

fillRect :: Colour -> UI e c ()
fillRect colour = UI $ \ctx st ->
  let call = FillRect (drawRect ctx) colour
  in ((), st { drawCalls = drawCalls st ++ [call] })

drawText :: Colour -> Text -> UI e c ()
drawText colour text = UI $ \ctx st ->
  let call = DrawText (drawRect ctx) text colour
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
  next <- UI $ \_ st -> (focusNext st, st)
  currentFocus <- getFocus
  claimedFromTab <-
    if currentFocus == Nothing
    then do
      setFocus eid
      UI $ \_ st -> ((), st { focusedRendered = True, focusNext = False })
      return next
    else return False
  hit <- regionHit
  btn <- getLeftButton
  let isClicked = hit && btn == ButtonReleased
  when hit $ UI $ \_ st -> ((), st { hoveredElement = Just eid })
  currentFocus' <- getFocus
  when (currentFocus' == Just eid) $
    UI $ \_ st -> ((), st { focusedRendered = True })
  when isClicked $ do
    setFocus eid
    UI $ \_ st -> ((), st { focusedRendered = True })
  isFocused <- (== Just eid) <$> getFocus
  input <- getInput
  consumed <- UI $ \_ st -> (tabConsumed st, st)
  let tabPressed = not consumed && any (\e -> key e == KeyTab && Shift `notElem` modifiers e) (keyEvents input)
      shiftTabPressed = not consumed && any (\e -> key e == KeyTab && Shift `elem` modifiers e) (keyEvents input)
  when (isFocused && tabPressed && not claimedFromTab) $ do
    clearFocus
    UI $ \_ st -> ((), st { focusNext = True, tabConsumed = True })
  prevCtrl <- UI $ \_ st -> (previousControl st, st)
  when (isFocused && shiftTabPressed && not claimedFromTab) $ do
    mapM_ setFocus prevCtrl
    UI $ \_ st -> ((), st { tabConsumed = True })
  UI $ \_ st -> ((), st { previousControl = Just eid })

button :: (Eq e, Ord e) => e -> Text -> UI e c Bool
button eid label = do
  control eid
  style <- getStyle eid
  isHovered <- (== Just eid) <$> getHovered
  isFocused <- (== Just eid) <$> getFocus
  btn <- getLeftButton
  input <- getInput
  let activated = any (\e -> key e == KeyReturn) (keyEvents input)
      clicked = (isHovered && btn == ButtonReleased) || (isFocused && activated)
  fillRect (background style)
  drawText (textColour style) label
  return clicked
