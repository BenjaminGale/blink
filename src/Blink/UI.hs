{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.UI
  ( UIContext (..)
  , UIState (..)
  , UI (..)
  , ControlState (..)
  , emptyUIState
  , getRect
  , getMousePos
  , getLeftButton
  , getHovered
  , getFocus
  , setFocus
  , clearFocus
  , getInput
  , getStyleSet
  , getStyle
  , layout
  , fillRect
  , drawText
  , clipToCurrent
  , regionHit
  , control
  , button
  , emitCommand
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.DrawCall (Colour (..), TextAlign (..), DrawCall (..))
import Blink.Geometry (Point, Rectangle, insetRect, containsPoint)
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

getStyleSet :: Ord e => e -> UI e c StyleSet
getStyleSet eid = do
  t <- UI $ \ctx st -> (uiTheme ctx, st)
  return $ Map.findWithDefault (defaultStyle t) eid (elementStyles t)

getStyle :: Ord e => e -> UI e c Style
getStyle eid = do
  StyleSet { normal = n, hovered = h, pressed = p, focused = f, disabled = _ } <- getStyleSet eid
  isHov <- (== Just eid) <$> getHovered
  isFoc <- (== Just eid) <$> getFocus
  btn <- getLeftButton
  let isPrs = isHov && btn == ButtonDown
  return $ if isPrs then p
           else if isHov then h
           else if isFoc then f
           else n

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

drawText :: Colour -> TextAlign -> Text -> UI e c ()
drawText colour align text = UI $ \ctx st ->
  let call = DrawText (drawRect ctx) text colour align
  in ((), st { drawCalls = drawCalls st ++ [call] })

clipToCurrent :: UI e c a -> UI e c a
clipToCurrent action = do
  r <- getRect
  UI $ \_ st -> ((), st { drawCalls = drawCalls st ++ [PushClip r] })
  result <- action
  UI $ \_ st -> ((), st { drawCalls = drawCalls st ++ [PopClip] })
  return result

emitCommand :: c -> UI e c ()
emitCommand cmd = UI $ \_ st ->
  ((), st { pendingCommands = pendingCommands st ++ [cmd] })

regionHit :: UI e c Bool
regionHit = do
  r <- getRect
  containsPoint r <$> getMousePos

data ControlState = ControlState
  { isClicked :: Bool
  , isPressed :: Bool
  , isHovered :: Bool
  , isFocused :: Bool
  }

control :: (Eq e, Ord e) => e -> UI e c () -> UI e c ControlState
control eid content = do
  ss <- getStyleSet eid
  r <- getRect
  let bgRect = insetRect (margin (normal ss)) r
      contentRect = insetRect (padding (normal ss)) bgRect
  next <- UI $ \_ st -> (focusNext st, st)
  currentFocus <- getFocus
  claimedFromTab <-
    if currentFocus == Nothing
    then do
      setFocus eid
      UI $ \_ st -> ((), st { focusedRendered = True, focusNext = False })
      return next
    else return False
  isHit <- layout bgRect regionHit
  btn <- getLeftButton
  let wasPressed = isHit && btn == ButtonDown
      wasClicked = isHit && btn == ButtonReleased
  when isHit $ UI $ \_ st -> ((), st { hoveredElement = Just eid })
  currentFocus' <- getFocus
  when (currentFocus' == Just eid) $
    UI $ \_ st -> ((), st { focusedRendered = True })
  when wasClicked $ do
    setFocus eid
    UI $ \_ st -> ((), st { focusedRendered = True })
  hasFocus <- (== Just eid) <$> getFocus
  input <- getInput
  consumed <- UI $ \_ st -> (tabConsumed st, st)
  let tabPressed = not consumed && any (\e -> key e == KeyTab && Shift `notElem` modifiers e) (keyEvents input)
      shiftTabPressed = not consumed && any (\e -> key e == KeyTab && Shift `elem` modifiers e) (keyEvents input)
      activated = not consumed && hasFocus && any (\e -> key e == KeyReturn) (keyEvents input)
  when (hasFocus && tabPressed && not claimedFromTab) $ do
    clearFocus
    UI $ \_ st -> ((), st { focusNext = True, tabConsumed = True })
  prevCtrl <- UI $ \_ st -> (previousControl st, st)
  when (hasFocus && shiftTabPressed && not claimedFromTab) $ do
    mapM_ setFocus prevCtrl
    UI $ \_ st -> ((), st { tabConsumed = True })
  UI $ \_ st -> ((), st { previousControl = Just eid })
  style <- getStyle eid
  layout bgRect $ fillRect (background style)
  layout contentRect $ clipToCurrent content
  return ControlState
    { isClicked = wasClicked || activated
    , isPressed = wasPressed
    , isHovered = isHit
    , isFocused = hasFocus
    }

button :: (Eq e, Ord e) => e -> Text -> UI e c Bool
button eid label = do
  cs <- control eid $ do
    style <- getStyle eid
    drawText (textColour style) (textAlign style) label
  return (isClicked cs)
