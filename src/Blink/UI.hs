{-# LANGUAGE DisambiguateRecordFields #-}

module Blink.UI
  ( UIContext (..)
  , UI (..)
  , emptyUIContext
  , getRect
  , getMousePos
  , getLeftButton
  , getHovered
  , getFocus
  , setFocus
  , clearFocus
  , getInput
  , getTabConsumed
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

data UIContext e c = UIContext
  { ctxBounds :: Rectangle
  , ctxInput :: InputState
  , ctxTheme :: Theme e
  , ctxDrawCalls :: [DrawCall]
  , ctxHoveredElement :: Maybe e
  , ctxFocusedElement :: Maybe e
  , ctxFocusedRendered :: Bool
  , ctxFocusNext :: Bool
  , ctxTabConsumed :: Bool
  , ctxPreviousControl :: Maybe e
  , ctxPendingCommands :: [c]
  }

newtype UI e c a = UI { runUI :: UIContext e c -> (a, UIContext e c) }

instance Functor (UI e c) where
  fmap f (UI g) = UI $ \ctx ->
    let (a, ctx') = g ctx
    in (f a, ctx')

instance Applicative (UI e c) where
  pure a = UI $ \ctx -> (a, ctx)
  UI f <*> UI x = UI $ \ctx ->
    let (g, ctx') = f ctx
        (a, ctx'') = x ctx'
    in (g a, ctx'')

instance Monad (UI e c) where
  return = pure
  UI x >>= f = UI $ \ctx ->
    let (a, ctx') = x ctx
        UI g = f a
    in g ctx'

emptyUIContext :: Rectangle -> InputState -> Theme e -> UIContext e c
emptyUIContext bounds input thm = UIContext
  { ctxBounds = bounds
  , ctxInput = input
  , ctxTheme = thm
  , ctxDrawCalls = []
  , ctxHoveredElement = Nothing
  , ctxFocusedElement = Nothing
  , ctxFocusedRendered = False
  , ctxFocusNext = False
  , ctxTabConsumed = False
  , ctxPreviousControl = Nothing
  , ctxPendingCommands = []
  }

getRect :: UI e c Rectangle
getRect = UI $ \ctx -> (ctxBounds ctx, ctx)

getMousePos :: UI e c Point
getMousePos = UI $ \ctx -> (mousePosition (ctxInput ctx), ctx)

getLeftButton :: UI e c ButtonState
getLeftButton = UI $ \ctx -> (leftButton (ctxInput ctx), ctx)

getInput :: UI e c InputState
getInput = UI $ \ctx -> (ctxInput ctx, ctx)

getTabConsumed :: UI e c Bool
getTabConsumed = UI $ \ctx -> (ctxTabConsumed ctx, ctx)

getStyleSet :: Ord e => e -> UI e c StyleSet
getStyleSet eid = do
  t <- UI $ \ctx -> (ctxTheme ctx, ctx)
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
getHovered = UI $ \ctx -> (ctxHoveredElement ctx, ctx)

getFocus :: UI e c (Maybe e)
getFocus = UI $ \ctx -> (ctxFocusedElement ctx, ctx)

setFocus :: e -> UI e c ()
setFocus eid = UI $ \ctx -> ((), ctx { ctxFocusedElement = Just eid })

clearFocus :: UI e c ()
clearFocus = UI $ \ctx -> ((), ctx { ctxFocusedElement = Nothing })

layout :: Rectangle -> UI e c a -> UI e c a
layout r (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxBounds = r })
  in (a, ctx' { ctxBounds = ctxBounds ctx })

fillRect :: Colour -> UI e c ()
fillRect colour = UI $ \ctx ->
  let call = FillRect (ctxBounds ctx) colour
  in ((), ctx { ctxDrawCalls = ctxDrawCalls ctx ++ [call] })

drawText :: Colour -> TextAlign -> Text -> UI e c ()
drawText colour align text = UI $ \ctx ->
  let call = DrawText (ctxBounds ctx) text colour align
  in ((), ctx { ctxDrawCalls = ctxDrawCalls ctx ++ [call] })

clipToCurrent :: UI e c a -> UI e c a
clipToCurrent action = do
  r <- getRect
  UI $ \ctx -> ((), ctx { ctxDrawCalls = ctxDrawCalls ctx ++ [PushClip r] })
  result <- action
  UI $ \ctx -> ((), ctx { ctxDrawCalls = ctxDrawCalls ctx ++ [PopClip] })
  return result

emitCommand :: c -> UI e c ()
emitCommand cmd = UI $ \ctx ->
  ((), ctx { ctxPendingCommands = ctxPendingCommands ctx ++ [cmd] })

regionHit :: UI e c Bool
regionHit = do
  r <- getRect
  containsPoint r <$> getMousePos

applyHover :: (Eq e, Ord e) => e -> Rectangle -> UI e c Bool
applyHover eid bgRect = do
  isHit <- layout bgRect regionHit
  when isHit $ UI $ \ctx -> ((), ctx { ctxHoveredElement = Just eid })
  return isHit

applyFocus :: (Eq e, Ord e) => e -> Bool -> UI e c Bool
applyFocus eid isHit = do
  next <- UI $ \ctx -> (ctxFocusNext ctx, ctx)
  currentFocus <- getFocus
  claimedFromTab <-
    if currentFocus == Nothing
    then do
      setFocus eid
      UI $ \ctx -> ((), ctx { ctxFocusedRendered = True, ctxFocusNext = False })
      return next
    else return False
  currentFocus' <- getFocus
  when (currentFocus' == Just eid) $
    UI $ \ctx -> ((), ctx { ctxFocusedRendered = True })
  btn <- getLeftButton
  when (isHit && btn == ButtonReleased) $ do
    setFocus eid
    UI $ \ctx -> ((), ctx { ctxFocusedRendered = True })
  return claimedFromTab

applyTabNavigation :: (Eq e, Ord e) => e -> Bool -> UI e c ()
applyTabNavigation eid claimedFromTab = do
  hasFocus <- (== Just eid) <$> getFocus
  input <- getInput
  consumed <- getTabConsumed
  let tabPressed = not consumed && any (\e -> key e == KeyTab && Shift `notElem` modifiers e) (keyEvents input)
      shiftTabPressed = not consumed && any (\e -> key e == KeyTab && Shift `elem` modifiers e) (keyEvents input)
  when (hasFocus && tabPressed && not claimedFromTab) $ do
    clearFocus
    UI $ \ctx -> ((), ctx { ctxFocusNext = True, ctxTabConsumed = True })
  prevCtrl <- UI $ \ctx -> (ctxPreviousControl ctx, ctx)
  when (hasFocus && shiftTabPressed && not claimedFromTab) $ do
    mapM_ setFocus prevCtrl
    UI $ \ctx -> ((), ctx { ctxTabConsumed = True })

control :: (Eq e, Ord e) => e -> UI e c () -> UI e c ()
control eid content = do
  s <- getStyle eid
  r <- getRect
  let bgRect = insetRect (margin s) r
      contentRect = insetRect (padding s) bgRect
  isHit <- applyHover eid bgRect
  claimedFromTab <- applyFocus eid isHit
  applyTabNavigation eid claimedFromTab
  UI $ \ctx -> ((), ctx { ctxPreviousControl = Just eid })
  style <- getStyle eid
  layout bgRect $ fillRect (background style)
  layout contentRect $ clipToCurrent content

button :: (Eq e, Ord e) => e -> Text -> UI e c Bool
button eid label = do
  control eid $ do
    style <- getStyle eid
    drawText (textColour style) (textAlign style) label
  isHit <- (== Just eid) <$> getHovered
  hasFocus <- (== Just eid) <$> getFocus
  btn <- getLeftButton
  input <- getInput
  consumed <- getTabConsumed
  let wasClicked = isHit && btn == ButtonReleased
      activated = not consumed && hasFocus && any (\e -> key e == KeyReturn) (keyEvents input)
  return (wasClicked || activated)
