{-# LANGUAGE DisambiguateRecordFields #-}
module Blink.Controls
  ( control
  , button
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Blink.Geometry (Rectangle, insetRect)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..))
import Blink.UI

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
  let tabPressed = any (\e -> key e == KeyTab && Shift `notElem` modifiers e) (keyEvents input)
      shiftTabPressed = any (\e -> key e == KeyTab && Shift `elem` modifiers e) (keyEvents input)
  when (hasFocus && tabPressed && not claimedFromTab) $ do
    clearFocus
    UI $ \ctx -> ((), ctx { ctxFocusNext = True })
  prevCtrl <- UI $ \ctx -> (ctxPreviousControl ctx, ctx)
  when (hasFocus && shiftTabPressed && not claimedFromTab) $
    mapM_ setFocus prevCtrl

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
  let wasClicked = isHit && btn == ButtonReleased
      activated = hasFocus && any (\e -> key e == KeyReturn) (keyEvents input)
  return (wasClicked || activated)
