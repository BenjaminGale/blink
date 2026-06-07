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
import Data.Maybe (isNothing)

applyHover :: (Eq e, Ord e) => e -> Rectangle -> UI e c Bool
applyHover eid bgRect = do
  isHit <- layout bgRect regionHit
  when isHit $ UI $ \ctx -> ((), ctx { ctxHoveredElement = Just eid })
  return isHit

consumeTabKey :: UI e c ()
consumeTabKey = UI $ \ctx ->
  let input = ctxInput ctx
      filtered = filter (\e -> key e /= KeyTab) (keyEvents input)
  in ((), ctx { ctxInput = input { keyEvents = filtered } })

applyFocus :: (Eq e, Ord e) => e -> Bool -> UI e c ()
applyFocus eid isHit = do
  currentFocus <- getFocus
  when (isNothing currentFocus) $ setFocus eid
  currentFocus' <- getFocus
  setFocusWhen (currentFocus' == Just eid) eid
  btn <- getLeftButton
  setFocusWhen (isHit && btn == ButtonReleased) eid

applyTabNavigation :: (Eq e, Ord e) => e -> UI e c ()
applyTabNavigation eid = do
  hasFocus <- (== Just eid) <$> getFocus
  input <- getInput
  let tabPressed = any (\e -> key e == KeyTab && Shift `notElem` modifiers e) (keyEvents input)
      shiftTabPressed = any (\e -> key e == KeyTab && Shift `elem` modifiers e) (keyEvents input)
  when (hasFocus && tabPressed) $ do
    clearFocus
    consumeTabKey
  prevCtrl <- UI $ \ctx -> (ctxPreviousControl ctx, ctx)
  when (hasFocus && shiftTabPressed) $ do
    mapM_ setFocus prevCtrl
    consumeTabKey

control :: (Eq e, Ord e) => e -> UI e c () -> UI e c ()
control eid content = do
  s <- getStyle eid
  r <- getRect
  let bgRect = insetRect (margin s) r
      contentRect = insetRect (padding s) bgRect
  isHit <- applyHover eid bgRect
  applyFocus eid isHit
  applyTabNavigation eid
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
