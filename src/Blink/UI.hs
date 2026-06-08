{-# LANGUAGE DisambiguateRecordFields #-}

module Blink.UI
  ( FocusState (..)
  , UIContext (..)
  , UI (..)
  , emptyUIContext
  , nextFrameContext
  , getRect
  , getMousePos
  , getLeftButton
  , getHovered
  , setHovered
  , getFocus
  , isFocused
  , setFocus
  , setFocusWhen
  , clearFocus
  , getInput
  , consumeKey
  , getPreviousControl
  , setPreviousControl
  , getStyleSet
  , getStyle
  , layout
  , fillRect
  , strokeRect
  , drawText
  , clipToCurrent
  , regionHit
  , emitCommand
  , control
  , getDrawCommands
  ) where

import Control.Monad (when)
import Data.List (find)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Geometry (Point, Rectangle, containsPoint, insetRect)
import Blink.Input (ButtonState (..), Key (..), Modifier (..), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))

-- | Tracks which element holds keyboard focus and whether it was visited
-- during the current frame's render pass.
data FocusState e = FocusState
  { focusedElement   :: Maybe e
    -- ^ The element that currently holds focus, or 'Nothing' if no element is focused.
  , focusedThisFrame :: Bool
    -- ^ 'True' if the focused element was encountered during this frame's render pass.
    -- Used to clear stale focus when a focused element is no longer present in the UI.
  }

data UIContext e c = UIContext
  { ctxBounds :: Rectangle
  , ctxInput :: InputState
  , ctxTheme :: Theme e
  , ctxDrawCommands :: [DrawCommand]
  , ctxHoveredElement :: Maybe e
  , ctxFocusState :: FocusState e
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
  , ctxDrawCommands = []
  , ctxHoveredElement = Nothing
  , ctxFocusState = FocusState { focusedElement = Nothing, focusedThisFrame = False }
  , ctxPreviousControl = Nothing
  , ctxPendingCommands = []
  }

nextFrameContext :: Rectangle -> InputState -> UIContext e c -> UIContext e c
nextFrameContext bounds input ctx = ctx
  { ctxBounds          = bounds
  , ctxInput           = input
  , ctxDrawCommands    = []
  , ctxHoveredElement  = Nothing
  , ctxFocusState      = (ctxFocusState ctx) { focusedThisFrame = False }
  , ctxPendingCommands = []
  }

gets :: (UIContext e c -> a) -> UI e c a
gets f = UI $ \ctx -> (f ctx, ctx)

modify :: (UIContext e c -> UIContext e c) -> UI e c ()
modify f = UI $ \ctx -> ((), f ctx)

getRect :: UI e c Rectangle
getRect = gets ctxBounds

getMousePos :: UI e c Point
getMousePos = mousePosition <$> getInput

getLeftButton :: UI e c ButtonState
getLeftButton = leftButton <$> getInput

getInput :: UI e c InputState
getInput = gets ctxInput

consumeKey :: Key -> UI e c ()
consumeKey k = modify $ \ctx ->
  let input = ctxInput ctx
  in ctx { ctxInput = input { keyEvents = filter (\e -> key e /= k) (keyEvents input) } }

getPreviousControl :: UI e c (Maybe e)
getPreviousControl = gets ctxPreviousControl

setPreviousControl :: e -> UI e c ()
setPreviousControl eid = modify $ \ctx -> ctx { ctxPreviousControl = Just eid }

getTheme :: UI e c (Theme e)
getTheme = gets ctxTheme

getStyleSet :: Ord e => e -> UI e c StyleSet
getStyleSet eid = do
  t <- getTheme
  pure $ Map.findWithDefault (defaultStyle t) eid (elementStyles t)

getStyle :: Ord e => e -> UI e c Style
getStyle eid = do
  styles <- getStyleSet eid
  isHov <- (== Just eid) <$> getHovered
  isFoc <- isFocused eid
  isPrs <- (&& isHov) . (== ButtonDown) <$> getLeftButton

  pure $
    if isPrs then pressed styles
    else if isHov then hovered styles
    else if isFoc then focused styles
    else normal styles

getHovered :: UI e c (Maybe e)
getHovered = gets ctxHoveredElement

setHovered :: e -> UI e c ()
setHovered eid = modify $ \ctx -> ctx { ctxHoveredElement = Just eid }

getFocus :: UI e c (Maybe e)
getFocus = gets (focusedElement . ctxFocusState)

isFocused :: Eq e => e -> UI e c Bool
isFocused eid = (== Just eid) <$> getFocus

setFocus :: e -> UI e c ()
setFocus eid = modify $ \ctx -> ctx { ctxFocusState = FocusState { focusedElement = Just eid, focusedThisFrame = True } }

setFocusWhen :: Bool -> e -> UI e c ()
setFocusWhen True eid  = setFocus eid
setFocusWhen False _   = pure ()

clearFocus :: UI e c ()
clearFocus = modify $ \ctx -> ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = Nothing } }

layout :: Rectangle -> UI e c a -> UI e c a
layout r (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxBounds = r })
  in (a, ctx' { ctxBounds = ctxBounds ctx })

emit :: DrawCommand -> UI e c ()
emit cmd = modify $ \ctx -> ctx { ctxDrawCommands = cmd : ctxDrawCommands ctx }

fillRect :: Colour -> UI e c ()
fillRect colour = do
  r <- getRect
  emit $ FillRect r colour

strokeRect :: Colour -> Double -> UI e c ()
strokeRect colour width = do
  r <- getRect
  emit $ StrokeRect r colour width

drawText :: Colour -> TextAlign -> Text -> UI e c ()
drawText colour align text = do
  r <- getRect
  emit $ DrawText r text colour align

clipToCurrent :: UI e c a -> UI e c a
clipToCurrent action = do
  r <- getRect
  emit $ PushClip r
  result <- action
  emit PopClip
  return result

emitCommand :: c -> UI e c ()
emitCommand cmd = UI $ \ctx ->
  ((), ctx { ctxPendingCommands = ctxPendingCommands ctx ++ [cmd] })

getDrawCommands :: UIContext e c -> [DrawCommand]
getDrawCommands = reverse . ctxDrawCommands

regionHit :: UI e c Bool
regionHit = do
  r <- getRect
  containsPoint r <$> getMousePos

applyHover :: (Eq e, Ord e) => e -> Rectangle -> UI e c ()
applyHover eid bgRect = do
  isHit <- layout bgRect regionHit
  when isHit $ setHovered eid

applyFocus :: (Eq e, Ord e) => e -> UI e c ()
applyFocus eid = do
  currentFocus <- getFocus
  isHit        <- (== Just eid) <$> getHovered
  btn          <- getLeftButton
  let nothingIsFocused  = isNothing currentFocus
      isRequestingFocus = currentFocus == Just eid
      wasClicked        = isHit && btn == ButtonReleased
  setFocusWhen (nothingIsFocused || isRequestingFocus || wasClicked) eid

applyTabNavigation :: (Eq e, Ord e) => e -> UI e c ()
applyTabNavigation eid = do
  hasFocus <- isFocused eid
  input    <- getInput
  prevCtrl <- getPreviousControl
  let tabKey          = find (\e -> key e == KeyTab) (keyEvents input)
      tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
      shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
  when (hasFocus && tabPressed) $ do
    clearFocus
    consumeKey KeyTab
  when (hasFocus && shiftTabPressed && isJust prevCtrl) $ do
    setFocus (fromJust prevCtrl)
    consumeKey KeyTab

control :: (Eq e, Ord e) => e -> UI e c () -> UI e c ()
control eid content = do
  s <- getStyle eid
  r <- getRect
  let bgRect      = insetRect (margin s) r
      contentRect = insetRect (padding s) bgRect
  applyHover eid bgRect
  applyFocus eid
  applyTabNavigation eid
  setPreviousControl eid
  style <- getStyle eid
  layout bgRect $ fillRect (background style)
  case borderColour style of
    Just c  -> layout bgRect $ strokeRect c (borderWidth style)
    Nothing -> pure ()
  layout contentRect $ clipToCurrent content

