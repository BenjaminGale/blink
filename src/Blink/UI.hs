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
  , drawText
  , clipToCurrent
  , regionHit
  , emitCommand
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Geometry (Point, Rectangle, containsPoint)
import Blink.Input (ButtonState (..), Key (..), KeyEvent (..), InputState (..))
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

fillRect :: Colour -> UI e c ()
fillRect colour = UI $ \ctx ->
  let call = FillRect (ctxBounds ctx) colour
  in ((), ctx { ctxDrawCommands = ctxDrawCommands ctx ++ [call] })

drawText :: Colour -> TextAlign -> Text -> UI e c ()
drawText colour align text = UI $ \ctx ->
  let call = DrawText (ctxBounds ctx) text colour align
  in ((), ctx { ctxDrawCommands = ctxDrawCommands ctx ++ [call] })

clipToCurrent :: UI e c a -> UI e c a
clipToCurrent action = do
  r <- getRect
  UI $ \ctx -> ((), ctx { ctxDrawCommands = ctxDrawCommands ctx ++ [PushClip r] })
  result <- action
  UI $ \ctx -> ((), ctx { ctxDrawCommands = ctxDrawCommands ctx ++ [PopClip] })
  return result

emitCommand :: c -> UI e c ()
emitCommand cmd = UI $ \ctx ->
  ((), ctx { ctxPendingCommands = ctxPendingCommands ctx ++ [cmd] })

regionHit :: UI e c Bool
regionHit = do
  r <- getRect
  containsPoint r <$> getMousePos

