{-# LANGUAGE DisambiguateRecordFields #-}

module Blink.UI
  ( UIContext (..)
  , UI (..)
  , emptyUIContext
  , nextFrameContext
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
  , emitCommand
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), TextAlign (..), DrawCommand (..))
import Blink.Geometry (Point, Rectangle, containsPoint)
import Blink.Input (ButtonState (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))

data UIContext e c = UIContext
  { ctxBounds :: Rectangle
  , ctxInput :: InputState
  , ctxTheme :: Theme e
  , ctxDrawCommands :: [DrawCommand]
  , ctxHoveredElement :: Maybe e
  , ctxFocusedElement :: Maybe e
  , ctxFocusedRendered :: Bool
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
  , ctxFocusedElement = Nothing
  , ctxFocusedRendered = False
  , ctxPreviousControl = Nothing
  , ctxPendingCommands = []
  }

nextFrameContext :: Rectangle -> InputState -> UIContext e c -> UIContext e c
nextFrameContext bounds input ctx = ctx
  { ctxBounds          = bounds
  , ctxInput           = input
  , ctxDrawCommands    = []
  , ctxHoveredElement  = Nothing
  , ctxFocusedRendered = False
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

