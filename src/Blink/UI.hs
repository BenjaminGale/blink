module Blink.UI
  ( FocusState (..)
  , UIContext (..)
  , UI (..)
  , emptyUIContext
  , nextFrameContext
  , getBounds
  , getMousePos
  , getLeftButton
  , isHovered
  , isClicked
  , isPressed
  , isKeyPressed
  , setHovered
  , isFocused
  , setFocus
  , setFocusWhen
  , clearFocus
  , getInput
  , consumeKey
  , getPreviousTabStop
  , setPreviousTabStop
  , getStyleSet
  , getStyle
  , isDisabled
  , disableWhen
  , withBounds
  , fillRect
  , strokeRect
  , drawText
  , clipToCurrent
  , regionHit
  , dispatch
  , changeTheme
  , control
  , renderControl
  , getDrawCommands
  , getCommands
  ) where

import Control.Monad (when, unless, guard)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), isOpaque, TextAlign (..), DrawCommand (..))
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
  , ctxPreviousTabStop :: Maybe e
  , ctxCommands :: [c]
  , ctxDisabled :: Bool
  , ctxThemeChangeRequested :: Bool
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
  , ctxPreviousTabStop = Nothing
  , ctxCommands = []
  , ctxDisabled = False
  , ctxThemeChangeRequested = False
  }

nextFrameContext :: Rectangle -> InputState -> UIContext e c -> UIContext e c
nextFrameContext bounds input ctx = ctx
  { ctxBounds          = bounds
  , ctxInput           = input
  , ctxDrawCommands    = []
  , ctxHoveredElement  = Nothing
  , ctxFocusState      = (ctxFocusState ctx) { focusedThisFrame = False }
  , ctxCommands             = []
  , ctxThemeChangeRequested = False
  }

gets :: (UIContext e c -> a) -> UI e c a
gets f = UI $ \ctx -> (f ctx, ctx)

modify :: (UIContext e c -> UIContext e c) -> UI e c ()
modify f = UI $ \ctx -> ((), f ctx)

getBounds :: UI e c Rectangle
getBounds = gets ctxBounds

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

getPreviousTabStop :: UI e c (Maybe e)
getPreviousTabStop = gets ctxPreviousTabStop

setPreviousTabStop :: e -> UI e c ()
setPreviousTabStop eid = modify $ \ctx -> ctx { ctxPreviousTabStop = Just eid }

getTheme :: UI e c (Theme e)
getTheme = gets ctxTheme

getStyleSet :: Ord e => e -> UI e c StyleSet
getStyleSet eid = do
  t <- getTheme
  pure $ Map.findWithDefault (themeDefaultStyle t) eid (themeElementStyles t)

getStyle :: Ord e => e -> UI e c Style
getStyle eid = do
  styles <- getStyleSet eid
  isDis  <- isDisabled
  isHov  <- isHovered eid
  isFoc  <- isFocused eid
  isPrs  <- isPressed eid
  let candidates =
        [ guard isDis $> styleSetDisabled styles
        , guard isPrs $> styleSetPressed  styles
        , guard isHov $> styleSetHovered  styles
        , guard isFoc $> styleSetFocused  styles
        ]
  pure $ fromMaybe (styleSetNormal styles) (asum candidates)

isHovered :: Eq e => e -> UI e c Bool
isHovered eid = (== Just eid) <$> gets ctxHoveredElement

isClicked :: Eq e => e -> UI e c Bool
isClicked eid = do
  isHov <- isHovered eid
  btn   <- getLeftButton
  pure (isHov && btn == ButtonReleased)

isPressed :: Eq e => e -> UI e c Bool
isPressed eid = do
  isHov <- isHovered eid
  btn   <- getLeftButton
  pure (isHov && btn == ButtonDown)

isKeyPressed :: Eq e => e -> Key -> UI e c Bool
isKeyPressed eid k = do
  hasFoc <- isFocused eid
  pressed <- any (\e -> key e == k) . keyEvents <$> getInput
  pure (hasFoc && pressed)

setHovered :: e -> UI e c ()
setHovered eid = modify $ \ctx -> ctx { ctxHoveredElement = Just eid }

getFocus :: UI e c (Maybe e)
getFocus = gets (focusedElement . ctxFocusState)

isFocused :: Eq e => e -> UI e c Bool
isFocused eid = (== Just eid) <$> getFocus

setFocus :: e -> UI e c ()
setFocus eid = modify $ \ctx -> ctx { ctxFocusState = FocusState { focusedElement = Just eid, focusedThisFrame = True } }

setFocusWhen :: Bool -> e -> UI e c ()
setFocusWhen b eid = when b (setFocus eid)

clearFocus :: UI e c ()
clearFocus = modify $ \ctx -> ctx { ctxFocusState = (ctxFocusState ctx) { focusedElement = Nothing } }

withBounds :: Rectangle -> UI e c a -> UI e c a
withBounds r (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxBounds = r })
  in (a, ctx' { ctxBounds = ctxBounds ctx })

isDisabled :: UI e c Bool
isDisabled = gets ctxDisabled

disableWhen :: Bool -> UI e c a -> UI e c a
disableWhen True (UI f) = UI $ \ctx ->
  let (a, ctx') = f (ctx { ctxDisabled = True })
  in (a, ctx' { ctxDisabled = ctxDisabled ctx })
disableWhen False action = action

draw :: DrawCommand -> UI e c ()
draw cmd = modify $ \ctx -> ctx { ctxDrawCommands = cmd : ctxDrawCommands ctx }

fillRect :: Colour -> UI e c ()
fillRect colour = do
  r <- getBounds
  draw $ FillRect r colour

strokeRect :: Colour -> Double -> UI e c ()
strokeRect colour width = do
  r <- getBounds
  draw $ StrokeRect r colour width

drawText :: Colour -> TextAlign -> Text -> UI e c ()
drawText colour align text = do
  r <- getBounds
  draw $ DrawText r text colour align

clipToCurrent :: UI e c a -> UI e c a
clipToCurrent action = do
  r <- getBounds
  draw $ PushClip r
  result <- action
  draw PopClip
  return result

dispatch :: c -> UI e c ()
dispatch cmd = modify $ \ctx -> ctx { ctxCommands = cmd : ctxCommands ctx }

changeTheme :: UI e c ()
changeTheme = modify $ \ctx -> ctx { ctxThemeChangeRequested = True }

getDrawCommands :: UIContext e c -> [DrawCommand]
getDrawCommands = reverse . ctxDrawCommands

getCommands :: UIContext e c -> [c]
getCommands = reverse . ctxCommands

regionHit :: UI e c Bool
regionHit = do
  r <- getBounds
  containsPoint r <$> getMousePos

whenEnabled :: UI e c () -> UI e c ()
whenEnabled ui = do
  isDisabl <- isDisabled
  unless isDisabl $ do
    ui

applyHover :: (Eq e, Ord e) => e -> UI e c ()
applyHover eid = do
  whenEnabled $ do
    s <- getStyle eid
    r <- getBounds
    let bgRect = insetRect (styleMargin s) r
    isHit <- withBounds bgRect regionHit
    when isHit $ do
      setHovered eid

applyFocus :: (Eq e, Ord e) => e -> UI e c ()
applyFocus eid = do
  whenEnabled $ do
    currentFocus <- getFocus
    isHit        <- isHovered eid
    btn          <- getLeftButton
    let nothingIsFocused  = isNothing currentFocus
        isRequestingFocus = currentFocus == Just eid
        wasClicked        = isHit && btn == ButtonReleased
    setFocusWhen (nothingIsFocused || isRequestingFocus || wasClicked) eid

applyTabNavigation :: (Eq e, Ord e) => e -> UI e c ()
applyTabNavigation eid = do
  hasFocus <- isFocused eid
  input    <- getInput
  prevCtrl <- getPreviousTabStop
  let tabKey          = find (\e -> key e == KeyTab) (keyEvents input)
      tabPressed      = maybe False (\e -> Shift `notElem` modifiers e) tabKey
      shiftTabPressed = maybe False (\e -> Shift `elem`    modifiers e) tabKey
  when (hasFocus && tabPressed) $ do
    clearFocus
    consumeKey KeyTab
  when (hasFocus && shiftTabPressed && isJust prevCtrl) $ do
    setFocus (fromJust prevCtrl)
    consumeKey KeyTab
  whenEnabled $ do
    setPreviousTabStop eid

renderControl :: Ord e => e -> UI e c () -> UI e c ()
renderControl eid content = do
  style <- getStyle eid
  r     <- getBounds
  let bgRect      = insetRect (styleMargin style) r
      contentRect = insetRect (stylePadding style) bgRect
  when (isOpaque (styleBackground style)) $ do
    withBounds bgRect $ do
      fillRect (styleBackground style)
  case styleBorderColour style of
    Just c  -> withBounds bgRect $ do
      strokeRect c (styleBorderWidth style)
    Nothing -> return ()
  withBounds contentRect $ clipToCurrent content

control :: (Eq e, Ord e) => e -> UI e c () -> UI e c ()
control eid content = do
  applyHover eid
  applyFocus eid
  applyTabNavigation eid
  renderControl eid content
