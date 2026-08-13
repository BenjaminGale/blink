{-# LANGUAGE OverloadedStrings #-}
-- | Shared test harness for exercising controls in isolation: a minimal
-- element type, a bare-bones theme, and helpers for constructing input and
-- inspecting the resulting context. Used by "Blink.ControlsSpec".
module Blink.ControlsTestSupport
  ( TestElement (..)
  , WidgetRunner
  , testColour
  , testStyle
  , testStyleSet
  , testTheme
  , testBorderColour
  , testStyleWithBorder
  , testStyleSetWithBorder
  , testThemeWithBorder
  , controlRect
  , bgRect
  , contentRect
  , mkCtx
  , withFocus
  , getFocused
  , settle
  , dispatchCount
  , noInput
  , mouseAt
  , withButtonReleased
  , insidePoints
  , outsidePoints
  , drawnTexts
  ) where

import qualified Data.Map.Strict as Map

import Data.Text (Text)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform, uniformBorder)
import Blink.Input (InputState (..), KeyEvent (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = TestControl | OtherControl
  deriving (Eq, Ord, Show)

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 10
  , stylePadding      = uniform 5
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet
  { styleSetNormal   = testStyle
  , styleSetHovered  = testStyle
  , styleSetPressed  = testStyle
  , styleSetFocused  = testStyle
  , styleSetDisabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme
  { themeElementStyles = Map.fromList [(TestControl, testStyleSet), (OtherControl, testStyleSet)]
  , themeDefaultStyle  = testStyleSet
  }

controlRect :: Rectangle
controlRect = Rectangle 0 0 100 100

bgRect :: Rectangle
bgRect = insetRect (uniform 10) controlRect

contentRect :: Rectangle
contentRect = insetRect (uniform 5) bgRect

mkCtx :: InputState -> UIContext TestElement ()
mkCtx input = emptyUIContext controlRect input testTheme noOpTextMeasurer

withFocus :: Maybe e -> UIContext e s -> UIContext e s
withFocus e ctx = ctx { ctxInteraction = (ctxInteraction ctx) { ixnFocus = (ixnFocus (ctxInteraction ctx)) { focusedElement = e } } }

getFocused :: UIContext e s -> Maybe e
getFocused = focusedElement . ixnFocus . ctxInteraction

-- | Applies the frame's queued 'UiEffect's (focus, scroll, selection)
-- directly to the context, without advancing to a new frame (draw commands,
-- hover state, and the animation flag are left untouched). Focus, scroll,
-- and selection writes are deferred — queued with 'emitUi' rather than
-- mutating the context immediately — so most assertions settle the context
-- first to see the result, the same way a host would after the frame
-- completes.
settle :: Ord e => UIContext e msg -> UIContext e msg
settle ctx = applyUiEffects (getUiEffects ctx) ctx

-- | The number of messages emitted during the frame.
dispatchCount :: UIContext e msg -> Int
dispatchCount = length . getMessages

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

mouseAt :: Point -> Bool -> [KeyEvent] -> InputState
mouseAt pos down keys = InputState
  { inputMousePosition  = pos
  , inputLeftButtonDown = down
  , inputKeyEvents      = keys
  , inputTypedText      = []
  }

-- | Sets 'ixnButtonReleased' so controls see a click this frame, without
-- requiring a prior down-frame in the test sequence.
withButtonReleased :: UIContext e s -> UIContext e s
withButtonReleased ctx = ctx
  { ctxInput       = (ctxInput ctx) { inputLeftButtonDown = False }
  , ctxInteraction = (ctxInteraction ctx) { ixnButtonDown = False, ixnButtonReleased = True }
  }

insidePoints :: [(String, Point)]
insidePoints =
  [ ("at the center",           Point 50 50)
  , ("at the top-left corner",  Point 10 10)
  , ("at the bottom-right corner", Point 90 90)
  ]

outsidePoints :: [(String, Point)]
outsidePoints =
  [ ("in the margin area",  Point 5 5)
  , ("outside the control", Point 200 200)
  ]

testBorderColour :: Colour
testBorderColour = RGBA 1 0 0 1

testStyleWithBorder :: Style
testStyleWithBorder = testStyle { styleBorderColour = Just testBorderColour, styleBorderEdges = uniformBorder 1 }

testStyleSetWithBorder :: StyleSet
testStyleSetWithBorder = StyleSet
  { styleSetNormal   = testStyleWithBorder
  , styleSetHovered  = testStyleWithBorder
  , styleSetPressed  = testStyleWithBorder
  , styleSetFocused  = testStyleWithBorder
  , styleSetDisabled = testStyleWithBorder
  }

testThemeWithBorder :: Theme TestElement
testThemeWithBorder = Theme
  { themeElementStyles = Map.fromList [(TestControl, testStyleSetWithBorder), (OtherControl, testStyleSetWithBorder)]
  , themeDefaultStyle  = testStyleSetWithBorder
  }

-- | Runs a widget against a prepared context and returns the settled result
-- — the common shape every shared behaviour spec (focus, tab, hover,
-- background\/border) is parameterized over.
type WidgetRunner = UIContext TestElement () -> IO (UIContext TestElement ())

-- | The text of every 'DrawText' command issued during the frame, in
-- submission order.
drawnTexts :: UIContext e s -> [Text]
drawnTexts ctx = [t | DrawText _ t _ _ <- getDrawCommands ctx]
