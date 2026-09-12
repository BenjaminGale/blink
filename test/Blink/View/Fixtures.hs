-- | Shared fixtures for the "Blink.View" test suite (test/Blink/View/*Spec.hs):
-- a bare two-element id type, default input/theme/style values, and the
-- small set of 'runView' wrappers nearly every test in that suite starts
-- from.
module Blink.View.Fixtures
  ( TwoElems (..)
  , twoElemTheme
  , mkTheme
  , noInput
  , buttonDown
  , mouseOnCenter
  , mouseOnCenterDown
  , emptyStyle
  , emptyMetrics
  , emptyStyleSet
  , emptyTheme
  , testBounds
  , run
  , runWith
  , runTwoElem
  , run0
  , freshCtx
  , advance
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View

data TwoElems = ElemA | ElemB deriving (Eq, Ord, Show)

-- | The same empty theme shape reused (with a different phantom element-id
-- type per caller) everywhere a test just needs *a* theme, not a specific
-- one.
mkTheme :: Theme e
mkTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (emptyMetrics, emptyStyleSet) }

twoElemTheme :: Theme TwoElems
twoElemTheme = mkTheme

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 0 0
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  , inputWheelDelta     = 0
  }

buttonDown :: InputState
buttonDown = noInput { inputLeftButtonDown = True }

mouseOnCenter :: InputState
mouseOnCenter = noInput { inputMousePosition = Point 50 50 }

mouseOnCenterDown :: InputState
mouseOnCenterDown = noInput { inputMousePosition = Point 50 50, inputLeftButtonDown = True }

emptyStyle :: Style
emptyStyle = Style
  { styleBackground = RGBA 0 0 0 1
  , styleTextColour = RGBA 0 0 0 1
  , styleTextAlign = AlignCenter
  , styleBorderColour = Nothing
  }

emptyMetrics :: Metrics
emptyMetrics = Metrics
  { metricsMargin = uniform 0
  , metricsPadding = uniform 0
  , metricsBorderEdges = noBorder
  }

emptyStyleSet :: StyleSet
emptyStyleSet = StyleSet { styleBase = emptyStyle, styleOverrides = Map.empty }

emptyTheme :: Theme ()
emptyTheme = mkTheme

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

run :: View () msg a -> IO (a, ViewContext () msg)
run ui = runView ui (emptyViewContext testBounds noInput emptyTheme)

runWith :: InputState -> View () msg a -> IO (a, ViewContext () msg)
runWith input ui = runView ui (emptyViewContext testBounds input emptyTheme)

runTwoElem :: View TwoElems msg a -> IO (a, ViewContext TwoElems msg)
runTwoElem ui = runView ui (emptyViewContext testBounds noInput twoElemTheme)

run0 :: View () Int a -> IO (a, ViewContext () Int)
run0 = run

freshCtx :: IO (ViewContext () Int)
freshCtx = snd <$> run0 (pure ())

-- | Advances @ctx@ to the next frame with @input@, carrying its theme and
-- animation state forward unchanged — the test-level equivalent of what a
-- real backend does every frame via 'Blink.App.buildCtx'.
advance :: Ord e => InputState -> ViewContext e msg -> ViewContext e msg
advance input ctx = nextFrameContext testBounds input (contextTheme ctx) (contextAnimation ctx) ctx
