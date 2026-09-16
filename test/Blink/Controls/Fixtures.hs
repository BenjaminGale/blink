-- | Shared fixtures for the "Blink.Controls" test suite
-- (test/Blink/Controls/*Spec.hs): the plain black-on-black style/theme
-- values nearly every spec in that suite builds from.
module Blink.Controls.Fixtures
  ( testColour
  , plainStyle
  , plainMetrics
  , standardMetrics
  , zeroMetrics
  , plainStyleSet
  , mkTestTheme
  , noInput
  , hitRectFor
  , contentRectFor
  , fullSizeAt
  , startAt
  ) where

import qualified Data.Map.Strict as Map

import Blink.Element (Element, elLayout, runElement)
import Blink.Geometry (Alignment (TopLeft), Insets, Point (..), Rectangle, insetRect, noBorder, uniform)
import Blink.Input (InputState (..), emptyInputState)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))
import Blink.View (View, ViewContext, runView)

testColour :: Colour
testColour = RGBA 0 0 0 1

-- | A style with the given colour for both background and text, centred,
-- no border -- the shape nearly every control test starts from before
-- overriding the one or two fields it actually cares about.
plainStyle :: Colour -> Style
plainStyle c = Style
  { styleBackground   = c
  , styleTextColour   = c
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Nothing
  }

plainMetrics :: Insets -> Insets -> Metrics
plainMetrics margin padding = Metrics
  { metricsMargin      = margin
  , metricsPadding     = padding
  , metricsBorderEdges = noBorder
  }

-- | 10px margin, 5px padding, no border -- the convention most control
-- tests render against.
standardMetrics :: Metrics
standardMetrics = plainMetrics (uniform 10) (uniform 5)

-- | No margin, no padding, no border -- the convention list/tree/table
-- family tests render against.
zeroMetrics :: Metrics
zeroMetrics = plainMetrics (uniform 0) (uniform 0)

plainStyleSet :: Style -> StyleSet
plainStyleSet s = StyleSet { styleBase = s, styleOverrides = Map.empty }

mkTestTheme :: Metrics -> StyleSet -> Theme e
mkTestTheme m s = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (m, s) }

-- | A cursor position well outside any control under test, so nothing
-- reads as hovered by default.
noInput :: InputState
noInput = emptyInputState { inputMousePosition = Point 200 200 }

-- | The margin-inset hit area for a control rendered at the given bounds
-- with 'standardMetrics'\'s 10px margin.
hitRectFor :: Rectangle -> Rectangle
hitRectFor = insetRect (metricsMargin standardMetrics)

-- | The margin-and-padding-inset content area for a control rendered at
-- the given bounds with 'standardMetrics'.
contentRectFor :: Rectangle -> Rectangle
contentRectFor = insetRect (metricsPadding standardMetrics) . hitRectFor

-- | Runs @el@ forced to fill its given bounds entirely -- the behaviour
-- every control had before controls started reporting their own
-- 'Layout' and sizing to content. Behaviour contracts written against
-- "fills the bounds" ask for this explicitly, the same way any other
-- caller would.
fullSizeAt :: Element e msg -> View e msg ()
fullSizeAt el = runElement el { elLayout = Layout fill fill TopLeft }

startAt :: ViewContext e msg -> View e msg () -> IO (ViewContext e msg)
startAt ctx v = snd <$> runView v ctx
