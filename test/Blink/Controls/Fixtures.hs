-- | Shared fixtures for the "Blink.Controls" test suite
-- (test/Blink/Controls/*Spec.hs): the plain black-on-black style/theme
-- values nearly every spec in that suite builds from.
module Blink.Controls.Fixtures
  ( testColour
  , plainStyle
  , plainMetrics
  , standardMetrics
  , plainStyleSet
  , mkTestTheme
  ) where

import qualified Data.Map.Strict as Map

import Blink.Geometry (Insets, noBorder, uniform)
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..))

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

plainStyleSet :: Style -> StyleSet
plainStyleSet s = StyleSet { styleBase = s, styleOverrides = Map.empty }

mkTestTheme :: Metrics -> StyleSet -> Theme e
mkTestTheme m s = Theme { themeElementStyles = Map.empty, themeDefaultStyle = (m, s) }
