module Blink.Style
  ( Style (..)
  , StyleSet (..)
  , Theme (..)
  , emptyTheme
  ) where

import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Geometry (Insets (..))

data Style = Style
  { styleBackground   :: Colour
  , styleTextColour   :: Colour
  , styleTextAlign    :: TextAlign
  , styleMargin       :: Insets
  , stylePadding      :: Insets
  , styleBorderColour :: Maybe Colour
  , styleBorderWidth  :: Double
  }

data StyleSet = StyleSet
  { styleSetNormal   :: Style
  , styleSetHovered  :: Style
  , styleSetPressed  :: Style
  , styleSetFocused  :: Style
  , styleSetDisabled :: Style
  }

data Theme e = Theme
  { themeElementStyles :: Map.Map e StyleSet
  , themeDefaultStyle  :: StyleSet
  }

emptyTheme :: StyleSet -> Theme e
emptyTheme def = Theme { themeElementStyles = Map.empty, themeDefaultStyle = def }
