module Blink.Style
  ( Style (..)
  , StyleSet (..)
  , Theme (..)
  , emptyTheme
  ) where

import qualified Data.Map.Strict as Map
import Blink.DrawCall (Colour (..), TextAlign (..))

data Style = Style
  { background :: Colour
  , textColour :: Colour
  , textAlign :: TextAlign
  }

data StyleSet = StyleSet
  { normal   :: Style
  , hovered  :: Style
  , pressed  :: Style
  , focused  :: Style
  , disabled :: Style
  }

data Theme e = Theme
  { elementStyles :: Map.Map e StyleSet
  , defaultStyle  :: StyleSet
  }

emptyTheme :: StyleSet -> Theme e
emptyTheme def = Theme { elementStyles = Map.empty, defaultStyle = def }
