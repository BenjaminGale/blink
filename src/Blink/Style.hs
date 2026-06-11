{- |
Theming types for Blink. Controls are styled through a three-level hierarchy:

  * 'Style' — visual properties (colours, spacing, border) for a control
    in a single interaction state.
  * 'StyleSet' — a bundle of five 'Style' variants, one per interaction
    state: normal, hovered, pressed, focused, and disabled.
  * 'Theme' — maps element IDs (@e@) to 'StyleSet' values, with a
    fallback default. Derived from application state each frame and
    passed into the UI via the 'Blink.App.App' record.

When the UI resolves the active style for a control, it looks up its
element ID in the 'Theme' (falling back to 'themeDefaultStyle' if none
is registered), then selects the appropriate state variant based on the
control's current interaction state. Construct a theme with 'emptyTheme'.
-}
module Blink.Style
  ( -- * Types
    Style (..)
  , StyleSet (..)
  , Theme (..)
    -- * Construction
  , emptyTheme
  ) where

import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Geometry (Insets (..))

-- | Visual properties for a control in a single interaction state.
-- Resolved from the active 'StyleSet' by 'Blink.UI.getStyle'.
data Style = Style
  { styleBackground :: Colour       -- ^ Fill colour for the background rectangle (inside the margin).
  , styleTextColour :: Colour       -- ^ Colour used for text and simple fill drawing.
  , styleTextAlign :: TextAlign     -- ^ Horizontal text alignment within the content rectangle.
  , styleMargin :: Insets           -- ^ Space between the slot edge and the background rectangle.
  , stylePadding :: Insets          -- ^ Space between the background rectangle and the content rectangle.
  , styleBorderColour :: Maybe Colour -- ^ Stroke colour for the border; 'Nothing' suppresses the border.
  , styleBorderWidth :: Double      -- ^ Border stroke width in pixels.
  }

-- | The five per-state 'Style' variants for a control. The active
-- variant is selected by 'Blink.UI.getStyle'; priority order is:
-- disabled > pressed > hovered > focused > normal.
data StyleSet = StyleSet
  { styleSetNormal :: Style   -- ^ Default appearance.
  , styleSetHovered :: Style  -- ^ Cursor is over the control.
  , styleSetPressed :: Style  -- ^ Primary mouse button held while hovered.
  , styleSetFocused :: Style  -- ^ Control holds keyboard focus.
  , styleSetDisabled :: Style -- ^ Control is inside a 'Blink.UI.disableWhen' subtree.
  }

-- | Maps element IDs of type @e@ to 'StyleSet' values. Construct with
-- 'emptyTheme' and populate 'themeElementStyles' for per-element overrides.
data Theme e = Theme
  { themeElementStyles :: Map.Map e StyleSet
    -- ^ Per-element style overrides, keyed by element ID.
  , themeDefaultStyle :: StyleSet
    -- ^ Fallback used when an element ID has no entry in 'themeElementStyles'.
  }

-- | Creates a 'Theme' with no per-element overrides; every element
-- resolves to @def@.
emptyTheme :: StyleSet -> Theme e
emptyTheme def = Theme { themeElementStyles = Map.empty, themeDefaultStyle = def }
