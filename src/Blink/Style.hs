{- |
Module: Blink.Style

Theming types for Blink. Controls are styled through a three-level hierarchy:

  * 'Style' — visual properties (colours, spacing, border) for a control
    in a single interaction state.
  * 'StyleSet' — a bundle of five 'Style' variants, one per interaction
    state: normal, hovered, pressed, focused, and disabled.
  * 'Theme' — maps 'StyleKey's (either a specific element's own id, or a
    named class) to 'StyleSet' values, with a fallback default. Derived
    from application state each frame and passed into the UI via the
    'Blink.App.App' record.

When the UI resolves the active style for a control, it looks up its
'StyleKey' in the 'Theme' (falling back to 'themeDefaultStyle' if none
is registered), then selects the appropriate state variant based on the
control's current interaction state. Construct a theme with 'emptyTheme'.
A ready-made control defaults its own key to a 'Class' named after itself
(e.g. @Class \"button\"@), overridable per-instance via its @style@ attr.

= Building a theme

There is no default 'Style' or 'StyleSet' — every field of every state
variant must be given explicitly. A typical theme starts from one base
style, derives the other four variants from it with record update, and
registers only the elements that need to look different from the default:

@
baseStyle :: Style
baseStyle = Style
  { styleBackground   = RGBA 0.2 0.2 0.2 1
  , styleTextColour   = RGBA 1 1 1 1
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 2
  , stylePadding      = uniform 4
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

baseStyleSet :: StyleSet
baseStyleSet = StyleSet
  { styleSetNormal   = baseStyle
  , styleSetHovered  = baseStyle { styleBackground = RGBA 0.3 0.3 0.3 1 }
  , styleSetPressed  = baseStyle { styleBackground = RGBA 0.15 0.15 0.15 1 }
  , styleSetFocused  = baseStyle { styleBorderColour = Just (RGBA 0.4 0.6 1 1)
                                  , styleBorderEdges = uniformBorder 1 }
  , styleSetDisabled = baseStyle { styleTextColour = RGBA 0.5 0.5 0.5 1 }
  }

myTheme :: Theme Element
myTheme = (emptyTheme baseStyleSet)
  { themeElementStyles = Map.fromList
      [ (ElementId DangerButton, baseStyleSet
          { styleSetNormal = baseStyle { styleBackground = RGBA 0.7 0.1 0.1 1 } })
      ]
  }
@
-}
module Blink.Style
  ( -- * Types
    Style (..)
  , StyleSet (..)
  , StyleKey (..)
  , Theme (..)
    -- * Construction
  , emptyTheme
    -- * Re-exports
  , BorderEdges (..)
  , noBorder
  , uniformBorder
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Geometry (Insets (..), BorderEdges (..), noBorder, uniformBorder)

-- | Visual properties for a control in a single interaction state.
-- Resolved from the active 'StyleSet' by 'Blink.UI.currentStyle'.
data Style = Style
  { styleBackground :: Colour       -- ^ Fill colour for the background rectangle (inside the margin).
  , styleTextColour :: Colour       -- ^ Colour used for text and simple fill drawing.
  , styleTextAlign :: TextAlign     -- ^ Horizontal text alignment within the content rectangle.
  , styleMargin :: Insets           -- ^ Space between the slot edge and the background rectangle.
  , stylePadding :: Insets          -- ^ Space between the background rectangle and the content rectangle.
  , styleBorderColour :: Maybe Colour -- ^ Stroke colour for the border; 'Nothing' suppresses the border.
  , styleBorderEdges :: BorderEdges  -- ^ Per-side border widths in pixels.
  }

-- | The five per-state 'Style' variants for a control. The active
-- variant is selected by 'Blink.UI.currentStyle'; priority order is:
-- disabled > pressed > hovered > focused > normal.
data StyleSet = StyleSet
  { styleSetNormal :: Style   -- ^ Default appearance.
  , styleSetHovered :: Style  -- ^ Cursor is over the control.
  , styleSetPressed :: Style  -- ^ Primary mouse button held while hovered.
  , styleSetFocused :: Style  -- ^ Control holds keyboard focus.
  , styleSetDisabled :: Style -- ^ Control is inside a 'Blink.UI.disableWhen' subtree.
  }

-- | Which entry in a 'Theme' a control resolves its 'StyleSet' from --
-- either that specific element's own id, or a named class shared by every
-- control that resolves to it. A ready-made control (button, checkbox, ...)
-- defaults to a 'Class' named after itself, so a theme can style every
-- instance of that kind of control at once without registering each
-- element id individually; passing 'ElementId', or a different 'Class', to
-- a control's @style@ attr overrides that default.
data StyleKey e
  = ElementId e
  | Class Text
  deriving (Eq, Ord, Show)

-- | Maps 'StyleKey's to 'StyleSet' values. Construct with 'emptyTheme' and
-- populate 'themeElementStyles' for per-element or per-class overrides.
data Theme e = Theme
  { themeElementStyles :: Map.Map (StyleKey e) StyleSet
    -- ^ Per-element or per-class style overrides, keyed by 'StyleKey'.
  , themeDefaultStyle :: StyleSet
    -- ^ Fallback used when a 'StyleKey' has no entry in 'themeElementStyles'.
  }

-- | Creates a 'Theme' with no per-element overrides; every element
-- resolves to @def@.
emptyTheme :: StyleSet -> Theme e
emptyTheme def = Theme { themeElementStyles = Map.empty, themeDefaultStyle = def }
