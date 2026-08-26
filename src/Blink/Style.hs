{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Blink.Style

Theming types for Blink, introduced in the order a theme author needs
them:

  * 'Palette' — a small set of named colours every built-in control's
    style is typically derived from.
  * 'Metrics' — a control's size (margin\/padding\/border width), looked
    up independently of interaction state.
  * 'VisualState' — the state(s) a control can be in: common
    (normal\/hovered\/pressed\/disabled), focus, and control-specific
    (e.g. a toggle's checked\/unchecked).
  * 'Style'\/'StyleSet' — a base 'Style' plus sparse per-'VisualState'
    overrides.
  * 'resolveStyle' — folds the active 'VisualState's over a 'StyleSet's
    base style to produce the 'Style' to draw with this frame.
  * 'Theme' — maps 'StyleKey's (either a specific element's own id, or a
    named class) to @('Metrics', 'StyleSet')@ pairs, with a fallback
    default. Derived from application state each frame and passed into
    the UI via the 'Blink.App.App' record.

When the UI resolves the active look for a control, it looks up its
'StyleKey' in the 'Theme' (falling back to 'themeDefaultStyle' if none is
registered) to get a @('Metrics', 'StyleSet')@ pair, then calls
'resolveStyle' with the control's current set of active 'VisualState's.
Construct a theme with 'emptyTheme'. A ready-made control defaults its
own key to a 'Class' named after itself (e.g. @Class \"button\"@),
overridable per-instance via its @style@ attr.

= Building a theme

A typical theme starts from one shared 'Palette', builds one base
'Style' and 'Metrics' value, and registers override functions only for
the states that actually look different from the base:

@
palette :: Palette
palette = Palette
  { paletteAccent           = RGBA 0.1 0.4 0.8 1
  , paletteFocusRing        = RGBA 0.4 0.6 1.0 1
  , paletteSurface          = RGBA 0.2 0.2 0.2 1
  , paletteSurfaceHover     = RGBA 0.3 0.3 0.3 1
  , paletteSurfaceDisabled  = RGBA 0.15 0.15 0.15 1
  , paletteTextPrimary      = RGBA 1 1 1 1
  , paletteTextMuted        = RGBA 0.5 0.5 0.5 1
  , paletteTextOnAccent     = RGBA 1 1 1 1
  , paletteBorder           = RGBA 0.4 0.4 0.4 1
  , paletteBorderHover      = RGBA 0.6 0.6 0.6 1
  }

baseStyle :: Style
baseStyle = Style
  { styleBackground   = paletteSurface palette
  , styleTextColour   = paletteTextPrimary palette
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Nothing
  }

baseMetrics :: Metrics
baseMetrics = Metrics
  { metricsMargin      = uniform 2
  , metricsPadding     = uniform 4
  , metricsBorderEdges = noBorder
  }

baseStyleSet :: StyleSet
baseStyleSet = StyleSet
  { styleBase      = baseStyle
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \\s -> s { styleBackground = paletteSurfaceHover palette })
      , (FocusFocused,    \\s -> s { styleBorderColour = Just (paletteFocusRing palette)
                                   })
      ]
  }

myTheme :: Theme Element
myTheme = (emptyTheme (baseMetrics, baseStyleSet))
  { themeElementStyles = Map.fromList
      [ (ElementId DangerButton, (baseMetrics, baseStyleSet
          { styleBase = baseStyle { styleBackground = RGBA 0.7 0.1 0.1 1 } }))
      ]
  }
@

A state with no visual difference from the base (or from whichever other
states are simultaneously active) simply has no entry in
'styleOverrides' — see 'resolveStyle'.

= Control-specific pseudo-states

A control that needs a look beyond the common\/focus states its
'Blink.Controls.Control.controlBase' already tracks (e.g. a toggle
button's checked\/unchecked look) defines its own pre-built 'Custom'
values in its own module and exports them as opaque constants, the same
way a control exports its default 'StyleKey' — callers never construct
'Custom' values themselves. See "Blink.Controls.Button" for
@toggleChecked@\/@toggleUnchecked@, a worked example.
-}
module Blink.Style
  ( -- * Palette
    Palette (..)
    -- * Metrics
  , Metrics (..)
    -- * Visual states
  , VisualState (..)
  , groupOf
    -- * Style
  , Style (..)
  , StyleSet (..)
  , resolveStyle
    -- * Theme
  , StyleKey (..)
  , Theme (..)
  , emptyTheme
    -- * Re-exports
  , BorderEdges (..)
  , noBorder
  , uniformBorder
  ) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Blink.Geometry (BorderEdges (..), Insets, noBorder, uniformBorder)
import Blink.Rendering (Colour (..), TextAlign (..))

-- * Palette

-- | A small set of named colours every built-in control's style is
-- typically derived from -- the shared "feel" a theme can vary
-- independently of the structural "look" its style-builder functions
-- define. Swapping the 'Palette' while reusing the same builders changes
-- colours only; swapping the builders while keeping the 'Palette' changes
-- structure while keeping colours consistent.
data Palette = Palette
  { paletteAccent :: Colour           -- ^ The theme's primary accent colour (pressed\/selected fills, etc.).
  , paletteFocusRing :: Colour        -- ^ Border colour for a focused control.
  , paletteSurface :: Colour          -- ^ Default background for a control's normal state.
  , paletteSurfaceHover :: Colour     -- ^ Background while hovered.
  , paletteSurfaceDisabled :: Colour  -- ^ Background while disabled.
  , paletteTextPrimary :: Colour      -- ^ Default text colour.
  , paletteTextMuted :: Colour        -- ^ Text colour for a disabled control.
  , paletteTextOnAccent :: Colour     -- ^ Text colour drawn over 'paletteAccent'.
  , paletteBorder :: Colour           -- ^ Default border colour.
  , paletteBorderHover :: Colour      -- ^ Border colour while hovered.
  } deriving (Eq, Show)

-- * Metrics

-- | A control's size, independent of interaction state: one 'Metrics'
-- value per 'StyleKey', looked up without knowing which 'VisualState's
-- are currently active.
data Metrics = Metrics
  { metricsMargin :: Insets            -- ^ Space between the slot edge and the background rectangle.
  , metricsPadding :: Insets           -- ^ Space between the background rectangle and the content rectangle.
  , metricsBorderEdges :: BorderEdges  -- ^ Per-side border widths in pixels.
  } deriving (Eq, Show)

-- * Visual states

-- | The state(s) a control can be in. A hybrid closed\/open sum type:
-- compile-time safety for the two groups every control shares
-- (@Common@-prefixed and @Focus@-prefixed states), plus an escape hatch
-- ('Custom') for control-specific pseudo-states.
--
-- @Common@ and @Focus@ states are each mutually exclusive /within their
-- own group/ but independent of each other and of any 'Custom' group --
-- a control can be 'CommonNormal' /and/ 'FocusFocused' /and/
-- @Custom \"Toggle\" \"Checked\"@ at once. See 'resolveStyle' for how
-- simultaneously-active states compose.
data VisualState
  = CommonNormal     -- ^ No mouse interaction and not disabled.
  | CommonMouseOver  -- ^ Cursor is over the control.
  | CommonPressed    -- ^ Primary mouse button held while hovered.
  | CommonDisabled   -- ^ Control is inside a 'Blink.UI.disableWhen' subtree.
  | FocusFocused     -- ^ Control holds keyboard focus.
  | FocusUnfocused   -- ^ Control does not hold keyboard focus.
  | Custom Text Text
    -- ^ A control-specific pseudo-state: a group name (so unrelated
    -- custom states never collide) and a state name within it, e.g.
    -- @Custom \"Toggle\" \"Checked\"@. Built and exported as opaque
    -- constants by the control that defines them -- see the module
    -- header.
  deriving (Eq, Ord, Show)

-- | The group a 'VisualState' belongs to: @\"Common\"@, @\"Focus\"@, or a
-- 'Custom' state's own group name. Used only by 'resolveStyle' to
-- partition a control's active states before folding; not something a
-- theme author needs to call directly.
groupOf :: VisualState -> Text
groupOf CommonNormal    = "Common"
groupOf CommonMouseOver = "Common"
groupOf CommonPressed   = "Common"
groupOf CommonDisabled  = "Common"
groupOf FocusFocused    = "Focus"
groupOf FocusUnfocused  = "Focus"
groupOf (Custom g _)    = g

-- * Style

-- | Visual properties for a control, before any per-state overrides are
-- applied. Resolved into the active 'Style' for this frame by
-- 'resolveStyle', and read back via 'Blink.UI.currentStyle'.
data Style = Style
  { styleBackground :: Colour         -- ^ Fill colour for the background rectangle (inside the margin).
  , styleTextColour :: Colour         -- ^ Colour used for text and simple fill drawing.
  , styleTextAlign :: TextAlign       -- ^ Horizontal text alignment within the content rectangle.
  , styleBorderColour :: Maybe Colour -- ^ Stroke colour for the border; 'Nothing' suppresses the border.
  } deriving (Eq, Show)

-- | A base 'Style' plus sparse, additive per-'VisualState' overrides. An
-- override is a plain record-update function -- a state that only
-- changes the background writes @\\s -> s { styleBackground = ... }@ and
-- everything else passes through unchanged.
--
-- 'styleOverrides' being sparse resolves two things directly:
--
--   * __A state that makes no visual difference needs no entry at
--     all.__ @FocusUnfocused@ (no ring), @Custom \"Toggle\" \"Unchecked\"@
--     (looks like the plain base) are simply absent from the map -- the
--     resolver's lookup misses and nothing is applied, so the base (or
--     whatever the previous layer produced) passes through unchanged.
--   * __Simultaneously-active states compose by folding, not by a
--     distinct combined entry.__ 'CommonMouseOver' and 'FocusFocused'
--     both active means \"apply MouseOver's function, then apply
--     Focused's function\" -- there is no way to register a look specific
--     to exactly that combination, on purpose (mirrors how rare
--     combinator-specific styling, CSS's @:hover:focus@, is in practice;
--     see 'resolveStyle').
data StyleSet = StyleSet
  { styleBase :: Style
  , styleOverrides :: Map VisualState (Style -> Style)
  }

-- | Folds every override in @active@ over @styleBase ss@, in the fixed
-- order __Common → Focus → Custom (in ascending 'VisualState' order)__.
-- Each layer only touches the fields its override function actually
-- record-updates, so e.g. 'FocusFocused' and
-- @Custom \"Toggle\" \"Checked\"@ compose rather than overwrite each
-- other. A 'VisualState' present in @active@ but absent from
-- 'styleOverrides' contributes nothing (see 'StyleSet'). Ordering
-- between two different 'Custom' groups active at once is unspecified.
resolveStyle :: StyleSet -> Set VisualState -> Style
resolveStyle ss active = foldl' applyState (styleBase ss) orderedStates
  where
    activeIn grp = [ s | s <- Set.toAscList active, groupOf s == grp ]
    orderedStates =
      take 1 (activeIn "Common") ++
      take 1 (activeIn "Focus") ++
      [ s | s <- Set.toAscList active, groupOf s /= "Common", groupOf s /= "Focus" ]
    applyState s vs = maybe s ($ s) (Map.lookup vs (styleOverrides ss))

-- * Theme

-- | Which entry in a 'Theme' a control resolves its @('Metrics',
-- 'StyleSet')@ from -- either that specific element's own id, or a named
-- class shared by every control that resolves to it. A ready-made
-- control (button, checkbox, ...) defaults to a 'Class' named after
-- itself, so a theme can style every instance of that kind of control at
-- once without registering each element id individually; passing
-- 'ElementId', or a different 'Class', to a control's @style@ attr
-- overrides that default.
data StyleKey e
  = ElementId e
  | Class Text
  deriving (Eq, Ord, Show)

-- | Maps 'StyleKey's to @('Metrics', 'StyleSet')@ pairs. Construct with
-- 'emptyTheme' and populate 'themeElementStyles' for per-element or
-- per-class overrides.
data Theme e = Theme
  { themeElementStyles :: Map.Map (StyleKey e) (Metrics, StyleSet)
    -- ^ Per-element or per-class overrides, keyed by 'StyleKey'.
  , themeDefaultStyle :: (Metrics, StyleSet)
    -- ^ Fallback used when a 'StyleKey' has no entry in 'themeElementStyles'.
  }

-- | Creates a 'Theme' with no per-element overrides; every element
-- resolves to @def@.
emptyTheme :: (Metrics, StyleSet) -> Theme e
emptyTheme def = Theme { themeElementStyles = Map.empty, themeDefaultStyle = def }
