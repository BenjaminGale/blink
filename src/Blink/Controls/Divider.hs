{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A plain visual separator: a thin line drawn along the full length of
-- whichever axis it's not thin on. A leaf, built directly on
-- 'control' -- nothing derives from it, it displays no label, and
-- it's never a tab stop (see 'divider'). The simplest control in
-- "Blink.Controls": no value, no id, and no events.
module Blink.Controls.Divider
  ( DividerConfig (..)
  , defaultDividerConfig
  , dividerStyleKey
  , dividerLineStyleKey
  , divider
  , orientation
  , thickness
    -- * Style
  , defaultStyleEntries
  ) where

import qualified Data.Set as Set

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Size (..), uniform)
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Element (Element (..), HasLayoutConfig (..), HasOrientation (..))
import Blink.Style
import Blink.Controls.Style (plainFillStyle, plainStyle, zeroMetrics)

-- | Every capability 'divider' resolves: the wrapped 'ControlConfig', the
-- axis it runs along, its thickness across that axis, and the caller's
-- layout attributes, applied over the default layout for that axis when
-- the divider is built.
data DividerConfig e msg = DividerConfig
  { dcControl     :: ControlConfig e msg
  , dcOrientation :: Orientation
  , dcThickness   :: Double
  , dcLayoutAttrs :: [Attribute Layout]
  }

-- | 'defaultControlConfig' (styled via 'dividerStyleKey'), 'Horizontal', a
-- thickness of 1, and no layout attributes.
defaultDividerConfig :: DividerConfig e msg
defaultDividerConfig = DividerConfig
  { dcControl     = defaultControlConfig { ccStyleKey = dividerStyleKey }
  , dcOrientation = Horizontal
  , dcThickness   = 1
  , dcLayoutAttrs = []
  }

instance HasControlConfig e msg (DividerConfig e msg) where
  overControl = nested dcControl (\dc x -> dc { dcControl = x })

instance HasLayoutConfig (DividerConfig e msg) where
  overLayout = appendTo dcLayoutAttrs (\dc as -> dc { dcLayoutAttrs = as })

-- | The divider's layout: the default for its 'orientation', with the
-- caller's 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'
-- applied over it, in whatever order they were given relative to
-- 'orientation'.
dividerLayout :: DividerConfig e msg -> Layout
dividerLayout dc = resolve (layoutFor (dcOrientation dc)) (dcLayoutAttrs dc)

-- | The default 'Layout' for a divider running along @o@: fills the space
-- it's given along that axis, and sizes itself to 'dcThickness' (plus
-- chrome -- see 'divider') across it.
layoutFor :: Orientation -> Layout
layoutFor Horizontal = Layout fill fitContent TopLeft
layoutFor Vertical   = Layout fitContent fill TopLeft

-- | Which axis the line runs along: 'Horizontal' (the default) draws a
-- line stretching left-to-right, for separating things stacked in a
-- 'Blink.Layout.Box.vBox'; 'Vertical' draws one stretching top-to-bottom,
-- for separating things side by side in an 'Blink.Layout.Box.hBox'.
-- Picks the default layout for that axis; 'Blink.Element.width'\/
-- 'Blink.Element.height' still override it, wherever they appear.
instance HasOrientation (DividerConfig e msg) where
  orientation o = Attribute (\dc -> dc { dcOrientation = o })

-- | How thick the drawn line is, across whichever axis 'orientation' isn't
-- running it along. Defaults to 1. Has no effect if 'Blink.Element.width'\/
-- 'Blink.Element.height' overrides that axis to something other
-- than 'Blink.Layout.Constraints.fitContent'.
thickness :: Double -> Attribute (DividerConfig e msg)
thickness t = Attribute (\dc -> dc { dcThickness = t })

-- | A plain visual separator (see the module header). Display-only: it
-- takes no id, raises no events, and is never focusable. Draws its line
-- as the 'dividerLineStyleKey' part. Defaults to filling the space it's
-- given along 'orientation' and sizing to 'thickness' (plus the current theme's
-- margin\/border\/padding, same as every other control) across it --
-- override with 'Blink.Element.width'\/'Blink.Element.height'\/
-- 'Blink.Element.align'; when placed in an 'Blink.Layout.Box.hBox'\/
-- 'Blink.Layout.Box.vBox' next to a taller\/wider sibling,
-- 'Blink.Element.align' picks where within that extra space the
-- line sits.
divider :: Ord e => [Attribute (DividerConfig e msg)] -> Element e msg
divider attrs = controlElement layout (Element layout intrinsicSize (pure ())) ctrl
  where
    cfg    = resolve defaultDividerConfig attrs
    layout = dividerLayout cfg
    ctrl = (dcControl cfg)
      { ccFocusPolicy  = NotFocusable
      , ccContent      = const body
      }
    t = dcThickness cfg
    -- | 'thickness' on both axes: whichever one the default 'Layout'
    -- actually consults (the 'fitContent' one) is the only one that
    -- matters, since the other is 'fill' and never reaches this at all --
    -- see 'Blink.Layout.Constraints.resolveLength'.
    intrinsicSize = const (pure (Size t t))
    body = drawPart dividerLineStyleKey (Set.singleton CommonNormal)

-- * Style

-- | The 'StyleKey' 'Blink.Controls.Divider.divider' resolves its
-- style from unless overridden via 'Blink.Controls.Control.style'.
dividerStyleKey :: StyleKey e
dividerStyleKey = Class "divider"

dividerMetrics :: Metrics
dividerMetrics = Metrics
  { metricsMargin      = uniform 4
  , metricsPadding     = uniform 0
  }

-- | The 'StyleKey' the line itself resolves its style from.
dividerLineStyleKey :: StyleKey e
dividerLineStyleKey = Class "dividerLine"

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme': its
-- own chrome and its line.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (dividerStyleKey,     (dividerMetrics, plainStyle p))
  , (dividerLineStyleKey, (zeroMetrics, plainFillStyle p (paletteBorder p)))
  ]
