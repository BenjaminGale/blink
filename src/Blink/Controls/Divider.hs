{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A plain visual separator: a thin line drawn along the full length of
-- whichever axis it's not thin on. A leaf, built directly on
-- 'controlBase' -- nothing derives from it, it displays no label, and
-- it's never a tab stop (see 'divider'). The simplest control in
-- "Blink.Controls": no value, and -- unlike every other control -- no id
-- required.
module Blink.Controls.Divider
  ( DividerConfig (..)
  , defaultDividerConfig
  , dividerStyleKey
  , divider
  , orientation
  , thickness
  ) where

import Control.Monad (forM_, void)

import Blink.Controls.Control
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Size (..))
import Blink.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill, fitContent)
import Blink.Style (Style (..))
import Blink.UI
import Blink.UI.Element (Element (..))

-- | Every capability 'divider' resolves: the wrapped 'ControlConfig', the
-- axis it runs along, its thickness across that axis, and the 'Layout'
-- 'orientation' derives from it.
data DividerConfig e msg = DividerConfig
  { dcControl     :: ControlConfig e msg
  , dcOrientation :: Orientation
  , dcThickness   :: Double
  , dcLayout      :: Layout
  }

-- | The 'StyleKey' 'divider' resolves its style from unless overridden via
-- 'style'.
dividerStyleKey :: StyleKey e
dividerStyleKey = Class "divider"

-- | 'defaultControlConfig' (styled via 'dividerStyleKey'), 'Horizontal', a
-- thickness of 1, and the 'Layout' that follows from those -- see
-- 'orientation'.
defaultDividerConfig :: DividerConfig e msg
defaultDividerConfig = DividerConfig
  { dcControl     = defaultControlConfig { ccStyleKey = dividerStyleKey }
  , dcOrientation = Horizontal
  , dcThickness   = 1
  , dcLayout      = layoutFor Horizontal
  }

instance HasElementConfig e msg (DividerConfig e msg) where
  overElement attr = Attribute (\dc -> dc { dcControl = runAttribute (overElement attr) (dcControl dc) })

instance HasControlConfig e msg (DividerConfig e msg) where
  overControl attr = Attribute (\dc -> dc { dcControl = runAttribute attr (dcControl dc) })

instance HasLayoutConfig (DividerConfig e msg) where
  overLayout attr = Attribute (\dc -> dc { dcLayout = runAttribute attr (dcLayout dc) })

-- | The default 'Layout' for a divider running along @o@: fills the space
-- it's given along that axis, and sizes itself to 'dcThickness' (plus
-- chrome -- see 'divider') across it. Set via 'orientation'; override
-- either axis afterwards with 'Blink.Layout.Constraints.width'\/
-- 'Blink.Layout.Constraints.height' as usual.
layoutFor :: Orientation -> Layout
layoutFor Horizontal = Layout fill fitContent TopLeft
layoutFor Vertical   = Layout fitContent fill TopLeft

-- | Which axis the line runs along: 'Horizontal' (the default) draws a
-- line stretching left-to-right, for separating things stacked in a
-- 'Blink.Layout.Box.vBox'; 'Vertical' draws one stretching top-to-bottom,
-- for separating things side by side in an 'Blink.Layout.Box.hBox'.
-- Resets the default 'Layout' the new axis implies -- apply this before any
-- 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'
-- override in the attribute list, or it will clobber them.
orientation :: Orientation -> Attribute (DividerConfig e msg)
orientation o = Attribute (\dc -> dc { dcOrientation = o, dcLayout = layoutFor o })

-- | How thick the drawn line is, across whichever axis 'orientation' isn't
-- running it along. Defaults to 1. Has no effect if 'Blink.Layout.Constraints.width'\/
-- 'Blink.Layout.Constraints.height' overrides that axis to something other
-- than 'Blink.Layout.Constraints.fitContent'.
thickness :: Double -> Attribute (DividerConfig e msg)
thickness t = Attribute (\dc -> dc { dcThickness = t })

-- | A plain visual separator (see the module header). Never focusable and
-- never claims focus on click, regardless of 'isFocusable'\/'style' --
-- fixed behaviour, not a default, the same way 'Blink.Controls.ProgressBar.progressBar'
-- fixes 'isFocusable' to 'False' itself. Draws nothing when the resolved
-- style's border colour is 'Nothing', the same as a control with no
-- border drawing no chrome border. Takes no id by default -- pass
-- 'elementId' to give one instance a stable identity and react to its
-- hover\/click\/focus events. Defaults to filling the space it's given
-- along 'orientation' and sizing to 'thickness' (plus the current theme's
-- margin\/border\/padding, same as every other control) across it --
-- override with 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'\/
-- 'Blink.Layout.Constraints.align'; when placed in an 'Blink.Layout.Box.hBox'\/
-- 'Blink.Layout.Box.vBox' next to a taller\/wider sibling,
-- 'Blink.Layout.Constraints.align' picks where within that extra space the
-- line sits.
divider :: Ord e => [Attribute (DividerConfig e msg)] -> Element e msg
divider attrs = Element
  { elLayout  = dcLayout cfg
  , elMeasure = measureChrome (ccStyleKey ctrl) (Element (dcLayout cfg) intrinsicSize (pure ()))
  , elRun     = void (controlBase ctrl)
  }
  where
    cfg  = resolve defaultDividerConfig attrs
    ctrl = (dcControl cfg)
      { ccIsFocusable  = False
      , ccFocusOnClick = NoFocus
      , ccContent      = body
      }
    t = dcThickness cfg
    -- | 'thickness' on both axes: whichever one the default 'Layout'
    -- actually consults (the 'fitContent' one) is the only one that
    -- matters, since the other is 'fill' and never reaches this at all --
    -- see 'Blink.Layout.Constraints.resolveLength'.
    intrinsicSize = const (pure (Size t t))
    body = do
      s <- currentStyle
      forM_ (styleBorderColour s) fillRect
