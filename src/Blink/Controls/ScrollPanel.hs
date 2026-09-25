{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A scrollable viewport onto a single child element, which may be
-- larger than the space the panel is given -- typically a layout like
-- 'Blink.Layout.Box.vBox' stacking several rows or sections. Shows a
-- vertical and\/or horizontal 'Blink.Controls.ScrollBar.scrollBar' along
-- whichever edge(s) the content overflows, via
-- 'Blink.Controls.ScrollBar.scrollViewport'.
--
-- Unlike 'Blink.Controls.List.list', the child isn't virtualised: it has
-- no fixed row shape to skip cheaply, so every part of it is built and
-- laid out every frame, only clipped\/offset at draw time.
--
-- @
-- control --> scrollViewport --> content
-- @
module Blink.Controls.ScrollPanel
  ( ScrollPanelPart (..)
  , ScrollPanelConfig (..)
  , defaultScrollPanelConfig
  , scrollPanel
  , content
    -- * Style
  , scrollPanelStyleKey
  , defaultStyleEntries
  ) where


import Blink.Controls.Control
import Blink.Controls.ScrollBar (ScrollViewportConfig (..), ScrollViewportPart, scrollViewport)
import Blink.Element (Element (..), HasLayoutConfig (..), emptyElement, runElement, HasContent (..))
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Size (..))
import Blink.Layout.Constraints (Available (..), Layout (..), MeasureCtx (..), fill)
import Blink.Style
import Blink.Controls.Style (toggleGroupMetrics, toggleGroupStyle)

-- | Identifies one part of a 'scrollPanel' for the purpose of building
-- element ids: the panel's own root, or a part of its
-- 'Blink.Controls.ScrollBar.scrollViewport'.
data ScrollPanelPart
  = ScrollPanel
  | ScrollPanelViewport ScrollViewportPart
  deriving (Eq, Ord, Show)

-- | Every capability 'scrollPanel' resolves.
data ScrollPanelConfig e msg = ScrollPanelConfig
  { spControl :: ControlConfig e msg
  , spLayout  :: Layout
  , spContent :: Element e msg
  }

instance HasControlConfig e msg (ScrollPanelConfig e msg) where
  overControl attr = Attribute (\c -> c { spControl = runAttribute (overControl attr) (spControl c) })

instance HasEventHandlers (ScrollPanelConfig e msg)

instance HasLayoutConfig (ScrollPanelConfig e msg) where
  overLayout attr = Attribute (\c -> c { spLayout = runAttribute attr (spLayout c) })

-- | Fills whatever space it's given on both axes -- sizing to the
-- content's own extent instead would leave nothing to ever scroll.
defaultScrollPanelConfig :: ScrollPanelConfig e msg
defaultScrollPanelConfig = ScrollPanelConfig
  { spControl = defaultControlConfig { ccStyleKey = scrollPanelStyleKey }
  , spLayout  = Layout fill fill TopLeft
  , spContent = emptyElement
  }

-- | The single child the panel scrolls.
instance HasContent e msg (ScrollPanelConfig e msg) where
  content el = Attribute (\c -> c { spContent = el })

-- | Pixels a single mouse-wheel notch scrolls.
wheelStepPx :: Double
wheelStepPx = 48

-- | A scrollable viewport onto @cfg@'s own 'spContent' (see 'content').
-- @tag@ builds every part's element id from a 'ScrollPanelPart'.
scrollPanel :: Ord e => (ScrollPanelPart -> e) -> [Attribute (ScrollPanelConfig e msg)] -> Element e msg
scrollPanel tag attrs = controlElement (spLayout cfg) measureEl ctrl
  where
    cfg   = resolve defaultScrollPanelConfig attrs
    child = spContent cfg

    -- Every other 'elMeasure' caller offers real, bounded space; this asks
    -- the child for its size with no limit on either axis, since that's
    -- what decides whether it needs to scroll at all. A 'fill' child (e.g.
    -- a row stretching to the container's width) has no natural size and
    -- measures 0 here -- correctly, since it can't overflow an axis it
    -- always just adapts to.
    childNaturalSize ctx = elMeasure child ctx { measureMain = Unbounded, measureCross = Unbounded }

    measureEl = Element (Layout fill fill TopLeft) childNaturalSize (pure ())

    naturalSize = do
      w <- sizeWidth  <$> childNaturalSize (MeasureCtx Horizontal Unbounded Unbounded)
      h <- sizeHeight <$> childNaturalSize (MeasureCtx Vertical Unbounded Unbounded)
      pure (Size w h)

    ctrl = (spControl cfg)
      { ccElementId   = Just (tag ScrollPanel)
      , ccFocusPolicy = NotFocusable
      , ccContent     = const viewport
      }

    viewport = do
      contentSize <- naturalSize
      scrollViewport (tag . ScrollPanelViewport) ScrollViewportConfig
        { svWheelStep   = wheelStepPx
        , svContentSize = contentSize
        , svContent     = const (runElement child)
        }

-- * Style

-- | The 'StyleKey' 'Blink.Controls.ScrollPanel.scrollPanel' resolves its
-- own chrome from unless overridden via 'Blink.Controls.Control.style'.
scrollPanelStyleKey :: StyleKey e
scrollPanelStyleKey = Class "scrollPanel"

-- | This control's entry in 'Blink.Style.Defaults.defaultTheme':
-- 'Blink.Controls.Style.toggleGroupStyle', the same transparent, borderless
-- wrapper look 'Blink.Controls.ScrollBar.scrollBar' uses for its own outer
-- container. A scroll panel exists to make existing content scrollable, not
-- to impose a visual boundary of its own, and, being
-- 'Blink.Controls.Control.NotFocusable', never needs a focus ring either.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (scrollPanelStyleKey, (toggleGroupMetrics, toggleGroupStyle p)) ]
