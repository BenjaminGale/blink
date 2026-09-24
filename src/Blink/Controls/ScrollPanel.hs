{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A scrollable viewport onto a single child element, which may be
-- larger than the space the panel is given -- typically a layout like
-- 'Blink.Layout.Box.vBox' stacking several rows or sections. Shows a
-- vertical and\/or horizontal 'Blink.Controls.ScrollBar.scrollBar' along
-- whichever edge(s) the content overflows.
--
-- Unlike 'Blink.Controls.List.list', the child isn't virtualised: it has
-- no fixed row shape to skip cheaply, so every part of it is built and
-- laid out every frame, only clipped\/offset at draw time.
--
-- @
-- scrollPanel --> vBox --> hBox --> content (clipped, offset)
--                       |        \\-> vertical scrollBar (if content overflows vertically)
--                       \\-> hBox --> horizontal scrollBar (if content overflows horizontally)
--                                \\-> corner spacer (if both bars show)
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

import Control.Monad (void, when)

import Blink.Controls.Control
import Blink.Controls.ScrollBar (ScrollBarPart (..), scrollBar, scrollBarOrientation, scrollBarThickness, visibleFraction)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, emptyElement, height, runElement, width)
import Blink.Geometry (Alignment (TopLeft), Orientation (..), Rectangle (..), Size (..))
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Available (..), Layout (..), MeasureCtx (..), exactly, fill)
import Blink.View
import Blink.View.Drawing (withClip)
import Blink.Style
import Blink.Controls.Style (toggleGroupMetrics, toggleGroupStyle)

-- | Identifies one of the two scrollbars 'scrollPanel' can composite in.
data ScrollPanelPart
  = ScrollPanelHBar ScrollBarPart
  | ScrollPanelVBar ScrollBarPart
  deriving (Eq, Ord, Show)

-- | Every capability 'scrollPanel' resolves.
data ScrollPanelConfig e msg = ScrollPanelConfig
  { spControl :: ControlConfig e msg
  , spLayout  :: Layout
  , spContent :: Element e msg
  }

instance HasControlConfig e msg (ScrollPanelConfig e msg) where
  overControl attr = Attribute (\c -> c { spControl = runAttribute (overControl attr) (spControl c) })

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
content :: Element e msg -> Attribute (ScrollPanelConfig e msg)
content el = Attribute (\c -> c { spContent = el })

-- | Pixels a single mouse-wheel notch scrolls.
wheelStepPx :: Double
wheelStepPx = 48

-- | A scrollable viewport onto @cfg@'s own 'spContent' (see 'content').
scrollPanel :: Ord e => (ScrollPanelPart -> e) -> [Attribute (ScrollPanelConfig e msg)] -> Element e msg
scrollPanel tag attrs = Element
  { elLayout  = spLayout cfg
  , elMeasure = measureChrome (ccStyleKey (spControl cfg)) measureEl
  , elRun     = void (control ctrl)
  }
  where
    cfg   = resolve defaultScrollPanelConfig attrs
    child = spContent cfg

    hScrollEid = tag (ScrollPanelHBar ScrollBar)
    vScrollEid = tag (ScrollPanelVBar ScrollBar)

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
      pure (w, h)

    ctrl = (spControl cfg)
      { ccFocusPolicy = NotFocusable
      , ccContent     = const viewport
      }

    -- A scrollbar shown on one axis takes space from the other, which can
    -- itself tip that axis into overflow -- so the overflow check runs
    -- twice: once against the full bounds, once against what's left after
    -- the first pass's own bar(s).
    viewport = do
      bounds <- getBounds
      (contentW, contentH) <- naturalSize
      let viewportW0 = rectWidth bounds
          viewportH0 = rectHeight bounds
          showV0     = contentH > viewportH0
          showH0     = contentW > viewportW0
          viewportW  = viewportW0 - (if showV0 then scrollBarThickness else 0)
          viewportH  = viewportH0 - (if showH0 then scrollBarThickness else 0)
          showV      = contentH > viewportH
          showH      = contentW > viewportW
      if not showV && not showH
        then runElement child
        else runElement (scrollableArea contentW contentH viewportW viewportH showV showH)

    scrollableArea contentW contentH viewportW viewportH showV showH = vBox
      [ children
          ( hBox
              [ children
                  ( elementWithLayout (Layout fill fill TopLeft)
                      (clippedContent contentW contentH viewportW viewportH showV showH)
                    : [ vBar | showV ]
                  )
              ]
            : [ hBox
                  [ height (exactly scrollBarThickness)
                  , children (hBar : [ corner | showV ])
                  ]
              | showH
              ]
          )
      ]
      where
        vBar = scrollBar (tag . ScrollPanelVBar)
          [ scrollBarOrientation Vertical, height fill, visibleFraction (viewportH / contentH) ]
        hBar = scrollBar (tag . ScrollPanelHBar)
          [ scrollBarOrientation Horizontal, width fill, visibleFraction (viewportW / contentW) ]
        corner = elementWithLayout (Layout (exactly scrollBarThickness) (exactly scrollBarThickness) TopLeft) (pure ())

    -- 'withClip' must capture this bounds -- the viewport's own, not yet
    -- offset -- before the child moves within it.
    clippedContent contentW contentH viewportW viewportH showV showH = do
      applyWheel contentW contentH viewportW viewportH showV showH
      bounds <- getBounds
      hFrac  <- if showH then getScrollState hScrollEid else pure 0
      vFrac  <- if showV then getScrollState vScrollEid else pure 0
      let offsetX = if showH then hFrac * (contentW - viewportW) else 0
          offsetY = if showV then vFrac * (contentH - viewportH) else 0
          contentBounds = bounds
            { rectX      = rectX bounds - offsetX
            , rectY      = rectY bounds - offsetY
            , rectWidth  = if showH then contentW else rectWidth bounds
            , rectHeight = if showV then contentH else rectHeight bounds
            }
      withClip $ withBounds contentBounds (runElement child)

    -- Only a vertical wheel delta exists in the input model, so it drives
    -- whichever axis actually scrolls, favouring vertical.
    applyWheel contentW contentH viewportW viewportH showV showH = do
      wheel <- getWheelDelta
      when (wheel /= 0) $ do
        over <- isRegionHit
        when over $ case (showV, showH) of
          (True, _)      -> scrollBy vScrollEid (contentH - viewportH) wheel
          (False, True)  -> scrollBy hScrollEid (contentW - viewportW) wheel
          (False, False) -> pure ()

    scrollBy eid maxOffset wheel =
      when (maxOffset > 0) $ requestScrollBy eid (wheel * wheelStepPx / maxOffset)

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
