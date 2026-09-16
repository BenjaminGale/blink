-- | 'popup', the entry point into Blink's deferred overlay layer: menus,
-- tooltips, and comboboxes are built on top of it. A call to 'popup' does
-- not run its content inline -- it captures an anchor, measures the
-- content, and queues both via 'Blink.View.queuePopup' instead, so the
-- content runs after the whole view tree for the frame, landing on top of
-- (and unclipped by) everything else, positioned by 'Blink.Geometry.placePopup'.
-- See "Blink.App" for where that later run and placement happens.
module Blink.Popup
  ( PopupConfig
  , content
  , at
  , placement
  , offset
  , Side (..)
  , Edge (..)
  , popup
  ) where

import Blink.Element (Attribute (..), Element (..), emptyElement, measureElement, resolve)
import Blink.Geometry (Edge (..), Point (..), Rectangle (..), Side (..))
import Blink.View (PendingPopup (..), View, getBounds, getCurrentScope, getWindowSize, queuePopup)

-- | Every capability 'popup' resolves: the content to show, where to anchor
-- it, and where it sits relative to that anchor. Defaults to 'emptyElement',
-- anchored to the calling control's own bounds (see 'at' to anchor at a
-- point instead), placed below the anchor and left-aligned with it
-- (@'SideBottom', 'Start'@), with no gap.
data PopupConfig e msg = PopupConfig
  { popContent   :: Element e msg
  , popAt        :: Maybe Point
  , popPlacement :: (Side, Edge)
  , popOffset    :: Double
  }

defaultPopupConfig :: PopupConfig e msg
defaultPopupConfig = PopupConfig
  { popContent   = emptyElement
  , popAt        = Nothing
  , popPlacement = (SideBottom, Start)
  , popOffset    = 0
  }

-- | The popup's content. Defaults to 'emptyElement' -- a 'popup' call with
-- no 'content' queues and later runs nothing.
content :: Element e msg -> Attribute (PopupConfig e msg)
content el = Attribute (\c -> c { popContent = el })

-- | Anchors the popup at an explicit point instead of the calling control's
-- own bounds -- for a context menu, where there is no anchor control, only
-- the point the triggering click landed at.
at :: Point -> Attribute (PopupConfig e msg)
at p = Attribute (\c -> c { popAt = Just p })

-- | Where the popup sits relative to its anchor: which edge it opens from,
-- and how it's aligned along that edge. Defaults to @'SideBottom' 'Start'@.
-- Always flips to the opposite 'Side' if the preferred one would overflow
-- the window -- not a caller-configurable behaviour, see
-- 'Blink.Geometry.placePopup'.
placement :: Side -> Edge -> Attribute (PopupConfig e msg)
placement side edge = Attribute (\c -> c { popPlacement = (side, edge) })

-- | The gap, in pixels, between the anchor's edge and the popup. Defaults
-- to 0.
offset :: Double -> Attribute (PopupConfig e msg)
offset d = Attribute (\c -> c { popOffset = d })

-- | Queues @content@ to run once the main view tree finishes this frame,
-- anchored either to the calling control's current bounds or, with 'at', to
-- an explicit point, and positioned per 'placement'\/'offset' once the
-- content's own size is known.
popup :: e -> [Attribute (PopupConfig e msg)] -> View e msg ()
popup eid attrs = do
  anchor <- case popAt cfg of
    Just p  -> pure (Rectangle (pointX p) (pointY p) 0 0)
    Nothing -> getBounds
  window      <- getWindowSize
  size        <- measureElement window (popContent cfg)
  originScope <- getCurrentScope
  queuePopup PendingPopup
    { popupId          = eid
    , popupAnchor      = anchor
    , popupSize        = size
    , popupPlacement   = popPlacement cfg
    , popupOffset      = popOffset cfg
    , popupRun         = elRun (popContent cfg)
    , popupOriginScope = originScope
    }
  where
    cfg = resolve defaultPopupConfig attrs
