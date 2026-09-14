-- | 'popup', the entry point into Blink's deferred overlay layer: menus,
-- tooltips, and comboboxes are built on top of it. A call to 'popup' does
-- not run its content inline -- it captures an anchor and queues the
-- content via 'Blink.View.queuePopup' instead, so the content runs after
-- the whole view tree for the frame, landing on top of (and unclipped by)
-- everything else. See "Blink.App" for where that later run happens.
module Blink.Popup
  ( PopupConfig
  , content
  , at
  , popup
  ) where

import Blink.Element (Attribute (..), Element (..), emptyElement, resolve)
import Blink.Geometry (Point (..), Rectangle (..))
import Blink.View (PendingPopup (..), View, getBounds, queuePopup)

-- | Every capability 'popup' resolves: the content to show, and where to
-- anchor it. Defaults to 'emptyElement', anchored to the calling control's
-- own bounds (see 'at' to anchor at a point instead).
data PopupConfig e msg = PopupConfig
  { popContent :: Element e msg
  , popAt      :: Maybe Point
  }

defaultPopupConfig :: PopupConfig e msg
defaultPopupConfig = PopupConfig { popContent = emptyElement, popAt = Nothing }

-- | The popup's content. Defaults to 'emptyElement' -- a 'popup' call with
-- no 'content' queues and later runs nothing.
content :: Element e msg -> Attribute (PopupConfig e msg)
content el = Attribute (\c -> c { popContent = el })

-- | Anchors the popup at an explicit point instead of the calling control's
-- own bounds -- for a context menu, where there is no anchor control, only
-- the point the triggering click landed at.
at :: Point -> Attribute (PopupConfig e msg)
at p = Attribute (\c -> c { popAt = Just p })

-- | Queues @content@ to run once the main view tree finishes this frame,
-- anchored either to the calling control's current bounds or, with 'at', to
-- an explicit point. Placement of the final on-screen rect relative to that
-- anchor is not yet implemented -- the content currently runs at the anchor
-- rect itself.
popup :: e -> [Attribute (PopupConfig e msg)] -> View e msg ()
popup eid attrs = do
  anchor <- case popAt cfg of
    Just p  -> pure (Rectangle (pointX p) (pointY p) 0 0)
    Nothing -> getBounds
  queuePopup PendingPopup { popupId = eid, popupAnchor = anchor, popupRun = elRun (popContent cfg) }
  where
    cfg = resolve defaultPopupConfig attrs
