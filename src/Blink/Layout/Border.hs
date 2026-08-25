-- | Divides the available space into up to five named regions.
module Blink.Layout.Border
  ( borderLayout
  , BorderContent (..)
  , emptyBorderContent
  ) where

import Data.Maybe (catMaybes)

import Blink.Geometry (Alignment (..))
import Blink.Layout.Box (hBox, vBox)
import Blink.Layout.Core (Layout (..), Length (..))
import Blink.UI (UI)

-- | Specifies which panels to show in a 'borderLayout'.
data BorderContent e msg = BorderContent
  { topPanel    :: Maybe (Double, UI e msg ())
  , bottomPanel :: Maybe (Double, UI e msg ())
  , leftPanel   :: Maybe (Double, UI e msg ())
  , rightPanel  :: Maybe (Double, UI e msg ())
  , centrePanel :: Maybe (UI e msg ())
  }

-- | All panels absent; use record update to populate only the ones you need.
--
-- @
-- borderLayout emptyBorderContent
--   { topPanel    = Just (3, header)
--   , centrePanel = Just body
--   }
-- @
emptyBorderContent :: BorderContent e msg
emptyBorderContent = BorderContent
  { topPanel    = Nothing
  , bottomPanel = Nothing
  , leftPanel   = Nothing
  , rightPanel  = Nothing
  , centrePanel = Nothing
  }

-- | Divides the available space into up to five named regions.
--
-- >  +------------------------------------------+
-- >  |                   top                    |
-- >  +--------+------------------------+--------+
-- >  |        |                        |        |
-- >  |  left  |         centre         | right  |
-- >  |        |                        |        |
-- >  +--------+------------------------+--------+
-- >  |                  bottom                  |
-- >  +------------------------------------------+
--
-- 'topPanel' and 'bottomPanel' each take a fixed height and span the full
-- width. 'leftPanel' and 'rightPanel' each take a fixed width within the
-- middle row. 'centrePanel' fills whatever space is left. Any panel may be
-- omitted (see 'emptyBorderContent'), in which case the remaining panels
-- expand to fill the gap.
--
-- No spacing or margin is applied. Clipping follows 'Blink.Layout.Box.vBox' and
-- 'Blink.Layout.Box.hBox': the top, middle, and bottom rows are clipped as a
-- group to the whole region, and within the middle row the left, centre, and
-- right panels are further clipped as a group to that row. An oversized
-- panel can still overlap its neighbours within the same row.
borderLayout :: BorderContent e msg -> UI e msg ()
borderLayout bc =
  vBox [] (catMaybes [topRow, middleRow, bottomRow])
  where
    topRow    = (\(h, ui) -> (Layout Fill (Exactly h) TopLeft, ui)) <$> topPanel bc
    bottomRow = (\(h, ui) -> (Layout Fill (Exactly h) TopLeft, ui)) <$> bottomPanel bc

    middleCells = catMaybes
      [ (\(w, ui) -> (Layout (Exactly w) Fill TopLeft, ui)) <$> leftPanel bc
      , (\ui      -> (Layout Fill        Fill TopLeft, ui)) <$> centrePanel bc
      , (\(w, ui) -> (Layout (Exactly w) Fill TopLeft, ui)) <$> rightPanel bc
      ]

    middleRow
      | null middleCells = Nothing
      | otherwise        = Just (Layout Fill Fill TopLeft, hBox [] middleCells)
