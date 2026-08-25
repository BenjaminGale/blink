-- | Divides the available space into up to five named regions.
module Blink.Layout.Border
  ( borderLayout
  , BorderContent
  , emptyBorderContent
  , top
  , bottom
  , left
  , right
  , centre
  ) where

import Data.Maybe (catMaybes)

import Blink.Attribute (Attribute (..), resolve)
import Blink.Geometry (Alignment (..))
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Layout (..), Length (..))
import Blink.UI (UI)

-- | Every capability 'borderLayout' resolves: its up to five named panels.
data BorderContent e msg = BorderContent
  { bcTop    :: Maybe (Double, UI e msg ())
  , bcBottom :: Maybe (Double, UI e msg ())
  , bcLeft   :: Maybe (Double, UI e msg ())
  , bcRight  :: Maybe (Double, UI e msg ())
  , bcCentre :: Maybe (UI e msg ())
  }

-- | All panels absent. Override only the ones you need:
--
-- @
-- borderLayout [top 3 header, centre body]
-- @
emptyBorderContent :: BorderContent e msg
emptyBorderContent = BorderContent
  { bcTop    = Nothing
  , bcBottom = Nothing
  , bcLeft   = Nothing
  , bcRight  = Nothing
  , bcCentre = Nothing
  }

-- | A fixed-height panel spanning the full width at the top.
top :: Double -> UI e msg () -> Attribute (BorderContent e msg)
top h ui = Attribute (\bc -> bc { bcTop = Just (h, ui) })

-- | A fixed-height panel spanning the full width at the bottom.
bottom :: Double -> UI e msg () -> Attribute (BorderContent e msg)
bottom h ui = Attribute (\bc -> bc { bcBottom = Just (h, ui) })

-- | A fixed-width panel on the left of the middle row.
left :: Double -> UI e msg () -> Attribute (BorderContent e msg)
left w ui = Attribute (\bc -> bc { bcLeft = Just (w, ui) })

-- | A fixed-width panel on the right of the middle row.
right :: Double -> UI e msg () -> Attribute (BorderContent e msg)
right w ui = Attribute (\bc -> bc { bcRight = Just (w, ui) })

-- | A panel filling whatever space is left in the middle row.
centre :: UI e msg () -> Attribute (BorderContent e msg)
centre ui = Attribute (\bc -> bc { bcCentre = Just ui })

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
-- 'top' and 'bottom' each take a fixed height and span the full width.
-- 'left' and 'right' each take a fixed width within the middle row.
-- 'centre' fills whatever space is left. Any panel may be omitted, in
-- which case the remaining panels expand to fill the gap.
--
-- No spacing or margin is applied. Clipping follows 'Blink.Layout.Box.vBox' and
-- 'Blink.Layout.Box.hBox': the top, middle, and bottom rows are clipped as a
-- group to the whole region, and within the middle row the left, centre, and
-- right panels are further clipped as a group to that row. An oversized
-- panel can still overlap its neighbours within the same row.
borderLayout :: [Attribute (BorderContent e msg)] -> UI e msg ()
borderLayout attrs =
  vBox [children (catMaybes [topRow, middleRow, bottomRow])]
  where
    bc = resolve emptyBorderContent attrs

    topRow    = (\(h, ui) -> (Layout Fill (Exactly h) TopLeft, ui)) <$> bcTop bc
    bottomRow = (\(h, ui) -> (Layout Fill (Exactly h) TopLeft, ui)) <$> bcBottom bc

    middleCells = catMaybes
      [ (\(w, ui) -> (Layout (Exactly w) Fill TopLeft, ui)) <$> bcLeft bc
      , (\ui      -> (Layout Fill        Fill TopLeft, ui)) <$> bcCentre bc
      , (\(w, ui) -> (Layout (Exactly w) Fill TopLeft, ui)) <$> bcRight bc
      ]

    middleRow
      | null middleCells = Nothing
      | otherwise        = Just (Layout Fill Fill TopLeft, hBox [children middleCells])
