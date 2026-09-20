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

import Blink.Geometry (Alignment (..))
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.View (View)
import Blink.Element (Attribute (..), Element (..), resolve, runElement)

-- | Every capability 'borderLayout' resolves: its up to five named panels.
data BorderContent e msg = BorderContent
  { bcTop    :: Maybe (Double, Element e msg)
  , bcBottom :: Maybe (Double, Element e msg)
  , bcLeft   :: Maybe (Double, Element e msg)
  , bcRight  :: Maybe (Double, Element e msg)
  , bcCentre :: Maybe (Element e msg)
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
top :: Double -> Element e msg -> Attribute (BorderContent e msg)
top h el = Attribute (\bc -> bc { bcTop = Just (h, el) })

-- | A fixed-height panel spanning the full width at the bottom.
bottom :: Double -> Element e msg -> Attribute (BorderContent e msg)
bottom h el = Attribute (\bc -> bc { bcBottom = Just (h, el) })

-- | A fixed-width panel on the left of the middle row.
left :: Double -> Element e msg -> Attribute (BorderContent e msg)
left w el = Attribute (\bc -> bc { bcLeft = Just (w, el) })

-- | A fixed-width panel on the right of the middle row.
right :: Double -> Element e msg -> Attribute (BorderContent e msg)
right w el = Attribute (\bc -> bc { bcRight = Just (w, el) })

-- | A panel filling whatever space is left in the middle row.
centre :: Element e msg -> Attribute (BorderContent e msg)
centre el = Attribute (\bc -> bc { bcCentre = Just el })

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
--
-- Each panel's own region dictates its size, so a passed-in element's own
-- 'Layout' is overridden -- there's nothing to gain from asking it, since
-- 'top'\/'bottom'\/'left'\/'right' fix one axis outright and 'centre' just
-- fills whatever is left.
borderLayout :: [Attribute (BorderContent e msg)] -> View e msg ()
borderLayout attrs =
  runElement $ vBox [children (catMaybes [topRow, middleRow, bottomRow])]
  where
    bc = resolve emptyBorderContent attrs

    topRow    = (\(h, el) -> el { elLayout = Layout fill (exactly h) TopLeft }) <$> bcTop bc
    bottomRow = (\(h, el) -> el { elLayout = Layout fill (exactly h) TopLeft }) <$> bcBottom bc

    middleCells = catMaybes
      [ (\(w, el) -> el { elLayout = Layout (exactly w) fill TopLeft }) <$> bcLeft bc
      , (\el      -> el { elLayout = Layout fill        fill TopLeft }) <$> bcCentre bc
      , (\(w, el) -> el { elLayout = Layout (exactly w) fill TopLeft }) <$> bcRight bc
      ]

    middleRow
      | null middleCells = Nothing
      | otherwise        = Just (hBox [children middleCells])
