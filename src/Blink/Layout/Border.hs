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
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.View (View)
import Blink.Element (Attribute (..), Element (..), resolve, runElement)

-- | Every capability 'borderLayout' resolves: its up to five named panels.
data BorderContent e msg = BorderContent
  { bcTop    :: Maybe (Element e msg)
  , bcBottom :: Maybe (Element e msg)
  , bcLeft   :: Maybe (Element e msg)
  , bcRight  :: Maybe (Element e msg)
  , bcCentre :: Maybe (Element e msg)
  }

-- | All panels absent. Override only the ones you need:
--
-- @
-- borderLayout [top header, centre body]
-- @
emptyBorderContent :: BorderContent e msg
emptyBorderContent = BorderContent
  { bcTop    = Nothing
  , bcBottom = Nothing
  , bcLeft   = Nothing
  , bcRight  = Nothing
  , bcCentre = Nothing
  }

-- | A panel spanning the full width at the top, as high as @el@'s own
-- height asks.
top :: Element e msg -> Attribute (BorderContent e msg)
top el = Attribute (\bc -> bc { bcTop = Just el })

-- | A panel spanning the full width at the bottom, as high as @el@'s own
-- height asks.
bottom :: Element e msg -> Attribute (BorderContent e msg)
bottom el = Attribute (\bc -> bc { bcBottom = Just el })

-- | A panel on the left of the middle row, as wide as @el@'s own width
-- asks.
left :: Element e msg -> Attribute (BorderContent e msg)
left el = Attribute (\bc -> bc { bcLeft = Just el })

-- | A panel on the right of the middle row, as wide as @el@'s own width
-- asks.
right :: Element e msg -> Attribute (BorderContent e msg)
right el = Attribute (\bc -> bc { bcRight = Just el })

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
-- 'top' and 'bottom' span the full width at their element's own height;
-- 'left' and 'right' fill the middle row's height at their element's own
-- width. 'centre' fills whatever space is left. Any panel may be omitted,
-- in which case the remaining panels expand to fill the gap. Give a panel
-- a 'fill' height (or width, for 'left' and 'right') and it shares the
-- leftover space with the middle row instead.
--
-- No spacing or margin is applied. Clipping follows 'Blink.Layout.Box.vBox' and
-- 'Blink.Layout.Box.hBox': the top, middle, and bottom rows are clipped as a
-- group to the whole region, and within the middle row the left, centre, and
-- right panels are further clipped as a group to that row. An oversized
-- panel can still overlap its neighbours within the same row.
borderLayout :: [Attribute (BorderContent e msg)] -> View e msg ()
borderLayout attrs =
  runElement $ vBox [children (catMaybes [topRow, middleRow, bottomRow])]
  where
    bc = resolve emptyBorderContent attrs

    fullWidth  el = el { elLayout = (elLayout el) { layoutWidth = fill } }
    fullHeight el = el { elLayout = (elLayout el) { layoutHeight = fill } }

    topRow    = fullWidth <$> bcTop bc
    bottomRow = fullWidth <$> bcBottom bc

    middleCells = catMaybes
      [ fullHeight <$> bcLeft bc
      , (\el -> el { elLayout = Layout fill fill TopLeft }) <$> bcCentre bc
      , fullHeight <$> bcRight bc
      ]

    middleRow
      | null middleCells = Nothing
      | otherwise        = Just (hBox [children middleCells])
