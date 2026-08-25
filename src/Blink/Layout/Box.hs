-- | Arranges a list of children along one axis -- 'hBox' left-to-right,
-- 'vBox' top-to-bottom.
module Blink.Layout.Box
  ( hBox
  , vBox
  , BoxConfig
  , defaultBoxConfig
  , spacing
  , margin
  , alignment
  , stretch
  , children
  , boxTotalSpacing
  ) where

import Data.List (foldl', scanl', sortBy)
import Data.Ord  (comparing)

import qualified Data.IntMap.Strict as IntMap

import Blink.Attribute (Attribute (..), resolve)
import Blink.Geometry (Alignment (..), Rectangle (..), alignRect, insetRect, uniform)
import Blink.Layout.Constraints (Layout (..), Length (..), layoutWithConstraints)
import Blink.UI (UI, clipToCurrent, getBounds, withBounds)

-- | Every capability 'hBox'\/'vBox' resolve: spacing, margin, alignment of
-- the content block, whether children stretch to fill the cross axis, and
-- the children themselves (see 'children').
data BoxConfig e msg = BoxConfig
  { bxSpacing    :: Double
  , bxMargin     :: Double
  , bxAlignment  :: Alignment
  , bxFillCross  :: Bool
  , bxChildren   :: [(Layout, UI e msg ())]
  }

-- | No spacing, no margin, 'TopLeft' alignment, cross-axis stretch enabled,
-- and no children. Override only the attributes you need:
--
-- @
-- hBox
--   [ spacing 8, margin 4
--   , children
--       [ (Layout (Exactly 80) Fill TopLeft, sidebar)
--       , (Layout Fill         Fill TopLeft, content)
--       ]
--   ]
-- @
defaultBoxConfig :: BoxConfig e msg
defaultBoxConfig = BoxConfig
  { bxSpacing   = 0
  , bxMargin    = 0
  , bxAlignment = TopLeft
  , bxFillCross = True
  , bxChildren  = []
  }

-- | Gap in pixels between consecutive children on the main axis. Defaults to @0@.
spacing :: Double -> Attribute (BoxConfig e msg)
spacing v = Attribute (\c -> c { bxSpacing = v })

-- | Uniform inset applied to all four sides of the panel before layout.
-- Defaults to @0@.
margin :: Double -> Attribute (BoxConfig e msg)
margin v = Attribute (\c -> c { bxMargin = v })

-- | Positions the content block within the content area on the main axis.
-- Controls where whitespace falls when children are smaller than the
-- content area, and which side clips when they overflow. Defaults to
-- 'TopLeft'.
--
-- When the children take up less space than the panel, this is where
-- the leftover whitespace goes:
--
-- >  +----------------------+-----------------+
-- >  |       children       |                 |
-- >  +----------------------+-----------------+
-- >  TopLeft: children lead, whitespace trails
--
-- >  +--------+----------------------+--------+
-- >  |        |       children       |        |
-- >  +--------+----------------------+--------+
-- >  Center: whitespace is split evenly
--
-- >  +-----------------+----------------------+
-- >  |                 |       children       |
-- >  +-----------------+----------------------+
-- >  BottomRight: whitespace leads, children trail
--
-- When the children take up more space than the panel, this is which
-- side gets clipped:
--
-- >  +------------------------------------------+
-- >  | children (too wide) -->                  |
-- >  +------------------------------------------+
-- >  TopLeft: the left edge is anchored, right side clips
--
-- >  +------------------------------------------+
-- >  |                  <-- children (too wide) |
-- >  +------------------------------------------+
-- >  BottomRight: the right edge is anchored, left side clips
alignment :: Alignment -> Attribute (BoxConfig e msg)
alignment v = Attribute (\c -> c { bxAlignment = v })

-- | Whether children stretch to fill the full cross-axis extent. Defaults
-- to 'True'.
stretch :: Bool -> Attribute (BoxConfig e msg)
stretch v = Attribute (\c -> c { bxFillCross = v })

-- | The children to arrange, each paired with the 'Layout' governing its
-- size and alignment within its slot (see 'hBox'\/'vBox'). Defaults to
-- @[]@; a later 'children' attribute replaces an earlier one rather than
-- adding to it, the same as every other attribute here.
children :: [(Layout, UI e msg ())] -> Attribute (BoxConfig e msg)
children cs = Attribute (\c -> c { bxChildren = cs })

-- | Total space consumed by all gaps between @n@ children -- the sum of
--   @(n - 1)@ spacings.
--
-- >>> boxTotalSpacing (resolve defaultBoxConfig [spacing 8]) 3
-- 16.0
-- >>> boxTotalSpacing (resolve defaultBoxConfig [spacing 8]) 1
-- 0.0
boxTotalSpacing :: BoxConfig e msg -> Int -> Double
boxTotalSpacing cfg n = bxSpacing cfg * fromIntegral (max 0 (n - 1))

-- | Abstracts over the two layout orientations so that 'box' can be written
--   once. Each field encodes the axis-specific behaviour; 'horizontal' and
--   'vertical' are the only two values.
data Axis = Axis
  { mainConstraint :: Layout -> Length
    -- ^ Extracts the child's constraint along the main axis.
  , mainLength     :: Rectangle -> Double
    -- ^ Length of a rectangle along the main axis.
  , crossLength    :: Rectangle -> Double
    -- ^ Length of a rectangle along the cross axis.
  , mainOrigin     :: Rectangle -> Double
    -- ^ Origin of a rectangle along the main axis.
  , crossOrigin    :: Rectangle -> Double
    -- ^ Origin of a rectangle along the cross axis.
  , makeSlot       :: Double -> Double -> Double -> Double -> Rectangle
    -- ^ Builds a slot rectangle from @(mainOrigin, crossOrigin, mainLen, crossLen)@.
  , fillCross      :: Layout -> Layout
    -- ^ Overrides the child's cross-axis constraint with 'Fill'.
  }

horizontal :: Axis
horizontal = Axis
  { mainConstraint = layoutWidth
  , mainLength     = rectWidth
  , crossLength    = rectHeight
  , mainOrigin     = rectX
  , crossOrigin    = rectY
  , makeSlot       = Rectangle
  , fillCross      = \rc -> rc { layoutHeight = Fill }
  }

vertical :: Axis
vertical = Axis
  { mainConstraint = layoutHeight
  , mainLength     = rectHeight
  , crossLength    = rectWidth
  , mainOrigin     = rectY
  , crossOrigin    = rectX
  , makeSlot       = \mo co ms cs -> Rectangle co mo cs ms
  , fillCross      = \rc -> rc { layoutWidth = Fill }
  }

-- | Arranges children left-to-right. Each child (see 'children') carries a
--   'Layout' governing its width and, when 'stretch' is 'False', its
--   height and vertical alignment. If a margin is set, children are laid
--   out within that inset.
--
--   The axis along which children are stacked (here, horizontal) is called
--   the /main axis/, and the perpendicular axis is the /cross axis/. 'vBox'
--   uses the same algorithm with the axes swapped, so its main axis runs
--   top-to-bottom and its cross axis runs left-to-right.
--
--   >  main axis (width) ------------------------->
--   >  +------------+--------------+------------+
--   >  |     A      |      B       |     C      |
--   >  +------------+--------------+------------+
--   >  cross axis (height)
--
--   * The panel fills its available space, minus an optional margin.
--   * Children are laid out in a line with optional gaps between them.
--   * Fixed-size children take exactly the space they ask for.
--   * Flexible children share whatever space is left over equally.
--   * If a flexible child has a maximum size and its share would exceed it,
--     it takes only its maximum and the remainder is shared among the
--     others.
--   * The group is aligned within the content area according to
--     'alignment'. When children are smaller than the content area this
--     controls where the whitespace goes. When they overflow it controls
--     which side clips.
--   * Once each child's space is allocated, it is positioned and aligned
--     within its slot according to its own 'Layout'.
--   * By default children are stretched to fill the panel on the cross axis,
--     but this can be disabled to let each child control its own size on
--     that axis.
--   * Children are clipped to the panel's content area as a group, not
--     individually. An oversized child can still overlap its neighbours; it
--     is only cut off once it reaches the edge of the panel itself.
--
-- @
-- hBox
--   [ spacing 4
--   , children
--       [ (Layout (Exactly 80) Fill TopLeft, button Btn1 [text "Back"])
--       , (Layout Fill         Fill TopLeft, button Btn2 [text "Title"])
--       , (Layout (Exactly 80) Fill TopLeft, button Btn3 [text "Next"])
--       ]
--   ]
-- @
--
-- Here the two outer buttons are fixed at 80px wide. The centre button
-- expands to fill whatever space remains. The 'Fill' height constraint in
-- each child means height is determined by the panel, not the child.
--
-- >  +--------+------------------------------------+--------+
-- >  |  Back  |               Title                |  Next  |
-- >  |  80px  |                Fill                |  80px  |
-- >  +--------+------------------------------------+--------+
--
-- 'stretch' (default 'True') controls the cross axis, which is height
-- in this example. When 'True', each child is stretched to the panel's full
-- height, as in the diagram above. When 'False', each child keeps its own
-- height (here, 'TopLeft'-aligned), leaving the rest of the panel blank.
--
-- >  +------------+--------------+------------+
-- >  |     A      |      B       |     C      |
-- >  +------------+--------------+------------+
-- >  |                                        |
-- >  +----------------------------------------+
hBox :: [Attribute (BoxConfig e msg)] -> UI e msg ()
hBox attrs = box horizontal (resolve defaultBoxConfig attrs)

-- | Arranges children top-to-bottom. Each child (see 'children') carries a
--   'Layout' governing its height and, when 'stretch' is 'False', its
--   width and horizontal alignment. Uses the same algorithm as 'hBox' with
--   the axes swapped. See its documentation for the full behaviour.
--
-- @
-- vBox
--   [ spacing 1
--   , children
--       [ (Layout Fill (Exactly 3) TopLeft, header)
--       , (Layout Fill Fill        TopLeft, body)
--       , (Layout Fill (Exactly 3) TopLeft, footer)
--       ]
--   ]
-- @
--
-- The header and footer are fixed at 3 rows tall. The body expands to fill
-- whatever space remains.
--
-- >  +------------------------------------------+
-- >  |           Header (Exactly 3px)           |
-- >  +------------------------------------------+
-- >  |                                          |
-- >  |               Body (Fill)                |
-- >  |                                          |
-- >  +------------------------------------------+
-- >  |           Footer (Exactly 3px)           |
-- >  +------------------------------------------+
vBox :: [Attribute (BoxConfig e msg)] -> UI e msg ()
vBox attrs = box vertical (resolve defaultBoxConfig attrs)

box :: Axis -> BoxConfig e msg -> UI e msg ()
box ax cfg = do
  r <- getBounds
  let kids         = bxChildren cfg
      contentArea  = insetRect (uniform (bxMargin cfg)) r
      totalSpacing = boxTotalSpacing cfg (length kids)
      availSpace   = mainLength ax contentArea - totalSpacing
      slotSizes    = preferredSizes availSpace (map (mainConstraint ax . fst) kids)
      crossOrig    = crossOrigin ax contentArea
      crossLen     = crossLength ax contentArea
      contentBlock = alignRect (bxAlignment cfg) contentArea
                       (makeSlot ax 0 0 (foldl' (+) 0 slotSizes + totalSpacing) crossLen)
      slotOrigins  = scanl' (\o s -> o + s + bxSpacing cfg) (mainOrigin ax contentBlock) slotSizes
  withBounds contentArea $ do
    clipToCurrent $ do
      sequence_ $ zipWith3
        (\slotOrigin slotSize (rc, ui) ->
          let slotRect    = makeSlot ax slotOrigin crossOrig slotSize crossLen
              effectiveRc = if bxFillCross cfg then fillCross ax rc else rc
          in withBounds slotRect $ layoutWithConstraints effectiveRc ui)
        slotOrigins slotSizes kids

preferredSizes :: Double -> [Length] -> [Double]
preferredSizes available constraints =
  let (mins, cs) = unzip [(minLength c, c) | c <- constraints]
      surplus    = max 0 (available - foldl' (+) 0 mins)
  in zipWith (+) mins (distributeSurplusSpace surplus cs)

minLength :: Length -> Double
minLength (Exactly w)    = w
minLength Fill           = 0
minLength (AtLeast w)    = w
minLength (AtMost _)     = 0
minLength (Between l _)  = l

canExpand :: Length -> Bool
canExpand (Exactly _) = False
canExpand _           = True

-- Computes how much extra space (above each constraint's minimum) each slot
-- receives, distributing surplus equally and redistributing any space left
-- over from slots that hit their cap.
distributeSurplusSpace :: Double -> [Length] -> [Double]
distributeSurplusSpace surplus constraints =
  let flexible = sortBy (comparing snd) [(i, cap c) | (i, c) <- zip [0..] constraints, canExpand c]
      shares   = go surplus (length flexible) flexible
  in let shareMap = IntMap.fromList shares
     in [IntMap.findWithDefault 0 i shareMap | i <- [0 .. length constraints - 1]]
  where
    go _ _ [] = []
    go s n ((i, c) : rest) =
      let share = s / fromIntegral n
      in if c <= share
         then (i, c) : go (s - c) (n - 1) rest
         else [(j, share) | (j, _) <- (i, c) : rest]
    cap (AtMost w)    = w
    cap (Between l h) = h - l
    cap _             = 1 / 0
