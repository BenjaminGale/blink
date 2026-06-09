{- |
Module: Blink.Layout

= How layout works

Every UI component in Blink receives a bounding rectangle and occupies it
entirely by default. The layout system controls what rectangle each component
receives.

= Single control layout

Since components are greedy, 'layoutWithConstraints' is the escape hatch for
sizing and aligning a single component within its parent bounds. A
'RectConstraint' specifies a 'Constraint' on each axis — controlling how much
of the parent space the component takes up — and an 'Alignment' controlling
where it sits within that space.

@
layoutWithConstraints (RectConstraint (Exactly 120) (Exactly 32) Center) $
  button MyBtn "Click me"
@

This renders the button at 120×32 pixels, centred in whatever space the parent
provides, regardless of how large that space is.

= Box layout

'hBox' lays out its children in a single horizontal row; 'vBox' lays them out
in a single vertical column. If a margin is set, children are laid out within
that inset.

Both share the same layout algorithm. The axis along which children are stacked
is called the /main axis/; the perpendicular axis is the /cross axis/.

  * The panel fills its available space, minus an optional margin.
  * Children are laid out in a line with optional gaps between them.
  * Fixed-size children take exactly the space they ask for.
  * Flexible children share whatever space is left over equally.
  * If a flexible child has a maximum size and its share would exceed it, it
    takes only its maximum and the remainder is shared among the others.
  * The group is aligned within the content area according to 'boxAlignment'.
    When children are smaller than the content area this controls where the
    whitespace goes; when they overflow it controls which side clips.
  * Once each child's space is allocated, 'layoutWithConstraints' positions
    the child within its slot.
  * By default children are stretched to fill the panel on the cross axis;
    this can be disabled to let each child control its own size on that axis.
  * Children are clipped to the panel's content area.

@
hBox (defaultBoxConfig { boxSpacing = 4 })
  [ (RectConstraint (Exactly 80) Fill TopLeft, button Btn1 "Back")
  , (RectConstraint Fill         Fill TopLeft, button Btn2 "Title")
  , (RectConstraint (Exactly 80) Fill TopLeft, button Btn3 "Next")
  ]
@

Here the two outer buttons are fixed at 80px wide; the centre button expands
to fill whatever space remains. The 'Fill' height constraint in each child
means height is determined by the panel, not the child.
-}
module Blink.Layout
  ( -- * Single control layout
    layoutWithConstraints
  , RectConstraint (..)
  , Constraint (..)
    -- * Box layout
  , hBox
  , vBox
  , BoxConfig (..)
  , defaultBoxConfig
    -- * Utilities
  , preferredSize
  ) where

import Data.List (sortBy)
import Data.Ord  (comparing)

import Blink.Geometry (Alignment (..), Rectangle (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getBounds, withBounds)
import Data.Maybe (fromMaybe)

-- | Describes how a child should be sized along a single axis.
data Constraint
  = Exactly Double
    -- ^ A fixed size. The available space is ignored.
  | Fill
    -- ^ Expands to fill all available space.
  | AtLeast Double
    -- ^ Expands to fill available space, but never smaller than the given minimum.
  | AtMost Double
    -- ^ Expands to fill available space, but never larger than the given maximum. Has no minimum — can shrink to zero.
  | Between Double Double
    -- ^ Expands to fill available space clamped between the given minimum and maximum.
  deriving (Eq, Show)

-- | Per-child sizing and alignment within a layout panel slot.
data RectConstraint = RectConstraint
  { rcWidth     :: Constraint
    -- ^ Constraint applied to the child's width.
  , rcHeight    :: Constraint
    -- ^ Constraint applied to the child's height.
  , rcAlignment :: Alignment
    -- ^ How the child is positioned within its slot when it does not fill the
    --   slot on one or both axes.
  }

-- | Configuration shared by 'hBox' and 'vBox'.
data BoxConfig = BoxConfig
  { boxSpacing    :: Double
    -- ^ Gap in pixels between consecutive children on the main axis.
  , boxMargin     :: Double
    -- ^ Uniform inset applied to all four sides of the panel before layout.
  , boxAlignment  :: Alignment
    -- ^ Positions the content block within the content area on the main axis.
    --   Controls where whitespace falls when children are smaller than the
    --   content area, and which side clips when they overflow.
  , boxFillCross  :: Bool
    -- ^ Whether children stretch to fill the full cross-axis extent.
  }

-- | A 'BoxConfig' with no spacing, no margin, 'TopLeft' alignment, and
--   'boxFillCross' set to 'True'. Override only the fields you need:
--
-- @
-- defaultBoxConfig { boxSpacing = 8, boxMargin = 4 }
-- @
defaultBoxConfig :: BoxConfig
defaultBoxConfig = BoxConfig
  { boxSpacing   = 0
  , boxMargin    = 0
  , boxAlignment = TopLeft
  , boxFillCross = True
  }

-- | Sizes and positions a component within its parent bounds according to a
--   'RectConstraint'. Used directly to constrain a single component, and used
--   internally by 'hBox' and 'vBox' to position each child within its
--   allocated slot.
--
-- @
-- layoutWithConstraints (RectConstraint (Exactly 120) (Exactly 32) Center) $
--   button MyBtn "OK"
-- @
layoutWithConstraints :: RectConstraint -> UI e c a -> UI e c a
layoutWithConstraints rc ui = do
  r <- getBounds
  let w = preferredSize (rcWidth rc) (rectWidth r)
      h = preferredSize (rcHeight rc) (rectHeight r)
  withBounds (alignRect (rcAlignment rc) r (Rectangle 0 0 w h)) ui

-- | Abstracts over the two layout orientations so that 'box' can be written
--   once. Each field encodes the axis-specific behaviour; 'horizontal' and
--   'vertical' are the only two values.
data Axis = Axis
  { mainConstraint :: RectConstraint -> Constraint
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
  , fillCross      :: RectConstraint -> RectConstraint
    -- ^ Overrides the child's cross-axis constraint with 'Fill'.
  }

horizontal :: Axis
horizontal = Axis
  { mainConstraint = rcWidth
  , mainLength     = rectWidth
  , crossLength    = rectHeight
  , mainOrigin     = rectX
  , crossOrigin    = rectY
  , makeSlot       = Rectangle
  , fillCross      = \rc -> rc { rcHeight = Fill }
  }

vertical :: Axis
vertical = Axis
  { mainConstraint = rcHeight
  , mainLength     = rectHeight
  , crossLength    = rectWidth
  , mainOrigin     = rectY
  , crossOrigin    = rectX
  , makeSlot       = \mo co ms cs -> Rectangle co mo cs ms
  , fillCross      = \rc -> rc { rcWidth = Fill }
  }

-- | Arranges children left-to-right. Each child is paired with a
--   'RectConstraint' governing its width and, when 'boxFillCross' is 'False',
--   its height and vertical alignment.
hBox :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
hBox = box horizontal

-- | Arranges children top-to-bottom. Each child is paired with a
--   'RectConstraint' governing its height and, when 'boxFillCross' is 'False',
--   its width and horizontal alignment.
vBox :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
vBox = box vertical

box :: Axis -> BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
box ax cfg children = do
  r <- getBounds
  let ca        = insetRect (uniform (boxMargin cfg)) r
      n         = length children
      sp        = boxSpacing cfg
      availMain = mainLength ax ca - sp * fromIntegral (max 0 (n - 1))
      slotMains = preferredSizes availMain (map (mainConstraint ax . fst) children)
      totalMain = sum slotMains + sp * fromIntegral (max 0 (n - 1))
      cb        = alignRect (boxAlignment cfg) ca (makeSlot ax 0 0 totalMain (crossLength ax ca))
      origins   = scanl (\o s -> o + s + sp) (mainOrigin ax cb) slotMains
  withBounds ca $ clipToCurrent $
    mapM_ (\(mo, ms, (rc, ui)) ->
      let slotRect    = makeSlot ax mo (crossOrigin ax cb) ms (crossLength ax cb)
          effectiveRc = if boxFillCross cfg then fillCross ax rc else rc
      in withBounds slotRect $ layoutWithConstraints effectiveRc ui
      ) (zip3 origins slotMains children)

-- | Returns the preferred size for a 'Constraint' given the amount of available space.
--
-- >>> preferredSize (Exactly 80) 200
-- 80.0
-- >>> preferredSize Fill 200
-- 200.0
-- >>> preferredSize (AtLeast 50) 200
-- 200.0
-- >>> preferredSize (AtLeast 50) 20
-- 50.0
-- >>> preferredSize (AtMost 150) 200
-- 150.0
-- >>> preferredSize (AtMost 150) 100
-- 100.0
-- >>> preferredSize (Between 50 150) 200
-- 150.0
-- >>> preferredSize (Between 50 150) 100
-- 100.0
-- >>> preferredSize (Between 50 150) 20
-- 50.0
preferredSize :: Constraint -> Double -> Double
preferredSize (Exactly w)     _         = w
preferredSize Fill            available  = available
preferredSize (AtLeast w)     available  = max w available
preferredSize (AtMost w)      available  = min w available
preferredSize (Between lo hi) available  = max lo (min hi available)

preferredSizes :: Double -> [Constraint] -> [Double]
preferredSizes available constraints =
  let mins    = map minLength constraints
      surplus = max 0 (available - sum mins)
  in zipWith (+) mins (distributeSurplusSpace surplus constraints)

minLength :: Constraint -> Double
minLength (Exactly w)    = w
minLength Fill           = 0
minLength (AtLeast w)    = w
minLength (AtMost _)     = 0
minLength (Between l _)  = l

canExpand :: Constraint -> Bool
canExpand (Exactly _) = False
canExpand _           = True

-- Computes how much extra space (above each constraint's minimum) each slot
-- receives, distributing surplus equally and redistributing any space left
-- over from slots that hit their cap.
distributeSurplusSpace :: Double -> [Constraint] -> [Double]
distributeSurplusSpace surplus constraints =
  let flexible = sortBy (comparing snd) [(i, cap c) | (i, c) <- zip [0..] constraints, canExpand c]
      shares   = go surplus flexible
  in [fromMaybe 0 (lookup i shares) | i <- [0 .. length constraints - 1]]
  where
    go _ [] = []
    go s slots@((i, c) : rest) =
      let share = s / fromIntegral (length slots)
      in if c <= share
         then (i, c) : go (s - c) rest
         else [(j, share) | (j, _) <- slots]
    cap (AtMost w)    = w
    cap (Between l h) = h - l
    cap _             = 1 / 0
