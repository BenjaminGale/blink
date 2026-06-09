{- |
Module: Blink.Layout

= How layout works

Every control in Blink is /greedy/ by default: it fills the rectangle it is
given. The layout system is how you control what rectangle that is.

The fundamental primitive is 'layoutWithConstraint'. It takes a
'RectConstraint' — describing the desired width, height, and alignment — and
a UI action, and runs that action in a rectangle computed from the constraint
rather than the full available space:

@
layoutWithConstraint (RectConstraint (Exactly 120) (Exactly 32) Center) $
  button MyBtn "Click me"
@

This renders the button at 120×32 pixels, centred in whatever space is
available, regardless of how large that space is.

= Arranging multiple children

'hBox' and 'vBox' arrange a list of children horizontally or vertically. Each
child is paired with its own 'RectConstraint', so children can have different
sizes:

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

= Sizing behaviour on the cross axis

By default ('boxFillCross' = 'True') all children are stretched to the full
cross-axis extent of the panel — children in an 'hBox' are as tall as the
panel, and children in a 'vBox' are as wide. Set 'boxFillCross' to 'False' to
let each child size and align itself on the cross axis using its own
'RectConstraint' instead.
-}
module Blink.Layout
  ( Constraint (..)
  , RectConstraint (..)
  , BoxConfig (..)
  , hBox
  , vBox
  , defaultBoxConfig
  , layoutWithConstraint
  , preferredSize
  ) where

import Data.List (sortBy)
import Data.Ord  (comparing)

import Blink.Geometry (Alignment (..), Rectangle (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getBounds, withBounds)
import Data.Maybe (fromMaybe)

{- | Describes how a child should be sized along a single axis.

When multiple expandable children share the same axis, the surplus space is
divided equally among them. If a child hits its ceiling during this pass, the
remaining surplus is redistributed across the uncapped children.
-}
data Constraint
  = Exactly Double
    -- ^ A fixed size. The available space is ignored.
  | Fill
    -- ^ Expands to fill all available space.
  | AtLeast Double
    -- ^ Expands to fill available space, but never smaller than the given minimum.
  | AtMost Double
    -- ^ Fills available space up to the given maximum.
  | Between Double Double
    -- ^ Fills available space clamped between the given minimum and maximum.
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

{- | Configuration shared by 'hBox' and 'vBox'.

The panel itself is always greedy — it fills its available rectangle. These
fields control spacing, margin, and how children are arranged within that space.

When 'boxFillCross' is 'True', each child stretches to fill the full cross-axis
extent of the panel (height for 'hBox', width for 'vBox'), overriding the
child's cross constraint and alignment. When 'False', the child's
'RectConstraint' governs the cross axis.

The panel clips its children to its content area.
-}
data BoxConfig = BoxConfig
  { boxSpacing    :: Double
    -- ^ Gap in pixels between consecutive children on the main axis.
  , boxMargin     :: Double
    -- ^ Uniform inset applied to all four sides of the panel before layout.
  , boxAlignment  :: Alignment
    -- ^ Positions the content block within the content area when the total
    --   child size is less than the available space on the main axis.
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

-- | Resolves both axes of a 'RectConstraint' against the current rectangle,
--   sizes the child accordingly, and positions it using 'rcAlignment'.
--
--   Controls are greedy by default; wrap them with this function at the call
--   site to opt in to constraint-based sizing:
--
-- @
-- layoutWithConstraint (RectConstraint (Exactly 120) (Exactly 32) Center) $
--   button MyBtn "OK"
-- @
layoutWithConstraint :: RectConstraint -> UI e c a -> UI e c a
layoutWithConstraint rc ui = do
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
      in withBounds slotRect $ layoutWithConstraint effectiveRc ui
      ) (zip3 origins slotMains children)

-- | Returns the preferred size for a 'Constraint' given the amount of available space.
--
-- >>> preferredSize (Exactly 80) 200
-- 80.0
-- >>> preferredSize Fill 200
-- 200.0
-- >>> preferredSize (AtLeast 50) 200
-- 200.0
-- >>> preferredSize (AtMost 150) 200
-- 150.0
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
