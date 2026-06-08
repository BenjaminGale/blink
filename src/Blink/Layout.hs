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
  , SumConstraint (..)
  , MaxConstraint (..)
  , RectConstraint (..)
  , BoxConfig (..)
  , hBox
  , vBox
  , defaultBoxConfig
  , layoutWithConstraint
  , preferredSize
  ) where

import Blink.Geometry (Alignment (..), Rectangle (..), Size (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getRect, layout)

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

-- | Combines two constraints by summing their space requirements.
-- Useful for computing the total space needed along the main axis of a box.
-- The identity is @Exactly 0@.
newtype SumConstraint = SumConstraint { getSumConstraint :: Constraint }
  deriving (Eq, Show)

-- | Combines two constraints by taking the maximum of their space requirements.
-- Useful for computing the space needed along the cross axis of a box.
-- The identity is @Exactly 0@.
newtype MaxConstraint = MaxConstraint { getMaxConstraint :: Constraint }
  deriving (Eq, Show)

instance Semigroup SumConstraint where
  SumConstraint (Exactly 0)      <> b                              = b
  a                              <> SumConstraint (Exactly 0)      = a
  SumConstraint (Exactly n)      <> SumConstraint (Exactly m)      = SumConstraint $ Exactly (n + m)
  SumConstraint (Exactly n)      <> SumConstraint Fill             = SumConstraint $ AtLeast n
  SumConstraint (Exactly n)      <> SumConstraint (AtLeast m)      = SumConstraint $ AtLeast (n + m)
  SumConstraint (Exactly n)      <> SumConstraint (AtMost m)       = SumConstraint $ Between n (n + m)
  SumConstraint (Exactly n)      <> SumConstraint (Between lo hi)  = SumConstraint $ Between (n + lo) (n + hi)
  SumConstraint Fill             <> SumConstraint Fill             = SumConstraint Fill
  SumConstraint Fill             <> SumConstraint (AtLeast m)      = SumConstraint $ AtLeast m
  SumConstraint Fill             <> SumConstraint (AtMost _)       = SumConstraint Fill
  SumConstraint Fill             <> SumConstraint (Between lo _)   = SumConstraint $ AtLeast lo
  SumConstraint (AtLeast n)      <> SumConstraint (AtLeast m)      = SumConstraint $ AtLeast (n + m)
  SumConstraint (AtLeast n)      <> SumConstraint (AtMost _)       = SumConstraint $ AtLeast n
  SumConstraint (AtLeast n)      <> SumConstraint (Between lo _)   = SumConstraint $ AtLeast (n + lo)
  SumConstraint (AtMost n)       <> SumConstraint (AtMost m)       = SumConstraint $ AtMost (n + m)
  SumConstraint (AtMost n)       <> SumConstraint (Between lo hi)  = SumConstraint $ Between lo (n + hi)
  SumConstraint (Between lo hi)  <> SumConstraint (Between lo2 hi2) = SumConstraint $ Between (lo + lo2) (hi + hi2)
  a                              <> b                              = b <> a

instance Monoid SumConstraint where
  mempty = SumConstraint (Exactly 0)

instance Semigroup MaxConstraint where
  MaxConstraint (Exactly 0)      <> b                              = b
  a                              <> MaxConstraint (Exactly 0)      = a
  MaxConstraint (Exactly n)      <> MaxConstraint (Exactly m)      = MaxConstraint $ Exactly (max n m)
  MaxConstraint (Exactly n)      <> MaxConstraint Fill             = MaxConstraint $ AtLeast n
  MaxConstraint (Exactly n)      <> MaxConstraint (AtLeast m)      = MaxConstraint $ AtLeast (max n m)
  MaxConstraint (Exactly n)      <> MaxConstraint (AtMost m)       = MaxConstraint $ if n <= m then Between n m else Exactly n
  MaxConstraint (Exactly n)      <> MaxConstraint (Between lo hi)  = MaxConstraint $ Between (max n lo) (max n hi)
  MaxConstraint Fill             <> MaxConstraint Fill             = MaxConstraint Fill
  MaxConstraint Fill             <> MaxConstraint (AtLeast m)      = MaxConstraint $ AtLeast m
  MaxConstraint Fill             <> MaxConstraint (AtMost _)       = MaxConstraint Fill
  MaxConstraint Fill             <> MaxConstraint (Between lo _)   = MaxConstraint $ AtLeast lo
  MaxConstraint (AtLeast n)      <> MaxConstraint (AtLeast m)      = MaxConstraint $ AtLeast (max n m)
  MaxConstraint (AtLeast n)      <> MaxConstraint (AtMost _)       = MaxConstraint $ AtLeast n
  MaxConstraint (AtLeast n)      <> MaxConstraint (Between lo _)   = MaxConstraint $ AtLeast (max n lo)
  MaxConstraint (AtMost n)       <> MaxConstraint (AtMost m)       = MaxConstraint $ AtMost (max n m)
  MaxConstraint (AtMost n)       <> MaxConstraint (Between lo hi)  = MaxConstraint $ Between lo (max n hi)
  MaxConstraint (Between lo hi)  <> MaxConstraint (Between lo2 hi2) = MaxConstraint $ Between (max lo lo2) (max hi hi2)
  a                              <> b                              = b <> a

instance Monoid MaxConstraint where
  mempty = MaxConstraint (Exactly 0)

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
  r <- getRect
  let w = preferredSize (rcWidth rc) (rectWidth r)
      h = preferredSize (rcHeight rc) (rectHeight r)
  layout (alignRect (rcAlignment rc) r (Size w h)) ui

-- | Arranges children left-to-right. Each child is paired with a
--   'RectConstraint' governing its width and, when 'boxFillCross' is 'False',
--   its height and vertical alignment.
hBox :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
hBox = box rcWidth rectWidth rectHeight rectX rectY
           (\m cr -> Size m cr)
           (\mo co ms cs -> Rectangle mo co ms cs)
           (\c rc -> rc { rcHeight = c })

-- | Arranges children top-to-bottom. Each child is paired with a
--   'RectConstraint' governing its height and, when 'boxFillCross' is 'False',
--   its width and horizontal alignment.
vBox :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
vBox = box rcHeight rectHeight rectWidth rectY rectX
           (\m cr -> Size cr m)
           (\mo co ms cs -> Rectangle co mo cs ms)
           (\c rc -> rc { rcWidth = c })

box
  :: (RectConstraint -> Constraint)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Double -> Double -> Size)
  -> (Double -> Double -> Double -> Double -> Rectangle)
  -> (Constraint -> RectConstraint -> RectConstraint)
  -> BoxConfig
  -> [(RectConstraint, UI e c ())]
  -> UI e c ()
box mainC mainLen crossLen mainOrig crossOrig mkSize mkSlot setCrossC cfg children = do
  r <- getRect
  let ca        = insetRect (uniform (boxMargin cfg)) r
      n         = length children
      sp        = boxSpacing cfg
      availMain = mainLen ca - sp * fromIntegral (max 0 (n - 1))
      slotMains = preferredSizes availMain (map (mainC . fst) children)
      totalMain = sum slotMains + sp * fromIntegral (max 0 (n - 1))
      cb        = alignRect (boxAlignment cfg) ca (mkSize totalMain (crossLen ca))
      origins   = scanl (\o s -> o + s + sp) (mainOrig cb) slotMains
  layout ca $ clipToCurrent $
    mapM_ (\(mo, ms, (rc, ui)) ->
      let slotRect    = mkSlot mo (crossOrig cb) ms (crossLen cb)
          effectiveRc = if boxFillCross cfg then setCrossC Fill rc else rc
      in layout slotRect $ layoutWithConstraint effectiveRc ui
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
  let minimums = map minLength constraints
      minTotal = sum minimums
      surplus  = max 0 (available - minTotal)
      indexed  = zip [0..] constraints
  in allocateSurplus surplus minimums indexed

minLength :: Constraint -> Double
minLength (Exactly w)    = w
minLength Fill           = 0
minLength (AtLeast w)    = w
minLength (AtMost _)     = 0
minLength (Between l _)  = l

data MaxLength = Unlimited | MaxLength Double

maxLength :: Constraint -> MaxLength
maxLength (AtMost w)    = MaxLength w
maxLength (Between _ h) = MaxLength h
maxLength _             = Unlimited

canExpand :: Constraint -> Bool
canExpand (Exactly _) = False
canExpand _           = True

data AllocPass = AllocPass
  { allocSizes  :: [Double]
  , allocLeft   :: Double
  , allocCapped :: Bool
  }

allocateSurplus :: Double -> [Double] -> [(Int, Constraint)] -> [Double]
allocateSurplus surplus sizes indexed =
  let flexible = filter (canExpand . snd) indexed
      n        = length flexible
  in if surplus <= 0 || n == 0
     then sizes
     else
       let share  = surplus / fromIntegral n
           result = foldl (shareStep share) (AllocPass sizes surplus False) flexible
       in if allocCapped result
          then allocateSurplus (allocLeft result) (allocSizes result) indexed
          else allocSizes result

shareStep :: Double -> AllocPass -> (Int, Constraint) -> AllocPass
shareStep share pass (i, c) =
  let cur      = allocSizes pass !! i
      proposed = cur + share
      setSizes s = pass { allocSizes = take i (allocSizes pass) ++ [s] ++ drop (i + 1) (allocSizes pass) }
  in case maxLength c of
       MaxLength cap | proposed > cap ->
         (setSizes cap) { allocLeft = allocLeft pass - (cap - cur), allocCapped = True }
       _ ->
         (setSizes proposed) { allocLeft = allocLeft pass - share }
