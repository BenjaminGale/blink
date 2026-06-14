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
  , Layout (..)
  , Length (..)
    -- * Box layout
  , hBox
  , vBox
  , BoxConfig (..)
  , defaultBoxConfig
  , boxTotalSpacing
    -- * Border layout
  , borderLayout
  , BorderContent (..)
  , emptyBorderContent
    -- * Utilities
  , preferredSize
  , AddLength (..)
  , MaxLength (..)
  , addLength
  , maxLength
  ) where

import Data.List (foldl', scanl', sortBy)
import Data.Ord  (comparing)

import qualified Data.IntMap.Strict as IntMap
import Blink.Geometry (Alignment (..), Rectangle (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getBounds, withBounds)
import Data.Maybe (catMaybes)

-- | Describes how a child should be sized along a single axis.
data Length
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
data Layout = Layout
  { layoutWidth :: Length
    -- ^ Constraint applied to the child's width.
  , layoutHeight :: Length
    -- ^ Constraint applied to the child's height.
  , layoutAlignment :: Alignment
    -- ^ How the child is positioned within its slot when it does not fill the
    --   slot on one or both axes.
  } deriving (Show)

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

-- | Total space consumed by all gaps between @n@ children — the sum of
--   @(n - 1)@ spacings.
boxTotalSpacing :: BoxConfig -> Int -> Double
boxTotalSpacing cfg n = boxSpacing cfg * fromIntegral (max 0 (n - 1))

-- | Sizes and positions a component within its parent bounds according to a
--   'RectConstraint'. Used directly to constrain a single component, and used
--   internally by 'hBox' and 'vBox' to position each child within its
--   allocated slot.
--
-- @
-- layoutWithConstraints (RectConstraint (Exactly 120) (Exactly 32) Center) $
--   button MyBtn "OK"
-- @
layoutWithConstraints :: Layout -> UI e s a -> UI e s a
layoutWithConstraints rc ui = do
  r <- getBounds
  let w = preferredSize (layoutWidth rc) (rectWidth r)
      h = preferredSize (layoutHeight rc) (rectHeight r)
  withBounds (alignRect (layoutAlignment rc) r (Rectangle 0 0 w h)) ui

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

-- | Arranges children left-to-right. Each child is paired with a
--   'RectConstraint' governing its width and, when 'boxFillCross' is 'False',
--   its height and vertical alignment.
hBox :: BoxConfig -> [(Layout, UI e s ())] -> UI e s ()
hBox = box horizontal

-- | Arranges children top-to-bottom. Each child is paired with a
--   'RectConstraint' governing its height and, when 'boxFillCross' is 'False',
--   its width and horizontal alignment.
vBox :: BoxConfig -> [(Layout, UI e s ())] -> UI e s ()
vBox = box vertical

box :: Axis -> BoxConfig -> [(Layout, UI e s ())] -> UI e s ()
box ax cfg children = do
  r <- getBounds
  let contentArea  = insetRect (uniform (boxMargin cfg)) r
      totalSpacing = boxTotalSpacing cfg (length children)
      availSpace   = mainLength ax contentArea - totalSpacing
      slotSizes    = preferredSizes availSpace (map (mainConstraint ax . fst) children)
      crossOrig    = crossOrigin ax contentArea
      crossLen     = crossLength ax contentArea
      contentBlock = alignRect (boxAlignment cfg) contentArea
                       (makeSlot ax 0 0 (foldl' (+) 0 slotSizes + totalSpacing) crossLen)
      slotOrigins  = scanl' (\o s -> o + s + boxSpacing cfg) (mainOrigin ax contentBlock) slotSizes
  withBounds contentArea $ do
    clipToCurrent $ do
      sequence_ $ zipWith3
        (\slotOrigin slotSize (rc, ui) ->
          let slotRect    = makeSlot ax slotOrigin crossOrig slotSize crossLen
              effectiveRc = if boxFillCross cfg then fillCross ax rc else rc
          in withBounds slotRect $ layoutWithConstraints effectiveRc ui)
        slotOrigins slotSizes children

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
preferredSize :: Length -> Double -> Double
preferredSize (Exactly w)     _         = w
preferredSize Fill            available  = available
preferredSize (AtLeast w)     available  = max w available
preferredSize (AtMost w)      available  = min w available
preferredSize (Between lo hi) available  = max lo (min hi available)

-- | Wraps 'Length' for the additive monoid: @'mempty' = 'Exactly' 0@,
-- @('<>') = 'addLength'@. Use 'mconcat' to sum a list of lengths.
newtype AddLength = AddLength { getAddLength :: Length }

instance Semigroup AddLength where
  AddLength a <> AddLength b = AddLength (add a b)
    where
      add Fill              _                   = Fill
      add _                 Fill                = Fill
      add (Exactly a')      (Exactly b')         = Exactly     (a' + b')
      add (AtLeast a')      (Exactly b')         = AtLeast     (a' + b')
      add (Exactly a')      (AtLeast b')         = AtLeast     (a' + b')
      add (AtMost a')       (Exactly b')         = AtMost      (a' + b')
      add (Exactly a')      (AtMost b')          = AtMost      (a' + b')
      add (Between lo hi)   (Exactly b')         = Between     (lo + b') (hi + b')
      add (Exactly a')      (Between lo hi)      = Between     (a' + lo) (a' + hi)
      add (AtLeast a')      (AtLeast b')         = AtLeast     (a' + b')
      add (AtMost a')       (AtMost b')          = AtMost      (a' + b')
      add (Between lo1 hi1) (Between lo2 hi2)    = Between     (lo1 + lo2) (hi1 + hi2)
      add (AtLeast a')      (AtMost _)           = AtLeast     a'
      add (AtMost _)        (AtLeast b')         = AtLeast     b'
      add (AtLeast a')      (Between lo _)       = AtLeast     (a' + lo)
      add (Between lo _)    (AtLeast b')         = AtLeast     (lo + b')
      add (AtMost a')       (Between lo hi)      = Between     lo (a' + hi)
      add (Between lo hi)   (AtMost b')          = Between     lo (hi + b')

instance Monoid AddLength where
  mempty = AddLength (Exactly 0)

-- | Wraps 'Length' for the max monoid: @'mempty' = 'Exactly' 0@,
-- @('<>') = 'maxLength' of two@. Use 'mconcat' to find the largest in a list.
newtype MaxLength = MaxLength { getMaxLength :: Length }

instance Semigroup MaxLength where
  MaxLength a <> MaxLength b = MaxLength (maxL a b)
    where
      maxL Fill              _                   = Fill
      maxL _                 Fill                = Fill
      maxL (Exactly a')      (Exactly b')         = Exactly     (max a' b')
      maxL (AtLeast a')      (AtLeast b')         = AtLeast     (max a' b')
      maxL (AtMost a')       (AtMost b')          = AtMost      (max a' b')
      maxL (Between lo1 hi1) (Between lo2 hi2)    = Between     (max lo1 lo2) (max hi1 hi2)
      maxL (Exactly a')      (AtLeast b')         = AtLeast     (max a' b')
      maxL (AtLeast a')      (Exactly b')         = AtLeast     (max a' b')
      maxL (Exactly a')      (AtMost b')          = AtMost      (max a' b')
      maxL (AtMost a')       (Exactly b')         = AtMost      (max a' b')
      maxL (Exactly a')      (Between lo hi)      = Between     (max a' lo) (max a' hi)
      maxL (Between lo hi)   (Exactly b')         = Between     (max lo b') (max hi b')
      maxL (AtLeast a')      (AtMost _)           = AtLeast     a'
      maxL (AtMost _)        (AtLeast b')         = AtLeast     b'
      maxL (AtLeast a')      (Between lo _)       = AtLeast     (max a' lo)
      maxL (Between lo _)    (AtLeast b')         = AtLeast     (max lo b')
      maxL (AtMost a')       (Between _ hi)       = AtMost      (max a' hi)
      maxL (Between _ hi)    (AtMost b')          = AtMost      (max hi b')

instance Monoid MaxLength where
  mempty = MaxLength (Exactly 0)

-- | Add two 'Length' constraints. Convenience wrapper around 'AddLength'.
addLength :: Length -> Length -> Length
addLength a b = getAddLength (AddLength a <> AddLength b)

-- | Return the maximum 'Length' across a list. Convenience wrapper around 'MaxLength'.
-- Returns @'Exactly' 0@ for an empty list.
maxLength :: [Length] -> Length
maxLength = getMaxLength . mconcat . map MaxLength

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

-- | Specifies which panels to show in a 'borderLayout'.
data BorderContent e s = BorderContent
  { topPanel    :: Maybe (Double, UI e s ())
  , bottomPanel :: Maybe (Double, UI e s ())
  , leftPanel   :: Maybe (Double, UI e s ())
  , rightPanel  :: Maybe (Double, UI e s ())
  , centrePanel :: Maybe (UI e s ())
  }

-- | All panels absent; use record update to populate only the ones you need.
emptyBorderContent :: BorderContent e s
emptyBorderContent = BorderContent
  { topPanel    = Nothing
  , bottomPanel = Nothing
  , leftPanel   = Nothing
  , rightPanel  = Nothing
  , centrePanel = Nothing
  }

-- | Divides the available space into up to five named regions.
--
-- Implemented as a 'vBox' of three rows where the middle row is an 'hBox'
-- containing the left, centre, and right panels. No spacing or margin is
-- applied; panels are clipped to their allocated region.
borderLayout :: BorderContent e s -> UI e s ()
borderLayout bc =
  vBox defaultBoxConfig (catMaybes [topRow, middleRow, bottomRow])
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
      | otherwise        = Just (Layout Fill Fill TopLeft, hBox defaultBoxConfig middleCells)
