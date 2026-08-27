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
  , children
  , boxTotalSpacing
  ) where

import Control.Monad (forM_)
import Data.List     (foldl', scanl', sortBy)
import Data.Ord      (comparing)

import qualified Data.IntMap.Strict as IntMap

import Blink.Attribute (Attribute (..), resolve)
import Blink.Geometry (Alignment (..), Orientation (..), Rectangle (..), Size (..), alignRect, insetRect, uniform)
import Blink.Layout.Constraints
  (Available (..), HasLayoutConfig (..), Layout (..), Length (..), MeasureCtx (..), layoutWithConstraints, shrink)
import Blink.UI (UI, clipToCurrent, getBounds, withBounds)
import Blink.UI.Element (Element (..), resolveLength)

-- | Every capability 'hBox'\/'vBox' resolve: the box's own size request,
-- spacing, margin, alignment of the content block, and the children
-- themselves (see 'children').
data BoxConfig e msg = BoxConfig
  { bxLayout     :: Layout
  , bxSpacing    :: Double
  , bxMargin     :: Double
  , bxAlignment  :: Alignment
  , bxChildren   :: [Element e msg]
  }

instance HasLayoutConfig (BoxConfig e msg) where
  overLayout attr = Attribute (\c -> c { bxLayout = runAttribute attr (bxLayout c) })

-- | Fills its parent on both axes, no spacing, no margin, 'TopLeft'
-- alignment, and no children. Override only the attributes you need:
--
-- @
-- hBox
--   [ spacing 8, margin 4
--   , children [ sidebarElement, contentElement ]
--   ]
-- @
defaultBoxConfig :: BoxConfig e msg
defaultBoxConfig = BoxConfig
  { bxLayout    = Layout Fill Fill TopLeft
  , bxSpacing   = 0
  , bxMargin    = 0
  , bxAlignment = TopLeft
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

-- | The children to arrange. Each carries its own size request (see
-- 'Blink.UI.Element.Element'), including how it behaves on the cross axis:
-- a 'Fill' cross request expands to the box's full cross extent, anything
-- else keeps its own size and is positioned within the row\/column by its
-- own alignment. Defaults to @[]@; a later 'children' attribute replaces an
-- earlier one rather than adding to it, the same as every other attribute
-- here.
children :: [Element e msg] -> Attribute (BoxConfig e msg)
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
  { axisOrientation :: Orientation
    -- ^ Which of the element's two axes is this box's main axis.
  , mainConstraint  :: Layout -> Length
    -- ^ Extracts the child's constraint along the main axis.
  , crossConstraint :: Layout -> Length
    -- ^ Extracts the child's constraint along the cross axis.
  , mainLength      :: Rectangle -> Double
    -- ^ Length of a rectangle along the main axis.
  , crossLength     :: Rectangle -> Double
    -- ^ Length of a rectangle along the cross axis.
  , mainOrigin      :: Rectangle -> Double
    -- ^ Origin of a rectangle along the main axis.
  , crossOrigin     :: Rectangle -> Double
    -- ^ Origin of a rectangle along the cross axis.
  , makeSlot        :: Double -> Double -> Double -> Double -> Rectangle
    -- ^ Builds a slot rectangle from @(mainOrigin, crossOrigin, mainLen, crossLen)@.
  , makeSize        :: Double -> Double -> Size
    -- ^ Builds a 'Size' from @(mainLen, crossLen)@.
  , setMain         :: Length -> Layout -> Layout
    -- ^ Sets the main-axis constraint on a 'Layout'.
  , setCross        :: Length -> Layout -> Layout
    -- ^ Sets the cross-axis constraint on a 'Layout'.
  }

horizontal :: Axis
horizontal = Axis
  { axisOrientation = Horizontal
  , mainConstraint  = layoutWidth
  , crossConstraint = layoutHeight
  , mainLength      = rectWidth
  , crossLength     = rectHeight
  , mainOrigin      = rectX
  , crossOrigin     = rectY
  , makeSlot        = Rectangle
  , makeSize        = Size
  , setMain         = \l rc -> rc { layoutWidth = l }
  , setCross        = \l rc -> rc { layoutHeight = l }
  }

vertical :: Axis
vertical = Axis
  { axisOrientation = Vertical
  , mainConstraint  = layoutHeight
  , crossConstraint = layoutWidth
  , mainLength      = rectHeight
  , crossLength     = rectWidth
  , mainOrigin      = rectY
  , crossOrigin     = rectX
  , makeSlot        = \mo co ms cs -> Rectangle co mo cs ms
  , makeSize        = \m c -> Size c m
  , setMain         = \l rc -> rc { layoutHeight = l }
  , setCross        = \l rc -> rc { layoutWidth = l }
  }

crossOrientation :: Axis -> Orientation
crossOrientation ax = case axisOrientation ax of
  Horizontal -> Vertical
  Vertical   -> Horizontal

-- | Arranges children left-to-right. Each child (see 'children') carries its
--   own 'Blink.Layout.Layout' governing its width and, on the cross axis
--   (height), whether it stretches to fill the row or keeps its own size.
--   If a margin is set, children are laid out within that inset.
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
--     within its slot according to its own 'Blink.Layout.Layout' -- a 'Fill'
--     cross-axis request expands to the row's full height, anything else
--     keeps the child's own size, aligned per its own request.
--   * Children are clipped to the panel's content area as a group, not
--     individually. An oversized child can still overlap its neighbours; it
--     is only cut off once it reaches the edge of the panel itself.
--
-- @
-- hBox
--   [ spacing 4
--   , children
--       [ button Btn1 [text "Back",  width (Exactly 80), height Fill]
--       , button Btn2 [text "Title", width Fill,          height Fill]
--       , button Btn3 [text "Next",  width (Exactly 80), height Fill]
--       ]
--   ]
-- @
--
-- Here the two outer buttons are fixed at 80px wide. The centre button
-- expands to fill whatever space remains. The 'Fill' height constraint in
-- each child means it stretches to the panel's full height.
--
-- >  +--------+------------------------------------+--------+
-- >  |  Back  |               Title                |  Next  |
-- >  |  80px  |                Fill                |  80px  |
-- >  +--------+------------------------------------+--------+
hBox :: [Attribute (BoxConfig e msg)] -> Element e msg
hBox attrs = box horizontal (resolve defaultBoxConfig attrs)

-- | Arranges children top-to-bottom. Each child (see 'children') carries its
--   own 'Blink.Layout.Layout' governing its height and, on the cross axis
--   (width), whether it stretches to fill the column or keeps its own size.
--   Uses the same algorithm as 'hBox' with the axes swapped. See its
--   documentation for the full behaviour.
--
-- @
-- vBox
--   [ spacing 1
--   , children
--       [ elementWithLayout (Layout Fill (Exactly 3) TopLeft) header
--       , elementWithLayout (Layout Fill Fill        TopLeft) body
--       , elementWithLayout (Layout Fill (Exactly 3) TopLeft) footer
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
vBox :: [Attribute (BoxConfig e msg)] -> Element e msg
vBox attrs = box vertical (resolve defaultBoxConfig attrs)

-- | Builds the box 'Element': its own size request is to fill whatever
-- space it is given, its measure sums (main axis) or maxes (cross axis) its
-- resolved children, and its run arranges them exactly as its measure
-- assumed.
box :: Axis -> BoxConfig e msg -> Element e msg
box ax cfg = Element
  { elLayout  = bxLayout cfg
  , elMeasure = boxMeasure ax cfg
  , elRun     = runBox ax cfg
  }

-- | Resolves each child's main-axis constraint into a concrete slot size,
-- given the box's /content/ extent -- margin already excluded by the
-- caller (see 'boxMeasure' and 'runBox'), leaving only the gaps between
-- children to account for here. When the content extent is @Bounded@,
-- finite space is distributed exactly as a fixed-size box always has been.
-- When the box is itself sizing to content ('Unbounded'), there is no space
-- to distribute, and each resolved length is taken at face value via
-- 'naturalLength'.
boxSlots :: Axis -> BoxConfig e msg -> MeasureCtx -> UI e msg [Double]
boxSlots ax cfg ctx = do
  let kids = bxChildren cfg
      gaps = boxTotalSpacing cfg (length kids)
      main = shrink gaps (measureMain ctx)
  lengths <- mapM (resolveLength (axisOrientation ax) (mainConstraint ax) main (measureCross ctx)) kids
  pure $ case main of
    Bounded avail -> preferredSizes avail lengths
    Unbounded     -> map naturalLength lengths

-- | The box's own preferred size: the sum of its resolved children (plus
-- gaps and margin) on the main axis, and the largest resolved child (plus
-- margin) on the cross axis. Excludes the margin from what it offers its
-- children (they only ever see the content extent) and adds it back once,
-- here, to each axis's total.
boxMeasure :: Axis -> BoxConfig e msg -> MeasureCtx -> UI e msg Size
boxMeasure ax cfg ctx = do
  let inset      = 2 * bxMargin cfg
      contentCtx = ctx
        { measureMain  = shrink inset (measureMain ctx)
        , measureCross = shrink inset (measureCross ctx)
        }
  slots <- boxSlots ax cfg contentCtx
  let mainTotal = foldl' (+) 0 slots + boxTotalSpacing cfg (length slots) + inset
  crossTotal <- (+ inset) <$> measureCrossExtent ax cfg contentCtx
  pure (makeSize ax mainTotal crossTotal)

-- | The largest of the children's resolved cross-axis sizes, given the
-- box's content extent -- margin already excluded by the caller, same as
-- 'boxSlots'.
measureCrossExtent :: Axis -> BoxConfig e msg -> MeasureCtx -> UI e msg Double
measureCrossExtent ax cfg ctx = do
  lengths <- mapM (resolveLength (crossOrientation ax) (crossConstraint ax) (measureCross ctx) (measureMain ctx))
                   (bxChildren cfg)
  pure $ case map naturalLength lengths of
    [] -> 0
    xs -> maximum xs

runBox :: Axis -> BoxConfig e msg -> UI e msg ()
runBox ax cfg = do
  r <- getBounds
  let kids        = bxChildren cfg
      contentArea = insetRect (uniform (bxMargin cfg)) r
      crossOrig   = crossOrigin ax contentArea
      crossLen    = crossLength ax contentArea
      -- Already margin-inset, so this is the box's content extent -- the
      -- same thing 'boxMeasure' builds via 'shrink' -- keeping the two
      -- callers of 'boxSlots' on the same contract.
      contentCtx  = MeasureCtx
        { measureAxis  = axisOrientation ax
        , measureMain  = Bounded (mainLength ax contentArea)
        , measureCross = Bounded crossLen
        }
  slotSizes <- boxSlots ax cfg contentCtx
  let totalSpacing = boxTotalSpacing cfg (length kids)
      contentBlock = alignRect (bxAlignment cfg) contentArea
                       (makeSlot ax 0 0 (foldl' (+) 0 slotSizes + totalSpacing) crossLen)
      slotOrigins  = scanl' (\o s -> o + s + bxSpacing cfg) (mainOrigin ax contentBlock) slotSizes
  withBounds contentArea $ clipToCurrent $
    forM_ (zip3 slotOrigins slotSizes kids) $ \(slotOrigin, slotSize, kid) -> do
      resolvedCross <- resolveLength (crossOrientation ax) (crossConstraint ax)
                          (Bounded crossLen) (Bounded slotSize) kid
      let slotRect = makeSlot ax slotOrigin crossOrig slotSize crossLen
          rc       = setCross ax resolvedCross (setMain ax (Exactly slotSize) (elLayout kid))
      withBounds slotRect $ layoutWithConstraints rc (elRun kid)

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
minLength FitContent     = 0
  -- Unreachable in practice: see Blink.Layout.Constraints.preferredSize.

-- | The size a resolved 'Length' takes when there is no space to
-- distribute -- used for a box that is itself sizing to content. Every
-- constructor that depends on measurement ('FitContent', 'AtLeast',
-- 'Between') has already been turned into an 'Exactly' by 'resolveLength'
-- before reaching here.
naturalLength :: Length -> Double
naturalLength (Exactly w)   = w
naturalLength (AtLeast w)   = w
naturalLength (AtMost w)    = w
naturalLength (Between l _) = l
naturalLength Fill          = 0
  -- Degenerate: see the spec's note on Fill inside a content-sized box.
naturalLength FitContent    = 0
  -- Unreachable: resolveLength never leaves a FitContent unresolved.

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
