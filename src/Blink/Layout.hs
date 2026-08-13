{- |
Module: Blink.Layout

= How layout works

By default, a 'UI' action fills the full space it is given by its parent.

>  +------------------------------------------+
>  |                                          |
>  |                component                 |
>  |        (fills parent by default)         |
>  |                                          |
>  +------------------------------------------+

This module provides combinators that allow more control over layout. They
are described below in order of increasing scope, from sizing and positioning
a single action, to arranging several actions together, to dividing a whole
screen into named regions. Reach for the smallest one that does what you need.
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
    -- * Advanced layout
    -- | Everything above covers a fixed arrangement decided in advance.
    --   Sometimes that isn't enough, in one of two ways:
    --
    --   * The arrangement itself needs to depend on something only known at
    --     render time — the current context.
    --   * A slot needs a constraint more specific than 'Fill' or a single
    --     hardcoded 'Exactly' — a finer-grained choice of 'Length'.
    --
    --   Both are special cases: they draw on 'getBounds' \/ 'withBounds', the
    --   application state passed into the enclosing view, a box combinator
    --   ('hBox' \/ 'vBox'), 'layoutWithConstraints', and the 'Length'
    --   arithmetic above all at once, rather than being handled by any one
    --   of them.
    --
    --   == Depending on the current context
    --
    --   \"Context\" here means either of two things available while
    --   building a view: the current layout bounds, queried with
    --   'getBounds' from inside the 'UI' monad, or the application state,
    --   which the enclosing view function already has in scope as an
    --   ordinary argument (see "Blink.App"). Branch on either, or both,
    --   with ordinary @if@\/@case@ — there is no dedicated combinator for
    --   this because a plain 'UI' action and a plain function argument
    --   already do the job:
    --
    --   @
    --   contextual :: AppState -> UI Element msg ()
    --   contextual state = do
    --     bounds <- getBounds
    --     if appCompactMode state || rectWidth bounds < 600
    --       then vBox defaultBoxConfig [ (Layout Fill (Exactly 200) TopLeft, sidebar)
    --                                   , (Layout Fill Fill        TopLeft, content)
    --                                   ]
    --       else hBox defaultBoxConfig [ (Layout (Exactly 200) Fill TopLeft, sidebar)
    --                                   , (Layout Fill         Fill TopLeft, content)
    --                                   ]
    --   @
    --
    --   Both branches use ordinary 'hBox'\/'vBox' — only the choice of
    --   which one to call, and how the two children are ordered, depends on
    --   the context. Here that context is a window narrower than 600px /or/
    --   an explicit compact-mode flag in the application state; the two
    --   compose freely, so either can drive the decision on its own or
    --   together.
    --
    --   == Choosing a finer-grained constraint
    --
    --   A slot's 'Length' does not have to be a value you wrote down ahead
    --   of time as 'Fill' or 'Exactly' — it can be computed, and it does
    --   not have to be either of those two constructors. 'AtLeast',
    --   'AtMost', and 'Between' give a slot room to flex within bounds
    --   instead of being either fully rigid or fully flexible; picking the
    --   right one is itself a form of custom layout:
    --
    --   @
    --   -- A sidebar that shrinks with the window but never drops below a
    --   -- readable width, alongside content that takes whatever is left.
    --   hBox defaultBoxConfig
    --     [ (Layout (AtLeast 180) Fill TopLeft, sidebar)
    --     , (Layout Fill          Fill TopLeft, content)
    --     ]
    --   @
    --
    --   An exact constraint can be computed the same way, rather than
    --   hardcoded. A control's chrome (margin, border, padding — see the
    --   Chrome section of "Blink.Controls") is not known until render time
    --   either, so sizing a slot tightly around a control's content plus
    --   its own chrome means computing an 'Exactly' rather than writing
    --   one down:
    --
    --   @
    --   -- Sizes a button tightly around its label plus its own chrome,
    --   -- instead of stretching to fill the parent.
    --   tightButton :: Ord e => e -> Text -> [Attr e ButtonEvent msg ButtonConfig] -> UI e msg ()
    --   tightButton eid txt attrs = do
    --     (chromeW, chromeH) <- measureChrome eid
    --     Size textW textH    <- measureText txt
    --     let w = addLength chromeW (Exactly (realToFrac textW))
    --         h = addLength chromeH (Exactly (realToFrac textH))
    --     layoutWithConstraints (Layout w h TopLeft) (button eid (text txt : attrs))
    --   @
    --
    --   The same pattern extends to a row of controls that must each be
    --   exactly as wide as their own content demands: compute a 'Length'
    --   per child this way, then pass the list straight to 'hBox' instead
    --   of a fixed 'Layout' for every slot.
  ) where

import Data.List (foldl', scanl', sortBy)
import Data.Ord  (comparing)

import qualified Data.IntMap.Strict as IntMap
import Blink.Geometry (Alignment (..), Rectangle (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getBounds, withBounds)
import Data.Maybe (catMaybes)

-- | Describes how a child should be sized along a single axis. See
--   'preferredSize' for worked examples of how each constructor resolves
--   against different amounts of available space.
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
    --
    --   When the children take up less space than the panel, this is where
    --   the leftover whitespace goes:
    --
    --   >  +----------------------+-----------------+
    --   >  |       children       |                 |
    --   >  +----------------------+-----------------+
    --   >  TopLeft: children lead, whitespace trails
    --
    --   >  +--------+----------------------+--------+
    --   >  |        |       children       |        |
    --   >  +--------+----------------------+--------+
    --   >  Center: whitespace is split evenly
    --
    --   >  +-----------------+----------------------+
    --   >  |                 |       children       |
    --   >  +-----------------+----------------------+
    --   >  BottomRight: whitespace leads, children trail
    --
    --   When the children take up more space than the panel, this is which
    --   side gets clipped:
    --
    --   >  +------------------------------------------+
    --   >  | children (too wide) -->                  |
    --   >  +------------------------------------------+
    --   >  TopLeft: the left edge is anchored, right side clips
    --
    --   >  +------------------------------------------+
    --   >  |                  <-- children (too wide) |
    --   >  +------------------------------------------+
    --   >  BottomRight: the right edge is anchored, left side clips
  , boxFillCross  :: Bool
    -- ^ Whether children stretch to fill the full cross-axis extent.
  }

-- | A 'BoxConfig' with no spacing, no margin, 'TopLeft' alignment, and
--   'boxFillCross' set to 'True'. Override only the fields you need:
--
-- @
-- hBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 4 })
--   [ (Layout (Exactly 80) Fill TopLeft, sidebar)
--   , (Layout Fill         Fill TopLeft, content)
--   ]
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
--
-- >>> boxTotalSpacing (defaultBoxConfig { boxSpacing = 8 }) 3
-- 16.0
-- >>> boxTotalSpacing (defaultBoxConfig { boxSpacing = 8 }) 1
-- 0.0
boxTotalSpacing :: BoxConfig -> Int -> Double
boxTotalSpacing cfg n = boxSpacing cfg * fromIntegral (max 0 (n - 1))

-- | Sizes and positions a component within its parent bounds according to a
--   'Layout'. Since components are greedy by default, this is the escape
--   hatch for sizing and aligning one that shouldn't fill its parent.
--   'layoutWidth' and 'layoutHeight' control how much of the parent space the
--   component takes up on each axis. 'layoutAlignment' controls where it
--   sits within that space. If the resulting size is larger than the parent
--   bounds, this function does not clip it: the component draws in full,
--   spilling past the parent's edges and potentially over any siblings,
--   unless something further up the tree (such as 'hBox' or 'vBox') clips it.
--
-- @
-- layoutWithConstraints (Layout (Exactly 120) (Exactly 32) Center) $
--   button MyBtn [text "Click me"]
-- @
--
-- This renders the button at 120×32 pixels, centred in whatever space the
-- parent provides, regardless of how large that space is.
--
-- >  +----------------------------------------+
-- >  |                                        |
-- >  |            +------------+              |
-- >  |            |   MyBtn    |  120 x 32    |
-- >  |            +------------+              |
-- >  |                                        |
-- >  +----------------------------------------+
-- >                   parent bounds
--
-- A few more variations help build intuition.
--
-- @
-- layoutWithConstraints (Layout Fill (Exactly 3) TopLeft) toolbar
-- @
--
-- 'Fill' on one axis and a fixed size on the other pins a full-width bar to
-- the top, regardless of the parent's height.
--
-- >  +------------------------------------------+
-- >  |            Toolbar (Fill x 3)            |
-- >  +------------------------------------------+
-- >  |                                          |
-- >  |                                          |
-- >  |                                          |
-- >  +------------------------------------------+
--
-- @
-- layoutWithConstraints (Layout (Exactly 14) (Exactly 3) BottomRight) badge
-- @
--
-- A fixed size with 'BottomRight' alignment pins a component to a corner.
--
-- >  +------------------------------------------+
-- >  |                                          |
-- >  |                                          |
-- >  |                                          |
-- >  |                            +-------------+
-- >  |                            |    Badge    |
-- >  +----------------------------+-------------+
layoutWithConstraints :: Layout -> UI e msg a -> UI e msg a
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

-- | Arranges children left-to-right. Each child is paired with a 'Layout'
--   governing its width and, when 'boxFillCross' is 'False', its height and
--   vertical alignment. If a margin is set, children are laid out within
--   that inset.
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
--     'boxAlignment'. When children are smaller than the content area this
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
-- hBox (defaultBoxConfig { boxSpacing = 4 })
--   [ (Layout (Exactly 80) Fill TopLeft, button Btn1 [text "Back"])
--   , (Layout Fill         Fill TopLeft, button Btn2 [text "Title"])
--   , (Layout (Exactly 80) Fill TopLeft, button Btn3 [text "Next"])
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
-- 'boxFillCross' (default 'True') controls the cross axis, which is height
-- in this example. When 'True', each child is stretched to the panel's full
-- height, as in the diagram above. When 'False', each child keeps its own
-- height (here, 'TopLeft'-aligned), leaving the rest of the panel blank.
--
-- >  +------------+--------------+------------+
-- >  |     A      |      B       |     C      |
-- >  +------------+--------------+------------+
-- >  |                                        |
-- >  +----------------------------------------+
hBox :: BoxConfig -> [(Layout, UI e msg ())] -> UI e msg ()
hBox = box horizontal

-- | Arranges children top-to-bottom. Each child is paired with a 'Layout'
--   governing its height and, when 'boxFillCross' is 'False', its width and
--   horizontal alignment. Uses the same algorithm as 'hBox' with the axes
--   swapped. See its documentation for the full behaviour.
--
-- @
-- vBox (defaultBoxConfig { boxSpacing = 1 })
--   [ (Layout Fill (Exactly 3) TopLeft, header)
--   , (Layout Fill Fill        TopLeft, body)
--   , (Layout Fill (Exactly 3) TopLeft, footer)
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
vBox :: BoxConfig -> [(Layout, UI e msg ())] -> UI e msg ()
vBox = box vertical

box :: Axis -> BoxConfig -> [(Layout, UI e msg ())] -> UI e msg ()
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

-- | Returns the preferred size for a 'Length' given the amount of available space.
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
--
-- >>> addLength (Exactly 10) (Exactly 20)
-- Exactly 30.0
-- >>> addLength (Exactly 10) Fill
-- Fill
-- >>> addLength (AtLeast 10) (Exactly 5)
-- AtLeast 15.0
-- >>> addLength (AtLeast 10) (AtMost 20)
-- AtLeast 10.0
-- >>> addLength (Between 10 20) (Exactly 5)
-- Between 15.0 25.0
addLength :: Length -> Length -> Length
addLength a b = getAddLength (AddLength a <> AddLength b)

-- | Return the maximum 'Length' across a list. Convenience wrapper around 'MaxLength'.
-- Returns @'Exactly' 0@ for an empty list.
--
-- >>> maxLength [Exactly 10, Exactly 30, Exactly 20]
-- Exactly 30.0
-- >>> maxLength [Fill, Exactly 100]
-- Fill
-- >>> maxLength [AtLeast 10, AtMost 20]
-- AtLeast 10.0
-- >>> maxLength []
-- Exactly 0.0
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
-- No spacing or margin is applied. Clipping follows 'vBox' and 'hBox': the
-- top, middle, and bottom rows are clipped as a group to the whole region,
-- and within the middle row the left, centre, and right panels are further
-- clipped as a group to that row. An oversized panel can still overlap its
-- neighbours within the same row.
borderLayout :: BorderContent e msg -> UI e msg ()
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
