module Blink.Layout
  ( Constraint (..)
  , Cell (..)
  , RectConstraint (..)
  , BoxConfig (..)
  , Bounds
  , Spacing
  , hBox
  , vBox
  , hBox2
  , vBox2
  , hBoxLayout
  , vBoxLayout
  , resolveConstraint
  ) where

import Blink.Geometry (Alignment, Rectangle (..), Size (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getRect, layout)

type Bounds = Rectangle
type Spacing = Double

data Constraint
  = Exactly Double
  | Fill
  | AtLeast Double
  | AtMost Double
  | Between Double Double

data Cell e c = Cell
  { width :: Constraint
  , height :: Constraint
  , alignment :: Alignment
  , content :: UI e c ()
  }

hBox :: Spacing -> [Cell e c] -> UI e c ()
hBox = box width height rectHeight (Size . rectWidth) hBoxLayout

vBox :: Spacing -> [Cell e c] -> UI e c ()
vBox = box height width rectWidth (\slot cross -> Size cross (rectHeight slot)) vBoxLayout

-- mainC    -- constraint accessor for the layout axis
-- crossC   -- constraint accessor for the cross axis
-- crossLen -- cross-axis length from the bounds, used to resolve crossC
-- mkSize   -- builds a Size from the slot rect and resolved cross-axis length
-- layoutFn -- hBoxLayout or vBoxLayout
box :: (Cell e c -> Constraint)
    -> (Cell e c -> Constraint)
    -> (Rectangle -> Double)
    -> (Rectangle -> Double -> Size)
    -> (Bounds -> Spacing -> [Constraint] -> [Bounds])
    -> Spacing -> [Cell e c] -> UI e c ()
box mainC crossC crossLen mkSize layoutFn spacing cells = do
  r <- getRect
  let slotRects = layoutFn r spacing (map mainC cells)
  mapM_ (\(slot, cell) ->
    let cross = resolveConstraint (crossC cell) (crossLen r)
        contentRect = alignRect (alignment cell) slot (mkSize slot cross)
    in layout contentRect (content cell)
    ) (zip slotRects cells)

hBoxLayout :: Bounds -> Spacing -> [Constraint] -> [Bounds]
hBoxLayout r = boxLayout rectWidth rectX (\x w -> Rectangle x (rectY r) w (rectHeight r)) r

vBoxLayout :: Bounds -> Spacing -> [Constraint] -> [Bounds]
vBoxLayout r = boxLayout rectHeight rectY (\y h -> Rectangle (rectX r) y (rectWidth r) h) r

-- Divides r into slots along one axis, distributing space according to
-- constraints with spacing pixels between each slot.
--   axisLen  -- extracts available length along the layout axis
--   axisOrig -- extracts the starting position along the layout axis
--   mkSlot   -- builds a slot rect from (position, size); caller closes over cross-axis values
boxLayout
  :: (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Double -> Double -> Rectangle)
  -> Bounds -> Spacing -> [Constraint] -> [Bounds]
boxLayout axisLen axisOrig mkSlot r spacing constraints =
  let available = axisLen r - spacingTotal
      spacingTotal = spacing * fromIntegral (max 0 (length constraints - 1))
      sizes = resolveConstraints available constraints
      origins = scanl (\o s -> o + s + spacing) (axisOrig r) sizes
  in zipWith mkSlot origins sizes

resolveConstraints :: Double -> [Constraint] -> [Double]
resolveConstraints available constraints =
  let minimums = map minLength constraints
      minTotal = sum minimums
      surplus = max 0 (available - minTotal)
      indexed = zip [0..] constraints
  in allocateSurplus surplus minimums indexed

resolveConstraint :: Constraint -> Double -> Double
resolveConstraint (Exactly w) _ = w
resolveConstraint Fill available = available
resolveConstraint (AtLeast w) available = max w available
resolveConstraint (AtMost w) available = min w available
resolveConstraint (Between lo hi) available = max lo (min hi available)

minLength :: Constraint -> Double
minLength (Exactly w) = w
minLength Fill = 0
minLength (AtLeast w) = w
minLength (AtMost _) = 0
minLength (Between l _) = l

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

data RectConstraint = RectConstraint
  { rcWidth     :: Constraint
  , rcHeight    :: Constraint
  , rcAlignment :: Alignment
  }

data BoxConfig = BoxConfig
  { boxSpacing    :: Double
  , boxMargin     :: Double
  , boxAlignment  :: Alignment
  , boxFillCross  :: Bool
  }

hBox2 :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
hBox2 = box2 rcWidth rcHeight rectWidth rectHeight rectX rectY
             (\m cr -> Size m cr)
             (\mo co ms cs -> Rectangle mo co ms cs)

vBox2 :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
vBox2 = box2 rcHeight rcWidth rectHeight rectWidth rectY rectX
             (\m cr -> Size cr m)
             (\mo co ms cs -> Rectangle co mo cs ms)

box2
  :: (RectConstraint -> Constraint)
  -> (RectConstraint -> Constraint)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Double -> Double -> Size)
  -> (Double -> Double -> Double -> Double -> Rectangle)
  -> BoxConfig
  -> [(RectConstraint, UI e c ())]
  -> UI e c ()
box2 mainC crossC mainLen crossLen mainOrig crossOrig mkSize mkSlot cfg children = do
  r <- getRect
  let ca         = insetRect (uniform (boxMargin cfg)) r
      n          = length children
      sp         = boxSpacing cfg
      availMain  = mainLen ca - sp * fromIntegral (max 0 (n - 1))
      slotMains  = resolveConstraints availMain (map (mainC . fst) children)
      totalMain  = sum slotMains + sp * fromIntegral (max 0 (n - 1))
      cb       = alignRect (boxAlignment cfg) ca (mkSize totalMain (crossLen ca))
      origins  = scanl (\o s -> o + s + sp) (mainOrig cb) slotMains
  layout ca $ clipToCurrent $
    mapM_ (\(mo, ms, (rc, ui)) ->
      let cross     = crossLen cb
          slotRect  = mkSlot mo (crossOrig cb) ms cross
          childRect = if boxFillCross cfg
                      then slotRect
                      else let childCross = resolveConstraint (crossC rc) cross
                           in alignRect (rcAlignment rc) slotRect (mkSize ms childCross)
      in layout childRect ui
      ) (zip3 origins slotMains children)
