module Blink.Layout
  ( Constraint (..)
  , Cell (..)
  , Bounds
  , Spacing
  , hBox
  , vBox
  , hBoxLayout
  , vBoxLayout
  , resolveConstraint
  ) where

import Blink.Geometry (Alignment, Rectangle (..), Size (..), alignRect)
import Blink.UI (UI, getRect, layout)

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
  let floors_ = map floorOf constraints
      totalFloor = sum floors_
      surplus = max 0 (available - totalFloor)
      expanders = zip [0..] constraints
  in distribute surplus floors_ expanders

resolveConstraint :: Constraint -> Double -> Double
resolveConstraint (Exactly w) _ = w
resolveConstraint Fill available = available
resolveConstraint (AtLeast w) available = max w available
resolveConstraint (AtMost w) available = min w available
resolveConstraint (Between lo hi) available = max lo (min hi available)

floorOf :: Constraint -> Double
floorOf (Exactly w) = w
floorOf Fill = 0
floorOf (AtLeast w) = w
floorOf (AtMost _) = 0
floorOf (Between l _) = l

ceiling_ :: Constraint -> Maybe Double
ceiling_ (AtMost w) = Just w
ceiling_ (Between _ h) = Just h
ceiling_ _ = Nothing

participates :: Constraint -> Bool
participates (Exactly _) = False
participates _           = True

distribute :: Double -> [Double] -> [(Int, Constraint)] -> [Double]
distribute surplus sizes expanders =
  let active = filter (participates . snd) expanders
      n = length active
  in if surplus <= 0 || n == 0
     then sizes
     else
       let share = surplus / fromIntegral n
           (sizes', remaining, capped) = foldl (applyShare share) (sizes, surplus, False) active
       in if capped
          then distribute remaining sizes' expanders
          else sizes'

applyShare :: Double -> ([Double], Double, Bool) -> (Int, Constraint) -> ([Double], Double, Bool)
applyShare share (sizes, remaining, anyCapped) (i, c) =
  let current = sizes !! i
      proposed = current + share
  in case ceiling_ c of
       Just cap | proposed > cap ->
         let sizes' = take i sizes ++ [cap] ++ drop (i + 1) sizes
         in (sizes', remaining - (cap - current), True)
       _ ->
         let sizes' = take i sizes ++ [proposed] ++ drop (i + 1) sizes
         in (sizes', remaining - share, anyCapped)
