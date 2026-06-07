module Blink.Layout
  ( Constraint (..)
  , Cell (..)
  , hBox
  , vBox
  , hBoxLayout
  , vBoxLayout
  , resolveConstraint
  ) where

import Blink.Geometry (Alignment, Rectangle (..), Size (..), alignRect)
import Blink.UI (UI, getRect, layout)

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

hBox :: Double -> [Cell e c] -> UI e c ()
hBox spacing cells = do
  r <- getRect
  let slotRects = hBoxLayout r spacing (map width cells)
  mapM_ (\(slot, cell) ->
    let h = resolveConstraint (height cell) (rectHeight r)
        contentRect = alignRect (alignment cell) slot (Size (rectWidth slot) h)
    in layout contentRect (content cell)
    ) (zip slotRects cells)

vBox :: Double -> [Cell e c] -> UI e c ()
vBox spacing cells = do
  r <- getRect
  let slotRects = vBoxLayout r spacing (map height cells)
  mapM_ (\(slot, cell) ->
    let w = resolveConstraint (width cell) (rectWidth r)
        contentRect = alignRect (alignment cell) slot (Size w (rectHeight slot))
    in layout contentRect (content cell)
    ) (zip slotRects cells)

hBoxLayout :: Rectangle -> Double -> [Constraint] -> [Rectangle]
hBoxLayout r spacing constraints =
  let sizes = resolveConstraints (rectWidth r - spacingTotal) constraints
      spacingTotal = spacing * fromIntegral (max 0 (length constraints - 1))
      xs = scanl (\x w -> x + w + spacing) (rectX r) sizes
  in zipWith (\x w -> Rectangle x (rectY r) w (rectHeight r)) xs sizes

vBoxLayout :: Rectangle -> Double -> [Constraint] -> [Rectangle]
vBoxLayout r spacing constraints =
  let sizes = resolveConstraints (rectHeight r - spacingTotal) constraints
      spacingTotal = spacing * fromIntegral (max 0 (length constraints - 1))
      ys = scanl (\y h -> y + h + spacing) (rectY r) sizes
  in zipWith (\y h -> Rectangle (rectX r) y (rectWidth r) h) ys sizes

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
