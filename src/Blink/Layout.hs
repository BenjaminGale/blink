module Blink.Layout
  ( Constraint (..)
  , RectConstraint (..)
  , BoxConfig (..)
  , hBox
  , vBox
  , hBoxLayout
  , vBoxLayout
  , layoutWithConstraint
  , resolveConstraint
  ) where

import Blink.Geometry (Alignment, Rectangle (..), Size (..), alignRect, insetRect, uniform)
import Blink.UI (UI, clipToCurrent, getRect, layout)

data Constraint
  = Exactly Double
  | Fill
  | AtLeast Double
  | AtMost Double
  | Between Double Double

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

layoutWithConstraint :: RectConstraint -> UI e c a -> UI e c a
layoutWithConstraint rc ui = do
  r <- getRect
  let w = resolveConstraint (rcWidth rc) (rectWidth r)
      h = resolveConstraint (rcHeight rc) (rectHeight r)
  layout (alignRect (rcAlignment rc) r (Size w h)) ui

hBox :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
hBox = box rcWidth rectWidth rectHeight rectX rectY
           (\m cr -> Size m cr)
           (\mo co ms cs -> Rectangle mo co ms cs)

vBox :: BoxConfig -> [(RectConstraint, UI e c ())] -> UI e c ()
vBox = box rcHeight rectHeight rectWidth rectY rectX
           (\m cr -> Size cr m)
           (\mo co ms cs -> Rectangle co mo cs ms)

box
  :: (RectConstraint -> Constraint)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Rectangle -> Double)
  -> (Double -> Double -> Size)
  -> (Double -> Double -> Double -> Double -> Rectangle)
  -> BoxConfig
  -> [(RectConstraint, UI e c ())]
  -> UI e c ()
box mainC mainLen crossLen mainOrig crossOrig mkSize mkSlot cfg children = do
  r <- getRect
  let ca        = insetRect (uniform (boxMargin cfg)) r
      n         = length children
      sp        = boxSpacing cfg
      availMain = mainLen ca - sp * fromIntegral (max 0 (n - 1))
      slotMains = resolveConstraints availMain (map (mainC . fst) children)
      totalMain = sum slotMains + sp * fromIntegral (max 0 (n - 1))
      cb        = alignRect (boxAlignment cfg) ca (mkSize totalMain (crossLen ca))
      origins   = scanl (\o s -> o + s + sp) (mainOrig cb) slotMains
  layout ca $ clipToCurrent $
    mapM_ (\(mo, ms, (rc, ui)) ->
      let slotRect = mkSlot mo (crossOrig cb) ms (crossLen cb)
      in if boxFillCross cfg
         then layout slotRect ui
         else layout slotRect $ layoutWithConstraint rc ui
      ) (zip3 origins slotMains children)

hBoxLayout :: Rectangle -> Double -> [Constraint] -> [Rectangle]
hBoxLayout r spacing constraints =
  let available = rectWidth r - spacing * fromIntegral (max 0 (length constraints - 1))
      sizes     = resolveConstraints available constraints
      origins   = scanl (\o s -> o + s + spacing) (rectX r) sizes
  in zipWith (\x w -> Rectangle x (rectY r) w (rectHeight r)) origins sizes

vBoxLayout :: Rectangle -> Double -> [Constraint] -> [Rectangle]
vBoxLayout r spacing constraints =
  let available = rectHeight r - spacing * fromIntegral (max 0 (length constraints - 1))
      sizes     = resolveConstraints available constraints
      origins   = scanl (\o s -> o + s + spacing) (rectY r) sizes
  in zipWith (\y h -> Rectangle (rectX r) y (rectWidth r) h) origins sizes

resolveConstraints :: Double -> [Constraint] -> [Double]
resolveConstraints available constraints =
  let minimums = map minLength constraints
      minTotal = sum minimums
      surplus  = max 0 (available - minTotal)
      indexed  = zip [0..] constraints
  in allocateSurplus surplus minimums indexed

resolveConstraint :: Constraint -> Double -> Double
resolveConstraint (Exactly w) _        = w
resolveConstraint Fill        available = available
resolveConstraint (AtLeast w) available = max w available
resolveConstraint (AtMost w)  available = min w available
resolveConstraint (Between lo hi) available = max lo (min hi available)

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
