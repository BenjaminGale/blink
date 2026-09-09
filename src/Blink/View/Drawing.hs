{- |
Module: Blink.View.Drawing

Drawing operations built on top of "Blink.View"'s minimal primitives
('Blink.View.draw', 'Blink.View.getBounds', 'Blink.View.getInteractionClip',
'Blink.View.withInteractionClip'). Controls draw with 'fillRect',
'strokeRect', and 'drawText' against the /current bounds/ (see
'Blink.View.getBounds'); 'withClip' narrows both drawing and mouse
hit-testing to a sub-tree's bounds.
-}
module Blink.View.Drawing
  ( fillRect
  , strokeRect
  , drawText
  , withClip
  , withBackground
  , withBorder
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Blink.Geometry (Rectangle, BorderEdges, intersectRect)
import Blink.View (View, draw, getBounds, getInteractionClip, withInteractionClip)
import Blink.Rendering (Colour, TextAlign, DrawCommand (..), isVisible)

-- | Builds a 'DrawCommand' from the current bounds and queues it.
drawAt :: (Rectangle -> DrawCommand) -> View e msg ()
drawAt mkCmd = do
  r <- getBounds
  draw (mkCmd r)

-- | Fills the current bounds with a solid colour.
fillRect :: Colour -> View e msg ()
fillRect colour = drawAt (\r -> FillRect r colour)

-- | Strokes the border of the current bounds with the given colour and per-side widths.
strokeRect :: Colour -> BorderEdges -> View e msg ()
strokeRect colour edges = drawAt (\r -> StrokeBorder r colour edges)

-- | Renders text within the current bounds using the given colour and alignment.
drawText :: Colour -> TextAlign -> Text -> View e msg ()
drawText colour align text = drawAt (\r -> DrawText r text colour align)

-- | Wraps a sub-tree in a clip region matching the current bounds. Draw
-- commands produced by the sub-tree that fall outside the region are discarded,
-- and mouse hit-testing is also restricted to the same region.
withClip :: View e msg a -> View e msg a
withClip action = do
  r    <- getBounds
  clip <- getInteractionClip
  let newClip = maybe r (intersectRect r) clip
  draw (PushClip r)
  a <- withInteractionClip (Just newClip) action
  draw PopClip
  pure a

-- | Fills the current bounds with @colour@ then runs @content@ on top.
-- Skips the fill when @colour@ is fully transparent.
withBackground :: Colour -> View e msg a -> View e msg a
withBackground colour content = do
  when (isVisible colour) $ fillRect colour
  content

-- | Runs @content@, then strokes a border around the current bounds on top.
-- Drawing the border after content ensures it is always visible over children.
-- Skips the stroke when @colour@ is fully transparent, mirroring
-- 'withBackground' — a caller that reserves border space in every state via
-- @styleBorderColour@ but only wants it to actually render in some of them
-- (e.g. a resting-state border that becomes visible on focus) relies on this.
withBorder :: Colour -> BorderEdges -> View e msg a -> View e msg a
withBorder colour edges content = do
  result <- content
  when (isVisible colour) $ strokeRect colour edges
  pure result
