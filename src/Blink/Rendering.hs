{- |
The draw command list produced by the UI each frame. After running the
UI tree, 'Blink.UI.getDrawCommands' extracts an ordered list of
'DrawCommand' values that the backend interprets to render the frame.

Commands are emitted in tree order (parent before child). Clip regions
form a stack: each 'PushClip' must be paired with a matching 'PopClip',
and clipping is the intersection of all currently active clip regions.
-}
module Blink.Rendering
  ( -- * Colour
    Colour (..)
  , isVisible
    -- * Text alignment
  , TextAlign (..)
    -- * Draw commands
  , DrawCommand (..)
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle)

-- | An RGBA colour with components in @[0, 1]@.
data Colour = RGBA Double Double Double Double
  deriving (Eq, Show)

-- | 'True' when the colour has a non-zero alpha component and will
-- contribute visible output when rendered. Used to skip draw calls for
-- fully transparent fills.
isVisible :: Colour -> Bool
isVisible (RGBA _ _ _ a) = a /= 0

-- | Horizontal alignment of text within its bounding rectangle.
data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Show)

-- | A single draw instruction in the frame's command list, produced by
-- the 'Blink.UI' drawing primitives and consumed by the backend renderer.
data DrawCommand
  = FillRect Rectangle Colour
    -- ^ Fill the rectangle with a solid colour.
  | StrokeRect Rectangle Colour Double
    -- ^ Stroke the rectangle border with the given colour and line width in pixels.
  | DrawText Rectangle Text Colour TextAlign
    -- ^ Render text within the rectangle using the given colour and alignment.
  | PushClip Rectangle
    -- ^ Push a clip region onto the clip stack; subsequent draw commands
    -- are clipped to this rectangle intersected with any outer clip regions.
  | PopClip
    -- ^ Pop the most recently pushed clip region from the clip stack.
  deriving (Eq, Show)
