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
    -- * Text measurement
  , TextMeasurer (..)
  , noOpTextMeasurer
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle)

-- | Text measurement operations provided to the UI for cursor positioning.
-- Construct one from your platform's font API and pass it to
-- 'Blink.App.configureContinuous' or 'Blink.App.configureEventDriven'.
data TextMeasurer = TextMeasurer
  { tmCharOffset   :: Text -> Int -> IO Float
    -- ^ X offset (pixels) of character index @n@ from the start of the string.
  , tmCharAtOffset :: Text -> Float -> IO Int
    -- ^ Character index closest to the given x offset.
  }

-- | A 'TextMeasurer' whose operations always return @0@. Use in tests or
-- when no font backend is available.
noOpTextMeasurer :: TextMeasurer
noOpTextMeasurer = TextMeasurer
  { tmCharOffset   = \_ _ -> pure 0
  , tmCharAtOffset = \_ _ -> pure 0
  }

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
