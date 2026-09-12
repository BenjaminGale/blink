{- |
Module: Blink.Rendering

The draw command list produced by the view each frame. After running the
view tree, 'Blink.View.getDrawCommands' extracts an ordered list of
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
  , ImagePath
  , DrawCommand (..)
    -- * Text measurement
  , TextMeasurer (..)
  , noOpTextMeasurer
    -- * Image measurement
  , ImageMeasurer (..)
  , noOpImageMeasurer
    -- * Measurers
  , Measurers (..)
  , noOpMeasurers
  ) where

import Data.Text (Text)
import Blink.Geometry (Rectangle, Size (..), BorderEdges)

-- | Text measurement operations provided to the View for cursor positioning.
-- Construct one from your platform's font API and pass it to
-- 'Blink.App.configureContinuous' or 'Blink.App.configureEventDriven'.
data TextMeasurer = TextMeasurer
  { tmCharOffset   :: Text -> Int -> IO Float
    -- ^ X offset (pixels) of character index @n@ from the start of the string.
  , tmCharAtOffset :: Text -> Float -> IO Int
    -- ^ Character index closest to the given x offset.
  , tmTextSize     :: Text -> IO Size
    -- ^ Pixel dimensions of the rendered string.
  }

-- | A 'TextMeasurer' whose operations always return @0@. Use in tests or
-- when no font backend is available.
noOpTextMeasurer :: TextMeasurer
noOpTextMeasurer = TextMeasurer
  { tmCharOffset   = \_ _ -> pure 0
  , tmCharAtOffset = \_ _ -> pure 0
  , tmTextSize     = \_ -> pure (Size 0 0)
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

-- | A path identifying an image asset (e.g. an SVG or PNG file), used as
-- both the backend's load key and its texture cache key.
type ImagePath = Text

-- | Image measurement provided to the View for sizing elements to an
-- image's natural pixel dimensions. Construct one from your platform's
-- image-loading API and pass it to 'Blink.App.configureContinuous' or
-- 'Blink.App.configureEventDriven'.
newtype ImageMeasurer = ImageMeasurer
  { imNaturalSize :: ImagePath -> IO Size
    -- ^ Pixel dimensions of the image at the given path.
  }

-- | An 'ImageMeasurer' whose operation always returns a zero size. Use in
-- tests or when no image backend is available.
noOpImageMeasurer :: ImageMeasurer
noOpImageMeasurer = ImageMeasurer
  { imNaturalSize = \_ -> pure (Size 0 0)
  }

-- | Every measurement service the backend supplies at configure time,
-- bundled so 'Blink.App.configureContinuous'\/'Blink.App.configureEventDriven'
-- and 'Blink.View.emptyViewContext' take one value rather than a growing
-- list of positional measurer arguments as new measurement kinds are added.
data Measurers = Measurers
  { msrText  :: TextMeasurer
  , msrImage :: ImageMeasurer
  }

-- | 'noOpTextMeasurer' and 'noOpImageMeasurer' bundled together -- the
-- usual starting point outside a real backend (tests, headless
-- rendering); override individual fields via record update.
noOpMeasurers :: Measurers
noOpMeasurers = Measurers
  { msrText  = noOpTextMeasurer
  , msrImage = noOpImageMeasurer
  }

-- | A single draw instruction in the frame's command list, produced by
-- the 'Blink.View' drawing primitives and consumed by the backend renderer.
data DrawCommand
  = FillRect Rectangle Colour
    -- ^ Fill the rectangle with a solid colour.
  | StrokeBorder Rectangle Colour BorderEdges
    -- ^ Stroke the border of the rectangle with the given colour and per-side widths in pixels.
  | DrawText Rectangle Text Colour TextAlign
    -- ^ Render text within the rectangle using the given colour and alignment.
  | DrawImage Rectangle ImagePath Colour
    -- ^ Render the image at the given path, stretched to fill the
    -- rectangle, tinted by the given colour -- a fully-opaque white
    -- ('RGBA 1 1 1 1') draws the image's own colours unaltered; any
    -- other colour multiplies over it, so tinting only has a visible
    -- effect on an image whose own pixels are white or greyscale (as
    -- Blink's own bundled icons are).
  | PushClip Rectangle
    -- ^ Push a clip region onto the clip stack; subsequent draw commands
    -- are clipped to this rectangle intersected with any outer clip regions.
  | PopClip
    -- ^ Pop the most recently pushed clip region from the clip stack.
  deriving (Eq, Show)
