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
    -- * Text
  , TextAlign (..)
  , Font (..)
  , FontWeight (..)
  , defaultFont
    -- * Draw commands
  , ImagePath
  , DrawCommand (..)
    -- * Cursor shape
  , CursorShape (..)
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
import Blink.Geometry (Rectangle, Size (..), Colour (..), isVisible, Border, CornerRadii)

-- | How heavy a font's strokes are.
data FontWeight = Regular | Bold
  deriving (Eq, Ord, Show)

-- | The font a piece of text is drawn and measured in.
data Font = Font
  { fontFamily :: Maybe Text
    -- ^ The family name, as the backend knows it. 'Nothing' means the
    -- backend's default family.
  , fontSize   :: Double
    -- ^ The size in points.
  , fontWeight :: FontWeight
  } deriving (Eq, Ord, Show)

-- | The backend's default family at 14 points, regular weight -- the font
-- every built-in control uses unless a theme says otherwise.
defaultFont :: Font
defaultFont = Font { fontFamily = Nothing, fontSize = 14, fontWeight = Regular }

-- | Text measurement operations provided to the View for cursor positioning.
-- Construct one from your platform's font API and pass it to
-- 'Blink.App.configureContinuous' or 'Blink.App.configureEventDriven'.
-- Every operation measures in the given 'Font', which must match how the
-- backend draws 'DrawText' in that font.
data TextMeasurer = TextMeasurer
  { tmCharOffset   :: Font -> Text -> Int -> IO Float
    -- ^ X offset (pixels) of character index @n@ from the start of the string.
  , tmCharAtOffset :: Font -> Text -> Float -> IO Int
    -- ^ Character index closest to the given x offset.
  , tmTextSize     :: Font -> Text -> IO Size
    -- ^ Pixel dimensions of the rendered string.
  }

-- | A 'TextMeasurer' whose operations always return @0@. Use in tests or
-- when no font backend is available.
noOpTextMeasurer :: TextMeasurer
noOpTextMeasurer = TextMeasurer
  { tmCharOffset   = \_ _ _ -> pure 0
  , tmCharAtOffset = \_ _ _ -> pure 0
  , tmTextSize     = \_ _ -> pure (Size 0 0)
  }

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
    -- ^ Fill the rectangle with a solid, square-cornered colour.
  | FillRoundedRect Rectangle CornerRadii Colour
    -- ^ Fill the rectangle with a solid colour, clipped to the given
    -- corner radii -- always the same radii as whatever border the
    -- control drawing this fill is about to stroke over it (see
    -- 'Blink.View.Drawing.withBackground'), so a themed background can
    -- never square off behind a rounded border the way 'FillRect' would.
    -- Kept as its own command, rather than folding a radii field into
    -- 'FillRect' itself, so the overwhelming majority of fills (every
    -- unrounded control there is) keep emitting the exact same command
    -- they always have.
  | StrokeBorder Rectangle Border
    -- ^ Stroke the rectangle's border with the given stack of layers,
    -- drawn back-to-front.
  | DrawText Rectangle Text Font Colour TextAlign
    -- ^ Render text within the rectangle in the given font, colour and
    -- alignment.
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

-- | The mouse pointer shape the backend should display, as requested by the
-- view for the current frame. 'Blink.View.getCursorShape' extracts the
-- frame's final value for the backend to apply; see 'Blink.View.requestCursor'
-- for how a control requests one.
data CursorShape
  = CursorArrow
    -- ^ The platform's default pointer. What every frame starts at.
  | CursorResizeHorizontal
    -- ^ A left-right resize indicator, e.g. while hovering or dragging a
    -- column divider.
  deriving (Eq, Show)
