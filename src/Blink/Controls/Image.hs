{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A single image, drawn with no interactive behaviour of its own. A
-- leaf, built directly on 'control' -- nothing derives from it, and (like
-- "Blink.Controls.Divider") it's never a tab stop and takes no id.
--
-- 'fitWidth'\/'fitHeight'\/'preserveRatio' follow JavaFX's own
-- @ImageView@: with neither fit dimension set, 'image' sizes itself to
-- the image's natural pixel size (see 'Blink.View.measureImage'); with
-- one or both set and 'preserveRatio' at its default of 'True', the
-- image is scaled to fit within whichever fit dimensions are given,
-- preserving its aspect ratio -- the missing dimension (or, with both
-- given, the tighter-fitting one) is derived rather than stretched to
-- fill; with 'preserveRatio' 'False', each set dimension is used exactly
-- as given, independent of the other.
module Blink.Controls.Image
  ( ImageConfig (..)
  , defaultImageConfig
  , imageStyleKey
  , image
  , source
  , fitWidth
  , fitHeight
  , preserveRatio
  , resolveImageSize
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)

import Blink.Controls.Control
import Blink.Controls.Image.Style (imageStyleKey)
import Blink.Geometry (Alignment (TopLeft), Size (..))
import Blink.Layout.Constraints (Layout (..), fitContent)
import Blink.Rendering (ImagePath)
import Blink.View (measureImage)
import Blink.View.Drawing (drawImage)
import Blink.Element (Element (..), HasLayoutConfig (..))

-- | Every capability 'image' resolves: the wrapped 'ControlConfig', the
-- image to draw, its fit dimensions, and whether they preserve aspect
-- ratio.
data ImageConfig e msg = ImageConfig
  { icControl        :: ControlConfig e msg
  , icSource         :: ImagePath
  , icFitWidth       :: Maybe Double
  , icFitHeight      :: Maybe Double
  , icPreserveRatio  :: Bool
  , icLayout         :: Layout
  }

-- | 'defaultControlConfig' (styled via 'imageStyleKey'), no source, no
-- fit dimensions (so 'image' sizes to the source's natural size until
-- 'fitWidth'\/'fitHeight' says otherwise), and 'preserveRatio' 'True'.
defaultImageConfig :: ImageConfig e msg
defaultImageConfig = ImageConfig
  { icControl       = defaultControlConfig { ccStyleKey = imageStyleKey }
  , icSource        = ""
  , icFitWidth      = Nothing
  , icFitHeight     = Nothing
  , icPreserveRatio = True
  , icLayout        = Layout fitContent fitContent TopLeft
  }

instance HasControlConfig e msg (ImageConfig e msg) where
  overControl attr = Attribute (\c -> c { icControl = runAttribute attr (icControl c) })

instance HasLayoutConfig (ImageConfig e msg) where
  overLayout attr = Attribute (\c -> c { icLayout = runAttribute (overLayout attr) (icLayout c) })

-- | Sets the image to draw. Defaults to @\"\"@ (drawing nothing
-- meaningful) when not given.
source :: ImagePath -> Attribute (ImageConfig e msg)
source p = Attribute (\c -> c { icSource = p })

-- | Constrains the displayed width -- see the module header for how it
-- combines with 'fitHeight'\/'preserveRatio'. Unset by default.
fitWidth :: Double -> Attribute (ImageConfig e msg)
fitWidth w = Attribute (\c -> c { icFitWidth = Just w })

-- | Constrains the displayed height -- see the module header for how it
-- combines with 'fitWidth'\/'preserveRatio'. Unset by default.
fitHeight :: Double -> Attribute (ImageConfig e msg)
fitHeight h = Attribute (\c -> c { icFitHeight = Just h })

-- | Whether 'fitWidth'\/'fitHeight' scale the image proportionally
-- (preserving its aspect ratio) or independently. Defaults to 'True'.
preserveRatio :: Bool -> Attribute (ImageConfig e msg)
preserveRatio b = Attribute (\c -> c { icPreserveRatio = b })

-- | The displayed size for an image whose natural pixel size is
-- @natural@, given 'fitWidth'\/'fitHeight'\/'preserveRatio'. Pure so it's
-- directly testable against hand-computed expectations, independent of
-- 'Blink.View.measureImage'.
--
-- Falls back to treating @natural@ as if 'preserveRatio' were 'False'
-- when either of its dimensions is @0@ (a source that hasn't loaded, or
-- genuinely has no size) -- preserving a ratio against a zero dimension
-- is undefined, and would otherwise divide by zero.
resolveImageSize :: Bool -> Maybe Double -> Maybe Double -> Size -> Size
resolveImageSize _ Nothing Nothing natural = natural
resolveImageSize preserve mFitW mFitH (Size w h)
  | not preserve || w == 0 || h == 0 =
      Size (fromMaybe w mFitW) (fromMaybe h mFitH)
  | otherwise =
      -- An unset fit dimension contributes +Infinity, so it never wins the
      -- `min` against the dimension that is set -- total, and equivalent to
      -- scaling from whichever single dimension was given.
      let scale = min (maybe (1 / 0) (/ w) mFitW) (maybe (1 / 0) (/ h) mFitH)
      in Size (w * scale) (h * scale)

-- | A single image (see the module header). Never focusable, the same
-- as 'Blink.Controls.Divider.divider'. Defaults to sizing itself to its
-- own (possibly fit-constrained) content on both axes; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
image :: Ord e => [Attribute (ImageConfig e msg)] -> Element e msg
image attrs = Element
  { elLayout  = icLayout cfg
  , elMeasure = measureChrome (ccStyleKey ctrl) (Element (icLayout cfg) intrinsicSize (pure ()))
  , elRun     = void (control ctrl)
  }
  where
    cfg  = resolve defaultImageConfig attrs
    ctrl = (icControl cfg)
      { ccFocusPolicy = NotFocusable
      , ccContent     = const (drawImage (icSource cfg))
      }
    intrinsicSize = const $ do
      natural <- measureImage (icSource cfg)
      pure (resolveImageSize (icPreserveRatio cfg) (icFitWidth cfg) (icFitHeight cfg) natural)
