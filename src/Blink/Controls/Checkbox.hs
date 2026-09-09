{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
-- Built on 'toggleBase' -- see "Blink.Controls.ToggleButton" for how it and every
-- other toggle-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Control.ControlConfig'\/'Blink.Controls.Control.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.ToggleButton.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.Checkbox
  ( checkbox
  , checkboxStyleKey
  ) where

import Control.Monad (void, when)
import Data.Text (Text)

import Blink.Controls.Button (ButtonConfig (..))
import Blink.Controls.Control
import Blink.Controls.Label (lcText)
import Blink.Controls.ToggleButton
  (ToggleConfig (..), defaultGlyphToggleConfig, glyphCaptionContent, glyphCaptionElement, toggleBase)
import Blink.Controls.Checkbox.Style (checkboxStyleKey)
import Blink.Geometry (Rectangle (..), uniformBorder)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.View (currentStyle, getBounds, withBounds)
import Blink.View.Drawing (drawText, strokeRect)
import Blink.Element (Element (..))

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

-- | The gap between the glyph and the caption beside it.
labelGap :: Double
labelGap = 6

-- | The margin left between the glyph column's edges and the drawn box, so
-- the stroked box doesn't touch the caption or the control's own bounds.
boxInset :: Double
boxInset = 2

-- | 'CHECK MARK' (U+2713) is near-universally supported, unlike the
-- 'BALLOT BOX WITH CHECK' glyph (U+2611) that would otherwise draw the
-- whole box-plus-tick in one character.
checkTick :: Text
checkTick = "\10003"

-- | A checkbox: a small box drawn with 'strokeRect', a tick inside it while
-- selected (see 'Blink.Controls.ToggleButton.isSelected'), beside a caption set via 'Blink.Controls.Label.text', toggled
-- together as one control -- clicking either the box or the caption
-- activates it, the same as 'Blink.Controls.ToggleButton.toggleButton'. Flips
-- every time it's activated; see 'Blink.Controls.ToggleButton.onSelectedChanged' for reacting to it.
-- Defaults to sizing itself to its own glyph-plus-caption content on both
-- axes, the same as 'Blink.Controls.Label.label'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
checkbox :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> Element e msg
checkbox eid attrs = Element
  { elLayout  = bcLayout btn
  , elMeasure = measureChrome (ccStyleKey ctrl) (glyphCaptionElement glyphWidth labelGap (lcText (bcLabelled btn)))
  , elRun     = void (toggleBase eid cfg')
  }
  where
    cfg      = resolve (defaultGlyphToggleConfig checkboxStyleKey) attrs
    btn      = tgcButton cfg
    selected = tgcSelected cfg
    -- | The box, drawn with 'strokeRect' and (while selected) a tick inside
    -- it, centred within the glyph column's own bounds.
    drawBox = do
      s      <- currentStyle
      bounds <- getBounds
      let boxSize = max 0 (min glyphWidth (rectHeight bounds) - boxInset)
          boxRect = Rectangle
            { rectX      = rectX bounds + (glyphWidth - boxSize) / 2
            , rectY      = rectY bounds + (rectHeight bounds - boxSize) / 2
            , rectWidth  = boxSize
            , rectHeight = boxSize
            }
      withBounds boxRect $ do
        strokeRect (styleTextColour s) (uniformBorder 1)
        when selected $ drawText (styleTextColour s) AlignCenter checkTick
    glyphContent = glyphCaptionContent glyphWidth labelGap drawBox (bcLabelled btn)
    ctrl = (bcControl btn) { ccContent = const glyphContent }
    cfg' = cfg
      { tgcNext   = not
      , tgcButton = btn { bcControl = ctrl }
      }
