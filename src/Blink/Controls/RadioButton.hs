{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'toggleBase' -- see "Blink.Controls.Toggle" for how it and every
-- other toggle-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Control.ControlConfig'\/'Blink.Controls.Control.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.Toggle.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.RadioButton
  ( radioButton
  , radioButtonStyleKey
  ) where

import Control.Monad (void)
import Data.Text (Text)

import Blink.Controls.Button (ButtonConfig (..))
import Blink.Controls.Control
import Blink.Controls.Label (lcText)
import Blink.Controls.Toggle
  (ToggleConfig (..), defaultGlyphToggleConfig, glyphCaptionContent, glyphCaptionElement, toggleBase)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (currentStyle, drawText)
import Blink.UI.Element (Element (..))

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

-- | The gap between the glyph and the caption beside it -- a radio button's
-- glyph, unlike 'Blink.Controls.Checkbox.checkbox''s, sits flush against
-- its caption.
glyphGap :: Double
glyphGap = 0

-- | 'CIRCLED BULLET' (U+25C9) would read better but isn't in most UI
-- fonts' coverage (including this project's demo font); 'BLACK CIRCLE' is
-- near-universally supported and pairs the same way
-- 'Blink.Controls.Checkbox.checkTick' pairs its box.
radioGlyph :: Bool -> Text
radioGlyph True  = "\9679" -- BLACK CIRCLE
radioGlyph False = "\9675" -- WHITE CIRCLE

-- | The 'StyleKey' 'radioButton' resolves its style from unless overridden
-- via 'style'.
radioButtonStyleKey :: StyleKey e
radioButtonStyleKey = Class "radioButton"

-- | A radio button: a glyph showing whether it's currently selected (see
-- 'Blink.Controls.Toggle.isSelected'), beside a caption set via
-- 'Blink.Controls.Label.text', selected together as one control --
-- clicking either the glyph or the caption activates it, the same as
-- 'Blink.Controls.Toggle.toggleButton'. Unlike a 'Blink.Controls.Toggle.toggleButton'
-- or 'Blink.Controls.Checkbox.checkbox', activating it never deselects it
-- -- only ever moves it from unselected to selected, since a radio button
-- gives up selection by a sibling in its group being selected instead,
-- never by being clicked again itself. See
-- 'Blink.Controls.Toggle.onSelectedChanged' for reacting to it. Defaults to
-- sizing itself to its own glyph-plus-caption content on both axes, the
-- same as 'Blink.Controls.Label.label'; override with
-- 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'\/'Blink.Layout.Constraints.align'.
radioButton :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> Element e msg
radioButton eid attrs = Element
  { elLayout  = bcLayout btn
  , elMeasure = measureChrome (ccStyleKey ctrl) (glyphCaptionElement glyphWidth glyphGap (lcText (bcLabelled btn)))
  , elRun     = void (toggleBase eid cfg')
  }
  where
    cfg      = resolve (defaultGlyphToggleConfig radioButtonStyleKey) attrs
    btn      = tgcButton cfg
    selected = tgcSelected cfg
    drawGlyph = do
      s <- currentStyle
      drawText (styleTextColour s) AlignCenter (radioGlyph selected)
    glyphContent = glyphCaptionContent glyphWidth glyphGap drawGlyph (bcLabelled btn)
    ctrl = (bcControl btn) { ccContent = glyphContent }
    cfg' = cfg
      { tgcNext   = const True
      , tgcButton = btn { bcControl = ctrl }
      }
