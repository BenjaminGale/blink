{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'toggleBase' -- see "Blink.Controls.ToggleButton" for how it and every
-- other toggle-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Control.ControlConfig'\/'Blink.Controls.Control.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.ToggleButton.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.RadioButton
  ( radioButton
  , radioButtonStyleKey
  ) where

import Control.Monad (void)
import qualified Data.Set as Set

import Blink.Controls.Button (ButtonConfig (..))
import Blink.Controls.Control
import Blink.Controls.Label (lcText)
import Blink.Controls.ToggleButton
  (ToggleConfig (..), defaultGlyphToggleConfig, glyphCaptionContent, glyphCaptionElement, toggleBase)
import Blink.Controls.RadioButton.Style (radioButtonStyleKey)
import Blink.Controls.Style (iconStyleKey)
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (ImagePath)
import Blink.Style (Style (..), VisualState (..), resolveStyle)
import Blink.View (getBounds, getStyleSet, isDisabled, isRegionHit, withBounds)
import Blink.View.Drawing (drawImage)
import Blink.Element (Element (..))

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

-- | The gap between the glyph and the caption beside it.
glyphGap :: Double
glyphGap = 6

-- | The bullet, filled while selected.
radioCheckedIcon :: ImagePath
radioCheckedIcon = "assets/icons/radio_button_checked.svg"

-- | The bullet, an empty ring while not selected.
radioUncheckedIcon :: ImagePath
radioUncheckedIcon = "assets/icons/radio_button_unchecked.svg"

-- | A radio button: a glyph showing whether it's currently selected (see
-- 'Blink.Controls.ToggleButton.isSelected'), beside a caption set via
-- 'Blink.Controls.Label.text', selected together as one control --
-- clicking either the glyph or the caption activates it, the same as
-- 'Blink.Controls.ToggleButton.toggleButton'. Unlike a 'Blink.Controls.ToggleButton.toggleButton'
-- or 'Blink.Controls.Checkbox.checkbox', activating it never deselects it
-- -- only ever moves it from unselected to selected, since a radio button
-- gives up selection by a sibling in its group being selected instead,
-- never by being clicked again itself. See
-- 'Blink.Controls.ToggleButton.onSelectedChanged' for reacting to it. Defaults to
-- sizing itself to its own glyph-plus-caption content on both axes, the
-- same as 'Blink.Controls.Label.label'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
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
    -- | The bullet icon, centred within the glyph column's own bounds --
    -- see 'Blink.Controls.Checkbox.checkbox's @drawBox@ for why this
    -- resolves 'iconStyleKey' itself (rather than using the row's own
    -- resolved style) and scopes hover to the icon's own rectangle.
    drawGlyph = do
      (_, iconStyleSet) <- getStyleSet iconStyleKey
      disabled          <- isDisabled
      bounds            <- getBounds
      let boxSize = max 0 (min glyphWidth (rectHeight bounds))
          boxRect = Rectangle
            { rectX      = rectX bounds + (glyphWidth - boxSize) / 2
            , rectY      = rectY bounds + (rectHeight bounds - boxSize) / 2
            , rectWidth  = boxSize
            , rectHeight = boxSize
            }
          icon = if selected then radioCheckedIcon else radioUncheckedIcon
      withBounds boxRect $ do
        hovered <- isRegionHit
        let iconState
              | disabled  = CommonDisabled
              | hovered   = CommonMouseOver
              | otherwise = CommonNormal
            colour = styleTextColour (resolveStyle iconStyleSet (Set.singleton iconState))
        drawImage colour icon
    glyphContent = glyphCaptionContent glyphWidth glyphGap drawGlyph (bcLabelled btn)
    ctrl = (bcControl btn) { ccContent = const glyphContent }
    cfg' = cfg
      { tgcNext   = const True
      , tgcButton = btn { bcControl = ctrl }
      }
