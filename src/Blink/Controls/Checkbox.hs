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
    -- * Style
  , defaultStyleEntries
  ) where

import Control.Monad (void)
import qualified Data.Set as Set

import Blink.Controls.Button (ButtonConfig (..))
import Blink.Controls.Control
import Blink.Controls.Label (lcText)
import Blink.Controls.ToggleButton
  (ToggleConfig (..), defaultGlyphToggleConfig, glyphCaptionContent, glyphCaptionElement, toggleBase)
import Blink.Controls.Style (flatRowMetrics, flatRowStyle, iconStyleKey)
import Blink.Geometry (Rectangle (..), Size (..))
import Blink.Rendering (ImagePath)
import Blink.View (getBounds, getStyleSet, isDisabled, isRegionHit, withBounds)
import Blink.View.Drawing (drawImage)
import Blink.Element (Element (..))
import Blink.Style

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 28

-- | The gap between the glyph and the caption beside it.
labelGap :: Double
labelGap = 6

-- | Space after the caption, matching the space the box icon leaves on its
-- left (the column's centring plus the icon's own transparent margin), so
-- the control looks evenly padded.
captionTrailingSpace :: Double
captionTrailingSpace = 5

-- | The margin left between the glyph column's edges and the drawn box
-- icon, so it doesn't touch the caption or the control's own bounds.
boxInset :: Double
boxInset = 2

-- | The whole box, drawn filled (with its own tick) while selected.
checkBoxIcon :: ImagePath
checkBoxIcon = "assets/icons/check_box.svg"

-- | The whole box, drawn empty while not selected.
checkBoxOutlineIcon :: ImagePath
checkBoxOutlineIcon = "assets/icons/check_box_outline_blank.svg"

-- | A checkbox: a box-plus-tick icon (see 'Blink.Controls.ToggleButton.isSelected'), beside a caption set via 'Blink.Controls.Label.text', toggled
-- together as one control -- clicking either the box or the caption
-- activates it, the same as 'Blink.Controls.ToggleButton.toggleButton'. Flips
-- every time it's activated; see 'Blink.Controls.ToggleButton.onSelectedChanged' for reacting to it.
-- Defaults to sizing itself to its glyph, caption and the space after it
-- on both axes, the same as 'Blink.Controls.Label.label'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
checkbox :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> Element e msg
checkbox eid attrs = Element
  { elLayout  = bcLayout btn
  , elMeasure = measureChrome (ccStyleKey ctrl) content
  , elRun     = void (toggleBase eid cfg')
  }
  where
    cfg      = resolve (defaultGlyphToggleConfig checkboxStyleKey) attrs
    content  = withTrailingSpace (glyphCaptionElement glyphWidth labelGap (lcText (bcLabelled btn)))
    withTrailingSpace el =
      el { elMeasure = fmap (\sz -> sz { sizeWidth = sizeWidth sz + captionTrailingSpace }) . elMeasure el }
    btn      = tgcButton cfg
    selected = tgcSelected cfg
    -- | The box-plus-tick icon, centred within the glyph column's own
    -- bounds, matching whichever state 'selected' is in. Tinted from
    -- 'Blink.Controls.Style.iconStyleKey' -- a separate 'StyleKey' from
    -- the row's own ('checkboxStyleKey'), resolved here against whether
    -- the cursor is over the /icon's own rectangle specifically/
    -- (checked via 'isRegionHit' only after narrowing to 'boxRect') --
    -- so hovering the caption beside it, still within the control's
    -- larger hit area, leaves the icon (and the caption's own colour,
    -- which never reads from 'iconStyleKey') alone.
    drawBox = do
      (_, iconStyleSet) <- getStyleSet iconStyleKey
      disabled          <- isDisabled
      bounds            <- getBounds
      let boxSize = max 0 (min glyphWidth (rectHeight bounds) - boxInset)
          boxRect = Rectangle
            { rectX      = rectX bounds + (glyphWidth - boxSize) / 2
            , rectY      = rectY bounds + (rectHeight bounds - boxSize) / 2
            , rectWidth  = boxSize
            , rectHeight = boxSize
            }
          icon = if selected then checkBoxIcon else checkBoxOutlineIcon
      withBounds boxRect $ do
        hovered <- isRegionHit
        let iconState
              | disabled  = CommonDisabled
              | hovered   = CommonMouseOver
              | otherwise = CommonNormal
            colour = styleTextColour (resolveStyle iconStyleSet (Set.singleton iconState))
        drawImage colour icon
    glyphContent = glyphCaptionContent glyphWidth labelGap drawBox (bcLabelled btn)
    ctrl = (bcControl btn) { ccContent = const glyphContent }
    cfg' = cfg
      { tgcNext   = not
      , tgcButton = btn { bcControl = ctrl }
      }

-- * Style

-- | The 'StyleKey' 'Blink.Controls.Checkbox.checkbox' resolves its
-- style from unless overridden via 'Blink.Controls.Control.style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (checkboxStyleKey, (flatRowMetrics, flatRowStyle p)) ]
