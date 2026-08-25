{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'toggleBase' -- see "Blink.Controls.Button" for how it and every
-- other button-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Control.ControlConfig'\/'Blink.Controls.Control.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.Button.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.RadioButton
  ( radioButton
  , radioButtonStyleKey
  ) where

import Data.Text (Text)

import Blink.Controls.Button (ButtonConfig (..), ToggleConfig (..), defaultToggleButtonConfig, toggleBase)
import Blink.Controls.Control
import Blink.Controls.Labelled (renderLabelledContent)
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText, getBounds, withBounds)

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

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

defaultRadioButtonConfig :: ToggleConfig e msg
defaultRadioButtonConfig = defaultToggleButtonConfig
  { tgcButton = (tgcButton defaultToggleButtonConfig)
      { bcControl = (bcControl (tgcButton defaultToggleButtonConfig)) { ccStyleKey = radioButtonStyleKey } }
  }

-- | A radio button: a glyph showing whether it's currently selected (see
-- 'Blink.Controls.Button.isSelected'), beside a caption set via
-- 'Blink.Controls.Labelled.text', selected together as one control --
-- clicking either the glyph or the caption activates it, the same as
-- 'Blink.Controls.Button.toggleButton'. Unlike a 'Blink.Controls.Button.toggleButton'
-- or 'Blink.Controls.Checkbox.checkbox', activating it never deselects it
-- -- only ever moves it from unselected to selected, since a radio button
-- gives up selection by a sibling in its group being selected instead,
-- never by being clicked again itself. See
-- 'Blink.Controls.Button.onSelectedChanged' for reacting to it.
radioButton :: Ord e => e -> [Attr (ToggleConfig e msg)] -> UI e msg ()
radioButton eid attrs = do
  let cfg      = resolve defaultRadioButtonConfig attrs
      btn      = tgcButton cfg
      selected = tgcSelected cfg
      glyphContent = do
        s      <- currentStyle
        bounds <- getBounds
        let glyphRect = bounds { rectWidth = glyphWidth }
            textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
        withBounds glyphRect $ drawText (styleTextColour s) AlignCenter (radioGlyph selected)
        withBounds textRect  $ renderLabelledContent (bcLabelled btn)
      ctrl = (bcControl btn) { ccContent = glyphContent }
      cfg' = cfg
        { tgcNext   = const True
        , tgcButton = btn { bcControl = ctrl }
        }
  () <$ toggleBase eid cfg'
