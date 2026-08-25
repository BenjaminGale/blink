{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
-- Built on 'toggleBase' -- see "Blink.Controls.Button" for how it and every
-- other button-like control fit together. A leaf: nothing derives from it,
-- so it has no 'Blink.Controls.Core.ControlConfig'\/'Blink.Controls.Core.ControlInteraction'-style pair of its own
-- beyond 'ToggleConfig'\/'Blink.Controls.Button.ToggleInteraction', which already have every field
-- it needs.
module Blink.Controls.Checkbox
  ( checkbox
  , checkboxStyleKey
  ) where

import Control.Monad (when)
import Data.Text (Text)

import Blink.Controls.Button (ButtonConfig (..), ToggleConfig (..), defaultToggleButtonConfig, toggleBase)
import Blink.Controls.Core
import Blink.Controls.Labelled (renderLabelledContent)
import Blink.Geometry (Rectangle (..), uniformBorder)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (UI, currentStyle, drawText, getBounds, strokeRect, withBounds)

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

-- | The gap between the glyph and the caption beside it.
labelGap :: Double
labelGap = 6

-- | 'CHECK MARK' (U+2713) is near-universally supported, unlike the
-- 'BALLOT BOX WITH CHECK' glyph (U+2611) that would otherwise draw the
-- whole box-plus-tick in one character.
checkTick :: Text
checkTick = "\10003"

-- | The 'StyleKey' 'checkbox' resolves its style from unless overridden via
-- 'style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

defaultCheckboxConfig :: ToggleConfig e msg
defaultCheckboxConfig = defaultToggleButtonConfig
  { tgcButton = (tgcButton defaultToggleButtonConfig)
      { bcControl = (bcControl (tgcButton defaultToggleButtonConfig)) { ccStyleKey = checkboxStyleKey } }
  }

-- | A checkbox: a small box drawn with 'strokeRect', a tick inside it while
-- selected (see 'Blink.Controls.Button.isSelected'), beside a caption set via 'Blink.Controls.Labelled.text', toggled
-- together as one control -- clicking either the box or the caption
-- activates it, the same as 'Blink.Controls.Button.toggleButton'. Flips
-- every time it's activated; see 'Blink.Controls.Button.onSelectedChanged' for reacting to it.
checkbox :: Ord e => e -> [Attr (ToggleConfig e msg)] -> UI e msg ()
checkbox eid attrs = do
  let cfg      = resolve defaultCheckboxConfig attrs
      btn      = tgcButton cfg
      selected = tgcSelected cfg
      glyphContent = do
        s      <- currentStyle
        bounds <- getBounds
        let glyphRect = bounds { rectWidth = glyphWidth }
            boxSize   = max 0 (min glyphWidth (rectHeight bounds) - 2)
            boxRect   = Rectangle
              { rectX      = rectX glyphRect + (glyphWidth - boxSize) / 2
              , rectY      = rectY glyphRect + (rectHeight glyphRect - boxSize) / 2
              , rectWidth  = boxSize
              , rectHeight = boxSize
              }
            textRect  = bounds
              { rectX     = rectX bounds + glyphWidth + labelGap
              , rectWidth = max 0 (rectWidth bounds - glyphWidth - labelGap)
              }
        withBounds boxRect $ do
          strokeRect (styleTextColour s) (uniformBorder 1)
          when selected $ drawText (styleTextColour s) AlignCenter checkTick
        withBounds textRect $ renderLabelledContent (bcLabelled btn)
      ctrl = (bcControl btn) { ccContent = glyphContent }
      cfg' = cfg
        { tgcNext   = not
        , tgcButton = btn { bcControl = ctrl }
        }
  () <$ toggleBase eid cfg'
