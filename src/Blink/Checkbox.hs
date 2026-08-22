{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
module Blink.Checkbox
  ( CheckboxConfig
  , checkbox
  , text
  , isSelected
  , onToggled
  , isTabStop
  , focusOnClick
  , FocusOnClick (..)
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Data.Text (Text)

import Blink.Attributes
  ( Attr, ControlConfig, FocusOnClick (..), HasControlConfig (..), HasTextConfig (..)
  , defaultControlConfig, configure, focusOnClick, isTabStop, text
  )
import Blink.Button (HasToggleConfig (..), ToggleConfig (..), isSelected, onToggled, toggleBase)
import Blink.Control (getStyle)
import Blink.Element
  ( ElementEvent
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (UI, drawText, getBounds, withBounds)

-- | Configuration for 'checkbox', set via 'text', 'isSelected', and
-- 'onToggled'. Defaults to no caption, not selected, and no reaction.
data CheckboxConfig e msg = CheckboxConfig
  { checkboxConfigControl :: ControlConfig e
  , checkboxConfigToggle  :: ToggleConfig e msg
  , checkboxConfigText    :: Text
  }

defaultCheckboxConfig :: CheckboxConfig e msg
defaultCheckboxConfig = CheckboxConfig
  { checkboxConfigControl = defaultControlConfig
  , checkboxConfigToggle  = ToggleConfig { tcSelected = False, tcOnToggled = const [] }
  , checkboxConfigText    = ""
  }

instance HasControlConfig e (CheckboxConfig e msg) where
  controlConfig    = checkboxConfigControl
  setControlConfig cc cfg = cfg { checkboxConfigControl = cc }

instance HasToggleConfig e msg (CheckboxConfig e msg) where
  toggleConfig    = checkboxConfigToggle
  setToggleConfig tc cfg = cfg { checkboxConfigToggle = tc }

instance HasTextConfig (CheckboxConfig e msg) where
  setText t cfg = cfg { checkboxConfigText = t }

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

checkboxGlyph :: Bool -> Text
checkboxGlyph True  = "\9745" -- BALLOT BOX WITH CHECK
checkboxGlyph False = "\9744" -- BALLOT BOX

-- | A checkbox: a glyph showing whether it's currently selected (see
-- 'isSelected'), beside a caption set via 'text', toggled together as one
-- control -- clicking either the glyph or the caption activates it, the
-- same as 'Blink.Button.toggleButton'. Flips every time it's activated;
-- see 'onToggled' for reacting to it.
checkbox :: Ord e => e -> [Attr e ElementEvent msg (CheckboxConfig e msg)] -> UI e msg ()
checkbox eid attrs = toggleBase not eid cfg attrs $ do
  style  <- getStyle eid
  bounds <- getBounds
  let glyphRect = bounds { rectWidth = glyphWidth }
      textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
  withBounds glyphRect $ drawText (styleTextColour style) AlignCenter (checkboxGlyph (tcSelected (checkboxConfigToggle cfg)))
  withBounds textRect  $ drawText (styleTextColour style) (styleTextAlign style) (checkboxConfigText cfg)
  where
    cfg = configure defaultCheckboxConfig attrs
