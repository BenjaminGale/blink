{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
-- Built on 'toggleBase' -- see "Blink.Button" for how it and every other
-- button-like control fit together.
module Blink.Checkbox
  ( CheckboxConfig
  , checkbox
  , checkboxStyleKey
  , ToggleEvent (..)
  , text
  , isSelected
  , onSelectedChanged
  , isFocusable
  , isEnabled
  , style
  , StyleKey (..)
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
  ( Attr, ControlConfig, HasControlConfig (..), HasTextConfig (..)
  , defaultControlConfig, configure, isEnabled, isFocusable, style, text
  )
import Blink.Button (HasToggleConfig (..), ToggleConfig (..), ToggleEvent (..), isSelected, onSelectedChanged, toggleBase)
import Blink.Element
  ( onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..), StyleKey (..))
import Blink.UI (UI, currentStyle, drawText, getBounds, withBounds)

-- | Configuration for 'checkbox', set via 'text', 'isSelected', and
-- 'onSelectedChanged'. Defaults to no caption, not selected, and no reaction.
data CheckboxConfig e = CheckboxConfig
  { checkboxConfigControl :: ControlConfig e
  , checkboxConfigToggle  :: ToggleConfig
  , checkboxConfigText    :: Text
  }

-- | The 'StyleKey' 'checkbox' resolves its style from unless overridden via
-- 'style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

defaultCheckboxConfig :: CheckboxConfig e
defaultCheckboxConfig = CheckboxConfig
  { checkboxConfigControl = defaultControlConfig checkboxStyleKey
  , checkboxConfigToggle  = ToggleConfig { tcSelected = False }
  , checkboxConfigText    = ""
  }

instance HasControlConfig e (CheckboxConfig e) where
  controlConfig    = checkboxConfigControl
  setControlConfig cc cfg = cfg { checkboxConfigControl = cc }

instance HasToggleConfig (CheckboxConfig e) where
  toggleConfig    = checkboxConfigToggle
  setToggleConfig tc cfg = cfg { checkboxConfigToggle = tc }

instance HasTextConfig (CheckboxConfig e) where
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
-- see 'onSelectedChanged' for reacting to it.
checkbox :: Ord e => e -> [Attr e ToggleEvent msg (CheckboxConfig e)] -> UI e msg ()
checkbox eid attrs = toggleBase not eid cfg attrs $ do
  s      <- currentStyle
  bounds <- getBounds
  let glyphRect = bounds { rectWidth = glyphWidth }
      textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
  withBounds glyphRect $ drawText (styleTextColour s) AlignCenter (checkboxGlyph (tcSelected (checkboxConfigToggle cfg)))
  withBounds textRect  $ drawText (styleTextColour s) (styleTextAlign s) (checkboxConfigText cfg)
  where
    cfg = configure defaultCheckboxConfig attrs
