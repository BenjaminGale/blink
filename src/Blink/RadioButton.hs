{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'toggleBase' -- see "Blink.Button" for how it and every other
-- button-like control fit together.
module Blink.RadioButton
  ( RadioButtonConfig
  , radioButton
  , radioButtonStyleKey
  , ToggleEvent (..)
  , text
  , isSelected
  , onSelectedChanged
  , isTabStop
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
  , defaultControlConfig, configure, isEnabled, isTabStop, style, text
  )
import Blink.Button (HasToggleConfig (..), ToggleConfig (..), ToggleEvent (..), isSelected, onSelectedChanged, toggleBase)
import Blink.Element
  ( onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..), StyleKey (..))
import Blink.UI (UI, currentStyle, drawText, getBounds, withBounds)

-- | Configuration for 'radioButton', set via 'text', 'isSelected', and
-- 'onSelectedChanged'. Defaults to no caption, not selected, and no reaction.
data RadioButtonConfig e = RadioButtonConfig
  { radioConfigControl :: ControlConfig e
  , radioConfigToggle  :: ToggleConfig
  , radioConfigText    :: Text
  }

-- | The 'StyleKey' 'radioButton' resolves its style from unless overridden
-- via 'style'.
radioButtonStyleKey :: StyleKey e
radioButtonStyleKey = Class "radioButton"

defaultRadioButtonConfig :: RadioButtonConfig e
defaultRadioButtonConfig = RadioButtonConfig
  { radioConfigControl = defaultControlConfig radioButtonStyleKey
  , radioConfigToggle  = ToggleConfig { tcSelected = False }
  , radioConfigText    = ""
  }

instance HasControlConfig e (RadioButtonConfig e) where
  controlConfig    = radioConfigControl
  setControlConfig cc cfg = cfg { radioConfigControl = cc }

instance HasToggleConfig (RadioButtonConfig e) where
  toggleConfig    = radioConfigToggle
  setToggleConfig tc cfg = cfg { radioConfigToggle = tc }

instance HasTextConfig (RadioButtonConfig e) where
  setText t cfg = cfg { radioConfigText = t }

-- | The fixed width reserved for the glyph, on the left of the caption.
glyphWidth :: Double
glyphWidth = 20

radioGlyph :: Bool -> Text
radioGlyph True  = "\9673" -- CIRCLED BULLET
radioGlyph False = "\9675" -- WHITE CIRCLE

-- | A radio button: a glyph showing whether it's currently selected (see
-- 'isSelected'), beside a caption set via 'text', selected together as one
-- control -- clicking either the glyph or the caption activates it, the
-- same as 'Blink.Button.toggleButton'. Unlike a 'Blink.Button.toggleButton'
-- or 'Blink.Checkbox.checkbox', activating it never deselects it -- only
-- ever moves it from unselected to selected, since a radio button gives up
-- selection by a sibling in its group being selected instead, never by
-- being clicked again itself. See 'onSelectedChanged' for reacting to it.
radioButton :: Ord e => e -> [Attr e ToggleEvent msg (RadioButtonConfig e)] -> UI e msg ()
radioButton eid attrs = toggleBase (const True) eid cfg attrs $ do
  s      <- currentStyle
  bounds <- getBounds
  let glyphRect = bounds { rectWidth = glyphWidth }
      textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
  withBounds glyphRect $ drawText (styleTextColour s) AlignCenter (radioGlyph (tcSelected (radioConfigToggle cfg)))
  withBounds textRect  $ drawText (styleTextColour s) (styleTextAlign s) (radioConfigText cfg)
  where
    cfg = configure defaultRadioButtonConfig attrs
