{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'toggleBase' -- see "Blink.Button" for how it and every other
-- button-like control fit together.
module Blink.RadioButton
  ( RadioButtonConfig
  , radioButton
  , text
  , isSelected
  , onSelectedChanged
  , isTabStop
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
  , defaultControlConfig, configure, isTabStop, text
  )
import Blink.Button (HasToggleConfig (..), ToggleConfig (..), isSelected, onSelectedChanged, toggleBase)
import Blink.Control (getStyle)
import Blink.Element
  ( ElementEvent
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (UI, drawText, getBounds, withBounds)

-- | Configuration for 'radioButton', set via 'text', 'isSelected', and
-- 'onSelectedChanged'. Defaults to no caption, not selected, and no reaction.
data RadioButtonConfig e msg = RadioButtonConfig
  { radioConfigControl :: ControlConfig e
  , radioConfigToggle  :: ToggleConfig e msg
  , radioConfigText    :: Text
  }

defaultRadioButtonConfig :: RadioButtonConfig e msg
defaultRadioButtonConfig = RadioButtonConfig
  { radioConfigControl = defaultControlConfig
  , radioConfigToggle  = ToggleConfig { tcSelected = False, tcOnSelectedChanged = const [] }
  , radioConfigText    = ""
  }

instance HasControlConfig e (RadioButtonConfig e msg) where
  controlConfig    = radioConfigControl
  setControlConfig cc cfg = cfg { radioConfigControl = cc }

instance HasToggleConfig e msg (RadioButtonConfig e msg) where
  toggleConfig    = radioConfigToggle
  setToggleConfig tc cfg = cfg { radioConfigToggle = tc }

instance HasTextConfig (RadioButtonConfig e msg) where
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
radioButton :: Ord e => e -> [Attr e ElementEvent msg (RadioButtonConfig e msg)] -> UI e msg ()
radioButton eid attrs = toggleBase (const True) eid cfg attrs $ do
  style  <- getStyle eid
  bounds <- getBounds
  let glyphRect = bounds { rectWidth = glyphWidth }
      textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
  withBounds glyphRect $ drawText (styleTextColour style) AlignCenter (radioGlyph (tcSelected (radioConfigToggle cfg)))
  withBounds textRect  $ drawText (styleTextColour style) (styleTextAlign style) (radioConfigText cfg)
  where
    cfg = configure defaultRadioButtonConfig attrs
