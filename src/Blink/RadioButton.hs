{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A radio button: a glyph and a caption selected together as one control.
-- Built on 'toggleBase' -- see "Blink.Button" for how it and every other
-- button-like control fit together.
module Blink.RadioButton
  ( RadioButtonAttributes
  , RadioButtonConfig
  , radioButton
  , radioButtonStyleKey
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

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Blink.Button (HasIsSelectedConfig (..), HasSelectedChangedEvents (..), isSelected, onSelectedChanged, toggleBase)
import Blink.Control
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (Out, UI, currentStyle, drawText, getBounds, withBounds)

-- | 'Blink.RadioButton.radioButton'\'s own closed attrs type: the common
-- capabilities every control has, plus 'text', 'isSelected', and
-- 'onSelectedChanged'.
data RadioButtonAttributes e msg
  = RadioButtonCommon (ControlProperties e)
  | RadioButtonEvent (ElementEvents e msg)
  | RadioButtonText Text
  | RadioButtonIsSelected Bool
  | RadioButtonOnSelectedChanged (Bool -> [Out e msg])

instance HasControlConfig e (RadioButtonAttributes e msg) where
  configureControlCapability = RadioButtonCommon
  extractControlCapability (RadioButtonCommon c) = Just c
  extractControlCapability _ = Nothing

instance HasElementEvents e msg (RadioButtonAttributes e msg) where
  configureElementEvent = RadioButtonEvent
  extractElementEvent (RadioButtonEvent c) = Just c
  extractElementEvent _ = Nothing

instance HasTextConfig (RadioButtonAttributes e msg) where
  configureText = RadioButtonText
  extractText (RadioButtonText t) = Just t
  extractText _ = Nothing

instance HasIsSelectedConfig (RadioButtonAttributes e msg) where
  configureIsSelected = RadioButtonIsSelected

instance HasSelectedChangedEvents e msg (RadioButtonAttributes e msg) where
  configureOnSelectedChanged = RadioButtonOnSelectedChanged

-- | Configuration for 'radioButton', resolved from a
-- @['RadioButtonAttributes' e msg]@.
data RadioButtonConfig e msg = RadioButtonConfig
  { rbcfgText              :: Text
  , rbcfgSelected          :: Bool
  , rbcfgOnClicked         :: [EventHandler e msg]
  , rbcfgOnSelectedChanged :: [Bool -> [Out e msg]]
  }

-- | The 'StyleKey' 'radioButton' resolves its style from unless overridden
-- via 'style'.
radioButtonStyleKey :: StyleKey e
radioButtonStyleKey = Class "radioButton"

defaultRadioButtonConfig :: RadioButtonConfig e msg
defaultRadioButtonConfig = RadioButtonConfig
  { rbcfgText = "", rbcfgSelected = False, rbcfgOnClicked = [], rbcfgOnSelectedChanged = [] }

resolveRadioButtonConfig :: [RadioButtonAttributes e msg] -> RadioButtonConfig e msg
resolveRadioButtonConfig = foldl' apply defaultRadioButtonConfig
  where
    apply cfg (RadioButtonText t)                     = cfg { rbcfgText = t }
    apply cfg (RadioButtonIsSelected b)               = cfg { rbcfgSelected = b }
    apply cfg (RadioButtonEvent (ElementOnClicked f)) = cfg { rbcfgOnClicked = rbcfgOnClicked cfg ++ [f] }
    apply cfg (RadioButtonOnSelectedChanged f)        = cfg { rbcfgOnSelectedChanged = rbcfgOnSelectedChanged cfg ++ [f] }
    apply cfg _                                       = cfg

toRadioButtonControlAttr :: RadioButtonAttributes e msg -> Maybe (ControlAttrs e msg)
toRadioButtonControlAttr (RadioButtonText _)              = Nothing
toRadioButtonControlAttr (RadioButtonIsSelected _)        = Nothing
toRadioButtonControlAttr (RadioButtonOnSelectedChanged _) = Nothing
toRadioButtonControlAttr a                                = translateCommon a

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
radioButton :: Ord e => e -> [RadioButtonAttributes e msg] -> UI e msg ()
radioButton eid attrs =
  toggleBase (const True) eid (mapMaybe toRadioButtonControlAttr attrs) (rbcfgOnClicked cfg) (rbcfgSelected cfg) (rbcfgOnSelectedChanged cfg) draw
  where
    cfg = resolveRadioButtonConfig attrs
    draw selected = do
      s      <- currentStyle
      bounds <- getBounds
      let glyphRect = bounds { rectWidth = glyphWidth }
          textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
      withBounds glyphRect $ drawText (styleTextColour s) AlignCenter (radioGlyph selected)
      withBounds textRect  $ drawText (styleTextColour s) (styleTextAlign s) (rbcfgText cfg)
