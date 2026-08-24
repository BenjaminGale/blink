{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
-- Built on 'toggleBase' -- see "Blink.Controls.Button" for how it and every other
-- button-like control fit together.
module Blink.Controls.Checkbox
  ( CheckboxAttributes
  , CheckboxConfig
  , checkbox
  , checkboxStyleKey
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

import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Blink.Controls.Button (HasIsSelectedConfig (..), HasSelectedChangedEvents (..), isSelected, onSelectedChanged, toggleBase)
import Blink.Controls.Control
import Blink.Geometry (Rectangle (..), uniformBorder)
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (Out, UI, currentStyle, drawText, getBounds, strokeRect, withBounds)

-- | 'Blink.Controls.Checkbox.checkbox'\'s own closed attrs type: the common
-- capabilities every control has, plus 'text', 'isSelected', and
-- 'onSelectedChanged'.
data CheckboxAttributes e msg
  = CheckboxCommon (ControlProperties e)
  | CheckboxEvent (ElementEvents e msg)
  | CheckboxText Text
  | CheckboxIsSelected Bool
  | CheckboxOnSelectedChanged (Bool -> [Out e msg])

instance HasControlConfig e (CheckboxAttributes e msg) where
  configureControlCapability = CheckboxCommon
  extractControlCapability (CheckboxCommon c) = Just c
  extractControlCapability _ = Nothing

instance HasElementEvents e msg (CheckboxAttributes e msg) where
  configureElementEvent = CheckboxEvent
  extractElementEvent (CheckboxEvent c) = Just c
  extractElementEvent _ = Nothing

instance HasTextConfig (CheckboxAttributes e msg) where
  configureText = CheckboxText
  extractText (CheckboxText t) = Just t
  extractText _ = Nothing

instance HasIsSelectedConfig (CheckboxAttributes e msg) where
  configureIsSelected = CheckboxIsSelected

instance HasSelectedChangedEvents e msg (CheckboxAttributes e msg) where
  configureOnSelectedChanged = CheckboxOnSelectedChanged

-- | Configuration for 'checkbox', resolved from a
-- @['CheckboxAttributes' e msg]@.
data CheckboxConfig e msg = CheckboxConfig
  { ckcfgText              :: Text
  , ckcfgSelected          :: Bool
  , ckcfgOnSelectedChanged :: [Bool -> [Out e msg]]
  }

-- | The 'StyleKey' 'checkbox' resolves its style from unless overridden via
-- 'style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

defaultCheckboxConfig :: CheckboxConfig e msg
defaultCheckboxConfig = CheckboxConfig
  { ckcfgText = "", ckcfgSelected = False, ckcfgOnSelectedChanged = [] }

resolveCheckboxConfig :: [CheckboxAttributes e msg] -> CheckboxConfig e msg
resolveCheckboxConfig = foldl' apply defaultCheckboxConfig
  where
    apply cfg (CheckboxText t)              = cfg { ckcfgText = t }
    apply cfg (CheckboxIsSelected b)        = cfg { ckcfgSelected = b }
    apply cfg (CheckboxOnSelectedChanged f) = cfg { ckcfgOnSelectedChanged = ckcfgOnSelectedChanged cfg ++ [f] }
    apply cfg _                             = cfg

toCheckboxControlAttr :: CheckboxAttributes e msg -> Maybe (ControlAttrs e msg)
toCheckboxControlAttr (CheckboxText _)              = Nothing
toCheckboxControlAttr (CheckboxIsSelected _)        = Nothing
toCheckboxControlAttr (CheckboxOnSelectedChanged _) = Nothing
toCheckboxControlAttr a                             = translateCommon a

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

-- | A checkbox: a small box drawn with 'strokeRect', a tick inside it while
-- selected (see 'isSelected'), beside a caption set via 'text', toggled
-- together as one control -- clicking either the box or the caption
-- activates it, the same as 'Blink.Controls.Button.toggleButton'. Flips every time
-- it's activated; see 'onSelectedChanged' for reacting to it.
checkbox :: Ord e => e -> [CheckboxAttributes e msg] -> UI e msg ()
checkbox eid attrs =
  toggleBase not eid (style checkboxStyleKey : mapMaybe toCheckboxControlAttr attrs) (ckcfgSelected cfg) (ckcfgOnSelectedChanged cfg) draw
  where
    cfg = resolveCheckboxConfig attrs
    draw selected = do
      s      <- currentStyle
      bounds <- getBounds
      let glyphRect  = bounds { rectWidth = glyphWidth }
          boxSize    = max 0 (min glyphWidth (rectHeight bounds) - 2)
          boxRect    = Rectangle
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
      withBounds textRect $ drawText (styleTextColour s) (styleTextAlign s) (ckcfgText cfg)
