{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A checkbox: a glyph and a caption toggled together as one control.
-- Built on 'toggleBase' -- see "Blink.Button" for how it and every other
-- button-like control fit together.
module Blink.Checkbox
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

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Blink.Button (HasIsSelectedConfig (..), HasSelectedChangedEvents (..), isSelected, onSelectedChanged, toggleBase)
import Blink.Control
import Blink.Geometry (Rectangle (..))
import Blink.Rendering (TextAlign (..))
import Blink.Style (Style (..))
import Blink.UI (Out, UI, currentStyle, drawText, getBounds, withBounds)

-- | 'Blink.Checkbox.checkbox'\'s own closed attrs type: the common
-- capabilities every control has, plus 'text', 'isSelected', and
-- 'onSelectedChanged'.
data CheckboxAttributes e msg
  = CheckboxIsFocusable Bool
  | CheckboxIsEnabled Bool
  | CheckboxStyle (StyleKey e)
  | CheckboxTabNavigation NavigationMode
  | CheckboxIsArrowNavigationEnabled Bool
  | CheckboxOnClicked (EventHandler e msg)
  | CheckboxOnFocusGained (EventHandler e msg)
  | CheckboxOnFocusLost (EventHandler e msg)
  | CheckboxOnMouseEntered (EventHandler e msg)
  | CheckboxOnMouseExited (EventHandler e msg)
  | CheckboxOnMouseDown (EventHandler e msg)
  | CheckboxOnMouseUp (EventHandler e msg)
  | CheckboxOnKeyPressed (KeyEventHandler e msg)
  | CheckboxText Text
  | CheckboxIsSelected Bool
  | CheckboxOnSelectedChanged (Bool -> [Out e msg])

instance HasControlConfig e (CheckboxAttributes e msg) where
  configureIsFocusable = CheckboxIsFocusable
  extractIsFocusable (CheckboxIsFocusable b) = Just b
  extractIsFocusable _ = Nothing
  configureIsEnabled = CheckboxIsEnabled
  extractIsEnabled (CheckboxIsEnabled b) = Just b
  extractIsEnabled _ = Nothing
  configureStyle = CheckboxStyle
  extractStyle (CheckboxStyle k) = Just k
  extractStyle _ = Nothing
  configureTabNavigation = CheckboxTabNavigation
  extractTabNavigation (CheckboxTabNavigation m) = Just m
  extractTabNavigation _ = Nothing
  configureIsArrowNavigationEnabled = CheckboxIsArrowNavigationEnabled
  extractIsArrowNavigationEnabled (CheckboxIsArrowNavigationEnabled b) = Just b
  extractIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (CheckboxAttributes e msg) where
  configureOnClicked = CheckboxOnClicked
  extractOnClicked (CheckboxOnClicked f) = Just f
  extractOnClicked _ = Nothing
  configureOnFocusGained = CheckboxOnFocusGained
  extractOnFocusGained (CheckboxOnFocusGained f) = Just f
  extractOnFocusGained _ = Nothing
  configureOnFocusLost = CheckboxOnFocusLost
  extractOnFocusLost (CheckboxOnFocusLost f) = Just f
  extractOnFocusLost _ = Nothing
  configureOnMouseEntered = CheckboxOnMouseEntered
  extractOnMouseEntered (CheckboxOnMouseEntered f) = Just f
  extractOnMouseEntered _ = Nothing
  configureOnMouseExited = CheckboxOnMouseExited
  extractOnMouseExited (CheckboxOnMouseExited f) = Just f
  extractOnMouseExited _ = Nothing
  configureOnMouseDown = CheckboxOnMouseDown
  extractOnMouseDown (CheckboxOnMouseDown f) = Just f
  extractOnMouseDown _ = Nothing
  configureOnMouseUp = CheckboxOnMouseUp
  extractOnMouseUp (CheckboxOnMouseUp f) = Just f
  extractOnMouseUp _ = Nothing
  configureOnKeyPressed = CheckboxOnKeyPressed
  extractOnKeyPressed (CheckboxOnKeyPressed f) = Just f
  extractOnKeyPressed _ = Nothing

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
  , ckcfgOnClicked         :: [EventHandler e msg]
  , ckcfgOnSelectedChanged :: [Bool -> [Out e msg]]
  }

-- | The 'StyleKey' 'checkbox' resolves its style from unless overridden via
-- 'style'.
checkboxStyleKey :: StyleKey e
checkboxStyleKey = Class "checkbox"

defaultCheckboxConfig :: CheckboxConfig e msg
defaultCheckboxConfig = CheckboxConfig
  { ckcfgText = "", ckcfgSelected = False, ckcfgOnClicked = [], ckcfgOnSelectedChanged = [] }

resolveCheckboxConfig :: [CheckboxAttributes e msg] -> CheckboxConfig e msg
resolveCheckboxConfig = foldl' apply defaultCheckboxConfig
  where
    apply cfg (CheckboxText t)              = cfg { ckcfgText = t }
    apply cfg (CheckboxIsSelected b)        = cfg { ckcfgSelected = b }
    apply cfg (CheckboxOnClicked f)         = cfg { ckcfgOnClicked = ckcfgOnClicked cfg ++ [f] }
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

checkboxGlyph :: Bool -> Text
checkboxGlyph True  = "\9745" -- BALLOT BOX WITH CHECK
checkboxGlyph False = "\9744" -- BALLOT BOX

-- | A checkbox: a glyph showing whether it's currently selected (see
-- 'isSelected'), beside a caption set via 'text', toggled together as one
-- control -- clicking either the glyph or the caption activates it, the
-- same as 'Blink.Button.toggleButton'. Flips every time it's activated;
-- see 'onSelectedChanged' for reacting to it.
checkbox :: Ord e => e -> [CheckboxAttributes e msg] -> UI e msg ()
checkbox eid attrs =
  toggleBase not eid (mapMaybe toCheckboxControlAttr attrs) (ckcfgOnClicked cfg) (ckcfgSelected cfg) (ckcfgOnSelectedChanged cfg) draw
  where
    cfg = resolveCheckboxConfig attrs
    draw selected = do
      s      <- currentStyle
      bounds <- getBounds
      let glyphRect = bounds { rectWidth = glyphWidth }
          textRect  = bounds { rectX = rectX bounds + glyphWidth, rectWidth = max 0 (rectWidth bounds - glyphWidth) }
      withBounds glyphRect $ drawText (styleTextColour s) AlignCenter (checkboxGlyph selected)
      withBounds textRect  $ drawText (styleTextColour s) (styleTextAlign s) (ckcfgText cfg)
