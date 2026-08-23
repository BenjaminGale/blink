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
  mkIsFocusable = CheckboxIsFocusable
  matchIsFocusable (CheckboxIsFocusable b) = Just b
  matchIsFocusable _ = Nothing
  mkIsEnabled = CheckboxIsEnabled
  matchIsEnabled (CheckboxIsEnabled b) = Just b
  matchIsEnabled _ = Nothing
  mkStyle = CheckboxStyle
  matchStyle (CheckboxStyle k) = Just k
  matchStyle _ = Nothing
  mkTabNavigation = CheckboxTabNavigation
  matchTabNavigation (CheckboxTabNavigation m) = Just m
  matchTabNavigation _ = Nothing
  mkIsArrowNavigationEnabled = CheckboxIsArrowNavigationEnabled
  matchIsArrowNavigationEnabled (CheckboxIsArrowNavigationEnabled b) = Just b
  matchIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (CheckboxAttributes e msg) where
  mkOnClicked = CheckboxOnClicked
  matchOnClicked (CheckboxOnClicked f) = Just f
  matchOnClicked _ = Nothing
  mkOnFocusGained = CheckboxOnFocusGained
  matchOnFocusGained (CheckboxOnFocusGained f) = Just f
  matchOnFocusGained _ = Nothing
  mkOnFocusLost = CheckboxOnFocusLost
  matchOnFocusLost (CheckboxOnFocusLost f) = Just f
  matchOnFocusLost _ = Nothing
  mkOnMouseEntered = CheckboxOnMouseEntered
  matchOnMouseEntered (CheckboxOnMouseEntered f) = Just f
  matchOnMouseEntered _ = Nothing
  mkOnMouseExited = CheckboxOnMouseExited
  matchOnMouseExited (CheckboxOnMouseExited f) = Just f
  matchOnMouseExited _ = Nothing
  mkOnMouseDown = CheckboxOnMouseDown
  matchOnMouseDown (CheckboxOnMouseDown f) = Just f
  matchOnMouseDown _ = Nothing
  mkOnMouseUp = CheckboxOnMouseUp
  matchOnMouseUp (CheckboxOnMouseUp f) = Just f
  matchOnMouseUp _ = Nothing
  mkOnKeyPressed = CheckboxOnKeyPressed
  matchOnKeyPressed (CheckboxOnKeyPressed f) = Just f
  matchOnKeyPressed _ = Nothing

instance HasTextConfig (CheckboxAttributes e msg) where
  mkText = CheckboxText
  matchText (CheckboxText t) = Just t
  matchText _ = Nothing

instance HasIsSelectedConfig (CheckboxAttributes e msg) where
  mkIsSelected = CheckboxIsSelected

instance HasSelectedChangedEvents e msg (CheckboxAttributes e msg) where
  mkOnSelectedChanged = CheckboxOnSelectedChanged

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
