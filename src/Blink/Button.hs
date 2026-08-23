{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Buttons and button-like controls: activated by a click or by pressing
-- Enter while focused.
--
-- = The controls in this family
--
-- 'button' is a plain, momentary button. 'toggleButton',
-- 'Blink.Checkbox.checkbox', and 'Blink.RadioButton.radioButton' all track
-- a selected\/unselected state instead: 'toggleButton' flips every time
-- it's clicked; a checkbox does too, drawing a checkmark glyph beside its
-- caption; a radio button can only become selected, never unselected, by
-- being clicked -- it gives up selection when another radio button in the
-- same group is selected instead.
--
-- @
-- control  --> buttonBase --> button
--                         --> toggleBase --> toggleButton
--                                         --> checkbox     (see "Blink.Checkbox")
--                                         --> radioButton  (see "Blink.RadioButton")
-- @
module Blink.Button
  ( ButtonAttributes
  , ButtonConfig
  , button
  , buttonStyleKey
  , ToggleButtonAttributes
  , ToggleButtonConfig
  , toggleButton
  , toggleButtonStyleKey
  , HasIsSelectedConfig (..)
  , isSelected
  , HasSelectedChangedEvents (..)
  , onSelectedChanged
  , buttonBase
  , toggleBase
  , text
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

import Blink.Control
import Blink.Input (Key (KeyReturn), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI (Out, UI, currentStyle, drawText, getInput, getStyleSet, isDisabled, isFocused)

-- | Runs @body@ as a normal interactive control (already translated down to
-- @controlAttrs@), and additionally fires every handler in
-- @onClickedHandlers@ -- alongside a real mouse click, which fires them via
-- 'control' itself -- when Enter is pressed while it holds focus and it
-- isn't disabled. The shape every button-like control ('button',
-- 'toggleButton', and 'Blink.Checkbox.checkbox'\/'Blink.RadioButton.radioButton')
-- is built from.
--
-- Always takes focus when clicked -- fixed behaviour, not a default, so
-- unlike a plain "Blink.Control" control it can't be redirected elsewhere
-- or turned off.
buttonBase :: Ord e => e -> [ControlAttrs e msg] -> [() -> [Out e msg]] -> UI e msg () -> UI e msg ()
buttonBase eid controlAttrs onClickedHandlers body = do
  control eid (controlAttrs ++ [focusOnClick FocusSelf, content body])
  focused  <- isFocused eid
  disabled <- isDisabled
  input    <- getInput
  let pressedReturn = any (\ev -> key ev == KeyReturn) (inputKeyEvents input)
  when (not disabled && focused && pressedReturn) $ runHandlers onClickedHandlers ()

-- | 'Blink.Button.button'\'s own closed attrs type: the capabilities every
-- control has, plus 'text'. Doesn't implement 'HasFocusOnClickConfig'\/
-- 'HasContentConfig' -- see the "Blink.Control" module header for why
-- @button eid [content ...]@ therefore fails to typecheck.
data ButtonAttributes e msg
  = ButtonIsFocusable Bool
  | ButtonIsEnabled Bool
  | ButtonStyle (StyleKey e)
  | ButtonTabNavigation NavigationMode
  | ButtonIsArrowNavigationEnabled Bool
  | ButtonOnClicked (() -> [Out e msg])
  | ButtonOnFocusGained (() -> [Out e msg])
  | ButtonOnFocusLost (() -> [Out e msg])
  | ButtonOnMouseEntered (() -> [Out e msg])
  | ButtonOnMouseExited (() -> [Out e msg])
  | ButtonOnMouseDown (() -> [Out e msg])
  | ButtonOnMouseUp (() -> [Out e msg])
  | ButtonOnKeyPressed (KeyEvent -> [Out e msg])
  | ButtonText Text

instance HasControlConfig e (ButtonAttributes e msg) where
  mkIsFocusable = ButtonIsFocusable
  matchIsFocusable (ButtonIsFocusable b) = Just b
  matchIsFocusable _ = Nothing
  mkIsEnabled = ButtonIsEnabled
  matchIsEnabled (ButtonIsEnabled b) = Just b
  matchIsEnabled _ = Nothing
  mkStyle = ButtonStyle
  matchStyle (ButtonStyle k) = Just k
  matchStyle _ = Nothing
  mkTabNavigation = ButtonTabNavigation
  matchTabNavigation (ButtonTabNavigation m) = Just m
  matchTabNavigation _ = Nothing
  mkIsArrowNavigationEnabled = ButtonIsArrowNavigationEnabled
  matchIsArrowNavigationEnabled (ButtonIsArrowNavigationEnabled b) = Just b
  matchIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (ButtonAttributes e msg) where
  mkOnClicked = ButtonOnClicked
  matchOnClicked (ButtonOnClicked f) = Just f
  matchOnClicked _ = Nothing
  mkOnFocusGained = ButtonOnFocusGained
  matchOnFocusGained (ButtonOnFocusGained f) = Just f
  matchOnFocusGained _ = Nothing
  mkOnFocusLost = ButtonOnFocusLost
  matchOnFocusLost (ButtonOnFocusLost f) = Just f
  matchOnFocusLost _ = Nothing
  mkOnMouseEntered = ButtonOnMouseEntered
  matchOnMouseEntered (ButtonOnMouseEntered f) = Just f
  matchOnMouseEntered _ = Nothing
  mkOnMouseExited = ButtonOnMouseExited
  matchOnMouseExited (ButtonOnMouseExited f) = Just f
  matchOnMouseExited _ = Nothing
  mkOnMouseDown = ButtonOnMouseDown
  matchOnMouseDown (ButtonOnMouseDown f) = Just f
  matchOnMouseDown _ = Nothing
  mkOnMouseUp = ButtonOnMouseUp
  matchOnMouseUp (ButtonOnMouseUp f) = Just f
  matchOnMouseUp _ = Nothing
  mkOnKeyPressed = ButtonOnKeyPressed
  matchOnKeyPressed (ButtonOnKeyPressed f) = Just f
  matchOnKeyPressed _ = Nothing

instance HasTextConfig (ButtonAttributes e msg) where
  mkText = ButtonText
  matchText (ButtonText t) = Just t
  matchText _ = Nothing

-- | Configuration for 'button', resolved from a @['ButtonAttributes' e msg]@.
data ButtonConfig e msg = ButtonConfig
  { bcfgText      :: Text
  , bcfgOnClicked :: [() -> [Out e msg]]
  }

-- | The 'StyleKey' 'button' resolves its style from unless overridden via
-- 'style'.
buttonStyleKey :: StyleKey e
buttonStyleKey = Class "button"

defaultButtonConfig :: ButtonConfig e msg
defaultButtonConfig = ButtonConfig { bcfgText = "", bcfgOnClicked = [] }

resolveButtonConfig :: [ButtonAttributes e msg] -> ButtonConfig e msg
resolveButtonConfig = foldl' apply defaultButtonConfig
  where
    apply cfg (ButtonText t)      = cfg { bcfgText = t }
    apply cfg (ButtonOnClicked f) = cfg { bcfgOnClicked = bcfgOnClicked cfg ++ [f] }
    apply cfg _                   = cfg

-- | Translates the common capabilities of a 'ButtonAttributes' down to
-- 'ControlAttrs' for 'control' -- @Nothing@ for 'ButtonText', which
-- 'control' has no capability for.
toButtonControlAttr :: ButtonAttributes e msg -> Maybe (ControlAttrs e msg)
toButtonControlAttr (ButtonText _) = Nothing
toButtonControlAttr a              = translateCommon a

-- | A clickable button labelled via 'text'. Fires every 'onClicked' handler
-- -- when activated by a left-click or by pressing Enter while focused.
button :: Ord e => e -> [ButtonAttributes e msg] -> UI e msg ()
button eid attrs = buttonBase eid (mapMaybe toButtonControlAttr attrs) (bcfgOnClicked cfg) bodyContent
  where
    cfg = resolveButtonConfig attrs
    bodyContent = do
      s <- currentStyle
      drawText (styleTextColour s) (styleTextAlign s) (bcfgText cfg)

-- | Implemented by any toggle-style control's own attrs type to say how it
-- carries whether it's currently selected -- lets 'isSelected' work
-- uniformly across every such control's differently-shaped attrs type.
class HasIsSelectedConfig cfg where
  mkIsSelected :: Bool -> cfg

-- | Whether the control is currently selected.
isSelected :: HasIsSelectedConfig cfg => Bool -> cfg
isSelected = mkIsSelected

-- | Implemented by any toggle-style control's own attrs type to say how it
-- carries a reaction to its selected state changing.
class HasSelectedChangedEvents e msg cfg | cfg -> e msg where
  mkOnSelectedChanged :: (Bool -> [Out e msg]) -> cfg

-- | Reacts when activating the control (a click or Enter while focused)
-- moves its selected state to a new value, with the value it changed to.
-- It's up to the reaction to actually store the new value and pass it back
-- in via 'isSelected' next frame.
onSelectedChanged :: HasSelectedChangedEvents e msg cfg => (Bool -> [Out e msg]) -> cfg
onSelectedChanged = mkOnSelectedChanged

-- | Runs @content@ as 'buttonBase', and additionally fires every
-- @onSelectedChangedHandlers@ reaction (only) when activating the control
-- would move its selected state (per @wasSelected@) to a different value
-- than @next@ computes from it -- e.g. @not@ for a control that flips every
-- time, or @const True@ for one that can only ever become selected by
-- being activated, never unselected that way (a radio button, whose
-- deselection instead comes from a sibling being selected). The shape
-- 'toggleButton' and any checkbox\/radio button share.
toggleBase
  :: Ord e
  => (Bool -> Bool)                -- ^ how activating the control changes its selected state
  -> e
  -> [ControlAttrs e msg]          -- ^ this widget's own attrs, already translated
  -> [() -> [Out e msg]]           -- ^ this widget's own resolved 'onClicked' handlers
  -> Bool                          -- ^ whether it was selected (per 'isSelected')
  -> [Bool -> [Out e msg]]         -- ^ this widget's own resolved 'onSelectedChanged' handlers
  -> (Bool -> UI e msg ())         -- ^ content, parameterised by whether it was selected
  -> UI e msg ()
toggleBase next eid controlAttrs onClickedHandlers wasSelected onSelectedChangedHandlers body =
  buttonBase eid (controlAttrs ++ [onClicked derived]) (onClickedHandlers ++ [derived]) (body wasSelected)
  where
    newSelected = next wasSelected
    derived ()
      | newSelected == wasSelected = []
      | otherwise                  = combineHandlers onSelectedChangedHandlers newSelected

-- | 'Blink.Button.toggleButton'\'s own closed attrs type: the same common
-- capabilities as 'ButtonAttributes', plus 'text', 'isSelected', and
-- 'onSelectedChanged'.
data ToggleButtonAttributes e msg
  = ToggleButtonIsFocusable Bool
  | ToggleButtonIsEnabled Bool
  | ToggleButtonStyle (StyleKey e)
  | ToggleButtonTabNavigation NavigationMode
  | ToggleButtonIsArrowNavigationEnabled Bool
  | ToggleButtonOnClicked (() -> [Out e msg])
  | ToggleButtonOnFocusGained (() -> [Out e msg])
  | ToggleButtonOnFocusLost (() -> [Out e msg])
  | ToggleButtonOnMouseEntered (() -> [Out e msg])
  | ToggleButtonOnMouseExited (() -> [Out e msg])
  | ToggleButtonOnMouseDown (() -> [Out e msg])
  | ToggleButtonOnMouseUp (() -> [Out e msg])
  | ToggleButtonOnKeyPressed (KeyEvent -> [Out e msg])
  | ToggleButtonText Text
  | ToggleButtonIsSelected Bool
  | ToggleButtonOnSelectedChanged (Bool -> [Out e msg])

instance HasControlConfig e (ToggleButtonAttributes e msg) where
  mkIsFocusable = ToggleButtonIsFocusable
  matchIsFocusable (ToggleButtonIsFocusable b) = Just b
  matchIsFocusable _ = Nothing
  mkIsEnabled = ToggleButtonIsEnabled
  matchIsEnabled (ToggleButtonIsEnabled b) = Just b
  matchIsEnabled _ = Nothing
  mkStyle = ToggleButtonStyle
  matchStyle (ToggleButtonStyle k) = Just k
  matchStyle _ = Nothing
  mkTabNavigation = ToggleButtonTabNavigation
  matchTabNavigation (ToggleButtonTabNavigation m) = Just m
  matchTabNavigation _ = Nothing
  mkIsArrowNavigationEnabled = ToggleButtonIsArrowNavigationEnabled
  matchIsArrowNavigationEnabled (ToggleButtonIsArrowNavigationEnabled b) = Just b
  matchIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (ToggleButtonAttributes e msg) where
  mkOnClicked = ToggleButtonOnClicked
  matchOnClicked (ToggleButtonOnClicked f) = Just f
  matchOnClicked _ = Nothing
  mkOnFocusGained = ToggleButtonOnFocusGained
  matchOnFocusGained (ToggleButtonOnFocusGained f) = Just f
  matchOnFocusGained _ = Nothing
  mkOnFocusLost = ToggleButtonOnFocusLost
  matchOnFocusLost (ToggleButtonOnFocusLost f) = Just f
  matchOnFocusLost _ = Nothing
  mkOnMouseEntered = ToggleButtonOnMouseEntered
  matchOnMouseEntered (ToggleButtonOnMouseEntered f) = Just f
  matchOnMouseEntered _ = Nothing
  mkOnMouseExited = ToggleButtonOnMouseExited
  matchOnMouseExited (ToggleButtonOnMouseExited f) = Just f
  matchOnMouseExited _ = Nothing
  mkOnMouseDown = ToggleButtonOnMouseDown
  matchOnMouseDown (ToggleButtonOnMouseDown f) = Just f
  matchOnMouseDown _ = Nothing
  mkOnMouseUp = ToggleButtonOnMouseUp
  matchOnMouseUp (ToggleButtonOnMouseUp f) = Just f
  matchOnMouseUp _ = Nothing
  mkOnKeyPressed = ToggleButtonOnKeyPressed
  matchOnKeyPressed (ToggleButtonOnKeyPressed f) = Just f
  matchOnKeyPressed _ = Nothing

instance HasTextConfig (ToggleButtonAttributes e msg) where
  mkText = ToggleButtonText
  matchText (ToggleButtonText t) = Just t
  matchText _ = Nothing

instance HasIsSelectedConfig (ToggleButtonAttributes e msg) where
  mkIsSelected = ToggleButtonIsSelected

instance HasSelectedChangedEvents e msg (ToggleButtonAttributes e msg) where
  mkOnSelectedChanged = ToggleButtonOnSelectedChanged

-- | Configuration for 'toggleButton', resolved from a
-- @['ToggleButtonAttributes' e msg]@.
data ToggleButtonConfig e msg = ToggleButtonConfig
  { tbcfgText              :: Text
  , tbcfgSelected          :: Bool
  , tbcfgOnClicked         :: [() -> [Out e msg]]
  , tbcfgOnSelectedChanged :: [Bool -> [Out e msg]]
  }

-- | The 'StyleKey' 'toggleButton' resolves its style from unless
-- overridden via 'style'.
toggleButtonStyleKey :: StyleKey e
toggleButtonStyleKey = Class "toggleButton"

defaultToggleButtonConfig :: ToggleButtonConfig e msg
defaultToggleButtonConfig = ToggleButtonConfig
  { tbcfgText = "", tbcfgSelected = False, tbcfgOnClicked = [], tbcfgOnSelectedChanged = [] }

resolveToggleButtonConfig :: [ToggleButtonAttributes e msg] -> ToggleButtonConfig e msg
resolveToggleButtonConfig = foldl' apply defaultToggleButtonConfig
  where
    apply cfg (ToggleButtonText t)              = cfg { tbcfgText = t }
    apply cfg (ToggleButtonIsSelected b)        = cfg { tbcfgSelected = b }
    apply cfg (ToggleButtonOnClicked f)         = cfg { tbcfgOnClicked = tbcfgOnClicked cfg ++ [f] }
    apply cfg (ToggleButtonOnSelectedChanged f) = cfg { tbcfgOnSelectedChanged = tbcfgOnSelectedChanged cfg ++ [f] }
    apply cfg _                                 = cfg

toToggleButtonControlAttr :: ToggleButtonAttributes e msg -> Maybe (ControlAttrs e msg)
toToggleButtonControlAttr (ToggleButtonText _)              = Nothing
toToggleButtonControlAttr (ToggleButtonIsSelected _)        = Nothing
toToggleButtonControlAttr (ToggleButtonOnSelectedChanged _) = Nothing
toToggleButtonControlAttr a                                 = translateCommon a

-- | A button labelled via 'text' that tracks an external selected\/unselected
-- state (see 'isSelected') instead of only ever being momentarily pressed,
-- flipping every time it's activated. Drawn in its pressed style while
-- selected, even without being physically pressed, unless disabled.
-- Activated the same way as 'button'; see 'onSelectedChanged' for reacting to it.
--
-- The pressed-while-selected look is always looked up by @eid@ directly
-- (ignoring any 'style' override) -- see the "Pseudo states" entry in
-- IDEAS.md; there's no general way yet for a control to borrow a specific
-- 'Blink.Style.StyleSet' variant from whatever key it actually resolved to.
toggleButton :: Ord e => e -> [ToggleButtonAttributes e msg] -> UI e msg ()
toggleButton eid attrs =
  toggleBase not eid (mapMaybe toToggleButtonControlAttr attrs) (tbcfgOnClicked cfg) (tbcfgSelected cfg) (tbcfgOnSelectedChanged cfg) draw
  where
    cfg = resolveToggleButtonConfig attrs
    draw selected = do
      base <- currentStyle
      s    <- toggleStyle eid base selected
      drawText (styleTextColour s) (styleTextAlign s) (tbcfgText cfg)

-- | The resolved style for a toggle button given its ordinary resolved
-- @base@ style (see 'currentStyle'): selected and enabled forces the
-- pressed variant regardless of hover\/focus.
toggleStyle :: Ord e => e -> Style -> Bool -> UI e msg Style
toggleStyle eid base selected = do
  disabled <- isDisabled
  if disabled || not selected
    then pure base
    else styleSetPressed <$> getStyleSet (ElementId eid)
