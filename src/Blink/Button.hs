{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  ( ButtonConfig
  , button
  , ToggleButtonConfig
  , toggleButton
  , ToggleEvent (..)
  , ToggleConfig (..)
  , HasToggleConfig (..)
  , isSelected
  , onSelectedChanged
  , buttonBase
  , toggleBase
  , text
  , isTabStop
  , isEnabled
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
import Data.Text (Text)

import Blink.Attributes
  ( Attr, ControlConfig (..), FocusOnClick (FocusSelf), HasControlConfig (..), HasTextConfig (..)
  , configAny, defaultControlConfig, configure, isEnabled, fire, isTabStop, onEvent, reactionsTo, text
  )
import Blink.Control (control, getStyle)
import Blink.Element
  ( ElementEvent (..), HasElementEvent (..)
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed, onFocusGained, onFocusLost
  )
import Blink.Input (Key (KeyReturn), KeyEvent (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI (Out, UI, drawText, getInput, getStyleSet, isDisabled, isFocused)

-- | Runs @content@ as a normal interactive control (see 'control'), and
-- additionally fires 'Blink.Element.Clicked' -- alongside a real mouse
-- click -- when Enter is pressed while it holds focus and it isn't
-- disabled. The shape every button-like control ('button', 'toggleButton',
-- and any future checkbox\/radio button) is built from.
--
-- Always takes focus when clicked -- fixed behaviour, not a default, so
-- unlike a plain "Blink.Control" control it can't be redirected elsewhere
-- or turned off.
buttonBase :: (Ord e, HasControlConfig e cfg, HasElementEvent ev) => e -> cfg -> [Attr e ev msg cfg] -> UI e msg () -> UI e msg ()
buttonBase eid cfg attrs content = do
  control eid cfg' attrs content
  focused  <- isFocused eid
  disabled <- isDisabled
  input    <- getInput
  let pressedReturn = any (\ev -> key ev == KeyReturn) (inputKeyEvents input)
  when (not disabled && focused && pressedReturn) $ fire attrs [liftElementEvent Clicked]
  where
    cfg' = setControlConfig ((controlConfig cfg) { ccFocusOnClick = FocusSelf }) cfg

-- | Configuration for 'button', set via 'text'. Defaults to @\"\"@.
data ButtonConfig e = ButtonConfig
  { buttonConfigControl :: ControlConfig e
  , buttonConfigText    :: Text
  }

defaultButtonConfig :: ButtonConfig e
defaultButtonConfig = ButtonConfig { buttonConfigControl = defaultControlConfig, buttonConfigText = "" }

instance HasControlConfig e (ButtonConfig e) where
  controlConfig    = buttonConfigControl
  setControlConfig cc cfg = cfg { buttonConfigControl = cc }

instance HasTextConfig (ButtonConfig e) where
  setText t cfg = cfg { buttonConfigText = t }

-- | A clickable button labelled via 'text'. Fires 'Blink.Element.Clicked'
-- -- handled with 'Blink.Element.onClicked' -- when activated by a
-- left-click or by pressing Enter while focused.
button :: Ord e => e -> [Attr e ElementEvent msg (ButtonConfig e)] -> UI e msg ()
button eid attrs = buttonBase eid cfg attrs $ do
  style <- getStyle eid
  drawText (styleTextColour style) (styleTextAlign style) (buttonConfigText cfg)
  where
    cfg = configure defaultButtonConfig attrs

-- | Fired for a toggle-style control ('toggleButton', or a checkbox\/radio
-- button built the same way): either one of the raw facts every control
-- reports (see 'Blink.Element.ElementEvent'), or 'SelectedChanged' when
-- activating the control moves its selected state to a new value.
data ToggleEvent
  = ToggleRaw ElementEvent
  | SelectedChanged Bool
  deriving (Eq, Show)

instance HasElementEvent ToggleEvent where
  liftElementEvent  = ToggleRaw
  matchElementEvent (ToggleRaw ev) = Just ev
  matchElementEvent _              = Nothing

-- | Whether a toggle-style control is currently selected, set via
-- 'isSelected'. Every such control's own @cfg@ carries one of these,
-- accessed uniformly via 'HasToggleConfig' -- the same pattern
-- 'HasControlConfig' already uses for 'ControlConfig'.
newtype ToggleConfig = ToggleConfig { tcSelected :: Bool }

defaultToggleConfig :: ToggleConfig
defaultToggleConfig = ToggleConfig { tcSelected = False }

-- | Implemented by any toggle-style control's own @cfg@ type to say how it
-- carries a 'ToggleConfig' -- lets 'isSelected' work uniformly across every
-- such control's differently-shaped @cfg@.
class HasToggleConfig cfg where
  toggleConfig    :: cfg -> ToggleConfig
  setToggleConfig :: ToggleConfig -> cfg -> cfg

-- | Whether the control is currently selected.
isSelected :: HasToggleConfig cfg => Bool -> Attr e ev msg cfg
isSelected b = configAny $ \cfg -> setToggleConfig ((toggleConfig cfg) { tcSelected = b }) cfg

-- | Reacts when activating the control (a click or Enter while focused)
-- moves its selected state to a new value, with the value it changed to.
-- It's up to the reaction to actually store the new value and pass it back
-- in via 'isSelected' next frame.
onSelectedChanged :: (Bool -> [Out e msg]) -> Attr e ToggleEvent msg cfg
onSelectedChanged reaction = onEvent $ \ev -> case ev of
  SelectedChanged b -> reaction b
  _                 -> []

-- | Runs @content@ as 'buttonBase', and additionally fires 'SelectedChanged'
-- (only) when activating the control would move its selected state
-- (per 'isSelected') to a different value than @next@ computes from it --
-- e.g. @not@ for a control that flips every time, or @const True@ for one
-- that can only ever become selected by being activated, never unselected
-- that way (a radio button, whose deselection instead comes from a
-- sibling being selected). The shape 'toggleButton' and any checkbox\/
-- radio button share.
toggleBase
  :: (Ord e, HasControlConfig e cfg, HasToggleConfig cfg)
  => (Bool -> Bool) -> e -> cfg -> [Attr e ToggleEvent msg cfg] -> UI e msg () -> UI e msg ()
toggleBase next eid cfg attrs content = buttonBase eid cfg (attrs ++ [derivedReaction]) content
  where
    tc          = toggleConfig cfg
    newSelected = next (tcSelected tc)
    derivedReaction
      | newSelected == tcSelected tc = onClicked (const [])
      | otherwise                    = onClicked (\() -> reactionsTo attrs (SelectedChanged newSelected))

-- | Configuration for 'toggleButton', set via 'text', 'isSelected', and
-- 'onSelectedChanged'. Defaults to no text, not selected, and no reaction.
data ToggleButtonConfig e = ToggleButtonConfig
  { toggleButtonConfigControl :: ControlConfig e
  , toggleButtonConfigToggle  :: ToggleConfig
  , toggleButtonConfigText    :: Text
  }

defaultToggleButtonConfig :: ToggleButtonConfig e
defaultToggleButtonConfig = ToggleButtonConfig
  { toggleButtonConfigControl = defaultControlConfig
  , toggleButtonConfigToggle  = defaultToggleConfig
  , toggleButtonConfigText    = ""
  }

instance HasControlConfig e (ToggleButtonConfig e) where
  controlConfig    = toggleButtonConfigControl
  setControlConfig cc cfg = cfg { toggleButtonConfigControl = cc }

instance HasToggleConfig (ToggleButtonConfig e) where
  toggleConfig    = toggleButtonConfigToggle
  setToggleConfig tc cfg = cfg { toggleButtonConfigToggle = tc }

instance HasTextConfig (ToggleButtonConfig e) where
  setText t cfg = cfg { toggleButtonConfigText = t }

-- | A button labelled via 'text' that tracks an external selected\/unselected
-- state (see 'isSelected') instead of only ever being momentarily pressed,
-- flipping every time it's activated. Drawn in its pressed style while
-- selected, even without being physically pressed, unless disabled.
-- Activated the same way as 'button'; see 'onSelectedChanged' for reacting to it.
toggleButton :: Ord e => e -> [Attr e ToggleEvent msg (ToggleButtonConfig e)] -> UI e msg ()
toggleButton eid attrs = toggleBase not eid cfg attrs $ do
  style <- toggleStyle eid (tcSelected (toggleButtonConfigToggle cfg))
  drawText (styleTextColour style) (styleTextAlign style) (toggleButtonConfigText cfg)
  where
    cfg = configure defaultToggleButtonConfig attrs

-- | The resolved style for a toggle button: its ordinary resolved style
-- (see 'getStyle'), except selected and enabled forces the pressed variant
-- regardless of hover\/focus.
toggleStyle :: Ord e => e -> Bool -> UI e msg Style
toggleStyle eid selected = do
  disabled <- isDisabled
  base     <- getStyle eid
  if disabled || not selected
    then pure base
    else styleSetPressed <$> getStyleSet eid
