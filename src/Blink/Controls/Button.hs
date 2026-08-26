{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Buttons and button-like controls: activated by a click or by pressing
-- Enter while focused.
--
-- = The controls in this family
--
-- 'button' is a plain, momentary button. 'toggleButton',
-- 'Blink.Controls.Checkbox.checkbox', and 'Blink.Controls.RadioButton.radioButton' all track
-- a selected\/unselected state instead: 'toggleButton' flips every time
-- it's clicked; a checkbox does too, drawing a checkmark glyph beside its
-- caption; a radio button can only become selected, never unselected, by
-- being clicked -- it gives up selection when another radio button in the
-- same group is selected instead.
--
-- @
-- controlBase --> buttonBase --> button
--                             --> toggleBase --> toggleButton
--                                             --> checkbox     (see "Blink.Controls.Checkbox")
--                                             --> radioButton  (see "Blink.Controls.RadioButton")
-- @
module Blink.Controls.Button
  ( -- * Button
    ButtonConfig (..)
  , ButtonInteraction (..)
  , HasButtonConfig (..)
  , defaultButtonConfig
  , buttonStyleKey
  , buttonBase
  , button
  , onActivated

    -- * Toggle
  , ToggleConfig (..)
  , ToggleInteraction (..)
  , defaultToggleButtonConfig
  , toggleButtonStyleKey
  , toggleGroup
  , toggleChecked
  , toggleUnchecked
  , toggleBase
  , toggleButton
  , isSelected
  , onSelectedChanged
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Set as Set

import Blink.Controls.Control
import Blink.Controls.Label (HasLabelledConfig (..), LabelledConfig (..), defaultLabelledConfig, renderLabelledContent)
import Blink.Input (Key (KeyReturn), KeyEvent (..))
import Blink.Style (Style (..), VisualState (..))
import Blink.UI (Out, UI, currentStyle, drawText)

-- * Button

-- | Every capability 'button' (and anything built on 'buttonBase') resolves:
-- the wrapped 'ControlConfig', its caption (see 'Blink.Controls.Label'),
-- and its 'onActivated' reactions. No content field and no override slot --
-- 'buttonBase' doesn't decide content, so it needs neither.
data ButtonConfig e msg = ButtonConfig
  { bcControl     :: ControlConfig e msg
  , bcLabelled    :: LabelledConfig e msg
  , bcOnActivated :: [EventHandler e msg]
  }

-- | 'defaultControlConfig' (styled via 'buttonStyleKey'), an empty caption,
-- and no 'onActivated' reactions.
defaultButtonConfig :: ButtonConfig e msg
defaultButtonConfig = ButtonConfig
  { bcControl     = defaultControlConfig { ccStyleKey = buttonStyleKey }
  , bcLabelled    = defaultLabelledConfig
  , bcOnActivated = []
  }

-- | The 'StyleKey' 'button' resolves its style from unless overridden via
-- 'style'.
buttonStyleKey :: StyleKey e
buttonStyleKey = Class "button"

instance HasElementConfig e msg (ButtonConfig e msg) where
  overElement attr = Attribute (\bc -> bc { bcControl = runAttribute (overElement attr) (bcControl bc) })

instance HasControlConfig e msg (ButtonConfig e msg) where
  overControl attr = Attribute (\bc -> bc { bcControl = runAttribute attr (bcControl bc) })

instance HasLabelledConfig e msg (ButtonConfig e msg) where
  overLabelled attr = Attribute (\bc -> bc { bcLabelled = runAttribute attr (bcLabelled bc) })

-- | Implemented by any config type that nests a 'ButtonConfig', letting
-- 'onActivated' be applied to it directly.
class HasButtonConfig e msg cfg | cfg -> e msg where
  overButton :: Attribute (ButtonConfig e msg) -> Attribute cfg

instance HasButtonConfig e msg (ButtonConfig e msg) where
  overButton = id

-- | Reacts when the control is activated: a click, or pressing Enter while
-- it holds focus. The event 'button'\/'toggleButton'\/'Blink.Controls.Checkbox.checkbox'\/
-- 'Blink.Controls.RadioButton.radioButton' actually want callers to bind for "the control was
-- activated" -- see 'onClicked' for the mouse-only, element-level event
-- this is split from.
onActivated :: HasButtonConfig e msg cfg => EventHandler e msg -> Attribute cfg
onActivated f = overButton (Attribute (\bc -> bc { bcOnActivated = bcOnActivated bc ++ [f] }))

-- | What 'buttonBase' reports back: the wrapped control's own
-- 'ControlInteraction', and whether it was activated this frame.
data ButtonInteraction e msg = ButtonInteraction
  { biControl   :: ControlInteraction e msg
  , biActivated :: Bool
  }

-- | Runs @cfg@ as a 'controlBase', and additionally fires every
-- 'onActivated' handler in @cfg@ when activated: by a click, or by
-- pressing Enter while it holds focus and isn't disabled. The shape every
-- button-like control ('button', 'toggleButton', and
-- 'Blink.Controls.Checkbox.checkbox'\/'Blink.Controls.RadioButton.radioButton') is
-- built from.
buttonBase :: Ord e => e -> ButtonConfig e msg -> UI e msg (ButtonInteraction e msg)
buttonBase eid cfg = do
  r <- controlBase eid (bcControl cfg)
  let e         = ciElement r
      enter     = any ((== KeyReturn) . key) (eiKeysPressed e)
      activated = eiClicked e || (eiFocused e && enter)
  when activated $ runHandlers (bcOnActivated cfg) ()
  pure (ButtonInteraction r activated)

-- | A clickable button labelled via 'Blink.Controls.Label.text'. Fires every 'onActivated'
-- handler when activated by a left-click or by pressing Enter while
-- focused. Always takes focus when clicked -- fixed behaviour, not a
-- default.
button :: Ord e => e -> [Attribute (ButtonConfig e msg)] -> UI e msg ()
button eid attrs = do
  let cfg  = resolve defaultButtonConfig attrs
      ctrl = (bcControl cfg) { ccContent = renderLabelledContent (bcLabelled cfg) }
  void (buttonBase eid cfg { bcControl = ctrl })

-- * Toggle

-- | Whether the control is currently selected.
isSelected :: Bool -> Attribute (ToggleConfig e msg)
isSelected b = Attribute (\cfg -> cfg { tgcSelected = b })

-- | Reacts when activating the control (a click or Enter while focused)
-- moves its selected state to a new value, with the value it changed to.
-- It's up to the reaction to actually store the new value and pass it back
-- in via 'isSelected' next frame.
onSelectedChanged :: (Bool -> [Out e msg]) -> Attribute (ToggleConfig e msg)
onSelectedChanged f = Attribute (\cfg -> cfg { tgcOnSelectedChanged = tgcOnSelectedChanged cfg ++ [f] })

-- | Every capability 'toggleButton' (and any checkbox\/radio button) shares:
-- the wrapped 'ButtonConfig', how activating the control changes its
-- selected state (fixed by the concrete widget below, never
-- attr-settable -- @not@ for 'toggleButton'\/checkbox, @const True@ for a
-- radio button), whether it was selected (per 'isSelected'), and its
-- 'onSelectedChanged' reactions.
data ToggleConfig e msg = ToggleConfig
  { tgcButton            :: ButtonConfig e msg
  , tgcNext              :: Bool -> Bool
  , tgcSelected          :: Bool
  , tgcOnSelectedChanged :: [Bool -> [Out e msg]]
  }

-- | 'defaultButtonConfig' (styled via 'toggleButtonStyleKey'), @not@ (a
-- placeholder -- every concrete widget fixes this itself), not selected,
-- and no 'onSelectedChanged' reactions.
defaultToggleButtonConfig :: ToggleConfig e msg
defaultToggleButtonConfig = ToggleConfig
  { tgcButton            = defaultButtonConfig { bcControl = (bcControl defaultButtonConfig) { ccStyleKey = toggleButtonStyleKey } }
  , tgcNext              = not
  , tgcSelected          = False
  , tgcOnSelectedChanged = []
  }

-- | The 'StyleKey' 'toggleButton' resolves its style from unless
-- overridden via 'style'.
toggleButtonStyleKey :: StyleKey e
toggleButtonStyleKey = Class "toggleButton"

-- | The 'VisualState' group name shared by 'toggleChecked'\/
-- 'toggleUnchecked' -- see "Blink.Style"'s module header for why a
-- control defines its own pseudo-states as opaque exported constants
-- rather than letting callers build 'Custom' values themselves.
toggleGroup :: Text
toggleGroup = "Toggle"

-- | The pseudo-state 'toggleBase' puts in 'ccActiveStates' while the
-- control is selected (see 'isSelected') -- a theme registers an
-- override for this on its own 'Blink.Style.StyleSet' (keyed to whatever 'StyleKey'
-- the control actually resolves to, e.g. 'toggleButtonStyleKey') to give
-- it a distinct "selected" look, composed with whatever
-- common\/focus state is also active.
toggleChecked :: VisualState
toggleChecked = Custom toggleGroup "Checked"

-- | The pseudo-state 'toggleBase' puts in 'ccActiveStates' while the
-- control is unselected. Themes typically register no override for this
-- -- the plain base look already reads as "unchecked".
toggleUnchecked :: VisualState
toggleUnchecked = Custom toggleGroup "Unchecked"

instance HasElementConfig e msg (ToggleConfig e msg) where
  overElement attr = Attribute (\tc -> tc { tgcButton = runAttribute (overElement attr) (tgcButton tc) })

instance HasControlConfig e msg (ToggleConfig e msg) where
  overControl attr = Attribute (\tc -> tc { tgcButton = runAttribute (overControl attr) (tgcButton tc) })

instance HasLabelledConfig e msg (ToggleConfig e msg) where
  overLabelled attr = Attribute (\tc -> tc { tgcButton = runAttribute (overLabelled attr) (tgcButton tc) })

instance HasButtonConfig e msg (ToggleConfig e msg) where
  overButton attr = Attribute (\tc -> tc { tgcButton = runAttribute attr (tgcButton tc) })

-- | What 'toggleBase' reports back: the wrapped button's own
-- 'ButtonInteraction', and the selected state after this frame's
-- activation, if any (see 'toggleBase').
data ToggleInteraction e msg = ToggleInteraction
  { tgiButton   :: ButtonInteraction e msg
  , tgiSelected :: Bool
  }

-- | Runs @cfg@ as 'buttonBase', and additionally fires every
-- 'onSelectedChanged' reaction (only) when activating the control would
-- move its selected state (per 'tgcSelected') to a different value than
-- 'tgcNext' computes from it -- e.g. a radio button that's already
-- selected stays selected when clicked again, so it fires nothing. The
-- shape 'toggleButton' and any checkbox\/radio button share.
toggleBase :: Ord e => e -> ToggleConfig e msg -> UI e msg (ToggleInteraction e msg)
toggleBase eid cfg = do
  r <- buttonBase eid (tgcButton cfg')
  let wasSelected = tgcSelected cfg
      newValue    = tgcNext cfg wasSelected
      changed     = biActivated r && newValue /= wasSelected
  when changed $ runHandlers (tgcOnSelectedChanged cfg) newValue
  pure (ToggleInteraction r (if biActivated r then newValue else wasSelected))
  where
    btn  = tgcButton cfg
    ctrl = bcControl btn
    pseudoState = if tgcSelected cfg then toggleChecked else toggleUnchecked
    cfg' = cfg { tgcButton = btn { bcControl = ctrl { ccActiveStates = Set.singleton pseudoState } } }

-- | A button labelled via 'Blink.Controls.Label.text' that tracks an external selected\/unselected
-- state (see 'isSelected') instead of only ever being momentarily pressed,
-- flipping every time it's activated. Drawn with 'toggleChecked' active
-- while selected -- see "Blink.Style" for how a theme gives that a
-- distinct look. Activated the same way as 'button'; see
-- 'onSelectedChanged' for reacting to it.
toggleButton :: Ord e => e -> [Attribute (ToggleConfig e msg)] -> UI e msg ()
toggleButton eid attrs = do
  let cfg  = resolve defaultToggleButtonConfig attrs
      btn  = tgcButton cfg
      draw = do
        s <- currentStyle
        drawText (styleTextColour s) (styleTextAlign s) (lcText (bcLabelled btn))
      ctrl = (bcControl btn) { ccContent = draw }
  void (toggleBase eid cfg { tgcNext = not, tgcButton = btn { bcControl = ctrl } })
