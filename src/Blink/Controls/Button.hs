{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , toggleBase
  , toggleButton
  , isSelected
  , onSelectedChanged
  ) where

import Control.Monad (when)

import Blink.Controls.Core
import Blink.Controls.Labelled
import Blink.Input (Key (KeyReturn), KeyEvent (..))
import Blink.Style (Style (..), StyleSet (..))
import Blink.UI (Out, UI, currentStyle, drawText, getStyleSet, isDisabled)

-- * Button

-- | Every capability 'button' (and anything built on 'buttonBase') resolves:
-- the wrapped 'ControlConfig', its caption (see 'Blink.Controls.Labelled'),
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
  overElement attr = Attr (\bc -> bc { bcControl = runAttr (overElement attr) (bcControl bc) })

instance HasControlConfig e msg (ButtonConfig e msg) where
  overControl attr = Attr (\bc -> bc { bcControl = runAttr attr (bcControl bc) })

instance HasLabelledConfig e msg (ButtonConfig e msg) where
  overLabelled attr = Attr (\bc -> bc { bcLabelled = runAttr attr (bcLabelled bc) })

-- | Implemented by any config type that nests a 'ButtonConfig', letting
-- 'onActivated' be applied to it directly.
class HasButtonConfig e msg cfg | cfg -> e msg where
  overButton :: Attr (ButtonConfig e msg) -> Attr cfg

instance HasButtonConfig e msg (ButtonConfig e msg) where
  overButton = id

-- | Reacts when the control is activated: a click, or pressing Enter while
-- it holds focus. The event 'button'\/'toggleButton'\/'Blink.Controls.Checkbox.checkbox'\/
-- 'Blink.Controls.RadioButton.radioButton' actually want callers to bind for "the control was
-- activated" -- see 'onClicked' for the mouse-only, element-level event
-- this is split from.
onActivated :: HasButtonConfig e msg cfg => EventHandler e msg -> Attr cfg
onActivated f = overButton (Attr (\bc -> bc { bcOnActivated = bcOnActivated bc ++ [f] }))

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

-- | A clickable button labelled via 'text'. Fires every 'onActivated'
-- handler when activated by a left-click or by pressing Enter while
-- focused. Always takes focus when clicked -- fixed behaviour, not a
-- default.
button :: Ord e => e -> [Attr (ButtonConfig e msg)] -> UI e msg ()
button eid attrs = do
  let cfg  = resolve defaultButtonConfig attrs
      ctrl = (bcControl cfg) { ccContent = renderLabelledContent (bcLabelled cfg) }
  () <$ buttonBase eid cfg { bcControl = ctrl }

-- * Toggle

-- | Whether the control is currently selected.
isSelected :: Bool -> Attr (ToggleConfig e msg)
isSelected b = Attr (\cfg -> cfg { tgcSelected = b })

-- | Reacts when activating the control (a click or Enter while focused)
-- moves its selected state to a new value, with the value it changed to.
-- It's up to the reaction to actually store the new value and pass it back
-- in via 'isSelected' next frame.
onSelectedChanged :: (Bool -> [Out e msg]) -> Attr (ToggleConfig e msg)
onSelectedChanged f = Attr (\cfg -> cfg { tgcOnSelectedChanged = tgcOnSelectedChanged cfg ++ [f] })

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

instance HasElementConfig e msg (ToggleConfig e msg) where
  overElement attr = Attr (\tc -> tc { tgcButton = runAttr (overElement attr) (tgcButton tc) })

instance HasControlConfig e msg (ToggleConfig e msg) where
  overControl attr = Attr (\tc -> tc { tgcButton = runAttr (overControl attr) (tgcButton tc) })

instance HasLabelledConfig e msg (ToggleConfig e msg) where
  overLabelled attr = Attr (\tc -> tc { tgcButton = runAttr (overLabelled attr) (tgcButton tc) })

instance HasButtonConfig e msg (ToggleConfig e msg) where
  overButton attr = Attr (\tc -> tc { tgcButton = runAttr attr (tgcButton tc) })

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
  r <- buttonBase eid (tgcButton cfg)
  let wasSelected = tgcSelected cfg
      newValue    = tgcNext cfg wasSelected
      changed     = biActivated r && newValue /= wasSelected
  when changed $ runHandlers (tgcOnSelectedChanged cfg) newValue
  pure (ToggleInteraction r (if biActivated r then newValue else wasSelected))

-- | The resolved style for a toggle button given its ordinary resolved
-- @base@ style (see 'currentStyle'): selected and enabled forces the
-- pressed variant regardless of hover\/focus.
--
-- Always looked up by @eid@ directly (ignoring any 'style' override) --
-- see the "Pseudo states" entry in IDEAS.md; there's no general way yet
-- for a control to borrow a specific 'Blink.Style.StyleSet' variant from
-- whatever key it actually resolved to.
toggleStyle :: Ord e => e -> Style -> Bool -> UI e msg Style
toggleStyle eid base selected = do
  disabled <- isDisabled
  if disabled || not selected
    then pure base
    else styleSetPressed <$> getStyleSet (ElementId eid)

-- | A button labelled via 'text' that tracks an external selected\/unselected
-- state (see 'isSelected') instead of only ever being momentarily pressed,
-- flipping every time it's activated. Drawn in its pressed style while
-- selected, even without being physically pressed, unless disabled.
-- Activated the same way as 'button'; see 'onSelectedChanged' for reacting
-- to it.
toggleButton :: Ord e => e -> [Attr (ToggleConfig e msg)] -> UI e msg ()
toggleButton eid attrs = do
  let cfg      = resolve defaultToggleButtonConfig attrs
      btn      = tgcButton cfg
      selected = tgcSelected cfg
      draw = do
        base <- currentStyle
        s    <- toggleStyle eid base selected
        drawText (styleTextColour s) (styleTextAlign s) (lcText (bcLabelled btn))
      ctrl = (bcControl btn) { ccContent = draw }
  () <$ toggleBase eid cfg { tgcNext = not, tgcButton = btn { bcControl = ctrl } }
