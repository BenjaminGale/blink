{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A plain, momentary button: activated by a click or by pressing Enter
-- while focused. See "Blink.Controls.Toggle" for the sibling family of
-- controls that track a selected\/unselected state instead.
--
-- @
-- controlBase --> buttonBase --> button
--                             --> toggleBase  (see "Blink.Controls.Toggle")
-- @
module Blink.Controls.Button
  ( ButtonConfig (..)
  , ButtonInteraction (..)
  , HasButtonConfig (..)
  , defaultButtonConfig
  , buttonStyleKey
  , buttonBase
  , button
  , onActivated
  ) where

import Control.Monad (void, when)

import Blink.Controls.Control
import Blink.Controls.Label
  (HasLabelledConfig (..), LabelledConfig (..), captionElement, defaultLabelledConfig, renderLabelledContent)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Input (Key (KeyReturn), KeyEvent (..))
import Blink.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill, fitContent)
import Blink.UI (UI)
import Blink.UI.Element (Element (..))

-- * Button

-- | Every capability 'button' (and anything built on 'buttonBase') resolves:
-- the wrapped 'ControlConfig', its caption (see 'Blink.Controls.Label'),
-- and its 'onActivated' reactions. No content field and no override slot --
-- 'buttonBase' doesn't decide content, so it needs neither.
data ButtonConfig e msg = ButtonConfig
  { bcControl     :: ControlConfig e msg
  , bcLabelled    :: LabelledConfig e msg
  , bcLayout      :: Layout
  , bcOnActivated :: [EventHandler e msg]
  }

-- | 'defaultControlConfig' (styled via 'buttonStyleKey'), an empty caption,
-- @Layout fill fitContent TopLeft@ (see 'button'), and no 'onActivated'
-- reactions.
defaultButtonConfig :: ButtonConfig e msg
defaultButtonConfig = ButtonConfig
  { bcControl     = defaultControlConfig { ccStyleKey = buttonStyleKey }
  , bcLabelled    = defaultLabelledConfig
  , bcLayout      = Layout fill fitContent TopLeft
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

instance HasLayoutConfig (ButtonConfig e msg) where
  overLayout attr = Attribute (\bc -> bc { bcLayout = runAttribute attr (bcLayout bc) })

-- | Implemented by any config type that nests a 'ButtonConfig', letting
-- 'onActivated' be applied to it directly.
class HasButtonConfig e msg cfg | cfg -> e msg where
  overButton :: Attribute (ButtonConfig e msg) -> Attribute cfg

instance HasButtonConfig e msg (ButtonConfig e msg) where
  overButton = id

-- | Reacts when the control is activated: a click, or pressing Enter while
-- it holds focus. The event 'button'\/'Blink.Controls.Toggle.toggleButton'\/'Blink.Controls.Checkbox.checkbox'\/
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
-- button-like control ('button', 'Blink.Controls.Toggle.toggleButton', and
-- 'Blink.Controls.Checkbox.checkbox'\/'Blink.Controls.RadioButton.radioButton') is
-- built from. Always identified by @eid@, regardless of any 'elementId'
-- a caller passes via @cfg@.
buttonBase :: Ord e => e -> ButtonConfig e msg -> UI e msg (ButtonInteraction e msg)
buttonBase eid cfg = do
  let ctrl = bcControl cfg
  r <- controlBase ctrl { ccElement = (ccElement ctrl) { ecElementId = Just eid } }
  let e         = ciElement r
      enter     = any ((== KeyReturn) . key) (eiKeysPressed e)
      activated = eiClicked e || (eiFocused e && enter)
  when activated $ runHandlers (bcOnActivated cfg) ()
  pure (ButtonInteraction r activated)

-- | A clickable button labelled via 'Blink.Controls.Label.text'. Fires every 'onActivated'
-- handler when activated by a left-click or by pressing Enter while
-- focused. Always takes focus when clicked -- fixed behaviour, not a
-- default. Defaults to filling the width it's given and sizing its height
-- to its own chrome-wrapped caption; override with 'Blink.Layout.Constraints.width'\/'Blink.Layout.Constraints.height'\/'Blink.Layout.Constraints.align'.
button :: Ord e => e -> [Attribute (ButtonConfig e msg)] -> Element e msg
button eid attrs = Element
  { elLayout  = bcLayout cfg
  , elMeasure = measureChrome (ccStyleKey (bcControl cfg)) (captionElement (lcText (bcLabelled cfg)))
  , elRun     = void (buttonBase eid cfg { bcControl = ctrl })
  }
  where
    cfg  = resolve defaultButtonConfig attrs
    ctrl = (bcControl cfg) { ccContent = renderLabelledContent (bcLabelled cfg) }
