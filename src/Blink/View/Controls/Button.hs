{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A plain, momentary button: activated by a click or by pressing Enter
-- while focused. See "Blink.View.Controls.ToggleButton" for the sibling family of
-- controls that track a selected\/unselected state instead.
--
-- @
-- control --> buttonBase --> button
--                             --> toggleBase  (see "Blink.View.Controls.ToggleButton")
--                             --> repeatButton  (see "Blink.View.Controls.RepeatButton")
-- @
module Blink.View.Controls.Button
  ( ButtonConfig (..)
  , ButtonActivation (..)
  , ButtonInteraction (..)
  , HasButtonConfig (..)
  , defaultButtonConfig
  , buttonStyleKey
  , buttonBase
  , button
  , onActivated
  , activation
  ) where

import Control.Monad (void, when)

import Blink.View.Controls.Control
import Blink.View.Controls.Label
  (HasLabelledConfig (..), LabelledConfig (..), captionElement, defaultLabelledConfig, renderLabelledContent)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Input (Key (KeyReturn), KeyEvent (..))
import Blink.View.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill, fitContent)
import Blink.View (View)
import Blink.View.Element (Element (..))

-- * Button

-- | Which raw event(s) count as this control being "activated" -- set via
-- 'activation'. 'buttonBase' is the only place that reads it.
data ButtonActivation
  = ActivateOnClick
    -- ^ The default: a completed click (a release back within bounds, or --
    -- per 'MouseActivation' -- any release while still holding capture), or
    -- pressing Enter while focused. An Enter held long enough to trigger
    -- the platform's own keyboard auto-repeat activates only once -- a
    -- repeat 'Blink.Input.keyRepeat' event is never treated as a fresh
    -- activation, since a single click or a single logical key press is
    -- the whole interaction.
  | ActivateOnPress
    -- ^ Activates the instant the mouse goes down within bounds, rather
    -- than waiting for a release -- for a control where holding is itself
    -- part of the interaction (see
    -- 'Blink.View.Controls.RepeatButton.repeatButton'). For the same reason,
    -- Enter's own platform auto-repeat activates it again on every repeat,
    -- unlike 'ActivateOnClick' -- holding Enter is this control's keyboard
    -- equivalent of holding the mouse down, so it repeats too, just at
    -- whatever cadence the platform's keyboard auto-repeat itself uses
    -- rather than a cadence 'Blink.View.Controls.RepeatButton.repeatButton'
    -- controls (there's no continuous
    -- "is this key still down" state to compute one from, unlike the
    -- mouse's).
  deriving (Eq, Show)

-- | Every capability 'button' (and anything built on 'buttonBase') resolves:
-- the wrapped 'ControlConfig', its caption (see 'Blink.View.Controls.Label'),
-- what counts as activating it, and its 'onActivated' reactions. No content
-- field and no override slot -- 'buttonBase' doesn't decide content, so it
-- needs neither.
data ButtonConfig e msg = ButtonConfig
  { bcControl     :: ControlConfig e msg
  , bcLabelled    :: LabelledConfig e msg
  , bcLayout      :: Layout
  , bcActivation  :: ButtonActivation
  , bcOnActivated :: [EventHandler e msg]
  }

-- | 'defaultControlConfig' (styled via 'buttonStyleKey'), an empty caption,
-- @Layout fill fitContent TopLeft@ (see 'button'), 'ActivateOnClick', and no
-- 'onActivated' reactions.
defaultButtonConfig :: ButtonConfig e msg
defaultButtonConfig = ButtonConfig
  { bcControl     = defaultControlConfig { ccStyleKey = buttonStyleKey }
  , bcLabelled    = defaultLabelledConfig
  , bcLayout      = Layout fill fitContent TopLeft
  , bcActivation  = ActivateOnClick
  , bcOnActivated = []
  }

-- | The 'StyleKey' 'button' resolves its style from unless overridden via
-- 'style'.
buttonStyleKey :: StyleKey e
buttonStyleKey = Class "button"

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
-- it holds focus. The event 'button'\/'Blink.View.Controls.ToggleButton.toggleButton'\/'Blink.View.Controls.Checkbox.checkbox'\/
-- 'Blink.View.Controls.RadioButton.radioButton' actually want callers to bind for "the control was
-- activated" -- see 'onClicked' for the mouse-only, element-level event
-- this is split from.
onActivated :: HasButtonConfig e msg cfg => EventHandler e msg -> Attribute cfg
onActivated f = overButton (Attribute (\bc -> bc { bcOnActivated = bcOnActivated bc ++ [f] }))

-- | Which raw event counts as activating the control -- see
-- 'ButtonActivation'. Defaults to 'ActivateOnClick'.
activation :: HasButtonConfig e msg cfg => ButtonActivation -> Attribute cfg
activation a = overButton (Attribute (\bc -> bc { bcActivation = a }))

-- | What 'buttonBase' reports back: the wrapped control's own
-- 'ControlInteraction', and whether it was activated this frame.
data ButtonInteraction e msg = ButtonInteraction
  { biControl   :: ControlInteraction e msg
  , biActivated :: Bool
  }

-- | Runs @cfg@ as a 'control', and additionally fires every
-- 'onActivated' handler in @cfg@ when activated: by a click, or by
-- pressing Enter while it holds focus and isn't disabled. The shape every
-- button-like control ('button', 'Blink.View.Controls.ToggleButton.toggleButton', and
-- 'Blink.View.Controls.Checkbox.checkbox'\/'Blink.View.Controls.RadioButton.radioButton') is
-- built from. Always identified by @eid@, regardless of any 'elementId'
-- a caller passes via @cfg@.
buttonBase :: Ord e => e -> ButtonConfig e msg -> View e msg (ButtonInteraction e msg)
buttonBase eid cfg = do
  let ctrl = bcControl cfg
  r <- control ctrl { ccElementId = Just eid }
  let e         = r
      isEnter ev = key ev == KeyReturn && case bcActivation cfg of
        ActivateOnClick -> not (keyRepeat ev)
        ActivateOnPress -> True
      enter     = any isEnter (ciKeysPressed e)
      mouseHit  = case bcActivation cfg of
        ActivateOnClick -> ciClicked e
        ActivateOnPress -> ciMouseDown e
      activated = mouseHit || (ciFocused e && enter)
  when activated $ runHandlers (bcOnActivated cfg) ()
  pure (ButtonInteraction r activated)

-- | A clickable button labelled via 'Blink.View.Controls.Label.text'. Fires every 'onActivated'
-- handler when activated by a left-click or by pressing Enter while
-- focused. Always takes focus when clicked -- fixed behaviour, not a
-- default. Defaults to filling the width it's given and sizing its height
-- to its own chrome-wrapped caption; override with 'Blink.View.Layout.Constraints.width'\/'Blink.View.Layout.Constraints.height'\/'Blink.View.Layout.Constraints.align'.
button :: Ord e => e -> [Attribute (ButtonConfig e msg)] -> Element e msg
button eid attrs = Element
  { elLayout  = bcLayout cfg
  , elMeasure = measureChrome (ccStyleKey (bcControl cfg)) (captionElement (lcText (bcLabelled cfg)))
  , elRun     = void (buttonBase eid cfg { bcControl = ctrl })
  }
  where
    cfg  = resolve defaultButtonConfig attrs
    ctrl = (bcControl cfg) { ccContent = const (renderLabelledContent (bcLabelled cfg)) }
