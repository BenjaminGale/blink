{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The attrs mechanism: 'Attr', the opaque per-control configuration\/
-- reaction list, and 'fire', which dispatches events against it.
--
-- This is the target design "Blink.Controls" is expected to migrate onto
-- later (removing its own, older copy of this mechanism, which still has a
-- separate @Shared@ constructor for 'Blink.Controls.ControlConfig'). Here,
-- @Attr@ has only two constructors -- @On@ and @Config@ -- and
-- 'ControlConfig' is just another @cfg@ a control's own config type opts
-- into carrying, via 'HasControlConfig', rather than a special third case
-- 'Attr' has to know about. "Blink.Element" builds @element@ (taking
-- @[Attr e ev msg cfg]@, matching @Blink.Controls.control@'s shape) on top
-- of this without ever needing 'HasControlConfig' at all, since
-- 'Blink.Element.element' only ever fires events (via 'fire', which only
-- reads @On@) and never resolves a @cfg@.
module Blink.Attributes
  ( Attr (..)
  , configure
  , fire
  , onEvent
  , configAny
  , FocusOnClick (..)
  , ControlConfig (..)
  , defaultControlConfig
  , autoClaimsFocus
  , HasControlConfig (..)
  , isTabStop
  , focusOnClick
  , HasTextConfig (..)
  , text
  ) where

import Data.List (foldl')
import Data.Text (Text)

import Blink.UI

-- | What clicking a control does to focus, set with 'isTabStop' \/
-- 'focusOnClick'.
data FocusOnClick e
  = FocusSelf
    -- ^ The control takes focus itself (the default for interactive controls).
  | FocusTarget e
    -- ^ The control hands focus to a different element instead of taking it
    -- itself — e.g. a checkbox's label redirecting focus onto the checkbox.
  | NoFocus
    -- ^ Clicking the control has no effect on focus at all.
  deriving (Eq, Show)

-- | Configuration shared by every control, regardless of that control's own
-- @cfg@: whether Tab lands on it ('ccIsTabStop') and what clicking it does to
-- focus ('ccFocusOnClick'). Every control's own @cfg@ carries one of these,
-- accessed uniformly via 'HasControlConfig'.
data ControlConfig e = ControlConfig
  { ccIsTabStop      :: Bool
  , ccFocusOnClick :: FocusOnClick e
  }

-- | The default 'ControlConfig': a tab stop that takes focus on click --
-- what a plain interactive control wants unless it overrides one or both
-- via 'isTabStop' \/ 'focusOnClick'.
defaultControlConfig :: ControlConfig e
defaultControlConfig = ControlConfig { ccIsTabStop = True, ccFocusOnClick = FocusSelf }

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all ('ccIsTabStop')
-- and configured to take focus itself on click ('ccFocusOnClick').
autoClaimsFocus :: Eq e => ControlConfig e -> Bool
autoClaimsFocus cc = ccIsTabStop cc && ccFocusOnClick cc == FocusSelf

-- | Implemented by any control's own @cfg@ type to say how it carries a
-- 'ControlConfig' -- lets 'isTabStop' \/ 'focusOnClick' work uniformly across
-- every control's differently-shaped @cfg@, the same pattern
-- 'Blink.Controls.HasTextConfig' already uses for 'Blink.Controls.text'.
class HasControlConfig e cfg | cfg -> e where
  controlConfig    :: cfg -> ControlConfig e
  setControlConfig :: ControlConfig e -> cfg -> cfg

-- | One entry in a control's attrs list — either a reaction to an event
-- ('onEvent' and the combinators built on it) or a change to the control's
-- own @cfg@ ('configAny' and the smart constructors built on it, including
-- 'isTabStop' \/ 'focusOnClick'). Opaque: built and consumed only through the
-- functions this module exports.
data Attr e ev msg cfg
  = On (ev -> [Out e msg])
  | Config (cfg -> cfg)

-- | Resolves a control's final @cfg@ by folding every @Config@ attr in the
-- list over the default, left to right — so later attrs override earlier
-- ones that touch the same field.
configure :: cfg -> [Attr e ev msg cfg] -> cfg
configure = foldl' apply
  where
    apply cfg (Config f) = f cfg
    apply cfg _          = cfg

-- | Raises each event in turn against every @On@ reaction in the attrs list,
-- dispatching the resulting 'Out's (emitting messages, queuing effects).
-- Used by 'Blink.Element.element' to fire its raw events, and (in
-- "Blink.Controls"'s own copy) by every control to fire its own.
fire :: [Attr e ev msg cfg] -> [ev] -> UI e msg ()
fire attrs evs = mapM_ (\ev -> mapM_ dispatch (concatMap ($ ev) [h | On h <- attrs])) evs
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | The raw escape hatch: react to an event with an arbitrary function to
-- 'Out's.
onEvent :: (ev -> [Out e msg]) -> Attr e ev msg cfg
onEvent = On

-- | The raw escape hatch for changing a control's own @cfg@.
configAny :: (cfg -> cfg) -> Attr e ev msg cfg
configAny = Config

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both.
isTabStop :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isTabStop b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsTabStop = b }) cfg

-- | What clicking this control does to focus — see 'FocusOnClick'.
focusOnClick :: HasControlConfig e cfg => FocusOnClick e -> Attr e ev msg cfg
focusOnClick foc = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccFocusOnClick = foc }) cfg

-- | Implemented by any control's own @cfg@ type to say how it carries
-- displayed text, letting 'text' work uniformly across them (same pattern
-- 'HasControlConfig' already uses for 'isTabStop' \/ 'focusOnClick').
class HasTextConfig cfg where
  setText :: Text -> cfg -> cfg

-- | Sets the text a control displays — a caption for a label or button, or
-- the current value for a text input. Defaults to @\"\"@ when not given.
text :: HasTextConfig cfg => Text -> Attr e ev msg cfg
text t = configAny (setText t)
