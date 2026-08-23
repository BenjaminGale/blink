{-# LANGUAGE MultiParamTypeClasses #-}
-- | The attrs mechanism: 'Attr', the opaque per-control configuration\/
-- reaction list, and 'fire', which dispatches events against it.
--
-- This is the target design "Blink.Controls" is expected to migrate onto
-- later (removing its own, older copy of this mechanism, which still has a
-- separate @Shared@ constructor for 'Blink.Controls.ControlConfig'). Here,
-- @Attr@ has only two constructors -- @On@ and @Config@.
module Blink.Attributes
  ( Attr (..)
  , configure
  , fire
  , reactionsTo
  , onEvent
  , configAny
  , HasTextConfig (..)
  , text
  ) where

import Data.List (foldl')
import Data.Text (Text)

import Blink.UI

-- | One entry in a control's attrs list — either a reaction to an event
-- ('onEvent' and the combinators built on it) or a change to the control's
-- own @cfg@ ('configAny' and the smart constructors built on it). Opaque:
-- built and consumed only through the functions this module exports.
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

-- | The 'Out's every matching @On@ reaction in the attrs list produces for
-- one event, without running any of them -- for a control that needs to
-- derive a further event of its own from one it's already reacting to
-- (e.g. a toggle control turning its own @Clicked@ into a selected-state
-- change) rather than dispatching immediately via 'fire'.
reactionsTo :: [Attr e ev msg cfg] -> ev -> [Out e msg]
reactionsTo attrs ev = concatMap ($ ev) [h | On h <- attrs]

-- | Raises each event in turn against every @On@ reaction in the attrs list,
-- dispatching the resulting 'Out's (emitting messages, queuing effects).
-- Used by 'Blink.Element.element' to fire its raw events, and (in
-- "Blink.Controls"'s own copy) by every control to fire its own.
fire :: [Attr e ev msg cfg] -> [ev] -> UI e msg ()
fire attrs = mapM_ (mapM_ dispatch . reactionsTo attrs)
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

-- | Implemented by any control's own @cfg@ type to say how it carries
-- displayed text, letting 'text' work uniformly across them.
class HasTextConfig cfg where
  setText :: Text -> cfg -> cfg

-- | Sets the text a control displays — a caption for a label or button, or
-- the current value for a text input. Defaults to @\"\"@ when not given.
text :: HasTextConfig cfg => Text -> Attr e ev msg cfg
text t = configAny (setText t)
