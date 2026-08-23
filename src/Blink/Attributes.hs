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
  , reactionsTo
  , onEvent
  , configAny
  , FocusOnClick (..)
  , NavigationMode (..)
  , ControlConfig (..)
  , defaultControlConfig
  , autoClaimsFocus
  , HasControlConfig (..)
  , isFocusable
  , isEnabled
  , tabNavigation
  , isArrowNavigationEnabled
  , style
  , HasTextConfig (..)
  , text
  ) where

import Data.List (foldl')
import Data.Text (Text)

import Blink.Style (StyleKey (..))
import Blink.UI

-- | What clicking a control does to focus -- set directly on
-- @ccFocusOnClick@ by whatever builds the control's own config, not a
-- public attr (every ready-made widget fixes its own click-to-focus
-- behaviour rather than leaving it caller-configurable).
data FocusOnClick e
  = FocusSelf
    -- ^ The control takes focus itself (the default for interactive controls).
  | FocusTarget e
    -- ^ The control hands focus to a different element instead of taking it
    -- itself — e.g. a checkbox's label redirecting focus onto the checkbox.
  | NoFocus
    -- ^ Clicking the control has no effect on focus at all.
  deriving (Eq, Show)

-- | How a control's children navigate via Tab\/Shift-Tab -- set with
-- 'tabNavigation'.
data NavigationMode
  = Flatten
    -- ^ Not a navigation container: this control's own slot (if it's a tab
    -- stop) and its children all fold into the same Tab sequence as its
    -- siblings, entering and leaving without any special trapping. The
    -- default -- and, for a control with no navigable children at all,
    -- indistinguishable from plain leaf behaviour.
  | Contained
    -- ^ Opens a focus scope for this control's children: Tab\/Shift-Tab
    -- cycle within it forever, never escaping back out that way.
    -- Ctrl+Tab\/Ctrl+Shift+Tab are the only way out, moving this control's
    -- own slot to the next\/previous one at the enclosing level -- and, if
    -- 'ccIsArrowNavigationEnabled' is set, the arrow keys also cycle within the
    -- scope, same as Tab\/Shift-Tab.
  deriving (Eq, Show)

-- | Configuration shared by every control, regardless of that control's own
-- @cfg@: whether Tab lands on it ('ccIsFocusable'), what clicking it does to
-- focus ('ccFocusOnClick'), whether it responds to input at all
-- ('ccIsEnabled'), how its children navigate via Tab ('ccTabNavigation'), and
-- (only meaningful when 'ccTabNavigation' is 'Contained') whether the
-- arrow keys also cycle within that same scope ('ccIsArrowNavigationEnabled').
-- Every control's own @cfg@ carries one of these, accessed uniformly via
-- 'HasControlConfig'.
data ControlConfig e = ControlConfig
  { ccIsFocusable          :: Bool
  , ccFocusOnClick         :: FocusOnClick e
  , ccIsEnabled            :: Bool
  , ccTabNavigation        :: NavigationMode
  , ccIsArrowNavigationEnabled :: Bool
  , ccStyleKey             :: StyleKey e
  }

-- | The default 'ControlConfig': focusable and takes focus on click, not a
-- navigation container ('Flatten'), styled via @key@ unless overridden by
-- 'style' -- what a plain interactive control wants unless it overrides one
-- or more via 'isFocusable' \/ 'isEnabled' \/ 'tabNavigation' \/
-- 'isArrowNavigationEnabled' (or, for @ccFocusOnClick@, directly on its own
-- config). @key@ is normally a 'Class' named after the control being built,
-- e.g. @Class \"button\"@.
defaultControlConfig :: StyleKey e -> ControlConfig e
defaultControlConfig key = ControlConfig
  { ccIsFocusable          = True
  , ccFocusOnClick         = FocusSelf
  , ccIsEnabled            = True
  , ccTabNavigation        = Flatten
  , ccIsArrowNavigationEnabled = False
  , ccStyleKey             = key
  }

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all ('ccIsFocusable')
-- and configured to take focus itself on click ('ccFocusOnClick').
autoClaimsFocus :: Eq e => ControlConfig e -> Bool
autoClaimsFocus cc = ccIsFocusable cc && ccFocusOnClick cc == FocusSelf

-- | Implemented by any control's own @cfg@ type to say how it carries a
-- 'ControlConfig' -- lets 'isFocusable' and friends work uniformly across
-- every control's differently-shaped @cfg@, the same pattern
-- 'Blink.Controls.HasTextConfig' already uses for 'Blink.Controls.text'.
class HasControlConfig e cfg | cfg -> e where
  controlConfig    :: cfg -> ControlConfig e
  setControlConfig :: ControlConfig e -> cfg -> cfg

-- | One entry in a control's attrs list — either a reaction to an event
-- ('onEvent' and the combinators built on it) or a change to the control's
-- own @cfg@ ('configAny' and the smart constructors built on it, including
-- 'isFocusable'). Opaque: built and consumed only through the functions this
-- module exports.
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

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both.
isFocusable :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isFocusable b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsFocusable = b }) cfg

-- | Whether the control responds to input at all. A disabled control still
-- renders (in its disabled style) but ignores hover, clicks, key presses,
-- and focus, and is skipped by Tab\/Shift-Tab. Defaults to 'True'.
isEnabled :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isEnabled b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsEnabled = b }) cfg

-- | How this control's children navigate via Tab\/Shift-Tab (and, since
-- they ride along with the same scope, Ctrl+Tab\/Ctrl+Shift+Tab) -- see
-- 'NavigationMode'. Defaults to 'Flatten'.
tabNavigation :: HasControlConfig e cfg => NavigationMode -> Attr e ev msg cfg
tabNavigation m = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccTabNavigation = m }) cfg

-- | Whether the arrow keys also cycle within this control's focus scope,
-- the same way Tab\/Shift-Tab do. Only meaningful when 'tabNavigation' is
-- 'Contained'. Defaults to 'False'.
isArrowNavigationEnabled :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isArrowNavigationEnabled b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsArrowNavigationEnabled = b }) cfg

-- | Which 'StyleKey' this control resolves its style from. Defaults to a
-- 'Class' named after the control; pass 'ElementId' to theme this one
-- instance differently, or a different 'Class' to group it with others.
style :: HasControlConfig e cfg => StyleKey e -> Attr e ev msg cfg
style k = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccStyleKey = k }) cfg

-- | Implemented by any control's own @cfg@ type to say how it carries
-- displayed text, letting 'text' work uniformly across them (same pattern
-- 'HasControlConfig' already uses for 'isFocusable').
class HasTextConfig cfg where
  setText :: Text -> cfg -> cfg

-- | Sets the text a control displays — a caption for a label or button, or
-- the current value for a text input. Defaults to @\"\"@ when not given.
text :: HasTextConfig cfg => Text -> Attr e ev msg cfg
text t = configAny (setText t)
