{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A composite, data-driven control: a row (or column) of toggle-shaped
-- widgets built from a list of data, one per item, that never lets more
-- than one be selected at once. The caller never builds the individual
-- widgets itself -- 'toggleButtonGroup' and 'radioButtonGroup' do, from
-- 'items' and 'toggleAttributes' -- the same way a hand-rolled row of
-- 'Blink.View.Controls.RadioButton.radioButton's already derives each
-- one's selected state from a single external value; this just centralizes
-- that pattern instead of it being copied at every call site.
--
-- @
-- control --> toggleGroup --> toggleButtonGroup
--                           --> radioButtonGroup
-- @
module Blink.View.Controls.ToggleGroup
  ( ToggleGroupConfig (..)
  , ToggleGroupPart (..)
  , defaultToggleGroupConfig
  , toggleGroup
  , toggleButtonGroup
  , radioButtonGroup
  , toggleButtonGroupStyleKey
  , radioButtonGroupStyleKey
  , items
  , toggleAttributes
  , groupOrientation
  , itemSpacing
  , selectedItem
  , allowDeselect
  , onSelectionChanged
  ) where

import Control.Monad (void)

import Blink.View.Controls.Control
import Blink.View.Controls.RadioButton (radioButton)
import Blink.View.Controls.ToggleButton (ToggleConfig, isSelected, onSelectedChanged, toggleButton)
import Blink.Geometry (Alignment (TopLeft), Orientation (..))
import Blink.View.Layout.Box (children, hBox, spacing, vBox)
import Blink.View.Layout.Constraints (HasLayoutConfig (..), Layout (..), fill)
import Blink.View (Out)
import Blink.View.Element (Element (..), runElement)

-- | Identifies one part of a 'toggleButtonGroup'\/'radioButtonGroup' for the
-- purpose of minting element ids: the group's own container
-- ('ToggleGroupRoot'), or one of its items, tagged by the item's own data
-- value rather than its position in the list -- so
-- reordering\/inserting\/removing items elsewhere in the list never
-- disturbs another item's hover\/focus\/capture state. Requires distinct
-- item values (see 'items') the same way 'selectedItem' already does, since
-- two equal items would otherwise mint the same id.
data ToggleGroupPart a
  = ToggleGroupRoot
  | ToggleGroupItem a
  deriving (Eq, Ord, Show)

-- | Every capability 'toggleButtonGroup'\/'radioButtonGroup' resolve: the
-- group's own control (style, enabled state, raw mouse\/key events -- never
-- itself a focus target, see 'toggleGroup'), its own size request and item
-- arrangement, the data to build items from and how to configure each
-- one's own toggle-shaped widget, which item (if any) is selected, whether
-- deselecting it is allowed, and its reactions to a selection change.
data ToggleGroupConfig e a msg = ToggleGroupConfig
  { tggControl            :: ControlConfig e msg
  , tggLayout             :: Layout
  , tggOrientation        :: Orientation
  , tggItemSpacing        :: Double
  , tggItems              :: [a]
  , tggToggleAttrs        :: a -> [Attribute (ToggleConfig e msg)]
  , tggSelected           :: Maybe a
  , tggAllowDeselect      :: Bool
  , tggOnSelectionChanged :: [Maybe a -> [Out e msg]]
  }

-- | 'defaultControlConfig' (styled via @styleKey@), filling its parent on
-- both axes, arranged 'Horizontal' with no gap between items, no items, no
-- per-item attrs, nothing selected, deselecting the current selection by
-- clicking it again disallowed, and no 'onSelectionChanged' reactions.
defaultToggleGroupConfig :: StyleKey e -> ToggleGroupConfig e a msg
defaultToggleGroupConfig styleKey = ToggleGroupConfig
  { tggControl            = defaultControlConfig { ccStyleKey = styleKey }
  , tggLayout             = Layout fill fill TopLeft
  , tggOrientation        = Horizontal
  , tggItemSpacing        = 0
  , tggItems              = []
  , tggToggleAttrs        = const []
  , tggSelected           = Nothing
  , tggAllowDeselect      = False
  , tggOnSelectionChanged = []
  }

instance HasControlConfig e msg (ToggleGroupConfig e a msg) where
  overControl attr = Attribute (\c -> c { tggControl = runAttribute (overControl attr) (tggControl c) })

instance HasLayoutConfig (ToggleGroupConfig e a msg) where
  overLayout attr = Attribute (\c -> c { tggLayout = runAttribute attr (tggLayout c) })

-- | The data to build one item from, in order. Defaults to @[]@; a later
-- 'items' attribute replaces an earlier one rather than adding to it.
items :: [a] -> Attribute (ToggleGroupConfig e a msg)
items xs = Attribute (\c -> c { tggItems = xs })

-- | Attributes for the widget built from one item -- exactly the attrs
-- you'd pass to 'Blink.View.Controls.ToggleButton.toggleButton' or
-- 'Blink.View.Controls.RadioButton.radioButton' directly (e.g.
-- 'Blink.View.Controls.Label.text', 'Blink.View.Controls.Control.isEnabled',
-- 'Blink.View.Controls.Control.style', sizing), computed once per item
-- rather than written out by hand for each. Resolved before the group's own
-- 'isSelected'\/'onSelectedChanged' (see 'toggleGroup'), so nothing here
-- can override the selection invariant.
toggleAttributes :: (a -> [Attribute (ToggleConfig e msg)]) -> Attribute (ToggleGroupConfig e a msg)
toggleAttributes f = Attribute (\c -> c { tggToggleAttrs = f })

-- | Arranges items left-to-right ('Horizontal', the default) or top-to-bottom ('Vertical').
groupOrientation :: Orientation -> Attribute (ToggleGroupConfig e a msg)
groupOrientation o = Attribute (\c -> c { tggOrientation = o })

-- | Gap in pixels between consecutive items. Defaults to @0@.
itemSpacing :: Double -> Attribute (ToggleGroupConfig e a msg)
itemSpacing v = Attribute (\c -> c { tggItemSpacing = v })

-- | Which item, if any, is currently selected -- the data that decides
-- which widget lights up. External state the caller owns and re-supplies
-- every frame, the same as 'Blink.View.Controls.ToggleButton.isSelected'
-- for a single toggle. Defaults to 'Nothing' -- every item starts unselected.
selectedItem :: Maybe a -> Attribute (ToggleGroupConfig e a msg)
selectedItem s = Attribute (\c -> c { tggSelected = s })

-- | Whether clicking the already-selected item deselects it (moving the
-- group's selection to 'Nothing') rather than leaving it selected. Defaults
-- to 'False' -- the group then behaves like a set of radio buttons, always
-- keeping exactly one item selected once anything has been picked. Has no
-- observable effect on 'radioButtonGroup': a
-- 'Blink.View.Controls.RadioButton.radioButton' never reports its own
-- boolean moving to 'False' by being clicked again in the first place (see
-- 'toggleGroup'), so there's nothing for this to act on either way.
allowDeselect :: Bool -> Attribute (ToggleGroupConfig e a msg)
allowDeselect b = Attribute (\c -> c { tggAllowDeselect = b })

-- | Reacts when selecting or clearing an item actually changes the group's
-- selection -- see 'toggleGroup' for exactly which clicks fire this.
onSelectionChanged :: (Maybe a -> [Out e msg]) -> Attribute (ToggleGroupConfig e a msg)
onSelectionChanged f = Attribute (\c -> c { tggOnSelectionChanged = tggOnSelectionChanged c ++ [f] })

-- | The 'StyleKey' 'toggleButtonGroup' resolves its own chrome from unless
-- overridden via 'Blink.View.Controls.Control.style'.
toggleButtonGroupStyleKey :: StyleKey e
toggleButtonGroupStyleKey = Class "toggleButtonGroup"

-- | The 'StyleKey' 'radioButtonGroup' resolves its own chrome from unless
-- overridden via 'Blink.View.Controls.Control.style'.
radioButtonGroupStyleKey :: StyleKey e
radioButtonGroupStyleKey = Class "radioButtonGroup"

-- | A row (or column, see 'groupOrientation') of
-- 'Blink.View.Controls.ToggleButton.toggleButton's built from 'items', one
-- per item, that never lets more than one be selected at once: each item's
-- own toggle button is selected exactly when it equals 'selectedItem', the
-- same external-state comparison a hand-rolled row of radio buttons already
-- uses, so only ever one can read as selected regardless of how many items
-- there are.
--
-- Built on the same shared engine as 'radioButtonGroup' -- see 'toggleGroup'
-- for exactly how the selection invariant is enforced, and what each
-- attribute does. The engine is exported directly should a caller want to
-- build a group from some other toggle-shaped widget of their own.
toggleButtonGroup :: (Ord e, Ord a) => (ToggleGroupPart a -> e) -> [Attribute (ToggleGroupConfig e a msg)] -> Element e msg
toggleButtonGroup = toggleGroup toggleButtonGroupStyleKey toggleButton

-- | A row (or column, see 'groupOrientation') of
-- 'Blink.View.Controls.RadioButton.radioButton's built from 'items', one
-- per item, that never lets more than one be selected at once -- see
-- 'toggleButtonGroup' for the flat-button-styled equivalent, and
-- 'toggleGroup' for how both share their selection logic. 'allowDeselect'
-- has no effect here -- see its own docs.
radioButtonGroup :: (Ord e, Ord a) => (ToggleGroupPart a -> e) -> [Attribute (ToggleGroupConfig e a msg)] -> Element e msg
radioButtonGroup = toggleGroup radioButtonGroupStyleKey radioButton

-- | The shared engine behind 'toggleButtonGroup' and 'radioButtonGroup':
-- given @styleKey@ (the group's own chrome) and @widget@ (the toggle-shaped
-- constructor -- 'Blink.View.Controls.ToggleButton.toggleButton' or
-- 'Blink.View.Controls.RadioButton.radioButton' -- to build one item from),
-- arranges one @widget@ per item from 'items', enforcing that never more
-- than one is selected at once.
--
-- Needs no extra logic to enforce that invariant beyond interpreting the
-- boolean @widget@ already reports (via
-- 'Blink.View.Controls.ToggleButton.toggleBase', which only fires when
-- activating a control would actually flip its own value): clicking an
-- unselected item always reports 'True', selecting it -- which, being
-- external state, simultaneously reads every other item back as unselected
-- next frame, with no separate deselection step. Clicking the
-- already-selected item reports 'False' only for a widget that can flip
-- back to unselected on its own (a 'Blink.View.Controls.ToggleButton.toggleButton');
-- the group only acts on that when 'allowDeselect' is set, otherwise it's a
-- no-op. A 'Blink.View.Controls.RadioButton.radioButton' never reports
-- 'False' from being clicked again at all, so it behaves as a plain,
-- never-deselectable radio group regardless of 'allowDeselect'.
--
-- The group itself is a genuine 'Blink.View.Controls.Control.control' --
-- 'Blink.View.Controls.Control.style'-able, disabled as a whole via
-- 'Blink.View.Controls.Control.isEnabled' (which disables every item too,
-- via the ambient 'Blink.View.disableWhen' every control already reacts
-- to), and able to raise its own raw mouse\/key events -- but its own id is
-- never a keyboard focus target ('Blink.View.Controls.Control.NotFocusable'
-- fixed, not attr-settable): Tab moves directly between its items, each
-- independently focusable, exactly as if the group weren't there.
--
-- @tag@ mints every part's element id from a 'ToggleGroupPart': the group's
-- own container id from 'ToggleGroupRoot', and each item's id from
-- 'ToggleGroupItem' applied to the item's own data -- so the caller never
-- writes a per-item id by hand.
toggleGroup
  :: (Ord e, Ord a)
  => StyleKey e
  -> (e -> [Attribute (ToggleConfig e msg)] -> Element e msg)
  -> (ToggleGroupPart a -> e)
  -> [Attribute (ToggleGroupConfig e a msg)]
  -> Element e msg
toggleGroup styleKey widget tag attrs = Element
  { elLayout  = tggLayout cfg
  , elMeasure = measureChrome (ccStyleKey (tggControl cfg)) box
  , elRun     = void (control ccfg)
  }
  where
    cfg = resolve (defaultToggleGroupConfig styleKey) attrs
    box = (if tggOrientation cfg == Horizontal then hBox else vBox)
            [ spacing (tggItemSpacing cfg), children (map toItem (tggItems cfg)) ]
    toItem item = widget (tag (ToggleGroupItem item))
      ( tggToggleAttrs cfg item
      ++ [ isSelected (Just item == tggSelected cfg)
         , onSelectedChanged (onItemToggled cfg item)
         ]
      )
    ccfg = (tggControl cfg)
      { ccElementId   = Just (tag ToggleGroupRoot)
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (runElement box)
      }

-- | Interprets a click on @item@'s own toggle-shaped widget as a change to
-- the group's selection: selecting @item@ when its own boolean would move
-- to 'True', deselecting (moving the group's selection to 'Nothing') when
-- it would move to 'False' and 'tggAllowDeselect' allows it, or reporting
-- nothing -- leaving the group's selection exactly as it was -- otherwise.
onItemToggled :: ToggleGroupConfig e a msg -> a -> Bool -> [Out e msg]
onItemToggled cfg item newlySelected
  | newlySelected        = fire (Just item)
  | tggAllowDeselect cfg = fire Nothing
  | otherwise            = []
  where
    fire selection = concatMap ($ selection) (tggOnSelectionChanged cfg)
