{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Module: Blink.ItemsControl

'itemsLayout' is a primitive building block, not a control: it has no
element id and draws no chrome, the same way 'Blink.Controls.virtualContent'
doesn't -- it just renders a list of plain data values via a
caller-supplied template, stacked horizontally or vertically. It exists to
be composed inside real controls, the way 'Blink.Controls.virtualContent'
is composed inside 'Blink.Controls.listBox'.

'selectionControl' is such a composite: it layers a single selected item
over 'itemsLayout', resolving 'items' and a 'SelectedItem' choice into a
per-item 'SelectionState', handing each item to the caller's
'itemContainer' template, and detecting clicks on items so it can report
which one was activated via 'onSelect'. It \"is\" a control in the sense
this codebase uses the word -- it has an id (one per item, via its tagging
function) and behaviour (click detection) -- but still draws no chrome of
its own; that stays the parent's decision, same as 'itemsLayout'.

Neither control owns interactive focus over its items -- a caller that
wants keyboard navigation or a fully interactive item builds it into its
own template using its own element ids, the same way any composed
'Blink.UI.UI' content does.
-}
module Blink.ItemsControl
  ( -- * Shared: items and panel
    -- | 'items', 'itemsPanel', and 'orientation' are set the same way on
    -- both 'itemsLayout' and 'selectionControl' -- see 'HasItemsConfig',
    -- 'HasItemsPanelConfig', and 'Blink.Controls.HasOrientationConfig'
    -- (re-exported from "Blink.Controls" so one import of this module is
    -- enough to configure either control). 'orientation' picks
    -- 'Blink.Layout.vBox' vs 'Blink.Layout.hBox'.
    HasItemsConfig (..)
  , HasItemsPanelConfig (..)
  , HasOrientationConfig (..)
  , Orientation (..)
  , items
  , itemsPanel
  , orientation

    -- * ItemsLayout
    -- | A primitive: renders a list of data via a template, with no id,
    -- chrome, selection, or interactivity of its own.
  , ItemTemplate
  , ItemsLayoutConfig
  , itemTemplate
  , itemsLayout

    -- * SelectionControl
    -- | Layers a single selected item over 'itemsLayout': resolves
    -- 'selected'\/'selectedIndex' into a per-item 'SelectionState', and
    -- detects clicks on items to report which one was activated.
  , SelectionState (..)
  , SelectedItem (..)
  , SelectionItemTemplate
  , SelectionEvent (..)
  , SelectionConfig
  , itemContainer
  , selected
  , selectedIndex
  , onSelect
  , selectionControl
  ) where

import Control.Monad (when)
import Data.Void (Void)

import Blink.Controls (Attr, HasOrientationConfig (..), configAny, configure, fire, isClickedOver, onEvent, orientation)
import Blink.Geometry (Alignment (..), Orientation (..))
import Blink.Layout (BoxConfig, Layout (..), Length (..), defaultBoxConfig, hBox, vBox)
import Blink.UI (Out, UI)

-- Shared: items and panel -------------------------------------------------

-- | Lets 'items' work across every config with a plain data-item list --
-- currently the configs behind 'itemsLayout' and 'selectionControl'.
class HasItemsConfig cfg a where
  setItems :: [a] -> cfg -> cfg

-- | Sets the raw data items, one per element, in order. Defaults to @[]@.
items :: HasItemsConfig cfg a => [a] -> Attr e ev msg cfg
items xs = configAny (setItems xs)

-- | Lets 'itemsPanel' work across every config with a box layout -- see
-- 'Blink.Layout.BoxConfig'. Combined with 'orientation' (to
-- choose 'Blink.Layout.vBox' vs 'Blink.Layout.hBox'), this is the whole of
-- how items are arranged; per-item sizing is a separate, per-item concern
-- -- see 'ItemTemplate'.
class HasItemsPanelConfig cfg where
  setItemsPanel :: BoxConfig -> cfg -> cfg

-- | Sets spacing\/margin\/alignment\/fill-cross for the item arrangement.
-- Defaults to 'Blink.Layout.defaultBoxConfig'.
itemsPanel :: HasItemsPanelConfig cfg => BoxConfig -> Attr e ev msg cfg
itemsPanel p = configAny (setItemsPanel p)

-- ItemsLayout ---------------------------------------------------------

-- | Renders one item of an 'itemsLayout', given its index and value, and
-- the 'Layout' it should occupy -- e.g. a fixed main-axis size for a
-- uniform row height, or 'Fill' to share space equally with the other
-- items. 'itemsPanel'\/'orientation' still govern the
-- overall arrangement and the cross axis (stretched to 'Fill' when
-- @'Blink.Layout.boxFillCross' = 'True'@, the default).
type ItemTemplate e msg a = Int -> a -> (Layout, UI e msg ())

-- | Configuration for 'itemsLayout', set via 'items', 'itemTemplate',
-- 'itemsPanel', and 'orientation'. Defaults to no items, a
-- blank template, and a vertical stack.
data ItemsLayoutConfig e msg a = ItemsLayoutConfig
  { itemsLayoutConfigItems       :: [a]
  , itemsLayoutConfigTemplate    :: ItemTemplate e msg a
  , itemsLayoutConfigOrientation :: Orientation
  , itemsLayoutConfigBoxConfig   :: BoxConfig
  }

defaultItemsLayoutConfig :: ItemsLayoutConfig e msg a
defaultItemsLayoutConfig = ItemsLayoutConfig
  { itemsLayoutConfigItems       = []
  , itemsLayoutConfigTemplate    = \_ _ -> (Layout Fill Fill TopLeft, pure ())
  , itemsLayoutConfigOrientation = Vertical
  , itemsLayoutConfigBoxConfig   = defaultBoxConfig
  }

instance HasItemsConfig (ItemsLayoutConfig e msg a) a where
  setItems xs cfg = cfg { itemsLayoutConfigItems = xs }

instance HasItemsPanelConfig (ItemsLayoutConfig e msg a) where
  setItemsPanel p cfg = cfg { itemsLayoutConfigBoxConfig = p }

instance HasOrientationConfig (ItemsLayoutConfig e msg a) where
  setOrientation o cfg = cfg { itemsLayoutConfigOrientation = o }

-- | Sets how each data item is rendered -- the DataTemplate. Defaults to
-- rendering nothing.
itemTemplate :: ItemTemplate e msg a -> Attr e ev msg (ItemsLayoutConfig e msg a)
itemTemplate f = configAny $ \cfg -> cfg { itemsLayoutConfigTemplate = f }

-- | Renders each item of 'items' via 'itemTemplate', stacked according to
-- 'orientation' and 'itemsPanel'. A primitive, not a
-- control -- see the module header.
--
-- @
-- itemsLayout
--   [ items [Small, Medium, Large]
--   , itemTemplate $ \\_ sz -> (Layout Fill Fill TopLeft, drawText black AlignLeft (describe sz))
--   ]
-- @
itemsLayout :: [Attr e Void msg (ItemsLayoutConfig e msg a)] -> UI e msg ()
itemsLayout attrs = do
  let cfg     = configure defaultItemsLayoutConfig attrs
      arrange = case itemsLayoutConfigOrientation cfg of
        Horizontal -> hBox
        Vertical   -> vBox
  arrange (itemsLayoutConfigBoxConfig cfg)
    [ itemsLayoutConfigTemplate cfg idx item
    | (idx, item) <- zip [0 ..] (itemsLayoutConfigItems cfg)
    ]

-- SelectionControl -------------------------------------------------------

-- | Whether an item is currently the selected one.
data SelectionState = Selected | Unselected
  deriving (Eq, Show)

-- | Which item, if any, is selected -- by value, by position, or none. Set
-- via 'selected' or 'selectedIndex'.
data SelectedItem a = None | Item a | ItemAtIndex Int
  deriving (Eq, Show)

-- | Renders one item of a 'selectionControl': its element id (used only
-- for click detection -- items never take focus), current 'SelectionState',
-- and value -- returning the 'Layout' it should occupy, same as
-- 'ItemTemplate'.
type SelectionItemTemplate e msg a = e -> SelectionState -> a -> (Layout, UI e msg ())

-- | Fired when a click lands on an item, carrying its index and value.
data SelectionEvent a = Activated Int a
  deriving (Eq, Show)

-- | Runs a reaction with the activated item's index and value on every
-- 'Activated'.
onSelect :: (Int -> a -> [Out e msg]) -> Attr e (SelectionEvent a) msg cfg
onSelect reaction = onEvent $ \ev -> case ev of
  Activated idx val -> reaction idx val

-- | Configuration for 'selectionControl', set via 'items', 'itemsPanel',
-- 'orientation', 'itemContainer', and
-- 'selected'\/'selectedIndex'. Defaults to no items, nothing selected, a
-- blank template, and a vertical stack.
data SelectionConfig e msg a = SelectionConfig
  { selectionConfigItems       :: [a]
  , selectionConfigSelection   :: SelectedItem a
  , selectionConfigTemplate    :: SelectionItemTemplate e msg a
  , selectionConfigOrientation :: Orientation
  , selectionConfigBoxConfig   :: BoxConfig
  }

defaultSelectionConfig :: SelectionConfig e msg a
defaultSelectionConfig = SelectionConfig
  { selectionConfigItems       = []
  , selectionConfigSelection   = None
  , selectionConfigTemplate    = \_ _ _ -> (Layout Fill Fill TopLeft, pure ())
  , selectionConfigOrientation = Vertical
  , selectionConfigBoxConfig   = defaultBoxConfig
  }

instance HasItemsConfig (SelectionConfig e msg a) a where
  setItems xs cfg = cfg { selectionConfigItems = xs }

instance HasItemsPanelConfig (SelectionConfig e msg a) where
  setItemsPanel p cfg = cfg { selectionConfigBoxConfig = p }

instance HasOrientationConfig (SelectionConfig e msg a) where
  setOrientation o cfg = cfg { selectionConfigOrientation = o }

-- | Sets how each item is rendered, given its element id, 'SelectionState',
-- and value -- see 'SelectionItemTemplate'. Defaults to rendering nothing.
itemContainer :: SelectionItemTemplate e msg a -> Attr e ev msg (SelectionConfig e msg a)
itemContainer f = configAny $ \cfg -> cfg { selectionConfigTemplate = f }

-- | Selects by value: an item is 'Selected' when it equals @v@. Defaults
-- to 'None'.
selected :: a -> Attr e ev msg (SelectionConfig e msg a)
selected v = configAny $ \cfg -> cfg { selectionConfigSelection = Item v }

-- | Selects by position: the item at index @i@ is 'Selected'. Defaults to
-- 'None'.
selectedIndex :: Int -> Attr e ev msg (SelectionConfig e msg a)
selectedIndex i = configAny $ \cfg -> cfg { selectionConfigSelection = ItemAtIndex i }

-- | Renders 'items' via 'itemContainer', each resolved against 'selected'
-- \/'selectedIndex' into a 'SelectionState', arranged by
-- 'orientation'\/'itemsPanel' (built on 'itemsLayout').
-- Detects a click on any item -- no focus, no keyboard navigation -- and
-- fires 'Activated' with that item's index and value via 'onSelect';
-- changing the selection is the caller's own responsibility, by feeding a
-- new 'selected'\/'selectedIndex' back in from its reaction. Draws no
-- chrome of its own -- see the module header.
--
-- @
-- data Element = SizeItem Int
--
-- selectionControl SizeItem
--   [ items [Small, Medium, Large]
--   , selected (currentSize model)
--   , onSelect (\\_ sz -> post (SetSize sz))
--   , itemContainer $ \\_ st sz ->
--       ( Layout Fill Fill TopLeft
--       , drawText black AlignLeft ((if st == Selected then "> " else "") \<\> describe sz)
--       )
--   ]
-- @
selectionControl
  :: (Ord e, Eq a)
  => (Int -> e)
  -> [Attr e (SelectionEvent a) msg (SelectionConfig e msg a)]
  -> UI e msg ()
selectionControl mkId attrs = do
  let cfg      = configure defaultSelectionConfig attrs
      itemList = selectionConfigItems cfg
      sel      = selectionConfigSelection cfg
      stateAt idx val = case sel of
        None          -> Unselected
        Item v        -> if v == val then Selected else Unselected
        ItemAtIndex i -> if i == idx then Selected else Unselected

  itemsLayout
    [ items itemList
    , orientation (selectionConfigOrientation cfg)
    , itemsPanel (selectionConfigBoxConfig cfg)
    , itemTemplate $ \idx val ->
        let eid               = mkId idx
            (layout, content) = selectionConfigTemplate cfg eid (stateAt idx val) val
            wrapped = do
              clicked <- isClickedOver eid
              when clicked $ fire attrs [Activated idx val]
              content
        in (layout, wrapped)
    ]
