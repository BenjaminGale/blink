{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A table whose column 0 is also a tree: built on 'listBase',
-- combining 'Blink.Controls.Tree.tree''s forest flattening, expansion,
-- and keyboard handling with 'Blink.Controls.Table.table''s columns,
-- header, resizing, and sorting. Column 0 of each row gets the indent
-- and chevron a plain tree row has; every other column is a plain
-- per-column cell, both from the same underlying node.
module Blink.Controls.TreeTable
  ( TreeTablePart (..)
  , TreeTableConfig (..)
  , defaultTreeTableConfig
  , treeTable
  , columns
  , sortedBy
  , onColumnSortRequested
  , forest
  , expanded
  , onExpansionChanged
  ) where

import Control.Monad (void)

import Blink.Controls.Control
import Blink.Controls.List
import Blink.Controls.Table
  ( ColumnConfig (..), ColumnsConfig (..), HasColumnsConfig (..), columnCell, columnRow, columns
  , defaultColumnsConfig, onColumnSortRequested, sortedBy, withColumns
  )
import Blink.Controls.Tree
  ( HasTreeDataConfig (..), TreeDataConfig (..), TreeItemState (..), TreeListConfig (..), defaultTreeDataConfig
  , expanded, forest, indentAndChevron, onExpansionChanged, treeListBase
  )
import Blink.Element (Element (..), HasLayoutConfig (..), HasSelection (..), HasSelectionChanged (..), elementWithLayout, runElement)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), fill)

-- | Identifies one part of a 'treeTable' for the purpose of building
-- element ids -- every part 'listBase' itself already needs, plus a
-- header cell and a column-resize handle (both by column index, as
-- 'Blink.Controls.Table.TablePart' has), and a node's own
-- expand\/collapse chevron (as 'Blink.Controls.Tree.TreePart' has).
data TreeTablePart a
  = TTRow (ListPart a)
  | TTHeaderCell Int
  | TTColumnDivider Int
  | TTChevron a
  deriving (Eq, Ord, Show)

-- | Every capability 'treeTable' resolves: the embedded 'ListConfig', its
-- columns and sort (via 'HasColumnsConfig', as
-- 'Blink.Controls.Table.TableConfig' has), and its forest and expansion
-- state (via 'HasTreeDataConfig', as 'Blink.Controls.Tree.TreeConfig' has).
data TreeTableConfig sel e msg a = TreeTableConfig
  { ttList     :: ListConfig sel e msg a
  , ttColumns  :: ColumnsConfig e msg a
  , ttTreeData :: TreeDataConfig e msg a
  }

instance HasControlConfig e msg (TreeTableConfig sel e msg a) where
  overControl = nested ttList (\tc x -> tc { ttList = x }) . overControl

instance HasEventHandlers (TreeTableConfig sel e msg a)

instance HasLayoutConfig (TreeTableConfig sel e msg a) where
  overLayout = nested ttList (\tc x -> tc { ttList = x }) . overLayout

instance HasListConfig sel e msg a (TreeTableConfig sel e msg a) where
  overList = nested ttList (\tc x -> tc { ttList = x })

instance HasSelection (sel a) (TreeTableConfig sel e msg a) where
  selection = overList . selection

instance HasSelectionChanged e msg (sel a) (TreeTableConfig sel e msg a) where
  onSelectionChanged = overList . onSelectionChanged

instance HasColumnsConfig e msg a (TreeTableConfig sel e msg a) where
  overColumns = nested ttColumns (\tc x -> tc { ttColumns = x })

instance HasTreeDataConfig e msg a (TreeTableConfig sel e msg a) where
  overTreeData = nested ttTreeData (\tc x -> tc { ttTreeData = x })

-- | 'defaultListConfig', 'defaultColumnsConfig', and
-- 'defaultTreeDataConfig'.
defaultTreeTableConfig :: (SelectionModel sel, EmptySelection sel) => TreeTableConfig sel e msg a
defaultTreeTableConfig = TreeTableConfig
  { ttList     = defaultListConfig
  , ttColumns  = defaultColumnsConfig
  , ttTreeData = defaultTreeDataConfig
  }

-- | A table whose column 0 is also a tree (see the module header).
-- @mkId@ builds every part's element id from a 'TreeTablePart', the
-- same relationship 'listBase's own @mkId@ has to 'ListPart'.
treeTable
  :: (Ord e, Ord a, SelectionModel sel, EmptySelection sel, Eq (sel a))
  => (TreeTablePart a -> e)
  -> [Attribute (TreeTableConfig sel e msg a)]
  -> Element e msg
treeTable mkId attrs =
  chromeElement (lcLayout (ttList cfg)) (ccStyleKey (lcControl (ttList cfg))) (listMeasure hasColumns (ttList cfg)) (void run)
  where
    cfg        = resolve defaultTreeTableConfig attrs
    cols       = ttColumns cfg
    td         = ttTreeData cfg
    hasColumns = not (null (csColumns cols))

    run = do
      (widths, listCfg) <- withColumns (mkId . TTHeaderCell) (mkId . TTColumnDivider) cols (ttList cfg)
      treeListBase (mkId . TTRow) TreeListConfig
        { tlList      = listCfg
        , tlTreeData  = td
        , tlRenderRow = renderRow widths
        }

    -- Column 0 gets the indent\/chevron treatment 'tree' itself gives a
    -- whole row; every other column is a plain 'columnCell'.
    renderRow widths tis = columnRow widths (csColumns cols) cellFor
      where
        st = tisState tis

        cellFor 0 w c = elementWithLayout (Layout w fill TopLeft) $
          runElement $ hBox
            [ children (indentAndChevron (mkId . TTChevron) td tis ++ [colCell c st]) ]
        cellFor _ w c = columnCell w c st
