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
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Forest)

import Blink.Controls.Control
import Blink.Controls.List
import Blink.Controls.Table
  (ColumnConfig (..), SortDirection (..), columnCell, columnHeaderRow, requestColumnSort, resolveColumnWidths, tableSpacer)
import Blink.Controls.Tree (handleExpansionKey, indentAndChevron, visibleNodes)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, runElement)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.View (Effect)

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

-- | Every capability 'treeTable' resolves: the embedded 'ListConfig',
-- its columns and caller-owned current sort (as
-- 'Blink.Controls.Table.TableConfig' has), and the forest\/expansion
-- state and reactions (as 'Blink.Controls.Tree.TreeConfig' has).
data TreeTableConfig sel e msg a = TreeTableConfig
  { ttList                  :: ListConfig sel e msg a
  , ttColumns               :: [ColumnConfig e msg a]
  , ttSort                  :: Maybe (Int, SortDirection)
  , ttOnColumnSortRequested :: [(Int, SortDirection) -> [Effect e msg]]
  , ttForest                :: Forest a
  , ttExpanded              :: Set a
  , ttOnExpansionChanged    :: [Set a -> [Effect e msg]]
  }

instance HasControlConfig e msg (TreeTableConfig sel e msg a) where
  overControl attr = Attribute (\tc -> tc { ttList = runAttribute (overControl attr) (ttList tc) })

instance HasLayoutConfig (TreeTableConfig sel e msg a) where
  overLayout attr = Attribute (\tc -> tc { ttList = runAttribute (overLayout attr) (ttList tc) })

instance HasListConfig sel e msg a (TreeTableConfig sel e msg a) where
  overList attr = Attribute (\tc -> tc { ttList = runAttribute attr (ttList tc) })

-- | 'defaultListConfig', no columns, no sort, an empty forest, and
-- nothing expanded.
defaultTreeTableConfig :: (SelectionModel sel, EmptySelection sel) => TreeTableConfig sel e msg a
defaultTreeTableConfig = TreeTableConfig
  { ttList                  = defaultListConfig
  , ttColumns               = []
  , ttSort                  = Nothing
  , ttOnColumnSortRequested = []
  , ttForest                = []
  , ttExpanded              = Set.empty
  , ttOnExpansionChanged    = []
  }

-- | The tree table's own columns, in order -- see
-- 'Blink.Controls.Table.columns'.
columns :: [ColumnConfig e msg a] -> Attribute (TreeTableConfig sel e msg a)
columns cs = Attribute (\c -> c { ttColumns = cs })

-- | Which column is currently sorted and which direction, if any -- see
-- 'Blink.Controls.Table.sortedBy'.
sortedBy :: Maybe (Int, SortDirection) -> Attribute (TreeTableConfig sel e msg a)
sortedBy s = Attribute (\c -> c { ttSort = s })

-- | Reacts to a sortable column's header click -- see
-- 'Blink.Controls.Table.onColumnSortRequested'.
onColumnSortRequested :: ((Int, SortDirection) -> [Effect e msg]) -> Attribute (TreeTableConfig sel e msg a)
onColumnSortRequested h = Attribute (\c -> c { ttOnColumnSortRequested = ttOnColumnSortRequested c ++ [h] })

-- | The tree table's own data, as a plain 'Forest' -- see
-- 'Blink.Controls.Tree.forest'.
forest :: Forest a -> Attribute (TreeTableConfig sel e msg a)
forest f = Attribute (\c -> c { ttForest = f })

-- | Which nodes are currently expanded -- see 'Blink.Controls.Tree.expanded'.
expanded :: Set a -> Attribute (TreeTableConfig sel e msg a)
expanded s = Attribute (\c -> c { ttExpanded = s })

-- | Reacts to a chevron click -- see
-- 'Blink.Controls.Tree.onExpansionChanged'.
onExpansionChanged :: (Set a -> [Effect e msg]) -> Attribute (TreeTableConfig sel e msg a)
onExpansionChanged h = Attribute (\c -> c { ttOnExpansionChanged = ttOnExpansionChanged c ++ [h] })

-- | A table whose column 0 is also a tree (see the module header).
-- @mkId@ builds every part's element id from a 'TreeTablePart', the
-- same relationship 'listBase's own @mkId@ has to 'ListPart'.
treeTable
  :: (Ord e, Ord a, SelectionModel sel, EmptySelection sel, Eq (sel a))
  => (TreeTablePart a -> e)
  -> [Attribute (TreeTableConfig sel e msg a)]
  -> Element e msg
treeTable mkId attrs = Element
  { elLayout  = lcLayout (ttList cfg)
  , elMeasure = measureChrome (ccStyleKey (lcControl (ttList cfg))) (tableSpacer (ttList cfg))
  , elRun     = void run
  }
  where
    cfg = resolve defaultTreeTableConfig attrs

    visRows  = visibleNodes (ttForest cfg) (ttExpanded cfg)
    nodeInfo = Map.fromList [ (x, (depth, hasChildren)) | (x, depth, hasChildren) <- visRows ]

    run = do
      widths <- resolveColumnWidths (mkId . TTColumnDivider) (ttColumns cfg)
      let listCfg = (ttList cfg)
            { lcRenderItem = renderRow widths
            , lcHeader     = if null (ttColumns cfg) then Nothing else
                Just (columnHeaderRow (mkId . TTHeaderCell) (mkId . TTColumnDivider)
                        (requestColumnSort (ttSort cfg) (ttOnColumnSortRequested cfg)) widths (ttColumns cfg))
            }
      li <- listBase (mkId . TTRow) listCfg
      mapM_ (handleExpansionKey (mkId . TTRow) listCfg visRows (ttExpanded cfg) (ttOnExpansionChanged cfg) (liViewportHeight li))
        (ciKeysPressed (liControl li))

    -- Column 0 gets the indent\/chevron treatment 'tree' itself gives a
    -- whole row; every other column is a plain 'columnCell'.
    renderRow widths st = hBox [children (zipWith cellFor [0 :: Int ..] (zip widths (ttColumns cfg)))]
      where
        x                     = isItem st
        (depth, hasChildren) = Map.findWithDefault (0, False) x nodeInfo

        cellFor 0 (w, c) = elementWithLayout (Layout w fill TopLeft) $
          runElement $ hBox
            [ children (indentAndChevron (mkId . TTChevron) (ttOnExpansionChanged cfg) (ttExpanded cfg) depth hasChildren x
                ++ [colCell c st])
            ]
        cellFor _ (w, c) = columnCell w c st
