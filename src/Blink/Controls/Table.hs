{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A table built on 'listBase': each row lays out one cell per
-- 'columns' entry, sized to that column's own width, with a header row
-- of the columns' header content aligned to the same widths -- fixed
-- above the scrollable rows (via 'lcHeader'), never itself a selectable
-- or focusable row. A draggable handle between each pair of header cells
-- resizes the two columns it sits between.
module Blink.Controls.Table
  ( TablePart (..)
  , ColumnWidth (..)
  , ColumnConfig (..)
  , defaultColumnConfig
  , column
  , header
  , cellWidth
  , cell
  , sortable
  , SortDirection (..)
  , TableConfig (..)
  , defaultTableConfig
  , table
  , columns
  , sortedBy
  , onColumnSortRequested
    -- * Shared column machinery
    -- | Used by 'table' itself and by 'Blink.Controls.TreeTable.treeTable'
    -- to lay out and resize the same kind of columns without duplicating
    -- the logic.
  , resolveColumnWidths
  , columnCell
  , columnHeaderRow
  , requestColumnSort
  , tableSpacer
  ) where

import Control.Monad (forM_, void, when)
import Data.Maybe (isJust)

import Blink.Controls.Control
import Blink.Controls.List
import Blink.Controls.Table.Style (tableColumnDividerStyleKey, tableHeaderStyleKey)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, emptyElement, noIntrinsicSize, runElement)
import Blink.Geometry (Alignment (TopLeft), Point (pointX), Rectangle (..))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), Length, exactly, fill)
import Blink.Style (Style (..))
import Blink.View
  (Effect, View, currentStyle, getBounds, getExtentState, getMousePos, isDragging, requestExtentBy, withBounds)
import Blink.View.Drawing (fillRect)

-- | Identifies one part of a 'table' for the purpose of building
-- element ids -- every part 'listBase' itself already needs (the root,
-- a row, a scrollbar part), plus a header cell and, between each pair
-- of header cells, the draggable handle that resizes them (both by
-- column index).
data TablePart a
  = TableRow (ListPart a)
  | TableHeaderCell Int
  | TableColumnDivider Int
  deriving (Eq, Ord, Show)

-- | A column's own width: a concrete, resizable pixel value, or a share
-- of whatever space is left (like 'Blink.Layout.Constraints.fill', and
-- not itself resizable by dragging -- its neighbour absorbs the change
-- instead).
data ColumnWidth
  = ColumnFixed Double
  | ColumnFill

-- | One column: its header content, its own width, how a row draws its
-- cell in it, and whether clicking its header requests a sort (see
-- 'onColumnSortRequested').
data ColumnConfig e msg a = ColumnConfig
  { colHeader   :: Element e msg
  , colWidth    :: ColumnWidth
  , colCell     :: ItemState a -> Element e msg
  , colSortable :: Bool
  }

-- | An empty header, a fill width, an empty cell, unsortable -- the
-- starting point 'column' resolves its attributes against.
defaultColumnConfig :: ColumnConfig e msg a
defaultColumnConfig = ColumnConfig
  { colHeader   = emptyElement
  , colWidth    = ColumnFill
  , colCell     = const emptyElement
  , colSortable = False
  }

-- | Builds one 'ColumnConfig' from attributes, the same list syntax
-- 'table' and 'Blink.Controls.TreeTable.treeTable' themselves take --
-- see 'header', 'cellWidth', 'cell', 'sortable'.
column :: [Attribute (ColumnConfig e msg a)] -> ColumnConfig e msg a
column = resolve defaultColumnConfig

-- | The column's header content.
header :: Element e msg -> Attribute (ColumnConfig e msg a)
header e = Attribute (\c -> c { colHeader = e })

-- | The column's own width -- see 'ColumnWidth'.
cellWidth :: ColumnWidth -> Attribute (ColumnConfig e msg a)
cellWidth w = Attribute (\c -> c { colWidth = w })

-- | How a row draws its cell in this column.
cell :: (ItemState a -> Element e msg) -> Attribute (ColumnConfig e msg a)
cell f = Attribute (\c -> c { colCell = f })

-- | Whether clicking this column's header requests a sort -- see
-- 'onColumnSortRequested'.
sortable :: Bool -> Attribute (ColumnConfig e msg a)
sortable s = Attribute (\c -> c { colSortable = s })

-- | Which way a sorted column's own header click requests next -- see
-- 'onColumnSortRequested'.
data SortDirection = Ascending | Descending
  deriving (Eq, Show)

-- | Every capability 'table' resolves: the embedded 'ListConfig' (for
-- 'Blink.Controls.List.selection'\/'Blink.Controls.List.rowHeight'\/etc,
-- via 'HasListConfig'), its columns, and the caller-owned current sort
-- (see 'sortedBy').
data TableConfig sel e msg a = TableConfig
  { tbList                  :: ListConfig sel e msg a
  , tbColumns               :: [ColumnConfig e msg a]
  , tbSort                  :: Maybe (Int, SortDirection)
  , tbOnColumnSortRequested :: [(Int, SortDirection) -> [Effect e msg]]
  }

instance HasControlConfig e msg (TableConfig sel e msg a) where
  overControl attr = Attribute (\tc -> tc { tbList = runAttribute (overControl attr) (tbList tc) })

instance HasLayoutConfig (TableConfig sel e msg a) where
  overLayout attr = Attribute (\tc -> tc { tbList = runAttribute (overLayout attr) (tbList tc) })

instance HasListConfig sel e msg a (TableConfig sel e msg a) where
  overList attr = Attribute (\tc -> tc { tbList = runAttribute attr (tbList tc) })

-- | 'defaultListConfig', no columns, no sort.
defaultTableConfig :: (SelectionModel sel, EmptySelection sel) => TableConfig sel e msg a
defaultTableConfig = TableConfig
  { tbList                  = defaultListConfig
  , tbColumns               = []
  , tbSort                  = Nothing
  , tbOnColumnSortRequested = []
  }

-- | The table's own columns, in order -- both a row's cells and the
-- header row are built from this same list, so they always line up.
columns :: [ColumnConfig e msg a] -> Attribute (TableConfig sel e msg a)
columns cs = Attribute (\c -> c { tbColumns = cs })

-- | Which column is currently sorted and which direction, if any --
-- caller-owned, the same stateless relationship 'Blink.Controls.List.selection'
-- has to the selection model: 'table' never sorts rows itself, only
-- reports the user's requested sort via 'onColumnSortRequested' for the
-- app to store, re-sort by, and pass back in here.
sortedBy :: Maybe (Int, SortDirection) -> Attribute (TableConfig sel e msg a)
sortedBy s = Attribute (\c -> c { tbSort = s })

-- | Reacts when clicking a sortable column's header cell (see
-- 'colSortable') requests a sort: 'Ascending' for a column not already
-- sorted, otherwise the opposite of its current direction.
onColumnSortRequested :: ((Int, SortDirection) -> [Effect e msg]) -> Attribute (TableConfig sel e msg a)
onColumnSortRequested h = Attribute (\c -> c { tbOnColumnSortRequested = tbOnColumnSortRequested c ++ [h] })

-- | Never let a drag squeeze a column narrower than this, however far
-- past it the pointer moves -- a column can always be dragged back out
-- again, since the drag itself is never clamped, only what it renders
-- as.
minColumnWidth :: Double
minColumnWidth = 20

-- | The width of the draggable handle between two header cells -- wider
-- than the 1px line it draws (see @resizeHandle@), so it's actually
-- grabbable.
handleWidth :: Double
handleWidth = 5

-- | A table built on 'listBase' (see the module header). @mkId@ builds
-- every part's element id from a 'TablePart', the same relationship
-- 'listBase's own @mkId@ has to 'ListPart'.
table
  :: (Ord e, Eq a, SelectionModel sel, EmptySelection sel, Eq (sel a))
  => (TablePart a -> e)
  -> [Attribute (TableConfig sel e msg a)]
  -> Element e msg
table mkId attrs = Element
  { elLayout  = lcLayout (tbList cfg)
  , elMeasure = measureChrome (ccStyleKey (lcControl (tbList cfg))) (tableSpacer (tbList cfg))
  , elRun     = void run
  }
  where
    cfg = resolve defaultTableConfig attrs

    -- Column widths depend on host-owned drag state (see
    -- 'resolveColumnWidths'), so they're resolved once per frame here,
    -- ahead of 'listBase', and closed over by both the header and every
    -- row's own cells -- never re-derived per row.
    run = do
      widths <- resolveColumnWidths (mkId . TableColumnDivider) (tbColumns cfg)
      let listCfg = (tbList cfg)
            { lcRenderItem = \st -> hBox [children (zipWith (\w c -> columnCell w c st) widths (tbColumns cfg))]
            , lcHeader     = if null (tbColumns cfg) then Nothing else
                Just (columnHeaderRow (mkId . TableHeaderCell) (mkId . TableColumnDivider)
                        (requestColumnSort (tbSort cfg) (tbOnColumnSortRequested cfg)) widths (tbColumns cfg))
            }
      listBase (mkId . TableRow) listCfg

-- | One cell, sized to its column's own resolved width, drawing its
-- 'colCell' content -- shared by 'table' and
-- 'Blink.Controls.TreeTable.treeTable'.
columnCell :: Length -> ColumnConfig e msg a -> ItemState a -> Element e msg
columnCell w c st = elementWithLayout (Layout w fill TopLeft) (runElement (colCell c st))

-- | 'Ascending' for a column not already sorted, otherwise the opposite
-- of whatever direction it's currently sorted in -- shared by 'table'
-- and 'Blink.Controls.TreeTable.treeTable'.
requestColumnSort :: Maybe (Int, SortDirection) -> [(Int, SortDirection) -> [Effect e msg]] -> Int -> View e msg ()
requestColumnSort currentSort handlers idx = runHandlers handlers (idx, nextDirection)
  where
    nextDirection = case currentSort of
      Just (i, dir) | i == idx -> flipDirection dir
      _                        -> Ascending
    flipDirection Ascending  = Descending
    flipDirection Descending = Ascending

-- | A row of header cells at the given widths, with a draggable
-- @resizeHandle@ (numbered the same as the cell just before it) woven in
-- between each pair rather than a border. Clicking a sortable column's
-- header (see 'colSortable') calls @onSortClick@ with its index -- shared
-- by 'table' and 'Blink.Controls.TreeTable.treeTable'.
columnHeaderRow
  :: Ord e
  => (Int -> e) -> (Int -> e) -> (Int -> View e msg ()) -> [Length] -> [ColumnConfig e msg a] -> Element e msg
columnHeaderRow mkHeaderId mkDividerId onSortClick widths cols = hBox [children (weave cells)]
  where
    cells = [ headerCell idx w c | (idx, w, c) <- zip3 [0 :: Int ..] widths cols ]
    weave = go 0
    go _ []                 = []
    go _ [x]                = [x]
    go i (x : rest@(_ : _)) = x : resizeHandle mkDividerId i : go (i + 1) rest

    -- Its own, unfocusable, hoverable 'control' (see 'tableHeaderStyleKey').
    headerCell idx w c = Element
      { elLayout  = Layout w fill TopLeft
      , elMeasure = noIntrinsicSize
      , elRun     = void $ control defaultControlConfig
          { ccElementId   = Just (mkHeaderId idx)
          , ccStyleKey    = tableHeaderStyleKey
          , ccFocusPolicy = NotFocusable
          , ccContent     = \hci -> do
              when (colSortable c && ciClicked hci) (onSortClick idx)
              runElement (colHeader c)
          }
      }

-- | Resolves every column's current 'Layout' width: a 'ColumnFixed'
-- column's own pixel value, adjusted by however far the dividers on
-- either side of it have been dragged -- growing when its right-hand
-- divider (its own index) moves right, shrinking when its left-hand
-- divider (the previous index) does, and never below @minColumnWidth@.
-- A 'ColumnFill' column is never adjusted directly; it simply absorbs
-- whatever its neighbours give up. Shared by 'table' and
-- 'Blink.Controls.TreeTable.treeTable'.
resolveColumnWidths :: Ord e => (Int -> e) -> [ColumnConfig e msg a] -> View e msg [Length]
resolveColumnWidths mkDividerId cols = do
  extents <- mapM (\i -> getExtentState (mkDividerId i)) [0 .. length cols - 2]
  let netDeltas = zipWith (-) (extents ++ [0]) (0 : extents)
  pure (zipWith effectiveLength cols netDeltas)
  where
    effectiveLength c delta = case colWidth c of
      ColumnFixed w -> exactly (max minColumnWidth (w + delta))
      ColumnFill    -> fill

-- | The draggable handle between the header cells for column @idx@ and
-- column @idx + 1@: while dragging, nudges its own extent state by
-- exactly the distance between the pointer and the handle's own current
-- centre, so next frame's handle (rendered from the newly resolved
-- widths) sits right where the pointer is -- the same self-correcting,
-- one-step-per-frame convergence 'Blink.Controls.ScrollBar.scrollBar's
-- own track drag uses, adapted to a delta ('Blink.View.requestExtentBy')
-- since (unlike a scrollbar's track) the handle's own position moves as
-- the drag proceeds, so there's no fixed range to map the pointer onto
-- absolutely.
resizeHandle :: Ord e => (Int -> e) -> Int -> Element e msg
resizeHandle mkDividerId idx = Element
  { elLayout  = Layout (exactly handleWidth) fill TopLeft
  , elMeasure = noIntrinsicSize
  , elRun     = void $ control defaultControlConfig
      { ccElementId       = Just eid
      , ccStyleKey        = tableColumnDividerStyleKey
      , ccFocusPolicy     = NotFocusable
      , ccMouseActivation = CaptureActivated
      , ccContent         = const body
      }
  }
  where
    eid = mkDividerId idx
    body = do
      dragging <- isDragging eid
      bounds   <- getBounds
      when dragging $ do
        mouseX <- pointX <$> getMousePos
        requestExtentBy eid (mouseX - (rectX bounds + handleWidth / 2))
      s <- currentStyle
      let lineRect = bounds { rectX = rectX bounds + (handleWidth - 1) / 2, rectWidth = 1 }
      forM_ (styleBorderColour s) (\c -> withBounds lineRect (fillRect c))

-- | A single fixed-height stand-in for the header (if any) plus every
-- current row stacked vertically -- used only to measure the table's
-- own height, the same reason 'Blink.Controls.List.rowsSpacer' exists
-- for a plain list, extended by one row's worth when there's a header.
tableSpacer :: SelectionModel sel => ListConfig sel e msg a -> Element e msg
tableSpacer cfg = elementWithLayout (Layout fill (exactly height) TopLeft) (pure ())
  where
    rowsHeight   = fromIntegral (length (itemStates (lcSelection cfg))) * lcRowHeight cfg
    headerHeight = if isJust (lcHeader cfg) then lcRowHeight cfg else 0
    height       = headerHeight + rowsHeight
