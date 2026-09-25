{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  , ColumnsConfig (..)
  , HasColumnsConfig (..)
  , defaultColumnsConfig
  , TableConfig (..)
  , defaultTableConfig
  , table
  , columns
  , sortedBy
  , onColumnSortRequested
    -- * Building column-based lists
    -- | For a widget built on 'listBase' that lays its rows out in
    -- resizable, sortable columns under a header, the way 'table' and
    -- 'Blink.Controls.TreeTable.treeTable' do.
  , withColumns
  , columnRow
  , columnCell
    -- * Style
  , tableHeaderStyleKey
  , tableColumnDividerStyleKey
  , defaultStyleEntries
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.Map.Strict as Map

import Blink.Controls.Control
import Blink.Controls.List hiding (defaultStyleEntries)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, emptyElement, noIntrinsicSize, runElement)
import Blink.Geometry (Alignment (TopLeft), Insets (..), Point (pointX), Rectangle (..), insetRect, uniform)
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), Length, exactly, fill)
import Blink.View
  ( CursorShape (..), Effect, View, currentStyle, getBounds, getExtentState, getMousePos, getStyleSet, isDragging
  , requestCursor, requestExtentBy, withBounds
  )
import Blink.View.Drawing (fillRect)
import Blink.Rendering (TextAlign (..))
import Blink.Style
import Blink.Controls.Style (transparent)

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

-- | A widget's columns, in order, and the caller-owned current sort with
-- its reactions.
data ColumnsConfig e msg a = ColumnsConfig
  { csColumns               :: [ColumnConfig e msg a]
  , csSort                  :: Maybe (Int, SortDirection)
  , csOnColumnSortRequested :: [(Int, SortDirection) -> [Effect e msg]]
  }

-- | No columns, no sort, and no sort reactions.
defaultColumnsConfig :: ColumnsConfig e msg a
defaultColumnsConfig = ColumnsConfig
  { csColumns               = []
  , csSort                  = Nothing
  , csOnColumnSortRequested = []
  }

-- | Implemented by any config type that nests a 'ColumnsConfig', letting
-- 'columns'\/'sortedBy'\/'onColumnSortRequested' be applied to it directly.
class HasColumnsConfig e msg a cfg | cfg -> e msg a where
  overColumns :: Attribute (ColumnsConfig e msg a) -> Attribute cfg

instance HasColumnsConfig e msg a (ColumnsConfig e msg a) where
  overColumns = id

-- | Every capability 'table' resolves: the embedded 'ListConfig' (for
-- 'Blink.Controls.List.selection'\/'Blink.Controls.List.rowHeight'\/etc,
-- via 'HasListConfig') and its columns and sort (via 'HasColumnsConfig').
data TableConfig sel e msg a = TableConfig
  { tbList    :: ListConfig sel e msg a
  , tbColumns :: ColumnsConfig e msg a
  }

instance HasControlConfig e msg (TableConfig sel e msg a) where
  overControl attr = Attribute (\tc -> tc { tbList = runAttribute (overControl attr) (tbList tc) })

instance HasEventHandlers (TableConfig sel e msg a)

instance HasLayoutConfig (TableConfig sel e msg a) where
  overLayout attr = Attribute (\tc -> tc { tbList = runAttribute (overLayout attr) (tbList tc) })

instance HasListConfig sel e msg a (TableConfig sel e msg a) where
  overList attr = Attribute (\tc -> tc { tbList = runAttribute attr (tbList tc) })

instance HasColumnsConfig e msg a (TableConfig sel e msg a) where
  overColumns attr = Attribute (\tc -> tc { tbColumns = runAttribute attr (tbColumns tc) })

-- | 'defaultListConfig' and 'defaultColumnsConfig'.
defaultTableConfig :: (SelectionModel sel, EmptySelection sel) => TableConfig sel e msg a
defaultTableConfig = TableConfig
  { tbList    = defaultListConfig
  , tbColumns = defaultColumnsConfig
  }

-- | The widget's own columns, in order -- both a row's cells and the
-- header row are built from this same list, so they always line up.
columns :: HasColumnsConfig e msg a cfg => [ColumnConfig e msg a] -> Attribute cfg
columns cs = overColumns (Attribute (\c -> c { csColumns = cs }))

-- | Which column is currently sorted and which direction, if any --
-- caller-owned, the same stateless relationship 'Blink.Controls.List.selection'
-- has to the selection model: the widget never sorts rows itself, only
-- reports the user's requested sort via 'onColumnSortRequested' for the
-- app to store, re-sort by, and pass back in here.
sortedBy :: HasColumnsConfig e msg a cfg => Maybe (Int, SortDirection) -> Attribute cfg
sortedBy s = overColumns (Attribute (\c -> c { csSort = s }))

-- | Reacts when clicking a sortable column's header cell (see
-- 'colSortable') requests a sort: 'Ascending' for a column not already
-- sorted, otherwise the opposite of its current direction.
onColumnSortRequested :: HasColumnsConfig e msg a cfg => ((Int, SortDirection) -> [Effect e msg]) -> Attribute cfg
onColumnSortRequested h = overColumns (Attribute (\c -> c { csOnColumnSortRequested = csOnColumnSortRequested c ++ [h] }))

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
table mkId attrs =
  chromeElement (lcLayout (tbList cfg)) (ccStyleKey (lcControl (tbList cfg))) (listMeasure hasColumns (tbList cfg)) (void run)
  where
    cfg        = resolve defaultTableConfig attrs
    cols       = tbColumns cfg
    hasColumns = not (null (csColumns cols))

    run = do
      listCfg <- withColumns (mkId . TableHeaderCell) (mkId . TableColumnDivider) cols renderRow (tbList cfg)
      listBase (mkId . TableRow) listCfg

    renderRow widths st = columnRow widths (csColumns cols) (\_ w c -> columnCell w c st)

-- | @listCfg@ with a header row built from @cols@ (when there are any
-- columns) and each row drawn by @renderRow@, both at the columns'
-- current widths. The widths depend on how far each divider has been
-- dragged, so they're resolved once here per frame and shared by the
-- header and every row.
withColumns
  :: Ord e
  => (Int -> e)                                   -- ^ header cell id, by column index
  -> (Int -> e)                                   -- ^ resize handle id, by the index of the column before it
  -> ColumnsConfig e msg a
  -> ([Length] -> ItemState a -> Element e msg)   -- ^ a row, given the columns' current widths
  -> ListConfig sel e msg a
  -> View e msg (ListConfig sel e msg a)
withColumns mkHeaderId mkDividerId cols renderRow listCfg = do
  widths <- resolveColumnWidths mkDividerId (csColumns cols)
  pure listCfg
    { lcRenderItem = renderRow widths
    , lcHeader     = if null (csColumns cols) then Nothing else
        Just (columnHeaderRow mkHeaderId mkDividerId
                (requestColumnSort (csSort cols) (csOnColumnSortRequested cols)) widths (csColumns cols))
    }

-- | A row's cells at the given widths, with a gap between each pair the
-- width of the header's resize handle, so every column's boundary lands at
-- the same x under the header and in a row. @cellFor@ builds column @i@'s
-- cell.
columnRow :: [Length] -> [ColumnConfig e msg a] -> (Int -> Length -> ColumnConfig e msg a -> Element e msg) -> Element e msg
columnRow widths cols cellFor = hBox [children (weaveColumns (const columnSpacer) (zipWith3 cellFor [0 ..] widths cols))]

-- | One cell, sized to its column's own resolved width, drawing its
-- 'colCell' content.
columnCell :: Length -> ColumnConfig e msg a -> ItemState a -> Element e msg
columnCell w c st = elementWithLayout (Layout w fill TopLeft) (runElement (colCell c st))

-- | 'Ascending' for a column not already sorted, otherwise the opposite
-- of whatever direction it's currently sorted in.
requestColumnSort :: Maybe (Int, SortDirection) -> [(Int, SortDirection) -> [Effect e msg]] -> Int -> View e msg ()
requestColumnSort currentSort handlers idx = runHandlers handlers (idx, nextDirection)
  where
    nextDirection = case currentSort of
      Just (i, dir) | i == idx -> flipDirection dir
      _                        -> Ascending
    flipDirection Ascending  = Descending
    flipDirection Descending = Ascending

-- | Inserts @between i@ between every adjacent pair of @xs@, numbered by
-- the index of the element just before it -- the header's own
-- @resizeHandle@s, and (via 'columnSpacer') the inert gap a row's cells
-- need at those same positions, so a column's boundary always lands at
-- the same x whether it's under the header or a row.
weaveColumns :: (Int -> Element e msg) -> [Element e msg] -> [Element e msg]
weaveColumns between = go 0
  where
    go _ []                 = []
    go _ [x]                = [x]
    go i (x : rest@(_ : _)) = x : between i : go (i + 1) rest

-- | An inert gap the width of @resizeHandle@, dropped between a row's
-- own cells (via 'weaveColumns') so each column lines up under its
-- header cell despite the draggable handle woven into the header alone.
columnSpacer :: Element e msg
columnSpacer = elementWithLayout (Layout (exactly handleWidth) fill TopLeft) (pure ())

-- | A row of header cells at the given widths, with a draggable
-- @resizeHandle@ (numbered the same as the cell just before it) woven in
-- between each pair rather than a border. Clicking a sortable column's
-- header (see 'colSortable') calls @onSortClick@ with its index. The
-- whole row is inset once by the same chrome a data row gets from its
-- own control (@listItemStyleKey@) -- each header /cell/ carries none of
-- its own (see 'tableHeaderStyleKey'), so every column's content starts
-- at the same x under the header as it does in its row.
columnHeaderRow
  :: Ord e
  => (Int -> e) -> (Int -> e) -> (Int -> View e msg ()) -> [Length] -> [ColumnConfig e msg a] -> Element e msg
columnHeaderRow mkHeaderId mkDividerId onSortClick widths cols =
  elementWithLayout (Layout fill fill TopLeft) $ do
    insets <- rowChromeInset
    bounds <- getBounds
    -- Left\/right only: a row's own chrome shifts where its content
    -- starts\/ends horizontally, but the header is exactly one
    -- 'lcRowHeight' tall already -- insetting its top\/bottom too would
    -- needlessly shrink every header cell's own height, and so its
    -- hover\/selection highlight.
    let horizontalInsets = insets { topInset = 0, bottomInset = 0 }
    withBounds (insetRect horizontalInsets bounds) $
      runElement $ hBox [children (weaveColumns (resizeHandle mkDividerId) cells)]
  where
    cells = [ headerCell idx w c | (idx, w, c) <- zip3 [0 :: Int ..] widths cols ]

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

-- | The chrome a data row insets its own content by, via its control's
-- @listItemStyleKey@ -- always resolved from 'styleBase', the same
-- interaction-state-independent convention 'Blink.Controls.Control.measureChrome'
-- already uses, so this never shifts with hover\/selection. 'columnHeaderRow'
-- applies this once around the whole header so its columns start at the
-- same x a row's own do.
rowChromeInset :: Ord e => View e msg Insets
rowChromeInset = do
  (m, styleSet) <- getStyleSet listItemStyleKey
  pure (chromeInsets m (styleBase styleSet))

-- | Resolves every column's current 'Layout' width: a 'ColumnFixed'
-- column's own pixel value, adjusted by however far the dividers on
-- either side of it have been dragged -- growing when its right-hand
-- divider (its own index) moves right, shrinking when its left-hand
-- divider (the previous index) does, and never below @minColumnWidth@.
-- A 'ColumnFill' column is never adjusted directly; it simply absorbs
-- whatever its neighbours give up.
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
      , ccContent         = body
      }
  }
  where
    eid = mkDividerId idx
    body ci = do
      dragging <- isDragging eid
      bounds   <- getBounds
      when (dragging || ciHovered ci) $ requestCursor CursorResizeHorizontal
      when dragging $ do
        mouseX <- pointX <$> getMousePos
        requestExtentBy eid (mouseX - (rectX bounds + handleWidth / 2))
      s <- currentStyle
      let lineRect = bounds { rectX = rectX bounds + (handleWidth - 1) / 2, rectWidth = 1 }
      forM_ (styleBorderColour s) (\c -> withBounds lineRect (fillRect c))

-- * Style

-- | The 'StyleKey' each of a 'Blink.Controls.Table.table''s header
-- cells resolves its style from unless overridden via
-- 'Blink.Controls.Control.style'.
tableHeaderStyleKey :: StyleKey e
tableHeaderStyleKey = Class "table-header"

-- | The 'StyleKey' a 'Blink.Controls.Table.table''s draggable
-- column-resize handle resolves its style from. Distinct from
-- 'Blink.Controls.Divider.dividerStyleKey' -- that one's default 4px
-- margin would eat the handle's own few pixels of width, since a
-- control's chrome insets both its hit area and its content bounds by
-- margin before running.
tableColumnDividerStyleKey :: StyleKey e
tableColumnDividerStyleKey = Class "table-column-divider"

-- | A shaded strip, no border, tinted on hover. A resize handle (see
-- 'tableColumnDividerStyleKey') between cells (see
-- 'Blink.Controls.Table.table') separates them instead of a border.
tableHeaderStyle :: Palette -> StyleSet
tableHeaderStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = noBorder
      }
  , styleOverrides = Map.singleton CommonMouseOver (\s -> s { styleBackground = paletteSurfaceHover p })
  }

-- | No margin\/padding of its own -- the header row is inset once as a
-- whole, by the same chrome a data row gets, rather than padding each
-- header cell individually; padding here too would double up on that and
-- push a header cell's content further right than the matching row cell's.
tableHeaderMetrics :: Metrics
tableHeaderMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  }

-- | No margin (unlike 'Blink.Controls.Divider.divider') -- the handle's
-- whole few-pixel width has to stay both hittable and drawable.
tableColumnDividerMetrics :: Metrics
tableColumnDividerMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  }

-- | A vertical line, 'paletteBorder' by default, tinted on hover the
-- same way a header cell is.
tableColumnDividerStyle :: Palette -> StyleSet
tableColumnDividerStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = soloBorder (paletteBorder p) 0
      }
  , styleOverrides = Map.singleton CommonMouseOver (\s -> s { styleBorder = withBorderColour (paletteSurfaceHover p) (styleBorder s) })
  }

-- | This control's own entries in 'Blink.Style.Defaults.defaultTheme'.
-- Needed at all only because 'Blink.Controls.Control.defaultControlConfig'
-- resolves an unset 'Blink.Controls.Control.ccStyleKey' to
-- 'Blink.Style.Defaults.defaultTheme''s boxed-control fallback look --
-- without them, the header would draw with a button's full chrome instead
-- of a header that reads distinctly from the rows beneath it.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (tableHeaderStyleKey, (tableHeaderMetrics, tableHeaderStyle p))
  , (tableColumnDividerStyleKey, (tableColumnDividerMetrics, tableColumnDividerStyle p))
  ]
