{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A table built on 'listBase': each row lays out one cell per
-- 'columns' entry, sized to that column's own width, with a header row
-- of the columns' header content aligned to the same widths -- fixed
-- above the scrollable rows (via 'lcHeader'), never itself a selectable
-- or focusable row.
module Blink.Controls.Table
  ( TablePart (..)
  , ColumnConfig (..)
  , TableConfig (..)
  , defaultTableConfig
  , table
  , columns
  ) where

import Control.Monad (void)
import Data.List (intersperse)
import Data.Maybe (isJust)

import Blink.Controls.Control
import Blink.Controls.Divider (divider, orientation)
import Blink.Controls.List
import Blink.Controls.Table.Style (tableHeaderStyleKey)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, noIntrinsicSize, runElement)
import Blink.Geometry (Alignment (TopLeft), Orientation (Vertical))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), Length, exactly, fill)

-- | Identifies one part of a 'table' for the purpose of building
-- element ids -- every part 'listBase' itself already needs (the root,
-- a row, a scrollbar part), plus a header cell (by column index),
-- tracked individually so each can respond to hover on its own.
data TablePart a
  = TableRow (ListPart a)
  | TableHeaderCell Int
  deriving (Eq, Ord, Show)

-- | One column: its header content, its own width, and how a row draws
-- its cell in it.
data ColumnConfig e msg a = ColumnConfig
  { colHeader :: Element e msg
  , colWidth  :: Length
  , colCell   :: ItemState a -> Element e msg
  }

-- | Every capability 'table' resolves: the embedded 'ListConfig' (for
-- 'Blink.Controls.List.selection'\/'Blink.Controls.List.rowHeight'\/etc,
-- via 'HasListConfig'), and its columns.
data TableConfig sel e msg a = TableConfig
  { tbList    :: ListConfig sel e msg a
  , tbColumns :: [ColumnConfig e msg a]
  }

instance HasControlConfig e msg (TableConfig sel e msg a) where
  overControl attr = Attribute (\tc -> tc { tbList = runAttribute (overControl attr) (tbList tc) })

instance HasLayoutConfig (TableConfig sel e msg a) where
  overLayout attr = Attribute (\tc -> tc { tbList = runAttribute (overLayout attr) (tbList tc) })

instance HasListConfig sel e msg a (TableConfig sel e msg a) where
  overList attr = Attribute (\tc -> tc { tbList = runAttribute attr (tbList tc) })

-- | 'defaultListConfig' and no columns.
defaultTableConfig :: SelectionModel sel => TableConfig sel e msg a
defaultTableConfig = TableConfig
  { tbList    = defaultListConfig
  , tbColumns = []
  }

-- | The table's own columns, in order -- both a row's cells and the
-- header row are built from this same list, so they always line up.
columns :: [ColumnConfig e msg a] -> Attribute (TableConfig sel e msg a)
columns cs = Attribute (\c -> c { tbColumns = cs })

-- | A table built on 'listBase' (see the module header). @mkId@ builds
-- every part's element id from a 'TablePart', the same relationship
-- 'listBase's own @mkId@ has to 'ListPart'.
table
  :: (Ord e, Eq a, SelectionModel sel, Eq (sel a))
  => (TablePart a -> e)
  -> [Attribute (TableConfig sel e msg a)]
  -> Element e msg
table mkId attrs = Element
  { elLayout  = lcLayout listCfg
  , elMeasure = measureChrome (ccStyleKey (lcControl listCfg)) (tableSpacer listCfg)
  , elRun     = void (listBase (mkId . TableRow) listCfg)
  }
  where
    cfg     = resolve defaultTableConfig attrs
    listCfg = (tbList cfg)
      { lcRenderItem = \st -> hBox [children [cellFor c st | c <- tbColumns cfg]]
      , lcHeader     = if null (tbColumns cfg) then Nothing else Just headerCells
      }

    cellFor c st = elementWithLayout (Layout (colWidth c) fill TopLeft) (runElement (colCell c st))

    -- Each header cell is its own, unfocusable, hoverable 'control'
    -- (see 'tableHeaderStyleKey'), separated by a plain vertical
    -- 'divider' rather than a border -- both the per-cell hover and the
    -- separators are what a future column-click sort\/resize will hang
    -- off, not just a look.
    headerCells = hBox
      [ children (intersperse (divider [orientation Vertical]) (zipWith headerCell [0 ..] (tbColumns cfg))) ]

    headerCell idx c = Element
      { elLayout  = Layout (colWidth c) fill TopLeft
      , elMeasure = noIntrinsicSize
      , elRun     = void $ control defaultControlConfig
          { ccElementId   = Just (mkId (TableHeaderCell idx))
          , ccStyleKey    = tableHeaderStyleKey
          , ccFocusPolicy = NotFocusable
          , ccContent     = const (runElement (colHeader c))
          }
      }

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
