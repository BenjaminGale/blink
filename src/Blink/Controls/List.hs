{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A list control generic over its /selection model/: a single 'list'
-- widget renders a vertical list of items and supports single,
-- required-single, multi, and contiguous-range selection without itself
-- knowing which one it has been given.
--
-- = Design
--
-- A model value carries both the items and the selection together --
-- 'selection' is the only attribute that sets either, so the types rule
-- out states like a selected item absent from the list. Every model is a
-- zipper (before\/focus\/after, before kept reversed so the item nearest
-- the focus is at its head), so a cursor or selection names a /position
-- in the data/ rather than an index that could point past the end.
-- Running off either end matches no case and leaves the model
-- unchanged -- boundaries are handled by the shape of the data, not by
-- comparing against a length.
--
-- Like every other Blink widget, 'list' holds no state of its own between
-- frames: every user-driven change comes back to the app as a complete
-- new model via 'onSelectionChanged', for the app to store and pass back
-- in via 'selection' next frame. The public surface talks in items (@a@,
-- with 'Eq' throughout), never positions -- clients never search their
-- own data.
--
-- = Cursor vs. selection
--
-- Two separate facts are tracked: the /selection/ (which items are
-- selected -- none, one, or many depending on the model) and the /cursor/
-- (the single row the keyboard is currently "on" -- Up\/Down move it,
-- Enter\/Space act on it; exactly one whenever the list is non-empty). In
-- 'SingleSelection'\/'RequiredSelection' the cursor /is/ the selected
-- item. In 'MultiSelection' they are independent. In 'RangeSelection' the
-- cursor is one end of the run; the other end is the anchor.
--
-- = Shift-click
--
-- 'extendTo' (the contiguous-run analogue of 'activate') is reachable
-- from the keyboard via Shift-Up\/Down. It is /not/ wired up to
-- Shift-click: Blink's mouse state carries no ambient modifier reading
-- (only a key /event/ carries 'Blink.Input.modifiers'), so a row's own
-- click handler has no way to tell a plain click from a Shift-click. A
-- platform backend that starts reporting held modifiers on the mouse
-- state could wire this up without changing the model API.
module Blink.Controls.List
  ( -- * Selection models
    SelectionModel (..)
  , EmptySelection (..)
  , ItemState (..)
  , Direction (..)
  , selectedItems
  , cursorItem

    -- ** Single selection
  , SingleSelection
  , unselected
  , selectItem
  , selectAt
  , selectFirst
  , singleSelection
  , singleItems

    -- ** Required (always-one) selection
  , RequiredSelection
  , requireItem
  , requireAt
  , requireFirst
  , requiredItems

    -- ** Multi selection
  , MultiSelection
  , multiSelection
  , multiSelected
  , multiSelectedAt
  , multiItems

    -- ** Contiguous-range selection
  , RangeSelection
  , End (..)
  , noRange
  , rangeAt
  , rangeFrom
  , rangeAtPositions
  , rangeItems
  , rangeEnd

    -- * The list widget
  , ListPart (..)
  , ListConfig (..)
  , defaultListConfig
  , requiredListConfig
  , ListInteraction (..)
  , HasListConfig (..)
  , listBase
  , list
  , requiredList
  , rowsSpacer
  , scrollRowIntoView
  , selection
  , renderItem
  , rowHeight
  , onSelectionChanged
  , onItemActivated
  ) where

import Control.Monad (void, when)
import Data.List (elemIndex, find, findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import Blink.Controls.Control
import Blink.Controls.List.Style (listCursor, listItemStyleKey, listNoCursor, listSelected, listStyleKey, listUnselected)
import Blink.Controls.ScrollBar (ScrollBarPart (..), scrollBar, visibleFraction)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, emptyElement, height, noIntrinsicSize, runElement)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..), insetRect)
import Blink.Input (Key (..), KeyEvent (..), Modifier (Shift))
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill, fitContent)
import Blink.Style (StyleSet (..))
import Blink.View
  ( Effect, View, getBounds, getCursorIndex, getScrollState, getStyleSet, getWheelDelta, isRegionHit
  , requestScrollBy, setCursorIndex, setScrollStateNow, withBounds
  )
import Blink.View.Drawing (withClip)

-- * Selection models

-- | One row, as 'list' draws it: the item itself, whether it's selected,
-- and whether it holds the keyboard cursor.
data ItemState a = ItemState
  { isItem     :: a
  , isSelected :: Bool
  , isCursor   :: Bool
  }

-- | Which way a cursor move or extension goes.
data Direction = Prev | Next
  deriving (Eq, Show)

-- | Everything 'list' needs from a selection model. Deliberately minimal
-- -- anything an app needs beyond this (e.g. "which items are selected")
-- is derived outside the class, via 'selectedItems'\/'cursorItem'.
class SelectionModel sel where
  -- | Every item, in order, with its selected\/cursor flags. What 'list'
  -- draws.
  itemStates :: sel a -> [ItemState a]

  -- | The user acted on @x@: clicked its row, or pressed Enter\/Space
  -- with the cursor on it. An @x@ not in the list leaves the model
  -- unchanged.
  activate :: Eq a => a -> sel a -> sel a

  -- | Plain Up\/Down.
  moveCursor :: Direction -> sel a -> sel a

  -- | Shift-click on @x@ (see the module header for why this never
  -- actually fires from a click yet). Models without a notion of a range
  -- treat it as 'activate'.
  extendTo :: Eq a => a -> sel a -> sel a
  extendTo = activate

  -- | Shift-Up\/Down. Models without a notion of a range treat it as
  -- 'moveCursor'.
  extendCursor :: Direction -> sel a -> sel a
  extendCursor = moveCursor

-- | A 'SelectionModel' that has a value with no items -- every model
-- except 'RequiredSelection', whose whole invariant is that one item is
-- always selected. Kept separate from 'SelectionModel' itself so that
-- invariant is enforced at compile time: generic code needing an empty
-- value (e.g. 'defaultListConfig') simply cannot be called at
-- 'RequiredSelection', rather than compiling and crashing at runtime.
class SelectionModel sel => EmptySelection sel where
  -- | The model with no items.
  emptySelection :: sel a

-- | Items the model currently reports as selected.
selectedItems :: SelectionModel sel => sel a -> [a]
selectedItems = map isItem . filter isSelected . itemStates

-- | The item currently holding the cursor, if the list isn't empty.
cursorItem :: SelectionModel sel => sel a -> Maybe a
cursorItem = fmap isItem . find isCursor . itemStates

-- | Splits @xs@ at the first item equal to @x@: (before, reversed), item,
-- after.
breakAt :: Eq a => a -> [a] -> Maybe ([a], a, [a])
breakAt x xs = case break (== x) xs of
  (b, y : a) -> Just (reverse b, y, a)
  _          -> Nothing

-- | Shifts a (before, cursor, after) zipper one step; 'Nothing' at either
-- end, where the shift has nothing to move into.
shiftZipper :: Direction -> ([a], a, [a]) -> Maybe ([a], a, [a])
shiftZipper Next (b, x, y : a) = Just (x : b, y, a)
shiftZipper Prev (y : b, x, a) = Just (b, y, x : a)
shiftZipper _    _             = Nothing

-- | Every item of a (before, cursor, after) zipper, the cursor marked
-- selected, everything else not.
zipperItemStates :: [a] -> a -> [a] -> [ItemState a]
zipperItemStates b x a = map plain (reverse b) ++ [ItemState x True True] ++ map plain a
  where plain y = ItemState y False False

-- | 'activate' for a plain (before, cursor, after) zipper: re-zips onto
-- whichever item now equals @x@, via @rebuild@, or leaves @s@ unchanged
-- if @x@ isn't in @items@.
activateZipper :: Eq a => (([a], a, [a]) -> sel a) -> a -> [a] -> sel a -> sel a
activateZipper rebuild x items s = maybe s rebuild (breakAt x items)

-- ** Single, optional

-- | At most one item selected; clicking or arrowing selects that item.
data SingleSelection a
  = Unselected [a]
  | Selected [a] a [a]          -- ^ before (reversed), selected, after
  deriving (Eq, Show)

-- | Every item, in order.
singleItems :: SingleSelection a -> [a]
singleItems (Unselected xs)  = xs
singleItems (Selected b x a) = reverse b ++ [x] ++ a

instance EmptySelection SingleSelection where
  emptySelection = Unselected []

instance SelectionModel SingleSelection where
  itemStates (Unselected xs)  = [ItemState x False False | x <- xs]
  itemStates (Selected b x a) = zipperItemStates b x a

  activate x s = activateZipper (\(b, y, a) -> Selected b y a) x (singleItems s) s

  moveCursor _ (Unselected (x : xs)) = Selected [] x xs   -- first key press selects the first item
  moveCursor _ (Unselected [])       = Unselected []
  moveCursor d s@(Selected b x a)    =
    maybe s (\(b', x', a') -> Selected b' x' a') (shiftZipper d (b, x, a))

-- | No selection, every item unselected.
unselected :: [a] -> SingleSelection a
unselected = Unselected

-- | Selects @x@ if it's in the list; 'unselected' otherwise. O(position),
-- needs 'Eq'.
selectItem :: Eq a => a -> [a] -> SingleSelection a
selectItem x xs = maybe (Unselected xs) (\(b, y, a) -> Selected b y a) (breakAt x xs)

-- | Selects the item at position @i@ if it's in range; 'unselected'
-- otherwise. O(i), needs nothing -- use this instead of 'selectItem' when
-- the app already has the position and not the item.
selectAt :: Int -> [a] -> SingleSelection a
selectAt i xs = case splitAt i xs of
  (b, x : a) | i >= 0 -> Selected (reverse b) x a
  _                   -> Unselected xs

-- | Selects the first item, or 'unselected' for an empty list.
selectFirst :: [a] -> SingleSelection a
selectFirst = selectAt 0

-- | Builds from separate items\/selected-item pieces at the app edge; an
-- item absent from the list, or 'Nothing', yields no selection.
singleSelection :: Eq a => [a] -> Maybe a -> SingleSelection a
singleSelection xs = maybe (Unselected xs) (`selectItem` xs)

-- ** Single, required

-- | Exactly one item always selected; an empty list is unrepresentable.
-- Consequently there is no 'EmptySelection' instance -- 'defaultListConfig'
-- cannot supply an initial value for this model, so a caller builds a
-- list with it via 'requiredList'\/'requiredListConfig' instead, which
-- take the starting selection as a mandatory argument.
data RequiredSelection a = RequiredSelection [a] a [a]
  deriving (Eq, Show)

-- | Every item, in order.
requiredItems :: RequiredSelection a -> [a]
requiredItems (RequiredSelection b x a) = reverse b ++ [x] ++ a

instance SelectionModel RequiredSelection where
  itemStates (RequiredSelection b x a) = zipperItemStates b x a

  activate x s = activateZipper (\(b, y, a) -> RequiredSelection b y a) x (requiredItems s) s

  moveCursor d s@(RequiredSelection b x a) =
    maybe s (\(b', x', a') -> RequiredSelection b' x' a') (shiftZipper d (b, x, a))

-- | Requires @x@ in @xs@; 'Nothing' otherwise.
requireItem :: Eq a => a -> [a] -> Maybe (RequiredSelection a)
requireItem x xs = (\(b, y, a) -> RequiredSelection b y a) <$> breakAt x xs

-- | Requires position @i@ in range; 'Nothing' otherwise.
requireAt :: Int -> [a] -> Maybe (RequiredSelection a)
requireAt i xs = case splitAt i xs of
  (b, x : a) | i >= 0 -> Just (RequiredSelection (reverse b) x a)
  _                   -> Nothing

-- | Selects the first item of a list known to be non-empty.
requireFirst :: NonEmpty a -> RequiredSelection a
requireFirst (x :| xs) = RequiredSelection [] x xs

-- ** Multi

-- | Any subset selected; click\/Space toggles an item independently. Has
-- a cursor distinct from the selection.
data MultiSelection a
  = MultiEmpty
  | MultiSelection [(Bool, a)] (Bool, a) [(Bool, a)]   -- ^ before (reversed), cursor, after
  deriving (Eq, Show)

instance EmptySelection MultiSelection where
  emptySelection = MultiEmpty

instance SelectionModel MultiSelection where
  itemStates MultiEmpty = []
  itemStates (MultiSelection b (sx, x) a) =
    map plain (reverse b) ++ [ItemState x sx True] ++ map plain a
    where plain (s, y) = ItemState y s False

  -- Move the cursor to x and flip its own flag.
  activate _ MultiEmpty = MultiEmpty
  activate x s@(MultiSelection b c a) =
    case break ((== x) . snd) (reverse b ++ [c] ++ a) of
      (b', (sx, y) : a') -> MultiSelection (reverse b') (not sx, y) a'
      _                  -> s

  moveCursor _ MultiEmpty                = MultiEmpty
  moveCursor d s@(MultiSelection b x a)  =
    maybe s (\(b', x', a') -> MultiSelection b' x' a') (shiftZipper d (b, x, a))

-- | Nothing selected, cursor on the first item.
multiSelection :: [a] -> MultiSelection a
multiSelection []       = MultiEmpty
multiSelection (x : xs) = MultiSelection [] (False, x) [(False, y) | y <- xs]

-- | Builds from separate items\/selected-subset pieces at the app edge,
-- cursor on the first item.
multiSelected :: Eq a => [a] -> [a] -> MultiSelection a
multiSelected []       _   = MultiEmpty
multiSelected (x : xs) sel = MultiSelection [] (x `elem` sel, x) [(y `elem` sel, y) | y <- xs]

-- | Builds from separate items\/selected-positions pieces at the app
-- edge, cursor on the first item. O(items * positions), needs nothing --
-- use this instead of 'multiSelected' when the app already has
-- positions and not items.
multiSelectedAt :: [Int] -> [a] -> MultiSelection a
multiSelectedAt _   []       = MultiEmpty
multiSelectedAt sel (x : xs) =
  MultiSelection [] (0 `elem` sel, x) [(i `elem` sel, y) | (i, y) <- zip [1 ..] xs]

-- | Every item, in order.
multiItems :: MultiSelection a -> [a]
multiItems MultiEmpty              = []
multiItems (MultiSelection b c a)  = reverse (map snd b) ++ [snd c] ++ map snd a

-- ** Contiguous range

-- | Which end of the selected run the cursor sits on; the other end is
-- the anchor.
data End = AtStart | AtEnd
  deriving (Eq, Show)

-- | Exactly one contiguous run of items selected (never empty once
-- anything has been selected -- 'NonEmpty'), plus which 'End' the cursor
-- is on.
data RangeSelection a
  = NoRange [a]
  | Range [a] (NonEmpty a) [a] End    -- ^ before (reversed), selected run, after, cursor end
  deriving (Eq, Show)

-- | Every item, in order.
rangeItems :: RangeSelection a -> [a]
rangeItems (NoRange xs)      = xs
rangeItems (Range b run a _) = reverse b ++ NE.toList run ++ a

-- | Which end of the run the cursor is on -- see the module header.
-- Distinguishes a single-item run's own two otherwise-identical-looking
-- states (about to grow towards, vs. away from, the anchor on the next
-- extend), which 'itemStates' alone can't.
rangeEnd :: RangeSelection a -> Maybe End
rangeEnd (NoRange _)     = Nothing
rangeEnd (Range _ _ _ e) = Just e

-- | The zipper (before reversed, focus, after) for the item currently
-- holding the cursor -- one end of @run@, per @end@.
cursorZipper :: [a] -> NonEmpty a -> [a] -> End -> ([a], a, [a])
cursorZipper b run a AtEnd   = (reverse (NE.init run) ++ b, NE.last run, a)
cursorZipper b run a AtStart = (b, NE.head run, NE.tail run ++ a)

-- | The run's fixed end: the end @extendTo@\/@extendCursor@ grow away
-- from and shrink towards.
anchorOf :: NonEmpty a -> End -> a
anchorOf run AtStart = NE.last run
anchorOf run AtEnd   = NE.head run

-- | The range spanning the items at positions @ia@ and @ic@ (inclusive),
-- cursor on @ic@'s end. O(length), needs nothing -- use this instead of
-- 'rangeFrom' when the app already has positions and not items.
rangeAtPositions :: Int -> Int -> [a] -> RangeSelection a
rangeAtPositions ia ic xs = case NE.nonEmpty runXs of
  Just ne -> Range (reverse before) ne after end
  Nothing -> NoRange xs
  where
    lo             = min ia ic
    hi             = max ia ic
    (before, rest) = splitAt lo xs
    (runXs, after) = splitAt (hi - lo + 1) rest
    end            = if ic >= ia then AtEnd else AtStart

instance EmptySelection RangeSelection where
  emptySelection = NoRange []

instance SelectionModel RangeSelection where
  itemStates (NoRange xs)        = [ItemState x False False | x <- xs]
  itemStates (Range b run a end) =
    map plain (reverse b) ++ marked ++ map plain a
    where
      runList    = NE.toList run
      cursorIdx  = case end of AtStart -> 0; AtEnd -> length runList - 1
      marked     = [ItemState y True (i == cursorIdx) | (i, y) <- zip [0 ..] runList]
      plain y    = ItemState y False False

  -- Collapse to the single item x.
  activate x s = rangeAt x (rangeItems s)

  -- Collapse to the cursor item, then move it one step.
  moveCursor _ (NoRange [])       = NoRange []
  moveCursor _ (NoRange (x : xs)) = Range [] (x :| []) xs AtEnd
  moveCursor d s@(Range b run a end) =
    let (cb, cx, ca) = cursorZipper b run a end
    in case d of
      Next -> case ca of
        (y : ys) -> Range (cx : cb) (y :| []) ys AtEnd
        []       -> s
      Prev -> case cb of
        (y : ys) -> Range ys (y :| []) (cx : ca) AtEnd
        []       -> s

  -- Run becomes anchor -> x, cursor on x.
  extendTo x (NoRange xs)          = rangeAt x xs
  extendTo x s@(Range _ run _ end) =
    case (elemIndex (anchorOf run end) xs, elemIndex x xs) of
      (Just ia, Just ix) -> rangeAtPositions ia ix xs
      _                  -> s
    where xs = rangeItems s

  -- Grow the run one step away from the anchor; shrink it one step
  -- towards the anchor; crossing the anchor flips 'End' and continues on
  -- the other side.
  extendCursor d s@(NoRange _) = moveCursor d s
  extendCursor d s@(Range b run a end) = case (d, end) of
    (Next, AtEnd) -> case a of
      (y : ys) -> Range b (run <> (y :| [])) ys AtEnd
      []       -> s
    (Prev, AtEnd) -> case NE.nonEmpty (NE.init run) of
      Just run' -> Range b run' (NE.last run : a) AtEnd
      Nothing   -> extendCursor Prev (Range b run a AtStart)
    (Prev, AtStart) -> case b of
      (y : ys) -> Range ys (y `NE.cons` run) a AtStart
      []       -> s
    (Next, AtStart) -> case NE.nonEmpty (NE.tail run) of
      Just run' -> Range (NE.head run : b) run' a AtStart
      Nothing   -> extendCursor Next (Range b run a AtEnd)

-- | No selection.
noRange :: [a] -> RangeSelection a
noRange = NoRange

-- | Selects the single-item run @{x}@ if @x@ is in the list; 'noRange'
-- otherwise.
rangeAt :: Eq a => a -> [a] -> RangeSelection a
rangeAt x xs = maybe (NoRange xs) (\(b, y, a) -> Range b (y :| []) a AtEnd) (breakAt x xs)

-- | The run from @anchor@ to @cursor@ (inclusive) if both are in the
-- list; 'noRange' otherwise.
rangeFrom :: Eq a => a -> a -> [a] -> RangeSelection a
rangeFrom anchor cursor xs = case (elemIndex anchor xs, elemIndex cursor xs) of
  (Just ia, Just ic) -> rangeAtPositions ia ic xs
  _                   -> NoRange xs

-- * The list widget

-- | Identifies one part of a 'list' for the purpose of building element
-- ids: the list's own root, one of its rows (tagged by the row's own item
-- value rather than its position in the list -- so
-- reordering\/inserting\/removing items elsewhere in the list never
-- disturbs another row's hover\/focus\/capture state), or a part of the
-- vertical scrollbar composited in once the rows overflow the list's own
-- bounds (see 'listBase'). The same pattern
-- 'Blink.Controls.ToggleGroup.ToggleGroupPart'\/'Blink.Controls.ScrollBar.ScrollBarPart'
-- already use: every part's id is built from one @mkId@ function (see
-- 'listBase'), so the root, its rows, and its scrollbar's own parts are
-- visibly related and can't collide, rather than being independently
-- chosen, unrelated ids.
data ListPart a
  = List
  | ListItem a
  | ListScrollBar ScrollBarPart
  deriving (Eq, Ord, Show)

-- | Every capability 'list' resolves: the wrapped 'ControlConfig'\/
-- 'Layout', the whole model (items and selection together), how a row
-- draws its item, the fixed height every row is drawn at, its
-- reactions, and an optional fixed header rendered above the scrollable
-- rows (see 'listBase') -- never set directly by a 'list' caller, only
-- by a wrapper built on 'listBase' (e.g. a table deriving one from its
-- own column headers).
data ListConfig sel e msg a = ListConfig
  { lcControl            :: ControlConfig e msg
  , lcLayout             :: Layout
  , lcSelection          :: sel a
  , lcRenderItem         :: ItemState a -> Element e msg
  , lcRowHeight          :: Double
  , lcOnSelectionChanged :: [sel a -> [Effect e msg]]
  , lcOnItemActivated    :: [a -> [Effect e msg]]
  , lcHeader             :: Maybe (Element e msg)
  }

instance HasControlConfig e msg (ListConfig sel e msg a) where
  overControl attr = Attribute (\c -> c { lcControl = runAttribute (overControl attr) (lcControl c) })

instance HasLayoutConfig (ListConfig sel e msg a) where
  overLayout attr = Attribute (\c -> c { lcLayout = runAttribute attr (lcLayout c) })

-- | Every control built on an embedded 'ListConfig' -- 'list' itself, and
-- the table\/tree\/tree-table wrappers built on 'listBase' -- implements
-- this so 'selection'\/'renderItem'\/'rowHeight'\/'onSelectionChanged'\/
-- 'onItemActivated' work on their own attribute lists directly, the same
-- way 'Blink.Controls.Button.HasButtonConfig' lets
-- 'Blink.Controls.Button.onActivated' work on any button-shaped control.
class HasListConfig sel e msg a cfg | cfg -> sel e msg a where
  overList :: Attribute (ListConfig sel e msg a) -> Attribute cfg

instance HasListConfig sel e msg a (ListConfig sel e msg a) where
  overList = id

-- | The row height 'list' uses when the caller sets no 'rowHeight' of its
-- own -- tall enough for a single line of body text plus
-- 'Blink.Controls.Style.flatRowMetrics' chrome at a typical UI font size,
-- the same weight 'listItemStyleKey' already styles rows with. Every real
-- font differs, so a caller whose rows clip or float in extra space
-- should set 'rowHeight' explicitly rather than lean on this guess.
defaultRowHeight :: Double
defaultRowHeight = 32

-- | How many rows a single mouse-wheel notch scrolls -- see @applyWheel@
-- in 'virtualizedRows'.
wheelRowsPerNotch :: Double
wheelRowsPerNotch = 3

-- | 'defaultControlConfig' (styled via @Class \"list\"@), filling its
-- parent's width and sizing its height to its own rows, no per-row render
-- (draws nothing), a 32px row height, and no reactions, starting from the
-- given selection.
baseListConfig :: sel a -> ListConfig sel e msg a
baseListConfig s0 = ListConfig
  { lcControl            = defaultControlConfig { ccStyleKey = listStyleKey }
  , lcLayout             = Layout fill fitContent TopLeft
  , lcSelection          = s0
  , lcRenderItem         = const emptyElement
  , lcRowHeight          = defaultRowHeight
  , lcOnSelectionChanged = []
  , lcOnItemActivated    = []
  , lcHeader             = Nothing
  }

-- | 'defaultControlConfig' (styled via @Class \"list\"@), filling its
-- parent's width and sizing its height to its own rows, 'emptySelection',
-- no per-row render (draws nothing), a 32px row height, and no reactions.
-- Every 'SelectionModel' except 'RequiredSelection' has an empty value to
-- start from this way -- see 'requiredListConfig' for that model instead.
defaultListConfig :: EmptySelection sel => ListConfig sel e msg a
defaultListConfig = baseListConfig emptySelection

-- | The same starting config 'defaultListConfig' builds, for
-- 'RequiredSelection', which has no empty value to start from -- @sel0@
-- is the initial selection instead, required up front rather than
-- defaulted.
requiredListConfig :: RequiredSelection a -> ListConfig RequiredSelection e msg a
requiredListConfig = baseListConfig

-- | The whole model -- items and selection together. The only way to set
-- either; @sel@ is inferred from this argument.
selection :: HasListConfig sel e msg a cfg => sel a -> Attribute cfg
selection s = overList (Attribute (\c -> c { lcSelection = s }))

-- | How a row draws its item; receives the row's selected\/cursor flags
-- for styling.
renderItem :: HasListConfig sel e msg a cfg => (ItemState a -> Element e msg) -> Attribute cfg
renderItem f = overList (Attribute (\c -> c { lcRenderItem = f }))

-- | The height every row is drawn at, overriding whatever height
-- 'renderItem'\/'s own element requests. Defaults to 32px.
rowHeight :: HasListConfig sel e msg a cfg => Double -> Attribute cfg
rowHeight h = overList (Attribute (\c -> c { lcRowHeight = h }))

-- | Reacts whenever a user-driven change actually moves the model to a
-- new value, with the complete new model. Without it the list still
-- reflects keyboard\/click interaction locally within the frame (see
-- 'list'), but the app never learns of it, so next frame's 'selection'
-- puts it right back -- the list is then read-only in practice.
onSelectionChanged :: HasListConfig sel e msg a cfg => (sel a -> [Effect e msg]) -> Attribute cfg
onSelectionChanged h = overList (Attribute (\c -> c { lcOnSelectionChanged = lcOnSelectionChanged c ++ [h] }))

-- | Reacts when the user acts on a specific item: a click on its row, or
-- Enter\/Space with the cursor on it. Fires whether or not that action
-- also changed the selection (a click on an already-selected row still
-- fires this) -- "the user chose this, act on it", distinct from
-- 'onSelectionChanged' keeping selection state in sync. Arrowing never
-- fires this.
onItemActivated :: HasListConfig sel e msg a cfg => (a -> [Effect e msg]) -> Attribute cfg
onItemActivated h = overList (Attribute (\c -> c { lcOnItemActivated = lcOnItemActivated c ++ [h] }))

-- | What 'listBase' reports back: the underlying 'control' call's own
-- 'ControlInteraction' (so a control built on top of 'listBase' -- e.g. a
-- tree, wanting Left\/Right for expand\/collapse -- can inspect keys
-- 'listBase' itself didn't consume, via 'ciKeysPressed'), the selection
-- model resolved against this frame's /keyboard/ input, the items
-- activated by keyboard (Enter\/Space) this frame, and the chrome-inset
-- viewport height 'scrollRowIntoView' expects, computed inside the
-- 'control' call's own content callback where that inset applies. A click's
-- resulting change is still only ever reported via 'onSelectionChanged'\/
-- 'onItemActivated' -- each row is its own independently-clicked
-- 'control', so its outcome isn't available to reflect here.
data ListInteraction sel e msg a = ListInteraction
  { liControl        :: ControlInteraction e msg
  , liSelection      :: sel a
  , liActivated      :: [a]
  , liViewportHeight :: Double
  }

-- | Renders @itemCount@ fixed-@rowHeight@ rows inside the current
-- bounds: plain if they already fit, otherwise a virtualized, scrollable
-- viewport composited with a vertical scrollbar (an 'hBox' of the two,
-- the L-shaped arrangement 'Blink.Controls.ScrollBar.scrollBar's own
-- module header points to as the pattern any real scrolling viewport
-- uses). Only the rows intersecting the viewport are ever requested from
-- @renderRows@, so a row off-screen is never built or hit-tested; a
-- spacer above and below keeps the total height at @itemCount * rowHeight@
-- regardless of which rows are currently skipped, so the scrollbar's
-- thumb geometry never shifts as the visible set changes.
--
-- @renderRows viewportHeight lo hi@ must return exactly @hi - lo@
-- elements, the rows at positions @[lo .. hi - 1]@, each already laid
-- out for @rowHeight@ and aware of @viewportHeight@ (needed for e.g. a
-- click handler's own 'scrollRowIntoView' call) -- the plain,
-- fits-without-scrolling path passes @itemCount * rowHeight@ itself as
-- @viewportHeight@, which guarantees 'scrollRowIntoView' always no-ops
-- there, correctly.
virtualizedRows
  :: Ord e
  => (ScrollBarPart -> e)
  -> Int
  -> Double
  -> (Double -> Int -> Int -> [Element e msg])
  -> View e msg ()
virtualizedRows scrollBarTag itemCount rh renderRows = do
  bounds <- getBounds
  let viewportHeight = rectHeight bounds
  if contentHeight > viewportHeight
    then runElement (scrollableRows viewportHeight)
    else runElement (vBox [children (renderRows contentHeight 0 itemCount)])
  where
    contentHeight = fromIntegral itemCount * rh
    listScrollEid = scrollBarTag ScrollBar

    scrollableRows viewportHeight = hBox
      [ children
          [ elementWithLayout (Layout fill fill TopLeft) (clippedRows viewportHeight)
          , scrollBar scrollBarTag [ height fill, visibleFraction (viewportHeight / contentHeight) ]
          ]
      ]

    -- 'withClip' captures /this/ bounds (the viewport's own, not yet
    -- offset) as the clip region before the rows move within it, the
    -- same ordering 'Blink.Layout.Box.hBox'\/'vBox' use for their own
    -- children.
    clippedRows viewportHeight = do
      applyWheel viewportHeight
      bounds     <- getBounds
      scrollFrac <- getScrollState listScrollEid
      let offsetY    = scrollFrac * (contentHeight - viewportHeight)
          rowsBounds = bounds { rectY = rectY bounds - offsetY, rectHeight = contentHeight }
      withClip $ withBounds rowsBounds (runElement (visibleRows offsetY viewportHeight))

    -- Mouse-wheel scrolling: moves the position by a fixed number of rows
    -- per wheel notch while the pointer is over the (unscrolled) viewport
    -- -- 'isRegionHit' is checked against 'getBounds' as it stands here,
    -- before 'clippedRows' offsets it for the content. Deferred via
    -- 'requestScrollBy', like every other user gesture (see
    -- 'Blink.View.Scroll.setScrollStateNow' for why this isn't applied
    -- immediately the way a keyboard/click-driven scroll correction is).
    applyWheel viewportHeight = do
      wheel <- getWheelDelta
      let maxOffset = contentHeight - viewportHeight
      when (wheel /= 0 && maxOffset > 0) $ do
        over <- isRegionHit
        when over $ requestScrollBy listScrollEid (wheel * wheelStepPx / maxOffset)

    wheelStepPx = rh * wheelRowsPerNotch

    visibleRows offsetY viewportHeight =
      vBox [children (spacer topSkipped : renderRows viewportHeight loIdx hiIdx ++ [spacer bottomSkipped])]
      where
        loIdx         = max 0 (floor (offsetY / rh))
        hiIdx         = min itemCount (ceiling ((offsetY + viewportHeight) / rh))
        topSkipped    = fromIntegral loIdx * rh
        bottomSkipped = fromIntegral (itemCount - hiIdx) * rh
        spacer h      = elementWithLayout (Layout fill (exactly h) TopLeft) (pure ())

-- | Everything 'list' does, minus being an 'Element': one 'Focusable'
-- stop (unless overridden via 'Blink.Controls.Control.focusPolicy')
-- whose rows are never tab stops. Up\/Down move the cursor, Shift-Up\/Down
-- extend a range, Enter\/Space act on the cursor, a click on a row
-- activates it. Every change is computed against the model exactly as
-- passed in via 'lcSelection' this frame, never against any
-- locally-derived value -- rendering, keyboard handling, and click
-- handling all read it as given, and 'lcOnSelectionChanged'\/
-- 'lcOnItemActivated' report the result for the app to store and pass
-- back in next frame.
--
-- @mkId@ builds every part's element id from a 'ListPart': the list's own
-- root id from 'List', and each row's id from 'ListItem' applied to the
-- row's own item value -- so the caller never writes a per-row id by
-- hand, and can't accidentally give the root and a row the same id (see
-- 'ListPart'). Any 'Blink.Controls.Control.elementId' attribute set on
-- 'lcControl' is discarded in favour of @mkId List@, the same as
-- 'Blink.Controls.ToggleGroup.toggleButtonGroup'.
--
-- The shape every list-like control ('list', and
-- table\/tree\/tree-table wrappers built on top of it) resolves from.
listBase
  :: (Ord e, Eq a, SelectionModel sel, Eq (sel a))
  => (ListPart a -> e)
  -> ListConfig sel e msg a
  -> View e msg (ListInteraction sel e msg a)
listBase mkId cfg = do
  r             <- control ccfg
  (m, styleSet) <- getStyleSet (ccStyleKey (lcControl cfg))
  outer         <- getBounds
  let (finalModel, activated) = keyboardResult (ciKeysPressed r)
      viewportHeight           = rectHeight (insetRect (chromeInsets m (styleBase styleSet)) outer) - headerHeight
  pure (ListInteraction r finalModel activated viewportHeight)
  where
    s0   = lcSelection cfg

    itemCount = length (itemStates s0)

    -- The fixed header (see 'lcHeader') eats into the rows' own viewport
    -- the same way chrome does -- 'liViewportHeight' has to account for
    -- it too, alongside 'ccContent's own 'composite', which actually
    -- lays the header out above the rows.
    headerHeight = maybe 0 (const (lcRowHeight cfg)) (lcHeader cfg)

    scrollBarTag = mkId . ListScrollBar

    fireSelectionChanged s = when (s /= s0) $ runHandlers (lcOnSelectionChanged cfg) s
    fireItemActivated      = runHandlers (lcOnItemActivated cfg)

    -- The selection model and activated items that this frame's keyboard
    -- input (if any) produces, starting from 'lcSelection' as given.
    -- Called both from 'ccContent' (to fire the reactions and adjust
    -- scroll, exactly as before this function existed) and again from
    -- 'listBase' itself, on the same 'ciKeysPressed' value, purely to
    -- report the result via 'ListInteraction' -- a click's own outcome
    -- can't be recovered the same way (see 'ListInteraction'), but a
    -- keyboard change never needs anything a row's own nested 'control'
    -- computed, so recomputing this pure fold a second time is exact and
    -- side-effect-free.
    keyboardResult = foldl stepKey (s0, [])

    ccfg = (lcControl cfg)
      { ccElementId = Just (mkId List)
      , ccContent = \ci -> do
          let (finalModel, activated) = keyboardResult (ciKeysPressed ci)
          fireSelectionChanged finalModel
          mapM_ fireItemActivated activated
          case lcHeader cfg of
            Nothing       -> rowsArea finalModel
            Just headerEl -> runElement $ vBox
              [ children
                  [ elementWithLayout (Layout fill (exactly (lcRowHeight cfg)) TopLeft) (runElement headerEl)
                  , elementWithLayout (Layout fill fill TopLeft) (rowsArea finalModel)
                  ]
              ]
      }

    -- Keyboard-driven scroll adjustment plus the rows themselves --
    -- composed via a real 'vBox' rather than manual bounds math when a
    -- header is present (see 'lcHeader'), so 'getBounds' here already
    -- reflects the space left after it, the same way it already does
    -- for 'virtualizedRows's own hBox split.
    rowsArea finalModel = do
      trackCursor finalModel
      renderViewport

    -- Scrolls the cursor into view whenever its row index differs from
    -- the last one recorded for this list -- covers a keyboard move and
    -- also a cursor that jumped for a reason outside this frame's own
    -- handling (e.g. the caller re-sorting its items). Ignores the very
    -- first observation so mounting doesn't force an initial scroll.
    trackCursor finalModel = do
      let mIdx = findIndex isCursor (itemStates finalModel)
      lastIdx <- getCursorIndex (mkId List)
      when (mIdx /= lastIdx) $ do
        case lastIdx of
          Just _  -> do
            bounds <- getBounds
            mapM_ (scrollRowIntoView mkId cfg itemCount (rectHeight bounds)) mIdx
          Nothing -> pure ()
        setCursorIndex (mkId List) mIdx

    -- The row-building side of 'virtualizedRows': slices the item states
    -- for whichever range it asks for, and builds each into a row at the
    -- viewport height it's given.
    renderViewport = virtualizedRows scrollBarTag itemCount (lcRowHeight cfg) renderRows
      where renderRows vh lo hi = zipWith (row vh) [lo ..] (take (hi - lo) (drop lo (itemStates s0)))

    stepKey (s, activated) ev = case key ev of
      KeyUp                        -> (move Prev, activated)
      KeyDown                      -> (move Next, activated)
      KeyReturn | not (keyRepeat ev) -> activateCursor
      KeySpace  | not (keyRepeat ev) -> activateCursor
      _                             -> (s, activated)
      where
        move d
          | Shift `elem` modifiers ev = extendCursor d s
          | otherwise                 = moveCursor d s
        activateCursor = case cursorItem s of
          Just x  -> (activate x s, activated ++ [x])
          Nothing -> (s, activated)

    -- A click always lands on a row that's at least partly visible (an
    -- off-screen, virtualised-out row is never built, so never hit-tested
    -- -- see 'virtualizedRows'), but that row can still be only partially
    -- within the viewport, straddling its top or bottom edge. Scrolling
    -- it fully into view on the same click, via 'scrollRowIntoView',
    -- matches keyboard navigation already doing the same for the cursor.
    rowActivated viewportHeight idx item = do
      let s' = activate item s0
      fireSelectionChanged s'
      fireItemActivated item
      scrollRowIntoView mkId cfg itemCount viewportHeight idx

    rowStates st = Set.fromList
      [ if isSelected st then listSelected else listUnselected
      , if isCursor   st then listCursor   else listNoCursor
      ]

    row viewportHeight idx st = Element
      { elLayout  = Layout fill (exactly (lcRowHeight cfg)) TopLeft
      , elMeasure = noIntrinsicSize
      , elRun     = void $ control defaultControlConfig
          { ccElementId    = Just (mkId (ListItem (isItem st)))
          , ccStyleKey     = listItemStyleKey
          , ccFocusPolicy  = NotFocusable
          , ccActiveStates = rowStates st
          , ccContent      = \rci -> do
              when (ciClicked rci) (rowActivated viewportHeight idx (isItem st))
              runElement (lcRenderItem cfg st)
          }
      }

-- | A list built on 'listBase', exposing exactly its own attributes
-- ('selection', 'renderItem', 'rowHeight', 'onSelectionChanged',
-- 'onItemActivated') and discarding the 'ListInteraction' it reports back
-- -- the same relationship 'Blink.Controls.Button.button' has to
-- 'Blink.Controls.Button.buttonBase'. Table\/tree\/tree-table wrappers
-- built on 'listBase' expose a different, narrower set of attributes of
-- their own instead.
--
-- Needs 'EmptySelection' to seed 'defaultListConfig' before 'selection'
-- (if given) overrides it, so this can't be called at 'RequiredSelection'
-- -- use 'requiredList' for that model instead.
list
  :: (Ord e, Eq a, SelectionModel sel, EmptySelection sel, Eq (sel a))
  => (ListPart a -> e)
  -> [Attribute (ListConfig sel e msg a)]
  -> Element e msg
list mkId attrs = listFrom mkId (resolve defaultListConfig attrs)

-- | 'list' for 'RequiredSelection', which has no 'EmptySelection' instance
-- to seed a default config with -- @sel0@ is the starting selection
-- instead, required up front rather than defaulted.
requiredList
  :: (Ord e, Eq a)
  => (ListPart a -> e)
  -> RequiredSelection a
  -> [Attribute (ListConfig RequiredSelection e msg a)]
  -> Element e msg
requiredList mkId sel0 attrs = listFrom mkId (resolve (requiredListConfig sel0) attrs)

listFrom
  :: (Ord e, Eq a, SelectionModel sel, Eq (sel a))
  => (ListPart a -> e)
  -> ListConfig sel e msg a
  -> Element e msg
listFrom mkId cfg = Element
  { elLayout  = lcLayout cfg
  , elMeasure = measureChrome (ccStyleKey (lcControl cfg)) (rowsSpacer cfg (itemStates (lcSelection cfg)))
  , elRun     = void (listBase mkId cfg)
  }

-- | Brings row @idx@ (0-based, into a flat list of @itemCount@ rows at
-- @cfg@'s own 'lcRowHeight') into a @viewportHeight@-tall viewport --
-- top-aligned if it currently falls above, bottom-aligned if below. A
-- no-op when the content already fits without scrolling, or the row is
-- already fully in view. Takes effect immediately, via
-- 'Blink.View.setScrollStateNow' -- this frame's own rendering (whichever
-- of 'listBase's callers runs after this one in the same pass) sees the
-- corrected position, rather than needing a further frame to catch up, as
-- a deferred 'Blink.View.requestScrollTo' would. That's correct here
-- because scrolling a row into view is never itself carrying user intent
-- forward the way e.g. a focus change is -- it's a pure function of this
-- frame's own item order, cursor position, and viewport, recomputed fresh
-- every time regardless of what the persisted scroll position happened to
-- be a moment ago.
--
-- 'listBase' itself uses this for both a keyboard-moved cursor and a
-- click landing on a row not yet fully in view; exported so a control
-- built on top (e.g. 'Blink.Controls.Tree.tree', for its own Left\/Right-
-- driven cursor moves) can keep its own moves in view the same way.
--
-- @viewportHeight@ is always the caller's own current bounds height
-- ('Blink.View.getBounds' read at the right point), never fetched here
-- -- a row's own nested 'control' sees only its own, much smaller,
-- bounds, not its list's, so the read has to happen at the right level
-- and be passed down.
scrollRowIntoView :: Ord e => (ListPart a -> e) -> ListConfig sel e msg a -> Int -> Double -> Int -> View e msg ()
scrollRowIntoView mkId cfg itemCount viewportHeight idx = when (maxOffset > 0) $ do
  scrollFrac <- getScrollState listScrollEid
  let rh        = lcRowHeight cfg
      rowTop    = fromIntegral idx * rh
      rowBottom = rowTop + rh
      offsetY   = scrollFrac * maxOffset
      newFrac
        | rowTop < offsetY                    = Just (rowTop / maxOffset)
        | rowBottom > offsetY + viewportHeight = Just ((rowBottom - viewportHeight) / maxOffset)
        | otherwise                            = Nothing
  mapM_ (setScrollStateNow listScrollEid) newFrac
  where
    listScrollEid = mkId (ListScrollBar ScrollBar)
    contentHeight = fromIntegral itemCount * lcRowHeight cfg
    maxOffset     = contentHeight - viewportHeight

-- | A single fixed-height stand-in for every current row stacked
-- vertically, used only to measure a list-like control's own height --
-- the virtualised rendering inside 'listBase' already substitutes a
-- plain spacer for a skipped row for exactly the same reason: since
-- every row is fixed at 'lcRowHeight', only the total count times that
-- height matters for measurement, never any row's actual content.
rowsSpacer :: ListConfig sel e msg a -> [ItemState a] -> Element e msg
rowsSpacer cfg states =
  elementWithLayout (Layout fill (exactly (fromIntegral (length states) * lcRowHeight cfg)) TopLeft) (pure ())
