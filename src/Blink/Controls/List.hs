{-# LANGUAGE FlexibleInstances #-}
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
  , ItemState (..)
  , Direction (..)
  , selectedItems
  , cursorItem

    -- ** Single selection
  , SingleSelection (..)
  , unselected
  , selectItem
  , selectAt
  , selectFirst
  , singleSelection

    -- ** Required (always-one) selection
  , RequiredSelection (..)
  , requireItem
  , requireAt
  , requireFirst

    -- ** Multi selection
  , MultiSelection (..)
  , multiSelection
  , multiSelected

    -- ** Contiguous-range selection
  , RangeSelection (..)
  , End (..)
  , noRange
  , rangeAt
  , rangeFrom

    -- * The list widget
  , ListPart (..)
  , ListConfig (..)
  , defaultListConfig
  , list
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
import Blink.Geometry (Alignment (TopLeft), Rectangle (..))
import Blink.Input (Key (..), KeyEvent (..), Modifier (Shift))
import Blink.Layout.Box (children, hBox, vBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill, fitContent)
import Blink.View (Out, getBounds, getScrollState, requestScrollTo, withBounds)
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
  -- | The model with no items.
  emptySelection :: sel a

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

-- ** Single, optional

-- | At most one item selected; clicking or arrowing selects that item.
data SingleSelection a
  = Unselected [a]
  | Selected [a] a [a]          -- ^ before (reversed), selected, after
  deriving (Eq, Show)

singleItems :: SingleSelection a -> [a]
singleItems (Unselected xs)  = xs
singleItems (Selected b x a) = reverse b ++ [x] ++ a

instance SelectionModel SingleSelection where
  emptySelection = Unselected []

  itemStates (Unselected xs)  = [ItemState x False False | x <- xs]
  itemStates (Selected b x a) =
    map plain (reverse b) ++ [ItemState x True True] ++ map plain a
    where plain y = ItemState y False False

  activate x s = maybe s (\(b, y, a) -> Selected b y a) (breakAt x (singleItems s))

  moveCursor Next (Selected b x (y : a)) = Selected (x : b) y a
  moveCursor Prev (Selected (y : b) x a) = Selected b y (x : a)
  moveCursor _    (Unselected (x : xs))  = Selected [] x xs   -- first key press selects the first item
  moveCursor _    s                      = s

-- | No selection, every item unselected.
unselected :: [a] -> SingleSelection a
unselected = Unselected

-- | Selects @x@ if it's in the list; 'Unselected' otherwise. O(position),
-- needs 'Eq'.
selectItem :: Eq a => a -> [a] -> SingleSelection a
selectItem x xs = maybe (Unselected xs) (\(b, y, a) -> Selected b y a) (breakAt x xs)

-- | Selects the item at position @i@ if it's in range; 'Unselected'
-- otherwise. O(i), needs nothing -- use this instead of 'selectItem' when
-- the app already has the position and not the item.
selectAt :: Int -> [a] -> SingleSelection a
selectAt i xs = case splitAt i xs of
  (b, x : a) | i >= 0 -> Selected (reverse b) x a
  _                   -> Unselected xs

-- | Selects the first item, or 'Unselected' for an empty list.
selectFirst :: [a] -> SingleSelection a
selectFirst = selectAt 0

-- | Builds from separate items\/selected-item pieces at the app edge; an
-- item absent from the list, or 'Nothing', yields no selection.
singleSelection :: Eq a => [a] -> Maybe a -> SingleSelection a
singleSelection xs = maybe (Unselected xs) (`selectItem` xs)

-- ** Single, required

-- | Exactly one item always selected; an empty list is unrepresentable.
-- Consequently there is no 'emptySelection' -- a caller of 'list' with
-- this model must always pass 'selection' every frame; 'defaultListConfig'
-- cannot supply one.
data RequiredSelection a = RequiredSelection [a] a [a]
  deriving (Eq, Show)

requiredItems :: RequiredSelection a -> [a]
requiredItems (RequiredSelection b x a) = reverse b ++ [x] ++ a

instance SelectionModel RequiredSelection where
  emptySelection = error "RequiredSelection has no empty value -- pass `selection` every frame"

  itemStates (RequiredSelection b x a) =
    map plain (reverse b) ++ [ItemState x True True] ++ map plain a
    where plain y = ItemState y False False

  activate x s = maybe s (\(b, y, a) -> RequiredSelection b y a) (breakAt x (requiredItems s))

  moveCursor Next (RequiredSelection b x (y : a)) = RequiredSelection (x : b) y a
  moveCursor Prev (RequiredSelection (y : b) x a) = RequiredSelection b y (x : a)
  moveCursor _    s                               = s

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

instance SelectionModel MultiSelection where
  emptySelection = MultiEmpty

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

  moveCursor Next (MultiSelection b x (y : a)) = MultiSelection (x : b) y a
  moveCursor Prev (MultiSelection (y : b) x a) = MultiSelection b y (x : a)
  moveCursor _    s                            = s

-- | Nothing selected, cursor on the first item.
multiSelection :: [a] -> MultiSelection a
multiSelection []       = MultiEmpty
multiSelection (x : xs) = MultiSelection [] (False, x) [(False, y) | y <- xs]

-- | Builds from separate items\/selected-subset pieces at the app edge,
-- cursor on the first item.
multiSelected :: Eq a => [a] -> [a] -> MultiSelection a
multiSelected []       _   = MultiEmpty
multiSelected (x : xs) sel = MultiSelection [] (x `elem` sel, x) [(y `elem` sel, y) | y <- xs]

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

rangeItems :: RangeSelection a -> [a]
rangeItems (NoRange xs)      = xs
rangeItems (Range b run a _) = reverse b ++ NE.toList run ++ a

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
-- cursor on @ic@'s end.
buildRange :: Int -> Int -> [a] -> RangeSelection a
buildRange ia ic xs = case NE.nonEmpty runXs of
  Just ne -> Range (reverse before) ne after end
  Nothing -> NoRange xs
  where
    lo             = min ia ic
    hi             = max ia ic
    (before, rest) = splitAt lo xs
    (runXs, after) = splitAt (hi - lo + 1) rest
    end            = if ic >= ia then AtEnd else AtStart

instance SelectionModel RangeSelection where
  emptySelection = NoRange []

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
      (Just ia, Just ix) -> buildRange ia ix xs
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

-- | Selects the single-item run @{x}@ if @x@ is in the list; 'NoRange'
-- otherwise.
rangeAt :: Eq a => a -> [a] -> RangeSelection a
rangeAt x xs = maybe (NoRange xs) (\(b, y, a) -> Range b (y :| []) a AtEnd) (breakAt x xs)

-- | The run from @anchor@ to @cursor@ (inclusive) if both are in the
-- list; 'NoRange' otherwise.
rangeFrom :: Eq a => a -> a -> [a] -> RangeSelection a
rangeFrom anchor cursor xs = case (elemIndex anchor xs, elemIndex cursor xs) of
  (Just ia, Just ic) -> buildRange ia ic xs
  _                   -> NoRange xs

-- * The list widget

-- | Identifies one part of a 'list' for the purpose of building element
-- ids: the list's own root, one of its rows (tagged by the row's own item
-- value rather than its position in the list -- so
-- reordering\/inserting\/removing items elsewhere in the list never
-- disturbs another row's hover\/focus\/capture state), or a part of the
-- vertical scrollbar composited in once the rows overflow the list's own
-- bounds (see 'list'). The same pattern
-- 'Blink.Controls.ToggleGroup.ToggleGroupPart'\/'Blink.Controls.ScrollBar.ScrollBarPart'
-- already use: every part's id is minted from one @tag@ function (see
-- 'list'), so the root, its rows, and its scrollbar's own parts are
-- visibly related and can't collide, rather than being independently
-- chosen, unrelated ids.
data ListPart a
  = List
  | ListItem a
  | ListScrollBar ScrollBarPart
  deriving (Eq, Ord, Show)

-- | Every capability 'list' resolves: the wrapped 'ControlConfig'\/
-- 'Layout', the whole model (items and selection together), how a row
-- draws its item, the fixed height every row is drawn at, and its
-- reactions.
data ListConfig sel e msg a = ListConfig
  { lcControl            :: ControlConfig e msg
  , lcLayout             :: Layout
  , lcSelection          :: sel a
  , lcRenderItem         :: ItemState a -> Element e msg
  , lcRowHeight          :: Double
  , lcOnSelectionChanged :: [sel a -> [Out e msg]]
  , lcOnItemActivated    :: [a -> [Out e msg]]
  }

instance HasControlConfig e msg (ListConfig sel e msg a) where
  overControl attr = Attribute (\c -> c { lcControl = runAttribute (overControl attr) (lcControl c) })

instance HasLayoutConfig (ListConfig sel e msg a) where
  overLayout attr = Attribute (\c -> c { lcLayout = runAttribute attr (lcLayout c) })

-- | The row height 'list' uses when the caller sets no 'rowHeight' of its
-- own -- tall enough for a single line of body text plus
-- 'Blink.Controls.Style.flatRowMetrics' chrome at a typical UI font size,
-- the same weight 'listItemStyleKey' already styles rows with. Every real
-- font differs, so a caller whose rows clip or float in extra space
-- should set 'rowHeight' explicitly rather than lean on this guess.
defaultRowHeight :: Double
defaultRowHeight = 32

-- | 'defaultControlConfig' (styled via @Class \"list\"@), filling its
-- parent's width and sizing its height to its own rows, 'emptySelection',
-- no per-row render (draws nothing), a 32px row height, and no reactions.
defaultListConfig :: SelectionModel sel => ListConfig sel e msg a
defaultListConfig = ListConfig
  { lcControl            = defaultControlConfig { ccStyleKey = listStyleKey }
  , lcLayout             = Layout fill fitContent TopLeft
  , lcSelection          = emptySelection
  , lcRenderItem         = const emptyElement
  , lcRowHeight          = defaultRowHeight
  , lcOnSelectionChanged = []
  , lcOnItemActivated    = []
  }

-- | The whole model -- items and selection together. The only way to set
-- either; @sel@ is inferred from this argument.
selection :: sel a -> Attribute (ListConfig sel e msg a)
selection s = Attribute (\c -> c { lcSelection = s })

-- | How a row draws its item; receives the row's selected\/cursor flags
-- for styling.
renderItem :: (ItemState a -> Element e msg) -> Attribute (ListConfig sel e msg a)
renderItem f = Attribute (\c -> c { lcRenderItem = f })

-- | The height every row is drawn at, overriding whatever height
-- 'renderItem'\/'s own element requests. Defaults to 32px.
rowHeight :: Double -> Attribute (ListConfig sel e msg a)
rowHeight h = Attribute (\c -> c { lcRowHeight = h })

-- | Reacts whenever a user-driven change actually moves the model to a
-- new value, with the complete new model. Without it the list still
-- reflects keyboard\/click interaction locally within the frame (see
-- 'list'), but the app never learns of it, so next frame's 'selection'
-- puts it right back -- the list is then read-only in practice.
onSelectionChanged :: (sel a -> [Out e msg]) -> Attribute (ListConfig sel e msg a)
onSelectionChanged h = Attribute (\c -> c { lcOnSelectionChanged = lcOnSelectionChanged c ++ [h] })

-- | Reacts when the user acts on a specific item: a click on its row, or
-- Enter\/Space with the cursor on it. Fires whether or not that action
-- also changed the selection (a click on an already-selected row still
-- fires this) -- "the user chose this, act on it", distinct from
-- 'onSelectionChanged' keeping selection state in sync. Arrowing never
-- fires this.
onItemActivated :: (a -> [Out e msg]) -> Attribute (ListConfig sel e msg a)
onItemActivated h = Attribute (\c -> c { lcOnItemActivated = lcOnItemActivated c ++ [h] })

-- | A list: one 'Focusable' stop (unless overridden via
-- 'Blink.Controls.Control.focusPolicy') whose rows are never tab stops.
-- Up\/Down move the cursor, Shift-Up\/Down extend a range, Enter\/Space
-- act on the cursor, a click on a row activates it. Every change is
-- computed against the model exactly as passed in via 'selection' this
-- frame, never against any locally-derived value -- rendering, keyboard
-- handling, and click handling all read 'lcSelection' as given, and
-- 'onSelectionChanged'\/'onItemActivated' report the result for the app
-- to store and pass back in next frame.
--
-- @tag@ builds every part's element id from a 'ListPart': the list's own
-- root id from 'List', and each row's id from 'ListItem' applied to the
-- row's own item value -- so the caller never writes a per-row id by
-- hand, and can't accidentally give the root and a row the same id (see
-- 'ListPart'). Any 'Blink.Controls.Control.elementId' attribute passed in
-- @attrs@ is discarded in favour of @tag List@, the same as
-- 'Blink.Controls.ToggleGroup.toggleButtonGroup'.
list
  :: (Ord e, Eq a, SelectionModel sel, Eq (sel a))
  => (ListPart a -> e)
  -> [Attribute (ListConfig sel e msg a)]
  -> Element e msg
list tag attrs = Element
  { elLayout  = lcLayout cfg
  , elMeasure = measureChrome (ccStyleKey (lcControl cfg)) rows
  , elRun     = void (control ccfg)
  }
  where
    cfg  = resolve defaultListConfig attrs
    s0   = lcSelection cfg
    -- 'contentHeight' as the viewport height: guarantees 'scrollRowIntoView'
    -- always no-ops for these rows, correctly, since they're never actually
    -- scrolled -- 'rows' is used only for measurement and for the plain,
    -- fits-without-scrolling render path (see 'renderViewport').
    rows = vBox [children (zipWith (row contentHeight) [0 :: Int ..] (itemStates s0))]

    itemCount = length (itemStates s0)

    -- The rows' own total extent, known exactly (no measuring) since
    -- every row is fixed at 'lcRowHeight' -- see 'rowHeight'.
    contentHeight = fromIntegral itemCount * lcRowHeight cfg

    scrollBarTag = tag . ListScrollBar

    -- The id 'scrollBar' itself reads\/writes its position under -- see
    -- its own module header.
    listScrollEid = scrollBarTag ScrollBar

    fireSelectionChanged s = when (s /= s0) $ runHandlers (lcOnSelectionChanged cfg) s
    fireItemActivated      = runHandlers (lcOnItemActivated cfg)

    ccfg = (lcControl cfg)
      { ccElementId = Just (tag List)
      , ccContent = \ci -> do
          let (finalModel, activated) = foldl stepKey (s0, []) (ciKeysPressed ci)
          fireSelectionChanged finalModel
          mapM_ fireItemActivated activated
          when (cursorItem finalModel /= cursorItem s0) (scrollCursorIntoView finalModel)
          renderViewport
      }

    -- Keeps a keyboard-moved cursor visible: once its row falls above or
    -- below the viewport, requests just enough scroll to bring that edge
    -- back into view (see 'scrollRowIntoView'). Only ever called when the
    -- cursor actually moved this frame (see 'ccContent' above) -- an
    -- unconditional check on every frame would fight a scroll position
    -- set some other way (a drag on the bar itself, or seeded directly)
    -- while the cursor sits still.
    scrollCursorIntoView s = case findIndex isCursor (itemStates s) of
      Nothing  -> pure ()
      Just idx -> do
        bounds <- getBounds
        scrollRowIntoView (rectHeight bounds) idx

    -- Requests just enough scroll to bring row @idx@ (0-based, into the
    -- full item list) into a @viewportHeight@-tall viewport -- top-aligned
    -- if it currently falls above, bottom-aligned if below. A no-op when
    -- the list isn't scrollable at all, or the row is already fully
    -- within the viewport. Shared by 'scrollCursorIntoView' (a keyboard
    -- move) and 'rowActivated' (a click landing on a row not yet fully
    -- scrolled into view -- see its own comment).
    scrollRowIntoView viewportHeight idx = when (maxOffset > 0) $ do
      scrollFrac <- getScrollState listScrollEid
      let rh        = lcRowHeight cfg
          rowTop    = fromIntegral idx * rh
          rowBottom = rowTop + rh
          offsetY   = scrollFrac * maxOffset
          newFrac
            | rowTop < offsetY                    = Just (rowTop / maxOffset)
            | rowBottom > offsetY + viewportHeight = Just ((rowBottom - viewportHeight) / maxOffset)
            | otherwise                            = Nothing
      mapM_ (requestScrollTo listScrollEid) newFrac
      where
        maxOffset = contentHeight - viewportHeight

    -- Renders 'rows' plain when they fit the list's own bounds; once they
    -- overflow, composites a vertical 'scrollBar' alongside them (an
    -- 'hBox' of the two, the same L-shaped arrangement
    -- 'Blink.Controls.ScrollBar.scrollBar's own module header points to
    -- as the pattern any real scrolling viewport uses), and offsets\/clips
    -- the rows by the bar's own scroll position.
    renderViewport = do
      bounds <- getBounds
      let viewportHeight = rectHeight bounds
      if contentHeight > viewportHeight
        then runElement (scrollableRows viewportHeight)
        else runElement rows

    scrollableRows viewportHeight = hBox
      [ children
          [ elementWithLayout (Layout fill fill TopLeft) (clippedRows viewportHeight)
          , scrollBar scrollBarTag [ height fill, visibleFraction (viewportHeight / contentHeight) ]
          ]
      ]

    -- Offsets the visible rows (see 'visibleRows') by the bar's current
    -- scroll fraction and clips them to the viewport slot -- 'withClip'
    -- captures /this/ bounds (the viewport's own, not yet offset) as the
    -- clip region before the rows move within it, the same ordering
    -- 'Blink.Layout.Box.hBox'\/'vBox' use for their own children.
    clippedRows viewportHeight = do
      bounds     <- getBounds
      scrollFrac <- getScrollState listScrollEid
      let offsetY    = scrollFrac * (contentHeight - viewportHeight)
          rowsBounds = bounds { rectY = rectY bounds - offsetY, rectHeight = contentHeight }
      withClip $ withBounds rowsBounds (runElement (visibleRows offsetY viewportHeight))

    -- Only the rows whose fixed-height span intersects the viewport
    -- (@offsetY@ to @offsetY + viewportHeight@) are actually built and
    -- run; a spacer above and below -- sized for however many rows are
    -- skipped on that side -- keeps this vBox's own total height at
    -- 'contentHeight' regardless of which rows are currently skipped, so
    -- the scrollbar's thumb geometry (driven by 'contentHeight') never
    -- shifts as the visible set changes.
    visibleRows offsetY viewportHeight =
      vBox [children (spacer topSkipped : zipWith (row viewportHeight) [loIdx ..] visibleStates ++ [spacer bottomSkipped])]
      where
        rh            = lcRowHeight cfg
        loIdx         = max 0 (floor (offsetY / rh))
        hiIdx         = min itemCount (ceiling ((offsetY + viewportHeight) / rh))
        visibleStates = take (hiIdx - loIdx) (drop loIdx (itemStates s0))
        topSkipped    = fromIntegral loIdx * rh
        bottomSkipped = fromIntegral (itemCount - hiIdx) * rh
        spacer h      = elementWithLayout (Layout fill (exactly h) TopLeft) (pure ())

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
    -- -- see 'visibleRows'), but that row can still be only partially
    -- within the viewport, straddling its top or bottom edge. Scrolling
    -- it fully into view on the same click, via 'scrollRowIntoView',
    -- matches keyboard navigation already doing the same for the cursor.
    rowActivated viewportHeight idx item = do
      let s' = activate item s0
      fireSelectionChanged s'
      fireItemActivated item
      scrollRowIntoView viewportHeight idx

    rowStates st = Set.fromList
      [ if isSelected st then listSelected else listUnselected
      , if isCursor   st then listCursor   else listNoCursor
      ]

    row viewportHeight idx st = Element
      { elLayout  = Layout fill (exactly (lcRowHeight cfg)) TopLeft
      , elMeasure = noIntrinsicSize
      , elRun     = void $ control defaultControlConfig
          { ccElementId    = Just (tag (ListItem (isItem st)))
          , ccStyleKey     = listItemStyleKey
          , ccFocusPolicy  = NotFocusable
          , ccActiveStates = rowStates st
          , ccContent      = \rci -> do
              when (ciClicked rci) (rowActivated viewportHeight idx (isItem st))
              runElement (lcRenderItem cfg st)
          }
      }
