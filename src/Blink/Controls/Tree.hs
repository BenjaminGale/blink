{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A tree control built on 'listBase': the caller's own hierarchy (a
-- plain @Data.Tree@ 'Forest') plus which nodes are expanded gets
-- flattened into the rows 'listBase' renders, with each row's own
-- content wrapped in an indent and a clickable expand\/collapse chevron.
--
-- = Design
--
-- Selection stays exactly what it is for 'list': a plain
-- 'Blink.Controls.List.SelectionModel' over the /visible/ items (built by
-- the caller the same way 'flattenVisible' builds it here -- dropping
-- depth leaves just the flat item order a selection model already
-- expects). Expansion is tracked entirely separately, as a plain
-- @Set a@ the caller passes in via 'expanded' and gets told about via
-- 'onExpansionChanged', the same shape 'lcSelection'\/'onSelectionChanged'
-- already have for the selection model.
module Blink.Controls.Tree
  ( flattenVisible
  , TreePart (..)
  , TreeItemState (..)
  , TreeDataConfig (..)
  , HasTreeDataConfig (..)
  , defaultTreeDataConfig
  , TreeConfig (..)
  , defaultTreeConfig
  , tree
  , forest
  , expanded
  , renderNode
  , onExpansionChanged
    -- * Building tree-shaped lists
    -- | For a widget built on 'listBase' whose rows are a flattened
    -- forest, with an indent and expand\/collapse chevron per row, the way
    -- 'tree' and 'Blink.Controls.TreeTable.treeTable' do.
  , TreeListConfig (..)
  , treeListBase
  , indentAndChevron
    -- * Style
  , treeChevronStyleKey
  , defaultStyleEntries
  ) where

import Control.Monad (void, when)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))

import Blink.Controls.Control
import Blink.Controls.List hiding (defaultStyleEntries)
import Blink.Element
  ( Element (..), HasLayoutConfig (..), HasSelection (..), HasSelectionChanged (..), elementWithLayout, emptyElement
  , noIntrinsicSize
  )
import Blink.Geometry (Alignment (TopLeft), uniform)
import Blink.Input (Key (..), KeyEvent (..))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (ImagePath, TextAlign (..))
import Blink.View (Effect, View, currentStyle)
import Blink.View.Drawing (drawImage)
import Blink.Style
import Blink.Controls.Style (transparent)

-- | Every currently visible row of @forest@, in document order, paired
-- with its depth (0 for a root). A node's children are only ever visited
-- when the node itself is a member of @expanded@ -- so collapsing a node
-- hides its whole subtree without needing to also drop its descendants
-- from @expanded@ (they simply aren't reached).
flattenVisible :: Ord a => Forest a -> Set a -> [(a, Int)]
flattenVisible forest0 expanded0 = [(x, d) | (x, d, _) <- visibleNodes forest0 expanded0]

-- | 'flattenVisible', plus whether each visible node has any children at
-- all -- what 'tree' additionally needs, to know whether a row gets a
-- chevron.
visibleNodes :: Ord a => Forest a -> Set a -> [(a, Int, Bool)]
visibleNodes forest0 expanded0 = go 0 forest0
  where
    go depth = concatMap (visit depth)

    visit depth (Node x kids)
      | Set.member x expanded0 = (x, depth, not (null kids)) : go (depth + 1) kids
      | otherwise               = [(x, depth, not (null kids))]

-- | Identifies one part of a 'tree' for the purpose of building element
-- ids: every part 'listBase' itself already needs (the root, a row, a
-- scrollbar part), plus a node's own expand\/collapse chevron.
data TreePart a
  = TreeRow (ListPart a)
  | TreeChevron a
  deriving (Eq, Ord, Show)

-- | What a tree row's own content renderer ('renderNode') receives: the
-- node's selected\/cursor flags (as 'list' already reports them, via
-- 'ItemState'), its depth, whether it has any children, and whether it's
-- currently expanded.
data TreeItemState a = TreeItemState
  { tisState       :: ItemState a
  , tisDepth        :: Int
  , tisHasChildren :: Bool
  , tisExpanded    :: Bool
  }

-- | A widget's forest, which of its nodes are expanded, and the reactions
-- to that set changing.
data TreeDataConfig e msg a = TreeDataConfig
  { tdForest             :: Forest a
  , tdExpanded           :: Set a
  , tdOnExpansionChanged :: [Set a -> [Effect e msg]]
  }

-- | An empty forest, nothing expanded, and no expansion reactions.
defaultTreeDataConfig :: TreeDataConfig e msg a
defaultTreeDataConfig = TreeDataConfig
  { tdForest             = []
  , tdExpanded           = Set.empty
  , tdOnExpansionChanged = []
  }

-- | Implemented by any config type that nests a 'TreeDataConfig', letting
-- 'forest'\/'expanded'\/'onExpansionChanged' be applied to it directly.
class HasTreeDataConfig e msg a cfg | cfg -> e msg a where
  overTreeData :: Attribute (TreeDataConfig e msg a) -> Attribute cfg

instance HasTreeDataConfig e msg a (TreeDataConfig e msg a) where
  overTreeData = id

-- | Every capability 'tree' resolves: the embedded 'ListConfig' (for
-- 'Blink.Controls.List.selection'\/'Blink.Controls.List.rowHeight'\/etc,
-- via 'HasListConfig'), the forest and expansion state (via
-- 'HasTreeDataConfig'), and how a node draws its own content (indent and
-- chevron are added around this by 'tree' itself).
data TreeConfig sel e msg a = TreeConfig
  { tcList       :: ListConfig sel e msg a
  , tcTreeData   :: TreeDataConfig e msg a
  , tcRenderNode :: TreeItemState a -> Element e msg
  }

instance HasControlConfig e msg (TreeConfig sel e msg a) where
  overControl attr = Attribute (\tc -> tc { tcList = runAttribute (overControl attr) (tcList tc) })

instance HasEventHandlers (TreeConfig sel e msg a)

instance HasLayoutConfig (TreeConfig sel e msg a) where
  overLayout attr = Attribute (\tc -> tc { tcList = runAttribute (overLayout attr) (tcList tc) })

instance HasListConfig sel e msg a (TreeConfig sel e msg a) where
  overList attr = Attribute (\tc -> tc { tcList = runAttribute attr (tcList tc) })

instance HasSelection (sel a) (TreeConfig sel e msg a) where
  selection = overList . selection

instance HasSelectionChanged e msg (sel a) (TreeConfig sel e msg a) where
  onSelectionChanged = overList . onSelectionChanged

instance HasTreeDataConfig e msg a (TreeConfig sel e msg a) where
  overTreeData attr = Attribute (\tc -> tc { tcTreeData = runAttribute attr (tcTreeData tc) })

-- | 'defaultListConfig', 'defaultTreeDataConfig', and no per-node render
-- (draws nothing).
defaultTreeConfig :: (SelectionModel sel, EmptySelection sel) => TreeConfig sel e msg a
defaultTreeConfig = TreeConfig
  { tcList       = defaultListConfig
  , tcTreeData   = defaultTreeDataConfig
  , tcRenderNode = const emptyElement
  }

-- | The widget's own data, as a plain 'Forest' of the caller's item type.
forest :: HasTreeDataConfig e msg a cfg => Forest a -> Attribute cfg
forest f = overTreeData (Attribute (\c -> c { tdForest = f }))

-- | Which nodes are currently expanded -- see the module header.
expanded :: HasTreeDataConfig e msg a cfg => Set a -> Attribute cfg
expanded s = overTreeData (Attribute (\c -> c { tdExpanded = s }))

-- | How a node draws its own content; 'tree' adds the indent and chevron
-- around whatever this returns.
renderNode :: (TreeItemState a -> Element e msg) -> Attribute (TreeConfig sel e msg a)
renderNode f = Attribute (\c -> c { tcRenderNode = f })

-- | Reacts when clicking a chevron toggles a node's membership in
-- 'expanded', with the complete new set -- the app stores and passes it
-- back in next frame, the same relationship
-- 'Blink.Controls.List.onSelectionChanged' has to
-- 'Blink.Controls.List.selection'.
onExpansionChanged :: HasTreeDataConfig e msg a cfg => (Set a -> [Effect e msg]) -> Attribute cfg
onExpansionChanged h = overTreeData (Attribute (\c -> c { tdOnExpansionChanged = tdOnExpansionChanged c ++ [h] }))

-- | The width of one level of indent, and of the chevron column every
-- row reserves regardless of whether it actually draws one -- so a leaf
-- row's own content still lines up with a sibling that has one.
treeStepWidth :: Double
treeStepWidth = 16

-- | The expanded state's chevron, pointing down.
chevronExpandedIcon :: ImagePath
chevronExpandedIcon = "assets/icons/expand_more.svg"

-- | The collapsed state's chevron, pointing right.
chevronCollapsedIcon :: ImagePath
chevronCollapsedIcon = "assets/icons/chevron_right.svg"

-- | A tree built on 'listBase' (see the module header). @mkId@ builds
-- every part's element id from a 'TreePart', the same relationship
-- 'listBase's own @mkId@ has to 'ListPart'. Left\/Right additionally
-- expand\/collapse a node with children, or move the cursor to its
-- first child\/parent once it's already expanded\/collapsed.
tree
  :: (Ord e, Ord a, SelectionModel sel, EmptySelection sel, Eq (sel a))
  => (TreePart a -> e)
  -> [Attribute (TreeConfig sel e msg a)]
  -> Element e msg
tree mkId attrs =
  chromeElement (lcLayout listCfg) (ccStyleKey (lcControl listCfg)) (listMeasure False listCfg) (void run)
  where
    cfg     = resolve defaultTreeConfig attrs
    td      = tcTreeData cfg
    listCfg = tcList cfg

    run = treeListBase (mkId . TreeRow) TreeListConfig
      { tlList      = listCfg
      , tlTreeData  = td
      , tlRenderRow = renderRow
      }

    renderRow tis = hBox [children (indentAndChevron (mkId . TreeChevron) td tis ++ [tcRenderNode cfg tis])]

-- | Every capability 'treeListBase' resolves: the list its rows are shown
-- in, the forest and expansion state they're flattened from, and how a
-- row draws its content given its node's 'TreeItemState'.
data TreeListConfig sel e msg a = TreeListConfig
  { tlList      :: ListConfig sel e msg a
  , tlTreeData  :: TreeDataConfig e msg a
  , tlRenderRow :: TreeItemState a -> Element e msg
  }

-- | Runs @cfg@'s list as 'listBase', with one row per visible node of its
-- forest, each drawn by 'tlRenderRow'. Left\/Right additionally expand or
-- collapse the node under the cursor, or move the cursor to its first
-- child or its parent. Reports the list's own 'ListInteraction'.
treeListBase
  :: (Ord e, Ord a, SelectionModel sel, Eq (sel a))
  => (ListPart a -> e)
  -> TreeListConfig sel e msg a
  -> View e msg (ListInteraction sel e msg a)
treeListBase mkRowId cfg = do
  li <- listBase mkRowId listCfg
  mapM_ (handleExpansionKey mkRowId listCfg visRows nodeInfo (tdExpanded td) (tdOnExpansionChanged td) (liViewportHeight li))
    (ciKeysPressed (liControl li))
  pure li
  where
    td       = tlTreeData cfg
    visRows  = visibleNodes (tdForest td) (tdExpanded td)
    nodeInfo = Map.fromList [ (x, (depth, hasChildren)) | (x, depth, hasChildren) <- visRows ]
    listCfg  = (tlList cfg) { lcRenderItem = tlRenderRow cfg . itemState }

    itemState st = TreeItemState st depth hasChildren (Set.member x (tdExpanded td))
      where
        x                    = itemValue st
        (depth, hasChildren) = Map.findWithDefault (0, False) x nodeInfo

-- | The indent (proportional to the node's depth) and, when the node has
-- children, a clickable chevron showing whether it's expanded, to go
-- before a row's own content. Clicking the chevron fires @td@'s
-- expansion reactions with the node's membership toggled.
indentAndChevron
  :: (Ord e, Ord a)
  => (a -> e) -> TreeDataConfig e msg a -> TreeItemState a -> [Element e msg]
indentAndChevron mkChevronId td tis =
  [indentCell, chevronCell]
  where
    indentCell = elementWithLayout (Layout (exactly (fromIntegral depth * treeStepWidth)) fill TopLeft) (pure ())

    chevronCell
      | not hasChildren = elementWithLayout (Layout (exactly treeStepWidth) fill TopLeft) (pure ())
      | otherwise = Element
          { elLayout  = Layout (exactly treeStepWidth) fill TopLeft
          , elMeasure = noIntrinsicSize
          , elRun     = void $ control defaultControlConfig
              { ccElementId   = Just (mkChevronId x)
              , ccStyleKey    = treeChevronStyleKey
              , ccFocusPolicy = NotFocusable
              , ccContent     = \ci -> do
                  when (ciClicked ci) (runHandlers onExpansionChanged0 (toggleMembership x))
                  s <- currentStyle
                  let icon = if Set.member x expanded0 then chevronExpandedIcon else chevronCollapsedIcon
                  drawImage (styleTextColour s) icon
              }
          }

    x                   = itemValue (tisState tis)
    depth               = tisDepth tis
    hasChildren         = tisHasChildren tis
    expanded0           = tdExpanded td
    onExpansionChanged0 = tdOnExpansionChanged td

    toggleMembership y
      | Set.member y expanded0 = Set.delete y expanded0
      | otherwise               = Set.insert y expanded0

-- | Handles Left\/Right against a flattened forest: Right expands a
-- collapsed node with children (cursor stays), or -- once it's already
-- expanded -- moves the cursor to the very next visible row, which
-- 'visibleNodes' always places right after it and is exactly its first
-- child. Left collapses an expanded node with children (cursor stays),
-- or otherwise moves the cursor up to the node's parent. Either way, a
-- change to the expansion set or the selection is reported the same way
-- clicking a chevron\/pressing Up\/Down already reports one, and a moved
-- cursor is scrolled into view the same way one moved by Up\/Down
-- already is.
--
-- Both moves reach the target purely via 'moveCursor' -- stepped once
-- for Right, as many times as needed to reach the parent for Left --
-- rather than 'activate', which also means "the user acted on this
-- item": it flips a 'MultiSelection' row's own checked state, and
-- collapses a 'RangeSelection' run to a single item. A cursor move
-- triggered by navigating the tree's shape must never carry either side
-- effect, whatever selection model the caller has chosen.
handleExpansionKey
  :: (Ord e, Ord a, SelectionModel sel, Eq (sel a))
  => (ListPart a -> e)
  -> ListConfig sel e msg a
  -> [(a, Int, Bool)]
  -> Map.Map a (Int, Bool)
  -> Set a
  -> [Set a -> [Effect e msg]]
  -> Double
  -> KeyEvent
  -> View e msg ()
handleExpansionKey mkRowId listCfg visRows nodeInfo expanded0 onExpansionChanged0 viewportHeight ev =
  case (key ev, cursorItem s0) of
    (KeyRight, Just x) -> case Map.lookup x nodeInfo of
      Just (_, True) | not (Set.member x expanded0) -> setExpanded (Set.insert x expanded0)
      Just _                                        -> moveCursorTo (moveCursor Next s0)
      Nothing                                       -> pure ()
    (KeyLeft, Just x) -> case Map.lookup x nodeInfo of
      Just (_, True) | Set.member x expanded0 -> setExpanded (Set.delete x expanded0)
      _                                        -> maybe (pure ()) climbToParent (stepsToParent x)
    _ -> pure ()
  where
    s0       = lcSelection listCfg

    setExpanded s'  = runHandlers onExpansionChanged0 s'
    moveCursorTo s' = when (s' /= s0) $ do
      runHandlers (lcOnSelectionChanged listCfg) s'
      let rowIndex = cursorItem s' >>= \x -> findIndex (\(y, _, _) -> y == x) visRows
      mapM_ (scrollRowIntoView mkRowId listCfg (length visRows) viewportHeight) rowIndex
    climbToParent n = moveCursorTo (applyN n (moveCursor Prev) s0)
    applyN n f       = (!! n) . iterate f

    -- How many rows back from @x@ its parent sits -- the nearest earlier
    -- visible row shallower than @x@'s own depth -- if it has one.
    stepsToParent x = listToMaybe
      [ n | (n, (_, d, _)) <- zip [1 ..] (reverse before), d < myDepth ]
      where
        (before, atX) = break (\(y, _, _) -> y == x) visRows
        myDepth       = case atX of { (_, d, _) : _ -> d; [] -> 0 }

-- * Style

-- | The 'StyleKey' a 'Blink.Controls.Tree.tree' row's chevron resolves
-- its style from unless overridden via 'Blink.Controls.Control.style'.
treeChevronStyleKey :: StyleKey e
treeChevronStyleKey = Class "tree-chevron"

-- | No margin\/padding\/border -- the chevron already sits in a fixed,
-- narrow column 'Blink.Controls.Tree.tree' reserves for it, so any
-- chrome inset would just crowd its glyph.
treeChevronMetrics :: Metrics
treeChevronMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = uniform 0
  }

-- | A plain, transparent, centred icon with no border -- the same shape
-- 'Blink.Controls.Label.labelStyle' has, just with no padding of
-- its own. Uses 'paletteIcon'\/'paletteIconHover' (the same colours
-- 'Blink.Controls.Checkbox.checkbox'\/'Blink.Controls.RadioButton.radioButton'
-- tint their own icons with), since the chevron is nothing but an icon.
treeChevronStyle :: Palette -> StyleSet
treeChevronStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteIcon p
      , styleTextAlign    = AlignCenter
      , styleBorder       = noBorder
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleTextColour = paletteIconHover p })
      , (CommonDisabled,  \s -> s { styleTextColour = paletteTextMuted p })
      ]
  }

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme'.
-- Needed at all only because 'Blink.Controls.Control.defaultControlConfig'
-- resolves an unset 'Blink.Controls.Control.ccStyleKey' to
-- 'Blink.Style.Defaults.defaultTheme''s boxed-control fallback look --
-- without it, the chevron would draw with a button's full
-- border\/background chrome instead of sitting flush in its narrow column.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (treeChevronStyleKey, (treeChevronMetrics, treeChevronStyle p)) ]
