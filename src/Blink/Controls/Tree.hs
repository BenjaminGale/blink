{-# LANGUAGE FlexibleInstances #-}
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
  , TreeConfig (..)
  , defaultTreeConfig
  , tree
  , forest
  , expanded
  , renderNode
  , onExpansionChanged
  ) where

import Control.Monad (void, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Tree (Forest, Tree (..))

import Blink.Controls.Control
import Blink.Controls.List
import Blink.Controls.Tree.Style (treeChevronStyleKey)
import Blink.Element (Element (..), HasLayoutConfig (..), elementWithLayout, emptyElement, noIntrinsicSize)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Input (Key (..), KeyEvent (..))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), exactly, fill)
import Blink.Rendering (TextAlign (AlignCenter))
import Blink.Style (Style (..))
import Blink.View (Out, currentStyle)
import Blink.View.Drawing (drawText)

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

-- | Every capability 'tree' resolves: the embedded 'ListConfig' (for
-- 'Blink.Controls.List.selection'\/'Blink.Controls.List.rowHeight'\/etc,
-- via 'HasListConfig'), the forest and expansion state, how a node draws
-- its own content (indent and chevron are added around this by 'tree'
-- itself), and the expansion reactions.
data TreeConfig sel e msg a = TreeConfig
  { tcList              :: ListConfig sel e msg a
  , tcForest            :: Forest a
  , tcExpanded          :: Set a
  , tcRenderNode        :: TreeItemState a -> Element e msg
  , tcOnExpansionChanged :: [Set a -> [Out e msg]]
  }

instance HasControlConfig e msg (TreeConfig sel e msg a) where
  overControl attr = Attribute (\tc -> tc { tcList = runAttribute (overControl attr) (tcList tc) })

instance HasLayoutConfig (TreeConfig sel e msg a) where
  overLayout attr = Attribute (\tc -> tc { tcList = runAttribute (overLayout attr) (tcList tc) })

instance HasListConfig sel e msg a (TreeConfig sel e msg a) where
  overList attr = Attribute (\tc -> tc { tcList = runAttribute attr (tcList tc) })

-- | 'defaultListConfig', an empty forest, nothing expanded, no per-node
-- render (draws nothing), and no expansion reactions.
defaultTreeConfig :: SelectionModel sel => TreeConfig sel e msg a
defaultTreeConfig = TreeConfig
  { tcList               = defaultListConfig
  , tcForest             = []
  , tcExpanded           = Set.empty
  , tcRenderNode         = const emptyElement
  , tcOnExpansionChanged = []
  }

-- | The tree's own data, as a plain 'Forest' of the caller's item type.
forest :: Forest a -> Attribute (TreeConfig sel e msg a)
forest f = Attribute (\c -> c { tcForest = f })

-- | Which nodes are currently expanded -- see the module header.
expanded :: Set a -> Attribute (TreeConfig sel e msg a)
expanded s = Attribute (\c -> c { tcExpanded = s })

-- | How a node draws its own content; 'tree' adds the indent and chevron
-- around whatever this returns.
renderNode :: (TreeItemState a -> Element e msg) -> Attribute (TreeConfig sel e msg a)
renderNode f = Attribute (\c -> c { tcRenderNode = f })

-- | Reacts when clicking a chevron toggles a node's membership in
-- 'expanded', with the complete new set -- the app stores and passes it
-- back in next frame, the same relationship
-- 'Blink.Controls.List.onSelectionChanged' has to
-- 'Blink.Controls.List.selection'.
onExpansionChanged :: (Set a -> [Out e msg]) -> Attribute (TreeConfig sel e msg a)
onExpansionChanged h = Attribute (\c -> c { tcOnExpansionChanged = tcOnExpansionChanged c ++ [h] })

-- | The width of one level of indent, and of the chevron column every
-- row reserves regardless of whether it actually draws one -- so a leaf
-- row's own content still lines up with a sibling that has one.
treeStepWidth :: Double
treeStepWidth = 16

-- | The same 'BLACK DOWN\/RIGHT-POINTING TRIANGLE' glyphs (U+25BC\/U+25B6)
-- 'Blink.Controls.ScrollBar.scrollBar's own arrows already use -- these
-- render fine; it was their small-triangle cousins (U+25BE\/U+25B8) that
-- are missing from the demo's font. Drawn directly the same way
-- 'Blink.Controls.Checkbox.checkbox' draws its own tick glyph, rather
-- than composing a full 'Blink.Controls.Button.button' underneath, which
-- has chrome a chevron doesn't want.
chevronGlyph :: Bool -> Text
chevronGlyph isExpanded = if isExpanded then "\9660" else "\9654"

-- | A tree built on 'listBase' (see the module header). @mkId@ builds
-- every part's element id from a 'TreePart', the same relationship
-- 'listBase's own @mkId@ has to 'ListPart'. Left\/Right additionally
-- expand\/collapse a node with children, or move the cursor to its
-- first child\/parent once it's already expanded\/collapsed.
tree
  :: (Ord e, Ord a, SelectionModel sel, Eq (sel a))
  => (TreePart a -> e)
  -> [Attribute (TreeConfig sel e msg a)]
  -> Element e msg
tree mkId attrs = Element
  { elLayout  = lcLayout listCfg
  , elMeasure = measureChrome (ccStyleKey (lcControl listCfg)) (rowsSpacer listCfg (itemStates (lcSelection listCfg)))
  , elRun     = void run
  }
  where
    cfg     = resolve defaultTreeConfig attrs
    listCfg = (tcList cfg) { lcRenderItem = renderRow }

    visRows  = visibleNodes (tcForest cfg) (tcExpanded cfg)
    nodeInfo = Map.fromList [ (x, (depth, hasChildren)) | (x, depth, hasChildren) <- visRows ]

    run = do
      li <- listBase (mkId . TreeRow) listCfg
      mapM_ (handleKey li) (ciKeysPressed (liControl li))

    -- | Right expands a collapsed node with children (cursor stays), or
    -- -- once it's already expanded -- moves the cursor to the very next
    -- visible row, which 'visibleNodes' always places right after it and
    -- is exactly its first child. Left collapses an expanded node with
    -- children (cursor stays), or otherwise moves the cursor up to the
    -- node's parent (see 'stepsToParent'). Either way, a change to the
    -- expansion set or the selection is reported the same way clicking a
    -- chevron\/pressing Up\/Down already reports one.
    --
    -- Both moves reach the target purely via 'moveCursor' -- stepped
    -- once for Right, as many times as 'stepsToParent' says for Left --
    -- rather than 'activate', which also means "the user acted on this
    -- item": it flips a 'MultiSelection' row's own checked state, and
    -- collapses a 'RangeSelection' run to a single item. A cursor move
    -- triggered by navigating the tree's shape must never carry either
    -- side effect, whatever selection model the caller has chosen.
    handleKey li ev = case (key ev, cursorItem (liSelection li)) of
      (KeyRight, Just x) -> case Map.lookup x nodeInfo of
        Just (_, True) | not (Set.member x (tcExpanded cfg)) -> setExpanded (Set.insert x (tcExpanded cfg))
        Just _                                               -> moveCursorTo (moveCursor Next (liSelection li))
        Nothing                                              -> pure ()
      (KeyLeft, Just x) -> case Map.lookup x nodeInfo of
        Just (_, True) | Set.member x (tcExpanded cfg) -> setExpanded (Set.delete x (tcExpanded cfg))
        _                                               -> maybe (pure ()) climbToParent (stepsToParent x)
      _ -> pure ()
      where
        setExpanded s'    = runHandlers (tcOnExpansionChanged cfg) s'
        moveCursorTo s'   = when (s' /= liSelection li) (runHandlers (lcOnSelectionChanged listCfg) s')
        climbToParent n   = moveCursorTo (applyN n (moveCursor Prev) (liSelection li))
        applyN n f        = (!! n) . iterate f

    -- | How many rows back from @x@ its parent sits -- the nearest
    -- earlier visible row shallower than @x@'s own depth -- if it has
    -- one.
    stepsToParent x = listToMaybe
      [ n | (n, (_, d, _)) <- zip [1 ..] (reverse before), d < myDepth ]
      where
        (before, atX) = break (\(y, _, _) -> y == x) visRows
        myDepth       = case atX of { (_, d, _) : _ -> d; [] -> 0 }

    renderRow st = hBox [children [indentCell depth, chevronCell x hasChildren, tcRenderNode cfg tis]]
      where
        x                     = isItem st
        (depth, hasChildren) = Map.findWithDefault (0, False) x nodeInfo
        tis                   = TreeItemState st depth hasChildren (Set.member x (tcExpanded cfg))

    indentCell depth = elementWithLayout (Layout (exactly (fromIntegral depth * treeStepWidth)) fill TopLeft) (pure ())

    chevronCell x hasChildren
      | not hasChildren = elementWithLayout (Layout (exactly treeStepWidth) fill TopLeft) (pure ())
      | otherwise = Element
          { elLayout  = Layout (exactly treeStepWidth) fill TopLeft
          , elMeasure = noIntrinsicSize
          , elRun     = void $ control defaultControlConfig
              { ccElementId   = Just (mkId (TreeChevron x))
              , ccStyleKey    = treeChevronStyleKey
              , ccFocusPolicy = NotFocusable
              , ccContent     = \ci -> do
                  when (ciClicked ci) (runHandlers (tcOnExpansionChanged cfg) (toggleMembership x))
                  s <- currentStyle
                  drawText (styleTextColour s) AlignCenter (chevronGlyph (Set.member x (tcExpanded cfg)))
              }
          }
      where
        toggleMembership y
          | Set.member y (tcExpanded cfg) = Set.delete y (tcExpanded cfg)
          | otherwise                     = Set.insert y (tcExpanded cfg)
