-- | The tree control's data-side layer: turning an application's own
-- hierarchy (a plain @Data.Tree@ 'Forest') plus which nodes are expanded
-- into the flat, depth-annotated row sequence a list-like control
-- renders. Kept separate from selection: 'flattenVisible' hands back
-- plain @(a, Int)@ pairs, and the items alone (dropping depth) still feed
-- whichever 'Blink.Controls.List.SelectionModel' the caller has chosen,
-- unchanged.
module Blink.Controls.Tree
  ( flattenVisible
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))

-- | Every currently visible row of @forest@, in document order, paired
-- with its depth (0 for a root). A node's children are only ever visited
-- when the node itself is a member of @expanded@ -- so collapsing a node
-- hides its whole subtree without needing to also drop its descendants
-- from @expanded@ (they simply aren't reached).
flattenVisible :: Ord a => Forest a -> Set a -> [(a, Int)]
flattenVisible forest expanded = go 0 forest
  where
    go depth = concatMap (visit depth)

    visit depth (Node x children)
      | Set.member x expanded = (x, depth) : go (depth + 1) children
      | otherwise              = [(x, depth)]
