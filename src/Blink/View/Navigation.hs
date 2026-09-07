{- |
Module: Blink.View.Navigation

Which key\/modifier combinations mean "advance" or "retreat" keyboard
focus.

No dependency on the 'Blink.View' monad -- 'Blink.View' holds a
'NavigationKeys' ambiently in its context and exposes monadic accessors
('Blink.View.getNavigationKeys', 'Blink.View.withNavigationKeys') built
on top of what's defined here, the same relationship "Blink.View.Focus"
has with the focus state 'Blink.View' threads through its own context.
-}
module Blink.View.Navigation
  ( NavigationKeys (..)
  , defaultNavigationKeys
  ) where

import Blink.Input (Key (..), Modifier (..))

-- | The specific key\/modifier combinations that currently mean "give up
-- focus here and let the next render claim it" ('navAdvance') or "return to
-- whichever tab stop was previous" ('navRetreat'). Every
-- 'Blink.View.Controls.Control.control' consults this instead of a hardcoded Tab\/
-- Shift-Tab, so a container can redefine it for its own children by
-- opening a new ambient set around them with 'Blink.View.withNavigationKeys'.
data NavigationKeys = NavigationKeys
  { navAdvance :: [(Key, [Modifier])]
  , navRetreat :: [(Key, [Modifier])]
  } deriving (Eq, Show)

-- | Plain Tab\/Shift-Tab — what every control uses unless some enclosing
-- container has redefined it. The root ambient default.
defaultNavigationKeys :: NavigationKeys
defaultNavigationKeys = NavigationKeys
  { navAdvance = [(KeyTab, [])]
  , navRetreat = [(KeyTab, [Shift])]
  }
