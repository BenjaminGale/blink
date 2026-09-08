{- |
Module: Blink.View.Navigation

Which key\/modifier combinations mean "advance" or "retreat" keyboard
focus: the pure 'NavigationKeys' type and 'defaultNavigationKeys', plus the
monadic accessors ('getNavigationKeys', 'withNavigationKeys') built on top
of the ambient set threaded through 'Blink.View.Context.ViewContext'. See
"Blink.View" for the module overview; import that instead of this module
directly.
-}
module Blink.View.Navigation
  ( NavigationKeys (..)
  , defaultNavigationKeys
  , getNavigationKeys
  , withNavigationKeys
  ) where

import Blink.View.Context

-- | The navigation keys currently ambient.
getNavigationKeys :: View e msg NavigationKeys
getNavigationKeys = gets ctxNavigationKeys

-- | Replaces the ambient navigation keys for @action@, restoring the
-- previous set once it completes — same save\/restore shape as
-- 'Blink.View.Context.disableWhen', except it replaces rather than only
-- escalating: a container fully redefines what its own children treat as
-- navigation keys, it doesn't merely add to an outer scope's set.
withNavigationKeys :: NavigationKeys -> View e msg a -> View e msg a
withNavigationKeys = withField ctxNavigationKeys (\v c -> c { ctxNavigationKeys = v })
