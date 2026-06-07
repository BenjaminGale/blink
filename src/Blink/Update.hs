module Blink.Update
  ( Effect
  , Update (..)
  , modify
  , effect
  , execCommands
  ) where

type Effect c = IO c

newtype Update s c a = Update { runUpdate :: s -> (a, s, [Effect c]) }

instance Functor (Update s c) where
  fmap f (Update g) = Update $ \s ->
    let (a, s', effs) = g s
    in (f a, s', effs)

instance Applicative (Update s c) where
  pure a = Update $ \s -> (a, s, [])
  Update f <*> Update x = Update $ \s ->
    let (g, s', effs) = f s
        (a, s'', effs') = x s'
    in (g a, s'', effs ++ effs')

instance Monad (Update s c) where
  return = pure
  Update x >>= f = Update $ \s ->
    let (a, s', effs) = x s
        Update g = f a
        (b, s'', effs') = g s'
    in (b, s'', effs ++ effs')

modify :: (s -> s) -> Update s c ()
modify f = Update $ \s -> ((), f s, [])

effect :: Effect c -> Update s c ()
effect eff = Update $ \s -> ((), s, [eff])

execCommands :: (c -> Update s c ()) -> [c] -> s -> s
execCommands updateFn cmds initial = foldl step initial cmds
  where
    step s cmd =
      let Update f = updateFn cmd
          ((), s', _) = f s
      in s'
