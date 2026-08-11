{- |
Module: Blink.Update

The host-side counterpart to 'Blink.UI.UI': a small state-threading monad
for turning a @msg@ emitted by the view into an updated application state.
It mirrors 'Blink.UI.UI's shape (a pure state-threading computation with
'modify'\/'gets' primitives) but over the application state @s@ rather than
'Blink.UI.UIContext', and carries no 'IO' — nothing in an @update@ handler
touches text measurement or any other backend concern.

= Writing an update function

Pass a function of type @msg -> Update s ()@ as 'Blink.App.App's @update@
field. The host loop runs it once per message emitted during the frame, in
emission order, threading the state through each call:

@
data Msg = Increment | SetName Text

update :: Msg -> Update AppState ()
update msg = case msg of
  Increment -> modify (\\s -> s { counter = counter s + 1 })
  SetName t -> modify (\\s -> s { name = t })
@

Use 'gets' to read part of the state, or 'get' \/ 'put' for the whole thing,
the same way you would with any state monad:

@
update :: Msg -> Update AppState ()
update ResetIfOverLimit = do
  n <- gets counter
  when (n > 100) $ put initialState
@
-}
module Blink.Update
  ( Update
  , runUpdate
  , get
  , put
  , gets
  , modify
  ) where

-- | A pure, state-threading computation over the application state @s@,
-- producing a result @a@. Compose with the 'Functor'\/'Applicative'\/'Monad'
-- instances; run with 'runUpdate'.
newtype Update s a = Update { runUpdateM :: s -> (a, s) }

instance Functor (Update s) where
  fmap f (Update g) = Update $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (Update s) where
  pure a = Update $ \s -> (a, s)
  Update f <*> Update g = Update $ \s ->
    let (h, s')  = f s
        (a, s'') = g s'
    in (h a, s'')

instance Monad (Update s) where
  Update g >>= f = Update $ \s ->
    let (a, s') = g s
    in runUpdateM (f a) s'

-- | The current application state.
get :: Update s s
get = gets id

-- | Replaces the application state.
put :: s -> Update s ()
put s = Update $ const ((), s)

-- | Projects a value out of the current application state.
gets :: (s -> a) -> Update s a
gets f = Update $ \s -> (f s, s)

-- | Applies a function to the current application state.
modify :: (s -> s) -> Update s ()
modify f = Update $ \s -> ((), f s)

-- | Runs an 'Update' computation from a starting state, discarding its
-- result and keeping only the final state.
runUpdate :: Update s a -> s -> s
runUpdate act s = snd (runUpdateM act s)
