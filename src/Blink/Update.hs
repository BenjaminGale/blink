{- |
Module: Blink.Update

The host-side counterpart to 'Blink.View.View': a small state-threading monad
for turning a @msg@ emitted by the view into an updated application state,
optionally requesting one or more 'Cmd's along the way. It mirrors
'Blink.View.View's shape (a pure state-threading computation with
'modify'\/'gets' primitives) but over the application state @s@ rather than
'Blink.View.ViewContext', and carries no 'IO' of its own -- a handler that
needs a side effect requests a 'Cmd' with 'cmd' instead of performing one
directly.

= Writing an update function

Pass a function of type @msg -> Update s msg ()@ as 'Blink.App.App's @update@
field. The host loop runs it once per message emitted during the frame, in
emission order, threading the state through each call:

@
data Msg = Increment | SetName Text

update :: Msg -> Update AppState Msg ()
update msg = case msg of
  Increment -> modify (\\s -> s { counter = counter s + 1 })
  SetName t -> modify (\\s -> s { name = t })
@

Use 'gets' to read part of the state, or 'get' \/ 'put' for the whole thing,
the same way you would with any state monad:

@
update :: Msg -> Update AppState Msg ()
update ResetIfOverLimit = do
  n <- gets counter
  when (n > 100) $ put initialState
@

= Requesting a command

'cmd' queues a 'Cmd' to run once the frame's messages have all been folded.
Its result is delivered back as an ordinary message on a later frame, once
the backend's 'Blink.App.MsgQueue' has it:

@
update :: Msg -> Update AppState Msg ()
update FetchClicked = do
  modify (\\s -> s { status = Loading })
  cmd (Cmd (FileLoaded \<$\> Control.Exception.try (readFile path)))
update (FileLoaded result) = modify (\\s -> s { status = Loaded result })
@
-}
module Blink.Update
  ( Update
  , runUpdate
  , runUpdateCmds
  , get
  , put
  , gets
  , modify
  , cmd
  ) where

import Blink.Cmd (Cmd)

-- | A pure, state-threading computation over the application state @s@,
-- producing a result @a@ and, via 'cmd', zero or more 'Cmd's carrying
-- messages of type @msg@. Compose with the 'Functor'\/'Applicative'\/'Monad'
-- instances; run with 'runUpdate' or 'runUpdateCmds'.
newtype Update s msg a = Update { runUpdateM :: s -> (a, s, [Cmd msg]) }

instance Functor (Update s msg) where
  fmap f (Update g) = Update $ \s -> let (a, s', cs) = g s in (f a, s', cs)

instance Applicative (Update s msg) where
  pure a = Update $ \s -> (a, s, [])
  Update f <*> Update g = Update $ \s ->
    let (h, s', cs1)  = f s
        (a, s'', cs2) = g s'
    in (h a, s'', cs1 ++ cs2)

instance Monad (Update s msg) where
  Update g >>= f = Update $ \s ->
    let (a, s', cs1)  = g s
        (b, s'', cs2) = runUpdateM (f a) s'
    in (b, s'', cs1 ++ cs2)

-- | The current application state.
get :: Update s msg s
get = gets id

-- | Replaces the application state.
put :: s -> Update s msg ()
put s = Update $ const ((), s, [])

-- | Projects a value out of the current application state.
gets :: (s -> a) -> Update s msg a
gets f = Update $ \s -> (f s, s, [])

-- | Applies a function to the current application state.
modify :: (s -> s) -> Update s msg ()
modify f = Update $ \s -> ((), f s, [])

-- | Requests that a 'Cmd' be run. Its result is folded back into the state
-- as an ordinary message, on whichever later frame the backend's
-- 'Blink.App.MsgQueue' delivers it.
cmd :: Cmd msg -> Update s msg ()
cmd c = Update $ \s -> ((), s, [c])

-- | Runs an 'Update' computation from a starting state, discarding its
-- result and any requested 'Cmd's, keeping only the final state.
runUpdate :: Update s msg a -> s -> s
runUpdate act s = let (_, s', _) = runUpdateM act s in s'

-- | Like 'runUpdate', but also returns any 'Cmd's the computation
-- requested via 'cmd'. This is what the frame loop uses; application code
-- driving an 'Update' directly usually only needs 'runUpdate'.
runUpdateCmds :: Update s msg a -> s -> (s, [Cmd msg])
runUpdateCmds act s = let (_, s', cs) = runUpdateM act s in (s', cs)
