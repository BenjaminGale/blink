{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module: Blink.Update

The host-side counterpart to 'Blink.View.View': a small state-threading monad
for turning a @msg@ emitted by the view into an updated application state,
optionally requesting one or more 'Cmd's or 'UiEffect's along the way. It
mirrors 'Blink.View.View's shape (a pure state-threading computation with
'modify'\/'gets' primitives) but over the application state @s@ rather than
'Blink.View.ViewContext', and carries no 'IO' of its own -- a handler that
needs a side effect requests a 'Cmd' with 'cmd' instead of performing one
directly.

= Writing an update function

Pass a function of type @msg -> Update s e msg ()@ as 'Blink.App.App's
@update@ field. The host loop runs it once per message emitted during the
frame, in emission order, threading the state through each call:

@
data Msg = Increment | SetName Text

update :: Msg -> Update AppState ElementId Msg ()
update msg = case msg of
  Increment -> modify (\\s -> s { counter = counter s + 1 })
  SetName t -> modify (\\s -> s { name = t })
@

Use 'gets' to read part of the state, or 'get' \/ 'put' for the whole thing,
the same way you would with any state monad:

@
update :: Msg -> Update AppState ElementId Msg ()
update ResetIfOverLimit = do
  n <- gets counter
  when (n > 100) $ put initialState
@

= Requesting a command

'cmd' queues a 'Cmd' to run once the frame's messages have all been folded.
Its result is delivered back as an ordinary message on a later frame, once
the backend's 'Blink.App.MsgQueue' has it:

@
update :: Msg -> Update AppState ElementId Msg ()
update FetchClicked = do
  modify (\\s -> s { status = Loading })
  cmd (FileLoaded \<$\> Control.Exception.try (readFile path))
update (FileLoaded result) = modify (\\s -> s { status = Loaded result })
@

= Requesting a UI effect

'Update' is a 'Blink.View.HasUiEffect' instance, so the same
'Blink.View.requestScrollTo', 'Blink.View.requestScrollBy',
'Blink.View.requestExtentBy', 'Blink.View.requestSelectionAt',
'Blink.View.requestFocus', and 'Blink.View.requestClearFocus' functions view
code already uses also work here, as a reaction to a message instead of an
input event. The effect takes hold from the next frame onward, the same way
it would from view code:

@
update :: Msg -> Update AppState ElementId Msg ()
update MessageReceived = do
  modify (\\s -> s { messages = messages s ++ [msg] })
  requestScrollTo MessageList 1
@
-}
module Blink.Update
  ( Update
  , runUpdate
  , runUpdateEffects
  , get
  , put
  , gets
  , modify
  , cmd
  ) where

import Blink.Cmd (Cmd (..))
import Blink.View.Context (HasUiEffect (..), UiEffect)

-- | A pure, state-threading computation over the application state @s@,
-- producing a result @a@ and, via 'cmd' or one of the 'HasUiEffect'
-- request functions, zero or more 'Cmd's carrying messages of type @msg@
-- and 'UiEffect's addressing elements of type @e@. Compose with the
-- 'Functor'\/'Applicative'\/'Monad' instances; run with 'runUpdate' or
-- 'runUpdateEffects'.
newtype Update s e msg a = Update { runUpdateM :: s -> (a, s, [Cmd msg], [UiEffect e]) }

instance Functor (Update s e msg) where
  fmap f (Update g) = Update $ \s -> let (a, s', cs, us) = g s in (f a, s', cs, us)

instance Applicative (Update s e msg) where
  pure a = Update $ \s -> (a, s, [], [])
  Update f <*> Update g = Update $ \s ->
    let (h, s', cs1, us1)  = f s
        (a, s'', cs2, us2) = g s'
    in (h a, s'', cs1 ++ cs2, us1 ++ us2)

instance Monad (Update s e msg) where
  Update g >>= f = Update $ \s ->
    let (a, s', cs1, us1)  = g s
        (b, s'', cs2, us2) = runUpdateM (f a) s'
    in (b, s'', cs1 ++ cs2, us1 ++ us2)

instance HasUiEffect e (Update s e msg) where
  queueEffect u = Update $ \s -> ((), s, [], [u])

-- | The current application state.
get :: Update s e msg s
get = gets id

-- | Replaces the application state.
put :: s -> Update s e msg ()
put s = Update $ const ((), s, [], [])

-- | Projects a value out of the current application state.
gets :: (s -> a) -> Update s e msg a
gets f = Update $ \s -> (f s, s, [], [])

-- | Applies a function to the current application state.
modify :: (s -> s) -> Update s e msg ()
modify f = Update $ \s -> ((), f s, [], [])

-- | Requests that an 'IO' action be run as a 'Cmd'. Its result is folded
-- back into the state as an ordinary message, on whichever later frame the
-- backend's 'Blink.App.MsgQueue' delivers it.
cmd :: IO msg -> Update s e msg ()
cmd io = Update $ \s -> ((), s, [Cmd io], [])

-- | Runs an 'Update' computation from a starting state, discarding its
-- result and any requested 'Cmd's\/'UiEffect's, keeping only the final
-- state.
runUpdate :: Update s e msg a -> s -> s
runUpdate act s = let (_, s', _, _) = runUpdateM act s in s'

-- | Like 'runUpdate', but also returns any 'Cmd's and 'UiEffect's the
-- computation requested via 'cmd' or a 'HasUiEffect' request function.
-- This is what the frame loop uses; application code driving an 'Update'
-- directly usually only needs 'runUpdate'.
runUpdateEffects :: Update s e msg a -> s -> (s, [Cmd msg], [UiEffect e])
runUpdateEffects act s = let (_, s', cs, us) = runUpdateM act s in (s', cs, us)
