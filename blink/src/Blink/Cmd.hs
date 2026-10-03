{- |
Module: Blink.Cmd

An asynchronous unit of work an 'Blink.Update.Update' handler can request via
'Blink.Update.cmd'. The frame loop runs it off the frame thread and folds its
result back into the application state as an ordinary message on a later
frame, via whatever 'Blink.App.MsgQueue' the backend supplies -- see
"Blink.App".
-}
module Blink.Cmd
  ( Cmd (..)
  ) where

-- | An 'IO' action that eventually produces a message. Build one from
-- whatever side effect you need (reading a file, making a request) and hand
-- it to 'Blink.Update.cmd'.
newtype Cmd msg = Cmd { runCmd :: IO msg }

instance Functor Cmd where
  fmap f (Cmd io) = Cmd (fmap f io)
