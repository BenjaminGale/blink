module Blink.App
  ( App (..)
  ) where

import Blink.UI (UI)
import Blink.Update (Update)

data App e s c = App
  { startUp :: IO s
  , view    :: s -> UI e c ()
  , update  :: c -> s -> Update s c ()
  }
