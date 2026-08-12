module Blink.Controls2
  ( Attr (..)
  , configure
  , fire
  , onAny
  , ControlEvent (..)
  , HasControlEvent (..)
  ) where

import Control.Monad (forM_)
import Data.List (foldl')

import Blink.UI

data ControlEvent
  = FocusGained
  | FocusLost
  deriving (Eq, Show)

class HasControlEvent ev where
  liftControl  :: ControlEvent -> ev
  matchControl :: ev -> Maybe ControlEvent

data Attr e ev msg cfg
  = On (ev -> [Out e msg])
  | Config (cfg -> cfg)

configure :: cfg -> [Attr e ev msg cfg] -> cfg
configure = foldl' apply
  where
    apply cfg (Config f) = f cfg
    apply cfg (On _)     = cfg

fire :: [Attr e ev msg cfg] -> [ev] -> UI e msg ()
fire attrs evs = forM_ evs $ \ev -> forM_ handlers $ \h -> mapM_ dispatch (h ev)
  where
    handlers = [h | On h <- attrs]
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

onAny :: (ev -> [Out e msg]) -> Attr e ev msg cfg
onAny = On
