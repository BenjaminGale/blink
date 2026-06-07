{-# LANGUAGE OverloadedStrings #-}
module UI (Element, Command (..), AppState (..), demoApp) where

import Blink

data Element = MyButton
  deriving (Eq, Ord)

data Command = Toggle

data AppState = AppState
  { toggled :: Bool
  }

demoApp :: App Element AppState Command
demoApp = App
  { startUp = pure (AppState False)
  , view = demoView
  , update = demoUpdate
  }

demoView :: AppState -> UI Element Command ()
demoView state = do
  let r = Rectangle 100 100 200 150
      label = if toggled state then "On" else "Off"
  layout r $ do
    clicked <- button MyButton label
    if clicked then emitCommand Toggle else return ()

demoUpdate :: Command -> AppState -> Update AppState Command ()
demoUpdate Toggle state = modify $ \s -> s { toggled = not (toggled state) }
