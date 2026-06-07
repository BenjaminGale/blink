{-# LANGUAGE OverloadedStrings #-}
module UI (Element, Command (..), AppState (..), demoApp) where

import Blink
import Control.Monad (when)
import Data.Text (Text)

data Element = AlignButton Alignment
  deriving (Eq, Ord)

data Command = Select Alignment

data AppState = AppState
  { selected :: Maybe Alignment
  }

demoApp :: App Element AppState Command
demoApp = App
  { startUp = pure (AppState (Just Center))
  , view = demoView
  , update = demoUpdate
  }

demoView :: AppState -> UI Element Command ()
demoView state =
  vBox 0
    [ row TopLeft TopCenter TopRight
    , row MiddleLeft Center MiddleRight
    , row BottomLeft BottomCenter BottomRight
    ]
  where
    row :: Alignment -> Alignment -> Alignment -> Cell Element Command
    row a b c = Cell Fill Fill Center $ hBox 0 [mkCell a, mkCell b, mkCell c]

    mkCell :: Alignment -> Cell Element Command
    mkCell a = Cell Fill (Exactly 40) a $ do
      let label = if selected state == Just a then "[" <> alignLabel a <> "]" else alignLabel a
      clicked <- button (AlignButton a) label
      when clicked $ emitCommand (Select a)

alignLabel :: Alignment -> Text
alignLabel TopLeft = "Top Left"
alignLabel TopCenter = "Top Center"
alignLabel TopRight = "Top Right"
alignLabel MiddleLeft = "Middle Left"
alignLabel Center = "Center"
alignLabel MiddleRight = "Middle Right"
alignLabel BottomLeft = "Bottom Left"
alignLabel BottomCenter = "Bottom Center"
alignLabel BottomRight = "Bottom Right"

demoUpdate :: Command -> AppState -> Update AppState Command ()
demoUpdate (Select a) _ = modify $ \s -> s { selected = Just a }
