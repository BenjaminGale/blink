{-# LANGUAGE OverloadedStrings #-}
module UI (Element, Command (..), AppState (..), demoApp) where

import Blink
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
demoView state = do
  winRect <- getRect
  let btnSize = Size 120 40
      alignments =
        [ TopLeft,    TopCenter,    TopRight
        , MiddleLeft, Center,       MiddleRight
        , BottomLeft, BottomCenter, BottomRight
        ]
  mapM_ (alignButton winRect btnSize (selected state)) alignments

alignButton :: Rectangle -> Size -> Maybe Alignment -> Alignment -> UI Element Command ()
alignButton winRect btnSize sel a =
  layout (alignRect a winRect btnSize) $ do
    let label = if sel == Just a then "[" <> alignLabel a <> "]" else alignLabel a
    clicked <- button (AlignButton a) label
    if clicked then emitCommand (Select a) else return ()

alignLabel :: Alignment -> Text
alignLabel TopLeft     = "Top Left"
alignLabel TopCenter   = "Top Center"
alignLabel TopRight    = "Top Right"
alignLabel MiddleLeft  = "Middle Left"
alignLabel Center      = "Center"
alignLabel MiddleRight = "Middle Right"
alignLabel BottomLeft  = "Bottom Left"
alignLabel BottomCenter = "Bottom Center"
alignLabel BottomRight = "Bottom Right"

demoUpdate :: Command -> AppState -> Update AppState Command ()
demoUpdate (Select a) _ = modify $ \s -> s { selected = Just a }
