{-# LANGUAGE OverloadedStrings #-}
module UI (Element, Command (..), AppState (..), demoApp) where

import Blink
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

data Element = AlignButton Alignment
  deriving (Eq, Ord)

data Command = Select Alignment

data AppState = AppState
  { selected :: Maybe Alignment
  }

demoTheme :: Theme Element
demoTheme = Theme
  { elementStyles = Map.fromList
      [ (AlignButton a, buttonSet (columnAlign a)) | a <- [minBound .. maxBound] ]
  , defaultStyle = buttonSet AlignCenter
  }

columnAlign :: Alignment -> TextAlign
columnAlign a
  | a `elem` [TopLeft, MiddleLeft, BottomLeft]   = AlignLeft
  | a `elem` [TopRight, MiddleRight, BottomRight] = AlignRight
  | otherwise                                      = AlignCenter

buttonSet :: TextAlign -> StyleSet
buttonSet align = StyleSet
  { normal   = base { background = RGBA 0.878 0.878 0.898 1, textColour = RGBA 0.11 0.11 0.12 1, borderColour = Just (RGBA 0.600 0.600 0.620 1) }
  , hovered  = base { background = RGBA 0.800 0.800 0.824 1, textColour = RGBA 0.11 0.11 0.12 1, borderColour = Just (RGBA 0.400 0.400 0.420 1) }
  , pressed  = base { background = RGBA 0.102 0.435 0.831 1, textColour = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.071 0.306 0.584 1) }
  , focused  = base { background = RGBA 0.667 0.769 0.941 1, textColour = RGBA 0.11 0.11 0.12 1, borderColour = Just (RGBA 0.102 0.435 0.831 1) }
  , disabled = base { background = RGBA 0.898 0.898 0.910 1, textColour = RGBA 0.682 0.682 0.698 1 }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = RGBA 0 0 0 1
      , textAlign    = align
      , margin       = uniform 3
      , padding      = uniform 6
      , borderColour = Nothing
      , borderWidth  = 0
      }

demoApp :: App Element AppState Command
demoApp = App
  { startUp = pure (AppState (Just Center))
  , theme = demoTheme
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

demoUpdate :: Command -> Update AppState Command ()
demoUpdate (Select a) = modify $ \s -> s { selected = Just a }
