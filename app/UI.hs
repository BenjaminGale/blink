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
  { elementStyles = Map.empty
  , defaultStyle = buttonStyleSet
  }

buttonStyleSet :: StyleSet
buttonStyleSet = StyleSet
  { normal   = Style { background = RGBA 0.878 0.878 0.898 1, textColour = RGBA 0.11 0.11 0.12 1 }
  , hovered  = Style { background = RGBA 0.800 0.800 0.824 1, textColour = RGBA 0.11 0.11 0.12 1 }
  , pressed  = Style { background = RGBA 0.102 0.435 0.831 1, textColour = RGBA 1.0  1.0  1.0  1 }
  , focused  = Style { background = RGBA 0.667 0.769 0.941 1, textColour = RGBA 0.11 0.11 0.12 1 }
  , disabled = Style { background = RGBA 0.898 0.898 0.910 1, textColour = RGBA 0.682 0.682 0.698 1 }
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
