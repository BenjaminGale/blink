{-# LANGUAGE OverloadedStrings #-}
module UI (Element, Command (..), AppState (..), demoApp) where

import Blink
import Data.Text (Text)
import qualified Data.Map.Strict as Map

data Element = Btn Int
  deriving (Eq, Ord)

data Command = Clicked Int

data AppState = AppState
  { lastClicked :: Maybe Int
  }

demoTheme :: Theme Element
demoTheme = Theme
  { elementStyles = Map.empty
  , defaultStyle  = btnStyle
  }

btnStyle :: StyleSet
btnStyle = StyleSet
  { normal   = base { background = RGBA 0.878 0.878 0.898 1, textColour = RGBA 0.11 0.11 0.12 1, borderColour = Just (RGBA 0.600 0.600 0.620 1) }
  , hovered  = base { background = RGBA 0.800 0.800 0.824 1, textColour = RGBA 0.11 0.11 0.12 1, borderColour = Just (RGBA 0.400 0.400 0.420 1) }
  , pressed  = base { background = RGBA 0.102 0.435 0.831 1, textColour = RGBA 1.0   1.0  1.0  1, borderColour = Just (RGBA 0.071 0.306 0.584 1) }
  , focused  = base { background = RGBA 0.667 0.769 0.941 1, textColour = RGBA 0.11 0.11 0.12 1, borderColour = Just (RGBA 0.102 0.435 0.831 1) }
  , disabled = base { background = RGBA 0.898 0.898 0.910 1, textColour = RGBA 0.682 0.682 0.698 1 }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = RGBA 0 0 0 1
      , textAlign    = AlignCenter
      , margin       = uniform 3
      , padding      = uniform 6
      , borderColour = Nothing
      , borderWidth  = 0
      }

demoApp :: App Element AppState Command
demoApp = App
  { startUp = pure (AppState Nothing)
  , theme   = demoTheme
  , view    = demoView
  , update  = demoUpdate
  }

btn :: Int -> Text -> UI Element Command ()
btn i label = do
  clicked <- button (Btn i) label
  if clicked then dispatch (Clicked i) else pure ()

withBg :: Colour -> UI Element Command () -> UI Element Command ()
withBg colour ui = fillRect colour >> ui

-- Row 1: fill behaviour — fixed | fill | fill | fixed (two fills share surplus evenly)
row1 :: UI Element Command ()
row1 = withBg (RGBA 0.95 0.87 0.87 1) $
  hBox (BoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center, boxFillCross = True })
    [ (RectConstraint (Exactly 80) Fill TopLeft, btn 1 "Left")
    , (RectConstraint Fill         Fill Center,  btn 2 "<fill 1>")
    , (RectConstraint Fill         Fill Center,  btn 3 "<fill 2>")
    , (RectConstraint (Exactly 80) Fill TopLeft, btn 4 "Right")
    ]

-- Row 2: fillCross = False, children top/centre/bottom aligned
row2 :: UI Element Command ()
row2 = withBg (RGBA 0.87 0.95 0.87 1) $
  hBox (BoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center, boxFillCross = False })
    [ (RectConstraint (Exactly 100) (Exactly 30) TopLeft,    btn 5 "Top")
    , (RectConstraint (Exactly 100) (Exactly 50) MiddleLeft, btn 6 "Mid")
    , (RectConstraint (Exactly 100) (Exactly 40) BottomLeft, btn 7 "Bot")
    ]

-- Row 3: same constraints as row 2, fillCross = True, content aligned to the right
row3 :: UI Element Command ()
row3 = withBg (RGBA 0.87 0.87 0.95 1) $
  hBox (BoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = MiddleRight, boxFillCross = True })
    [ (RectConstraint (Exactly 100) (Exactly 30) TopLeft,    btn 8 "Top")
    , (RectConstraint (Exactly 100) (Exactly 50) MiddleLeft, btn 9 "Mid")
    , (RectConstraint (Exactly 100) (Exactly 40) BottomLeft, btn 10 "Bot")
    ]

demoView :: AppState -> UI Element Command ()
demoView _ = vBox (BoxConfig { boxSpacing = 8, boxMargin = 8, boxAlignment = TopLeft, boxFillCross = True })
  [ (RectConstraint Fill (Exactly 50) TopLeft, row1)
  , (RectConstraint Fill Fill         TopLeft, row2)
  , (RectConstraint Fill (Exactly 80) TopLeft, row3)
  ]

demoUpdate :: Command -> Update AppState Command ()
demoUpdate (Clicked i) = modify $ \s -> s { lastClicked = Just i }
