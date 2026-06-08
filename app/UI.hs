{-# LANGUAGE OverloadedStrings #-}
module UI (Element, Command (..), AppState (..), demoApp) where

import Blink
import Data.Text (Text)
import qualified Data.Map.Strict as Map

data Element = Btn Int | TextInput1
             | CheckboxBox1 | CheckboxLabel1
             | CheckboxBox2 | CheckboxLabel2
             | CheckboxBox3 | CheckboxLabel3
  deriving (Eq, Ord)

data Command = Clicked Int | TextChanged Text
             | Checkbox1Toggled Bool | Checkbox2Toggled Bool | Checkbox3Toggled Bool

data AppState = AppState
  { lastClicked :: Maybe Int
  , inputText   :: Text
  , isChecked1  :: Bool
  , isChecked2  :: Bool
  , isChecked3  :: Bool
  }

demoTheme :: Theme Element
demoTheme = Theme
  { elementStyles = Map.fromList
      [ (TextInput1,     textInputStyle)
      , (CheckboxBox1,   checkboxBoxStyle)
      , (CheckboxBox2,   checkboxBoxStyle)
      , (CheckboxBox3,   checkboxBoxStyle)
      , (CheckboxLabel1, labelStyle)
      , (CheckboxLabel2, labelStyle)
      , (CheckboxLabel3, labelStyle)
      ]
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

textInputStyle :: StyleSet
textInputStyle = StyleSet
  { normal   = base { background = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.600 0.600 0.620 1) }
  , hovered  = base { background = RGBA 0.97 0.97 0.97 1, borderColour = Just (RGBA 0.400 0.400 0.420 1) }
  , pressed  = base { background = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.102 0.435 0.831 1) }
  , focused  = base { background = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.102 0.435 0.831 1) }
  , disabled = base { background = RGBA 0.95 0.95 0.95 1, textColour   = RGBA 0.682 0.682 0.698 1 }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = RGBA 0.11 0.11 0.12 1
      , textAlign    = AlignLeft
      , margin       = uniform 3
      , padding      = uniform 6
      , borderColour = Nothing
      , borderWidth  = 1
      }

labelStyle :: StyleSet
labelStyle = StyleSet
  { normal   = base
  , hovered  = base
  , pressed  = base
  , focused  = base
  , disabled = base { textColour = RGBA 0.682 0.682 0.698 1 }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 0
      , textColour   = RGBA 0.11 0.11 0.12 1
      , textAlign    = AlignLeft
      , margin       = uniform 0
      , padding      = uniform 0
      , borderColour = Nothing
      , borderWidth  = 0
      }

checkboxBoxStyle :: StyleSet
checkboxBoxStyle = StyleSet
  { normal   = base { background = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.600 0.600 0.620 1) }
  , hovered  = base { background = RGBA 0.97 0.97 0.97 1, borderColour = Just (RGBA 0.400 0.400 0.420 1) }
  , pressed  = base { background = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.102 0.435 0.831 1) }
  , focused  = base { background = RGBA 1.0  1.0  1.0  1, borderColour = Just (RGBA 0.102 0.435 0.831 1) }
  , disabled = base { background = RGBA 0.95 0.95 0.95 1, textColour   = RGBA 0.682 0.682 0.698 1 }
  }
  where
    base = Style
      { background   = RGBA 0 0 0 1
      , textColour   = RGBA 0.11 0.11 0.12 1
      , textAlign    = AlignCenter
      , margin       = uniform 3
      , padding      = uniform 2
      , borderColour = Nothing
      , borderWidth  = 1
      }

demoApp :: App Element AppState Command
demoApp = App
  { startUp = pure (AppState Nothing "" False False False)
  , theme   = demoTheme
  , view    = demoView
  , update  = demoUpdate
  }

btn :: Int -> Text -> UI Element Command ()
btn i txt = do
  clicked <- button (Btn i) txt
  if clicked then dispatch (Clicked i) else pure ()

withBg :: Colour -> UI Element Command () -> UI Element Command ()
withBg colour ui = fillRect colour >> ui

-- Row 1: fill behaviour — fixed | fill | fill | fixed (two fills share surplus evenly)
row1 :: UI Element Command ()
row1 = withBg (RGBA 0.95 0.87 0.87 1) $
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center })
    [ (RectConstraint (Exactly 80) Fill TopLeft, btn 1 "Left")
    , (RectConstraint Fill         Fill Center,  btn 2 "<fill 1>")
    , (RectConstraint Fill         Fill Center,  btn 3 "<fill 2>")
    , (RectConstraint (Exactly 80) Fill TopLeft, btn 4 "Right")
    ]

-- Row 2: fillCross = False, children top/centre/bottom aligned
row2 :: UI Element Command ()
row2 = withBg (RGBA 0.87 0.95 0.87 1) $
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = Center, boxFillCross = False })
    [ (RectConstraint (Exactly 100) (Exactly 30) TopLeft,    btn 5 "Top")
    , (RectConstraint (Exactly 100) (Exactly 50) MiddleLeft, btn 6 "Mid")
    , (RectConstraint (Exactly 100) (Exactly 40) BottomLeft, btn 7 "Bot")
    ]

-- Row 3: same constraints as row 2, fillCross = True, content aligned to the right
row3 :: UI Element Command ()
row3 = withBg (RGBA 0.87 0.87 0.95 1) $
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4, boxAlignment = MiddleRight })
    [ (RectConstraint (Exactly 100) (Exactly 30) TopLeft,    btn 8 "Top")
    , (RectConstraint (Exactly 100) (Exactly 50) MiddleLeft, btn 9 "Mid")
    , (RectConstraint (Exactly 100) (Exactly 40) BottomLeft, btn 10 "Bot")
    ]

-- Row 4: text input
row4 :: AppState -> UI Element Command ()
row4 s = withBg (RGBA 0.95 0.95 0.87 1) $
  hBox (defaultBoxConfig { boxSpacing = 4, boxMargin = 4 })
    [ (RectConstraint Fill (Exactly 30) MiddleLeft,
         textInput TextInput1 (inputText s) TextChanged) ]

-- Row 5: checkboxes
row5 :: AppState -> UI Element Command ()
row5 s = withBg (RGBA 0.95 0.87 0.95 1) $
  hBox (defaultBoxConfig { boxSpacing = 16, boxMargin = 4 })
    [ (RectConstraint (Exactly 160) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox1 CheckboxLabel1 "Enable feature" (isChecked1 s) Checkbox1Toggled)
    , (RectConstraint (Exactly 160) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox2 CheckboxLabel2 "Dark mode"      (isChecked2 s) Checkbox2Toggled)
    , (RectConstraint (Exactly 160) (Exactly 30) MiddleLeft,
         checkbox CheckboxBox3 CheckboxLabel3 "Notifications"  (isChecked3 s) Checkbox3Toggled)
    ]

demoView :: AppState -> UI Element Command ()
demoView s = vBox (defaultBoxConfig { boxSpacing = 8, boxMargin = 8 })
  [ (RectConstraint Fill (Exactly 50) TopLeft, row1)
  , (RectConstraint Fill Fill         TopLeft, row2)
  , (RectConstraint Fill (Exactly 80) TopLeft, row3)
  , (RectConstraint Fill (Exactly 50) TopLeft, row4 s)
  , (RectConstraint Fill (Exactly 50) TopLeft, row5 s)
  ]

demoUpdate :: Command -> Update AppState Command ()
demoUpdate (Clicked i)           = modify $ \s -> s { lastClicked = Just i }
demoUpdate (TextChanged t)       = modify $ \s -> s { inputText = t }
demoUpdate (Checkbox1Toggled v)  = modify $ \s -> s { isChecked1 = v }
demoUpdate (Checkbox2Toggled v)  = modify $ \s -> s { isChecked2 = v }
demoUpdate (Checkbox3Toggled v)  = modify $ \s -> s { isChecked3 = v }
