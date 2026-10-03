{-# LANGUAGE OverloadedStrings #-}
-- | Builds a small application with nothing but @import Blink@, so a name
-- an application needs but "Blink" does not export fails to compile here.
module Blink.PublicApiSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Blink

data ControlId = NewItemInput | AddButton | ItemCheckbox Int
  deriving (Eq, Ord)

data Msg
  = SetNewItemText Text
  | AddItem
  | ToggleItem Int Bool

data Todos = Todos
  { newItemText :: Text
  , todoItems   :: [(Text, Bool)]
  } deriving (Eq, Show)

todoView :: Todos -> Element ControlId Msg
todoView s = vBox
  [ children
      ( hBox
          [ height (exactly 30)
          , children
              [ textInput NewItemInput [value (newItemText s), onInput (postWith SetNewItemText), width fill, height fill]
              , button AddButton
                  [ text "Add", onActivated (post AddItem), isEnabled (not (T.null (newItemText s)))
                  , style (Class "accent"), width (exactly 80), height fill
                  ]
              ]
          ]
      : [ checkbox (ItemCheckbox i)
            [text itemLabel, isSelected done, onSelectedChanged (postWith (ToggleItem i)), width fill, height (exactly 24)]
        | (i, (itemLabel, done)) <- zip [0 ..] (todoItems s)
        ]
      )
  ]

todoUpdate :: Msg -> Update Todos ControlId Msg ()
todoUpdate msg = case msg of
  SetNewItemText t -> modify $ \s -> s { newItemText = t }
  AddItem          -> modify $ \s -> s { todoItems = todoItems s ++ [(newItemText s, False)], newItemText = "" }
  ToggleItem i on  -> modify $ \s -> s
    { todoItems = [ if j == i then (t, on) else item | (j, item@(t, _)) <- zip [0 ..] (todoItems s) ] }

accent :: Colour
accent = RGBA 1 0 0 1

todoTheme :: Theme ControlId
todoTheme = (emptyTheme (plainMetrics, plainStyleSet))
  { themeElementStyles = Map.fromList [(Class "accent", (plainMetrics, plainStyleSet { styleBase = plainStyle { styleBackground = accent } }))] }
  where
    plainMetrics  = Metrics { metricsMargin = uniform 0, metricsPadding = uniform 0 }
    plainStyle    = Style { styleBackground = RGBA 0 0 0 1, styleTextColour = RGBA 0 0 0 1, styleTextAlign = AlignLeft, styleBorder = noBorder }
    plainStyleSet = StyleSet { styleBase = plainStyle, styleOverrides = mempty }

todoApp :: Todos -> App ControlId Msg Todos
todoApp initial = App
  { startUp = pure initial
  , theme   = const todoTheme
  , view    = todoView
  , update  = todoUpdate
  }

start :: Todos -> IO (BlinkHandle Todos)
start initial = configureContinuous (todoApp initial) (MsgQueue (\_ -> pure ()) (pure [])) noOpMeasurers

frame :: Point -> Bool -> FrameInput
frame p down = emptyFrameInput { mousePosition = p, mouseButtonDown = down, windowSize = Size 200 200 }

clickAt :: BlinkHandle s -> Point -> IO (FrameResult s)
clickAt handle p = stepFrame handle (frame p True) >> stepFrame handle (frame p False)

stateOf :: FrameResult s -> s
stateOf (Continue _ _ s) = s
stateOf (Quit _ _ s)     = s

drawsOf :: FrameResult s -> [DrawCommand]
drawsOf (Continue ds _ _) = ds
drawsOf (Quit ds _ _)     = ds

addButtonAt, firstItemAt :: Point
addButtonAt = Point 160 15
firstItemAt = Point 10 42

spec :: Spec
spec = describe "an application written against import Blink alone" $ do
  it "reports typed text through a postWith handler" $ do
    handle <- start (Todos "" [])
    _      <- stepFrame handle (frame (Point 0 0) False)
    result <- stepFrame handle (frame (Point 0 0) False) { typedText = ["Milk"] }
    newItemText (stateOf result) `shouldBe` "Milk"

  it "reports a click through a post handler" $ do
    handle <- start (Todos "Milk" [])
    result <- clickAt handle addButtonAt
    stateOf result `shouldBe` Todos "" [("Milk", False)]

  it "ignores a click on a control disabled with isEnabled" $ do
    handle <- start (Todos "" [])
    result <- clickAt handle addButtonAt
    todoItems (stateOf result) `shouldBe` []

  it "reports a toggle with its new value through a postWith handler" $ do
    handle <- start (Todos "" [("Milk", False)])
    result <- clickAt handle firstItemAt
    todoItems (stateOf result) `shouldBe` [("Milk", True)]

  it "draws a control with the class chosen by style" $ do
    handle <- start (Todos "Milk" [])
    result <- stepFrame handle (frame (Point 0 0) False)
    [c | FillRect _ c <- drawsOf result] `shouldContain` [accent]
