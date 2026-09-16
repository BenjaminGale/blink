{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ToggleButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.Fixtures (mkTestTheme, plainStyle, plainStyleSet, standardMetrics, testColour)
import Blink.Controls.ToggleButton (ToggleConfig, isSelected, toggleButton, toggleChecked)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)

import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, uniform)
import Blink.Input (InputState (..), emptyInputState)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (StyleSet (..), Theme, styleTextColour)
import Blink.View
import Blink.Element (elLayout, runElement)

data TestElement = Ok deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

toggleTestTheme :: Theme TestElement
toggleTestTheme = mkTestTheme standardMetrics
  ((plainStyleSet (plainStyle testColour))
    { styleOverrides = Map.singleton toggleChecked (\s -> s { styleTextColour = pressedColour }) })

noInput :: InputState
noInput = emptyInputState { inputMousePosition = Point 200 200 }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput toggleTestTheme

-- | See 'Blink.Controls.ButtonSpec.fullSize' -- same reasoning, for
-- 'toggleButton'.
fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = runElement (toggleButton Ok attrs) { elLayout = Layout fill fill TopLeft }

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = snd <$> runView (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.ToggleButton" $ do
  describe "toggleButton" $ do
    toggleBehaviourSpec not testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSize

    it "draws in its normal style while not selected" $ do
      ctx <- start []
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" testColour AlignCenter]

    it "draws in its pressed style while selected, even without being physically pressed" $ do
      ctx <- start [isSelected True]
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" pressedColour AlignCenter]
