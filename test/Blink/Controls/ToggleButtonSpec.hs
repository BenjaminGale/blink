{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ToggleButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.Fixtures
  ( contentRectFor, fullSizeAt, hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics
  , startAt, testColour
  )
import Blink.Controls.ToggleButton (ToggleConfig, isSelected, toggleButton, toggleChecked)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)

import Blink.Geometry (Point (..), Rectangle (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (StyleSet (..), Theme, styleTextColour)
import Blink.View

data TestElement = Ok deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

toggleTestTheme :: Theme TestElement
toggleTestTheme = mkTestTheme standardMetrics
  ((plainStyleSet (plainStyle testColour))
    { styleOverrides = Map.singleton toggleChecked (\s -> s { styleTextColour = pressedColour }) })

hitRect :: Rectangle
hitRect = hitRectFor testBounds

contentRect :: Rectangle
contentRect = contentRectFor testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput toggleTestTheme

fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = fullSizeAt (toggleButton Ok attrs)

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = startAt seedCtx (fullSize attrs)

spec :: Spec
spec = describe "Blink.Controls.ToggleButton" $ do
  describe "toggleButton" $ do
    toggleBehaviourSpec not testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSize

    it "draws in its normal style while not selected" $ do
      ctx <- start []
      getDrawCommands ctx `shouldContain` [DrawText contentRect "" testColour AlignCenter]

    it "draws in its pressed style while selected, even without being physically pressed" $ do
      ctx <- start [isSelected True]
      getDrawCommands ctx `shouldContain` [DrawText contentRect "" pressedColour AlignCenter]
