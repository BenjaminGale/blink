{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Button (ButtonConfig, ToggleConfig, button, isSelected, toggleButton)
import Blink.Controls.ButtonBehaviour (buttonBehaviourSpec)
import Blink.Controls.Element (Attr)
import Blink.Controls.Label (text)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)
import Blink.Geometry (Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = Ok deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 10
  , stylePadding      = uniform 5
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet
  { styleSetNormal   = testStyle
  , styleSetHovered  = testStyle
  , styleSetPressed  = testStyle
  , styleSetFocused  = testStyle
  , styleSetDisabled = testStyle
  }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = testStyleSet }

pressedColour :: Colour
pressedColour = RGBA 1 1 1 1

toggleTestTheme :: Theme TestElement
toggleTestTheme = Theme
  { themeElementStyles = Map.empty
  , themeDefaultStyle  = testStyleSet { styleSetPressed = testStyle { styleTextColour = pressedColour } }
  }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin every test style here uses.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attr' = Attr (ButtonConfig TestElement String)

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

start :: [Attr'] -> IO (UIContext TestElement String)
start attrs = snd <$> runUI (button Ok attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Button" $ do
  buttonBehaviourSpec testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) (button Ok)

  it "draws its text in the resolved style" $ do
    ctx <- start [text "OK"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "OK" testColour AlignCenter]

  describe "toggleButton" $ do
    toggleBehaviourSpec not testBounds toggleSeedCtx Ok (Point 5 5) hitRect (Point 200 200) (toggleButton Ok)

    it "draws in its normal style while not selected" $ do
      ctx <- startToggle []
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" testColour AlignCenter]

    it "draws in its pressed style while selected, even without being physically pressed" $ do
      ctx <- startToggle [isSelected True]
      getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "" pressedColour AlignCenter]

type ToggleAttr' = Attr (ToggleConfig TestElement String)

toggleSeedCtx :: UIContext TestElement String
toggleSeedCtx = emptyUIContext testBounds noInput toggleTestTheme noOpTextMeasurer

startToggle :: [ToggleAttr'] -> IO (UIContext TestElement String)
startToggle attrs = snd <$> runUI (toggleButton Ok attrs) toggleSeedCtx
