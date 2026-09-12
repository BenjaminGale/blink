{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.RadioButtonSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.Label (text)
import Blink.Controls.RadioButton (radioButton)
import Blink.Controls.Style (iconStyleKey)
import Blink.Controls.ToggleButton (ToggleConfig, isSelected)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)
import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), insetRect, noBorder, uniform)
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (Metrics (..), Style (..), StyleSet (..), Theme (..), VisualState (CommonMouseOver))
import Blink.View
import Blink.Element (elLayout, runElement)

data TestElement = OptionA deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testColour :: Colour
testColour = RGBA 0 0 0 1

testStyle :: Style
testStyle = Style
  { styleBackground   = testColour
  , styleTextColour   = testColour
  , styleTextAlign    = AlignCenter
  , styleBorderColour = Nothing
  }

testMetrics :: Metrics
testMetrics = Metrics
  { metricsMargin      = uniform 10
  , metricsPadding     = uniform 5
  , metricsBorderEdges = noBorder
  }

testStyleSet :: StyleSet
testStyleSet = StyleSet { styleBase = testStyle, styleOverrides = Map.empty }

-- | Distinct from 'testColour', so a test can tell whether the icon
-- actually resolved 'iconHoverColour' the way it does elsewhere via
-- 'Blink.Controls.Style.iconStyleKey', versus falling back to the
-- default style like 'radioButtonStyleKey' itself does in this test theme.
iconHoverColour :: Colour
iconHoverColour = RGBA 0 0 1 1

iconStyleSet :: StyleSet
iconStyleSet = StyleSet
  { styleBase      = testStyle
  , styleOverrides = Map.singleton CommonMouseOver (\s -> s { styleTextColour = iconHoverColour })
  }

testTheme :: Theme TestElement
testTheme = Theme
  { themeElementStyles = Map.singleton iconStyleKey (testMetrics, iconStyleSet)
  , themeDefaultStyle  = (testMetrics, testStyleSet)
  }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  , inputWheelDelta     = 0
  }

-- | The margin-inset hit area for a control rendered at 'testBounds' with
-- the 10px margin the test style here uses -- covers both the radio
-- button's glyph (x: 15-35) and caption (x: 41-85), so random points from
-- within it exercise both halves.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a radio button that fills its given bounds
-- entirely, as every control did before controls reported their own
-- 'Layout'. 'radioButton' now defaults to sizing itself to its own content,
-- so these tests ask for the old full-size behaviour explicitly, the same
-- way any other caller would.
fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = runElement (radioButton OptionA attrs) { elLayout = Layout fill fill TopLeft }

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = snd <$> runView (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.RadioButton" $ do
  -- Unlike a flipping toggle, a radio button only ever moves from
  -- unselected to selected -- activating it while already selected leaves
  -- it selected, so it reports nothing.
  toggleBehaviourSpec (const True) testBounds seedCtx OptionA (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws the unselected-bullet icon and its caption while not selected" $ do
    ctx <- start [text "Option A"]
    getDrawCommands ctx `shouldContain`
      [ DrawImage (Rectangle 15 40 20 20) "assets/icons/radio_button_unchecked.svg" testColour
      , DrawText (Rectangle 41 15 44 70) "Option A" testColour AlignCenter
      ]

  it "draws the selected-bullet icon while selected" $ do
    ctx <- start [text "Option A", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawImage (Rectangle 15 40 20 20) "assets/icons/radio_button_checked.svg" testColour]

  it "tints the icon a different colour while the cursor is over the icon itself" $ do
    -- Point 25 50 sits inside the icon's own rect (15,40)-(35,60).
    result <- runInteractions testBounds seedCtx (fullSize [text "Option A"])
                [] [MoveTo (Point 25 50)]
    resultDraws result `shouldContain`
      [ DrawImage (Rectangle 15 40 20 20) "assets/icons/radio_button_unchecked.svg" iconHoverColour ]

  it "leaves the icon's resting colour alone when only the caption is hovered" $ do
    -- Point 70 50 sits over the caption (x: 41-85), well outside the
    -- icon's own rect -- confirms hovering the row elsewhere doesn't
    -- also tint the icon.
    result <- runInteractions testBounds seedCtx (fullSize [text "Option A"])
                [] [MoveTo (Point 70 50)]
    resultDraws result `shouldContain`
      [ DrawImage (Rectangle 15 40 20 20) "assets/icons/radio_button_unchecked.svg" testColour ]
