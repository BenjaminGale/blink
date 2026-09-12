{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.CheckboxSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Control (Attribute)
import Blink.Controls.Label (text)
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

data TestElement = Remember deriving (Eq, Ord, Show)

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
-- default style like 'checkboxStyleKey' itself does in this test theme.
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
-- the 10px margin every test style here uses -- covers both the checkbox's
-- glyph (x: 15-43) and caption (x: 49-85), so random points from within it
-- exercise both halves.
hitRect :: Rectangle
hitRect = insetRect (uniform 10) testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

-- | The behaviour contracts below are about interaction, not sizing --
-- they're written against a checkbox that fills its given bounds entirely,
-- as every control did before controls reported their own 'Layout'.
-- 'checkbox' now defaults to sizing itself to its own content, so these
-- tests ask for the old full-size behaviour explicitly, the same way any
-- other caller would.
fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = runElement (checkbox Remember attrs) { elLayout = Layout fill fill TopLeft }

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = snd <$> runView (fullSize attrs) seedCtx

spec :: Spec
spec = describe "Blink.Controls.Checkbox" $ do
  toggleBehaviourSpec not testBounds seedCtx Remember (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws the empty-box icon and its caption while not selected" $ do
    ctx <- start [text "Remember me"]
    let cmds = getDrawCommands ctx
    cmds `shouldContain`
      [ DrawImage (Rectangle 16 37 26 26) "assets/icons/check_box_outline_blank.svg" testColour
      , DrawText (Rectangle 49 15 36 70) "Remember me" testColour AlignCenter
      ]
    cmds `shouldNotContain` [DrawImage (Rectangle 16 37 26 26) "assets/icons/check_box.svg" testColour]

  it "draws the checked-box icon while selected" $ do
    ctx <- start [text "Remember me", isSelected True]
    getDrawCommands ctx `shouldContain` [DrawImage (Rectangle 16 37 26 26) "assets/icons/check_box.svg" testColour]

  it "tints the icon a different colour while the cursor is over the icon itself" $ do
    -- Point 25 50 sits inside the icon's own rect (16,37)-(42,63).
    result <- runInteractions testBounds seedCtx (fullSize [text "Remember me"])
                [] [MoveTo (Point 25 50)]
    resultDraws result `shouldContain`
      [ DrawImage (Rectangle 16 37 26 26) "assets/icons/check_box_outline_blank.svg" iconHoverColour ]

  it "leaves the icon's resting colour alone when only the caption is hovered" $ do
    -- Point 70 50 sits over the caption (x: 49-85), well outside the
    -- icon's own rect -- confirms hovering the row elsewhere doesn't
    -- also tint the icon.
    result <- runInteractions testBounds seedCtx (fullSize [text "Remember me"])
                [] [MoveTo (Point 70 50)]
    resultDraws result `shouldContain`
      [ DrawImage (Rectangle 16 37 26 26) "assets/icons/check_box_outline_blank.svg" testColour ]
