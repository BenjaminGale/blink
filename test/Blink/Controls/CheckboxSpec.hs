{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.CheckboxSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Controls.Checkbox (checkbox)
import Blink.Controls.Control (Attribute)
import Blink.Controls.Fixtures (fullSizeAt, hitRectFor, monospaceTextMeasurer, noInput, plainStyle, plainStyleSet, standardMetrics, startAt, testColour)
import Blink.Controls.Label (text)
import Blink.Controls.Style (iconStyleKey)
import Blink.Controls.ToggleButton (ToggleConfig, isSelected)
import Blink.Controls.ToggleBehaviour (toggleBehaviourSpec)
import Blink.Element (measureElement)
import Blink.Geometry (Point (..), Rectangle (..), Size (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..), TextAlign (..))
import Blink.Style (StyleSet (..), Theme (..), VisualState (CommonMouseOver), styleTextColour)
import Blink.View

data TestElement = Remember | FocusHolder deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testStyleSet :: StyleSet
testStyleSet = plainStyleSet (plainStyle testColour)

-- | Distinct from 'testColour', so a test can tell whether the icon
-- actually resolved 'iconHoverColour' the way it does elsewhere via
-- 'Blink.Controls.Style.iconStyleKey', versus falling back to the
-- default style like 'checkboxStyleKey' itself does in this test theme.
iconHoverColour :: Colour
iconHoverColour = RGBA 0 0 1 1

iconStyleSet :: StyleSet
iconStyleSet = StyleSet
  { styleBase      = plainStyle testColour
  , styleOverrides = Map.singleton CommonMouseOver (\s -> s { styleTextColour = iconHoverColour })
  }

testTheme :: Theme TestElement
testTheme = Theme
  { themeElementStyles = Map.singleton iconStyleKey (standardMetrics, iconStyleSet)
  , themeDefaultStyle  = (standardMetrics, testStyleSet)
  }

-- | Covers both the checkbox's glyph (x: 15-43) and caption (x: 49-85),
-- so random points from within it exercise both halves.
hitRect :: Rectangle
hitRect = hitRectFor testBounds

type Attribute' = Attribute (ToggleConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = fullSizeAt (checkbox Remember attrs)

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = startAt seedCtx (fullSize attrs)

spec :: Spec
spec = describe "Blink.Controls.Checkbox" $ do
  toggleBehaviourSpec not testBounds seedCtx Remember FocusHolder (Point 5 5) hitRect (Point 200 200) fullSize

  it "is as wide as its glyph, the gap, its caption and the space after it, plus chrome, by default" $ do
    let ctx = withMeasurers (noOpMeasurers { msrText = monospaceTextMeasurer 10 12 }) seedCtx
    (size, _) <- runView (measureElement testBounds (checkbox Remember [text "Dark"])) ctx
    sizeWidth size `shouldBe` 2 * (10 + 5) + 28 + 6 + 40 + 5 -- margin and padding, glyph, gap, caption, space after

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
