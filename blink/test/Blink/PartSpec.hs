{-# LANGUAGE OverloadedStrings #-}
module Blink.PartSpec (spec) where

import Test.Hspec

import Blink.Controls.Button (button, onActivated)
import Blink.Controls.Fixtures (mkTestTheme, noInput, plainStyle, plainStyleSet, testColour, zeroMetrics)
import Blink.Controls.Label (text)
import Blink.Element (Element, height, part, runElement)
import Blink.Geometry (Point (..), Rectangle (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (fill)
import Blink.Style (Theme)
import Blink.Testing
import Blink.View (ControlId (..), View)

data TestElement = Picker | Other | FocusHolder
  deriving (Eq, Ord, Show)

testTheme :: Theme TestElement
testTheme = mkTestTheme zeroMetrics (plainStyleSet (plainStyle testColour))

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

-- | The given elements side by side across 'testBounds'.
side :: [Element TestElement String] -> View TestElement String ()
side els = runElement (hBox [children els])

-- | A button filling its slot's height, so it can be clicked even though
-- the test's text measurer gives its caption no size.
btn :: TestElement -> String -> Element TestElement String
btn eid msg = button eid [text "B", height fill, onActivated msg]

-- | Rendered first, so it takes focus by itself; focus only moves to the
-- control under test if the click reaches it.
holder :: Element TestElement String
holder = btn FocusHolder "holder"

rightHalf :: Point
rightHalf = Point 75 50

clickOn :: Point -> [Interaction]
clickOn p = [MoveTo p, ClickAt p]

spec :: Spec
spec = describe "Blink.Element.part" $ do
  it "tracks controls sharing one id in different parts separately" $ do
    let view = side
          [ part Picker "left"  (btn Picker "left")
          , part Picker "right" (btn Picker "right")
          ]
    result <- runInteractions testBounds seedCtx view [] (clickOn rightHalf)
    resultMessages result `shouldBe` ["right"]

  it "stores a control in nested parts under the joined path" $ do
    let view = side [holder, part Picker "outer" (part Picker "inner" (btn Picker "inner"))]
    result <- runInteractions testBounds seedCtx view [] (clickOn rightHalf ++ [Wait 1])
    contextFocus (resultContext result) `shouldBe` Just (Part Picker "outer/inner")

  it "leaves a control with a different id inside a part as itself" $ do
    let view = side [holder, part Picker "inner" (btn Other "other")]
    result <- runInteractions testBounds seedCtx view [] (clickOn rightHalf ++ [Wait 1])
    contextFocus (resultContext result) `shouldBe` Just (Control Other)
