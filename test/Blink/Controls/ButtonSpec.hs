{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.ButtonSpec (spec) where

import Test.Hspec

import Blink.Controls.Button (ButtonActivation (..), ButtonConfig, activation, button, onActivated)
import Blink.Controls.ButtonBehaviour (buttonBehaviourSpec, defaultButtonBehaviourConfig)
import Blink.Controls.Control (Attribute, post)
import Blink.Controls.Fixtures
  ( contentRectFor, fullSizeAt, hitRectFor, mkTestTheme, monospaceTextMeasurer, noInput, plainStyle, plainStyleSet, standardMetrics
  , startAt, testColour
  )
import Blink.Controls.Label (text)
import qualified Data.Text as T

import Blink.Geometry (Alignment (TopLeft), Point (..), Rectangle (..), Size (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Rendering (DrawCommand (..), TextAlign (..))
import Blink.Style (Theme)
import Blink.View
import Blink.Element (elLayout, runElement)

data TestElement = Ok deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testTheme :: Theme TestElement
testTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

hitRect :: Rectangle
hitRect = hitRectFor testBounds

contentRect :: Rectangle
contentRect = contentRectFor testBounds

type Attribute' = Attribute (ButtonConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = fullSizeAt (button Ok attrs)

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = startAt seedCtx (fullSize attrs)

spec :: Spec
spec = describe "Blink.Controls.Button" $ do
  buttonBehaviourSpec defaultButtonBehaviourConfig testBounds seedCtx Ok (Point 5 5) hitRect (Point 200 200) fullSize

  it "draws its text in the resolved style" $ do
    ctx <- start [text "OK"]
    getDrawCommands ctx `shouldContain` [DrawText contentRect "OK" testColour AlignCenter]

  it "defaults to filling the given width and fitting its own content height" $ do
    -- Every other test in this file overrides 'elLayout' ('fullSize' to
    -- 'Layout fill fill TopLeft', or 'fitContentEl' below); this is the
    -- only check of the actual default a caller gets without overriding it.
    let layout = elLayout (button Ok [text "OK"])
    layoutWidth layout `shouldBe` fill
    layoutHeight layout `shouldBe` fitContent
    layoutAlignment layout `shouldBe` TopLeft

  describe "activation ActivateOnPress" $ do
    -- 'buttonBehaviourSpec' above only covers the default 'ActivateOnClick'
    -- activation -- these check that 'button' itself, not just
    -- 'Blink.Controls.RepeatButton.repeatButton', honours the
    -- 'ActivateOnPress' override 'activation' exposes.
    let insidePoint = Point 50 50
        taggedActivated = [activation ActivateOnPress, onActivated (post "Activated")]

    it "fires onActivated immediately on press, not on release" $ do
      result <- runInteractions testBounds seedCtx (fullSize taggedActivated) [] [MouseDown insidePoint]
      resultMessages result `shouldBe` ["Activated"]

    it "does not fire onActivated again on release" $ do
      result <- runInteractions testBounds seedCtx (fullSize taggedActivated) [MouseDown insidePoint] [MouseUp insidePoint]
      resultMessages result `shouldBe` []

  describe "FitContent sizing" $ do
    -- Spec scenario: a button with 'width fitContent' (here, both axes, via
    -- a direct 'elLayout' override -- there is no public 'width'/'height'
    -- attribute for controls yet) sizes itself to its own chrome-wrapped
    -- caption. Verified against a manually computed 'Exactly' from the same
    -- style, to the pixel, per invariant 5 (chrome insets defined once).
    let chromeWidth  = 2 * (10 + 5)  -- margin + padding, both sides; no border
        chromeHeight = 2 * (10 + 5)
        fitContentEl attrs = runElement (button Ok attrs) { elLayout = Layout fitContent fitContent TopLeft }
        fitCtx = withMeasurers (noOpMeasurers { msrText = monospaceTextMeasurer 10 12 })
                   (emptyViewContext (Rectangle 0 0 500 500) noInput testTheme)
        -- The background rect 'renderStyled' fills is the outer bounds inset
        -- by margin (10px each side) -- not the outer bounds themselves.
        expectedBgFor caption =
          let contentSize = Size (fromIntegral (T.length caption) * 10) 12
              expectedW   = sizeWidth contentSize + chromeWidth
              expectedH   = sizeHeight contentSize + chromeHeight
          in Rectangle 10 10 (expectedW - 20) (expectedH - 20)

    it "sizes to its chrome-wrapped caption, matching a manual computation from the same style" $ do
      let caption = "OK"
      ctx <- snd <$> runView (fitContentEl [text caption]) fitCtx
      getDrawCommands ctx `shouldContain` [FillRect (expectedBgFor caption) testColour]

    it "sizes to just its chrome when the caption is empty" $ do
      let caption = ""
      ctx <- snd <$> runView (fitContentEl [text caption]) fitCtx
      getDrawCommands ctx `shouldContain` [FillRect (expectedBgFor caption) testColour]
