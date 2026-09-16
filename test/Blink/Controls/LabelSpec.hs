{-# LANGUAGE OverloadedStrings #-}
module Blink.Controls.LabelSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Blink.Controls.Control (Attribute)
import Blink.Controls.ControlBehaviour (ControlBehaviourConfig (..), controlBehaviourSpec)
import Blink.Controls.FixedFocusBehaviour (fixedNotFocusableSpec)
import Blink.Controls.Fixtures
  (fullSizeAt, hitRectFor, mkTestTheme, noInput, plainStyle, plainStyleSet, standardMetrics, startAt, testColour)
import Blink.Geometry (Point (..), Rectangle (..), Size (..))
import Blink.Input (InputState (..))
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Controls.Label (LabelConfig, label, mnemonic, target, text)
import Blink.Rendering (DrawCommand (..), TextAlign (..))
import Blink.Style (Theme)
import Blink.View

data TestElement = Caption | Target deriving (Eq, Ord, Show)

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

testTheme :: Theme TestElement
testTheme = mkTestTheme standardMetrics (plainStyleSet (plainStyle testColour))

onCaption :: Point
onCaption = Point 50 50

hitRect :: Rectangle
hitRect = hitRectFor testBounds

type Attribute' = Attribute (LabelConfig TestElement String)

seedCtx :: ViewContext TestElement String
seedCtx = emptyViewContext testBounds noInput testTheme

fullSize :: [Attribute'] -> View TestElement String ()
fullSize attrs = fullSizeAt (label Caption attrs)

start :: [Attribute'] -> IO (ViewContext TestElement String)
start attrs = startAt seedCtx (fullSize attrs)

-- | Same as 'seedCtx', but with Alt reported held this frame.
altHeldCtx :: ViewContext TestElement String
altHeldCtx = emptyViewContext testBounds (noInput { inputAltHeld = True }) testTheme

startWithAltHeld :: [Attribute'] -> IO (ViewContext TestElement String)
startWithAltHeld attrs = startAt altHeldCtx (fullSize attrs)

-- | Where 'mnemonic'\'s underline lands for @\"Hello\"@\'s @\'e\'@ under
-- 'testStyle'\'s 'AlignCenter' and 'noOpMeasurers' (every measurement,
-- including text height, 0): centred both ways in the 15,15-85,85 content
-- rect (a zero-height glyph's "bottom edge" is its vertical centre), one
-- pixel tall.
mnemonicUnderline :: DrawCommand
mnemonicUnderline = FillRect (Rectangle 50 50 0 1) testColour

-- | 10px per character, 20px tall, regardless of the string -- enough to
-- tell a glyph-relative underline (correct) apart from a bounds-relative
-- one (the bug the underline's own y once had: it used the bottom of
-- @bounds@, correct only when @bounds@ happens to be glyph-height, which a
-- real "Blink.Controls.MenuBar" label filling its bar's full height never
-- is).
fakeTextMeasurer :: TextMeasurer
fakeTextMeasurer = TextMeasurer
  { tmCharOffset   = \_ n -> pure (fromIntegral n * 10)
  , tmCharAtOffset = \_ x -> pure (round (x / 10))
  , tmTextSize     = \t -> pure (Size (fromIntegral (T.length t) * 10) 20)
  }

-- | Much taller than the text it holds -- a 200x200 box under 'testMetrics'
-- (10px margin, 5px padding) leaves a 170x170 content rect for 20px-tall
-- text, standing in for a menu-bar label filling its bar's full height.
tallBounds :: Rectangle
tallBounds = Rectangle 0 0 200 200

tallAltHeldCtx :: ViewContext TestElement String
tallAltHeldCtx =
  withMeasurers (noOpMeasurers { msrText = fakeTextMeasurer })
    (emptyViewContext tallBounds (noInput { inputAltHeld = True }) testTheme)

startTall :: [Attribute'] -> IO (ViewContext TestElement String)
startTall attrs = startAt tallAltHeldCtx (fullSize attrs)

-- | Same shape as 'fakeTextMeasurer', but 40px tall -- taller than
-- 'shortBounds'\'s own content rect, standing in for a real font whose
-- line height exceeds a tightly-sized menu-bar row.
fakeOverflowingTextMeasurer :: TextMeasurer
fakeOverflowingTextMeasurer = fakeTextMeasurer { tmTextSize = \t -> pure (Size (fromIntegral (T.length t) * 10) 40) }

-- | A 200x50 box under 'testMetrics' leaves a 170x20 content rect --
-- shorter than 'fakeOverflowingTextMeasurer'\'s 40px text, so the glyph
-- itself overflows the content rect 'Blink.View.Drawing.withClip' clips
-- 'control''s content to.
shortBounds :: Rectangle
shortBounds = Rectangle 0 0 200 50

startShortOverflowing :: [Attribute'] -> IO (ViewContext TestElement String)
startShortOverflowing attrs =
  startAt
    (withMeasurers (noOpMeasurers { msrText = fakeOverflowingTextMeasurer })
      (emptyViewContext shortBounds (noInput { inputAltHeld = True }) testTheme))
    (fullSize attrs)

spec :: Spec
spec = describe "Blink.Controls.Label" $ do
  controlBehaviourSpec (ControlBehaviourConfig { cbcAutoClaims = False, cbcClickFocuses = False })
    testBounds seedCtx Caption (Point 5 5) hitRect (Point 200 200) fullSize

  fixedNotFocusableSpec testBounds seedCtx fullSize

  it "draws its text in the resolved style" $ do
    ctx <- start [text "Hello"]
    getDrawCommands ctx `shouldContain` [DrawText (Rectangle 15 15 70 70) "Hello" testColour AlignCenter]

  describe "mnemonic" $ do
    it "draws no underline when no mnemonic is set, even with Alt held" $ do
      ctx <- startWithAltHeld [text "Hello"]
      getDrawCommands ctx `shouldNotContain` [mnemonicUnderline]

    it "draws no underline for a set mnemonic while Alt isn't held" $ do
      ctx <- start [text "Hello", mnemonic 'e']
      getDrawCommands ctx `shouldNotContain` [mnemonicUnderline]

    it "underlines the mnemonic letter while Alt is held" $ do
      ctx <- startWithAltHeld [text "Hello", mnemonic 'e']
      getDrawCommands ctx `shouldContain` [mnemonicUnderline]

    it "matches the mnemonic letter case-insensitively" $ do
      ctx <- startWithAltHeld [text "Hello", mnemonic 'E']
      getDrawCommands ctx `shouldContain` [mnemonicUnderline]

    it "sits just under the glyph itself, not the bottom of bounds much taller than it" $ do
      ctx <- startTall [text "Hi", mnemonic 'H']
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 90 110 10 1) testColour]

    it "stays inside bounds shorter than the glyph, rather than landing in the clipped-away overflow" $ do
      ctx <- startShortOverflowing [text "Hi", mnemonic 'H']
      getDrawCommands ctx `shouldContain` [FillRect (Rectangle 90 34 10 1) testColour]

  it "never claims focus, even with nothing else focused" $ do
    result <- runInteractions testBounds seedCtx (fullSize []) [] []
    contextFocus (resultContext result) `shouldBe` Nothing

  it "does not take focus on mouse-down by default" $ do
    result <- runInteractions testBounds seedCtx (fullSize []) [] [MouseDown onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Nothing

  it "does not redirect focus onto target on mouse-down alone -- only on a full click" $ do
    let attrs = [target Target]
    result <- runInteractions testBounds seedCtx (fullSize attrs) [] [MouseDown onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Nothing

  it "redirects a click's focus onto the element named by target" $ do
    let attrs = [target Target]
    result <- runInteractions testBounds seedCtx (fullSize attrs) [] [ClickAt onCaption, Wait 1]
    contextFocus (resultContext result) `shouldBe` Just Target
