module Blink.ElementSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes (Attr, onEvent)
import Blink.Element (ElementEvent (..), element, onKeyPressed)
import Blink.ElementBehaviour (elementBehaviourSpec)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Rendering (Colour (..), TextAlign (..))
import Blink.Style (Style (..), StyleSet (..), Theme (..))
import Blink.UI

data TestElement = ElemA | ElemB deriving (Eq, Ord, Show)

-- | Left and right halves of 'testBounds', so 'ElemA' and 'ElemB' occupy
-- disjoint regions -- lets a single frame drive both elements through the
-- same input and have the hit test tell them apart.
rectA, rectB :: Rectangle
rectA = Rectangle 0 0 50 100
rectB = Rectangle 50 0 50 100

testBounds :: Rectangle
testBounds = Rectangle 0 0 100 100

emptyStyle :: Style
emptyStyle = Style
  { styleBackground   = RGBA 0 0 0 1
  , styleTextColour   = RGBA 0 0 0 1
  , styleTextAlign    = AlignCenter
  , styleMargin       = uniform 0
  , stylePadding      = uniform 0
  , styleBorderColour = Nothing
  , styleBorderEdges  = noBorder
  }

emptyStyleSet :: StyleSet
emptyStyleSet = StyleSet
  { styleSetNormal   = emptyStyle
  , styleSetHovered  = emptyStyle
  , styleSetPressed  = emptyStyle
  , styleSetFocused  = emptyStyle
  , styleSetDisabled = emptyStyle
  }

testTheme :: Theme TestElement
testTheme = Theme { themeElementStyles = Map.empty, themeDefaultStyle = emptyStyleSet }

noInput :: InputState
noInput = InputState
  { inputMousePosition  = Point 200 200
  , inputLeftButtonDown = False
  , inputKeyEvents      = []
  , inputTypedText      = []
  }

down :: Point -> InputState
down p = noInput { inputMousePosition = p, inputLeftButtonDown = True }

heldAt :: Point -> InputState
heldAt p = noInput { inputMousePosition = p, inputLeftButtonDown = True }

releasedAt :: Point -> InputState
releasedAt p = noInput { inputMousePosition = p, inputLeftButtonDown = False }

onA, offBoth, onB :: Point
onA     = Point 10 50
offBoth = Point 200 200
onB     = Point 60 50

-- | Runs 'ElemA' at 'rectA' and 'ElemB' at 'rectB' together, each firing its
-- 'ElementEvent's as a message tagged with which element it came from.
both :: UI TestElement (TestElement, ElementEvent) ()
both = do
  withBounds rectA $ element ElemA [onEvent (\ev -> [OutMsg (ElemA, ev)])]
  withBounds rectB $ element ElemB [onEvent (\ev -> [OutMsg (ElemB, ev)])]

startBoth :: InputState -> IO (UIContext TestElement (TestElement, ElementEvent))
startBoth input = snd <$> runUI both (emptyUIContext testBounds input testTheme noOpTextMeasurer)

runBoth :: InputState -> UIContext TestElement (TestElement, ElementEvent) -> IO (UIContext TestElement (TestElement, ElementEvent))
runBoth input ctx = snd <$> runUI both (advance input ctx)

advance :: Ord e => InputState -> UIContext e msg -> UIContext e msg
advance input ctx = nextFrameContext testBounds input (contextTheme ctx) (contextAnimation ctx) ctx

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

spec :: Spec
spec = describe "Blink.Element" $ do
  elementBehaviourSpec testBounds seedCtx ElemA testBounds offBoth (element ElemA :: [Attr TestElement ElementEvent String ()] -> UI TestElement String ())

  describe "onKeyPressed" $
    it "reacts with the triggering KeyEvent" $ do
      let attrs :: [Attr TestElement ElementEvent KeyEvent ()]
          attrs = [onKeyPressed (\k -> [OutMsg k])]
      ctx0 <- snd <$> runUI (setFocus ElemA) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx  <- snd <$> runUI (element ElemA attrs) (advance (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0)
      getMessages ctx `shouldBe` [KeyEvent KeyReturn []]

  describe "cross-element interaction" $ do
    it "reports MouseDown for the element the press started on, and MouseUp for whichever element the release happens over" $ do
      -- Mouse goes down over ElemA, is dragged (still held) onto ElemB, and
      -- released there. ElemA should only ever see MouseDown (it's not hit
      -- by the time the button comes up); ElemB should only ever see
      -- MouseUp (it wasn't hit when the button went down).
      ctx1 <- startBoth (down onA)          -- down edge, over A
      ctx2 <- runBoth (heldAt onB) ctx1      -- held, over B
      ctx3 <- runBoth (releasedAt onB) ctx2  -- up edge, over B
      -- ctx1 is also the first frame the mouse is over A, so MouseEntered
      -- fires alongside MouseDown; ctx2 is where the cursor crosses from A
      -- to B, so both elements' hover edges fire there too.
      getMessages ctx1 `shouldBe` [(ElemA, MouseEntered), (ElemA, MouseDown)]
      getMessages ctx2 `shouldBe` [(ElemA, MouseExited), (ElemB, MouseEntered)]
      getMessages ctx3 `shouldBe` [(ElemB, MouseUp)]
      -- ElemB's messages here are exactly [MouseUp], with no Clicked --
      -- confirming a drag begun on ElemA and released over ElemB doesn't
      -- count as a click for ElemB.

    it "reports FocusLost for the loser and FocusGained for the winner, one frame after a focus request" $ do
      ctx0 <- snd <$> runUI (setFocus ElemA >> requestFocus Nothing ElemB)
                            (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx1 <- runBoth noInput ctx0
      ctx2 <- runBoth noInput ctx1
      getMessages ctx1 `shouldBe` [(ElemA, FocusLost), (ElemB, FocusGained)]
      getMessages ctx2 `shouldBe` []
