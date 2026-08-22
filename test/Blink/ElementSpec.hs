module Blink.ElementSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Attributes (Attr, onEvent)
import Blink.Element
  ( ElementEvent (..), element
  , onMouseEntered, onMouseExited, onMouseDown, onMouseUp, onClicked, onKeyPressed
  , onFocusGained, onFocusLost
  )
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

hoverAt :: Point -> InputState
hoverAt p = noInput { inputMousePosition = p }

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

-- | Captures every 'ElementEvent' fired for a single element in isolation.
capture :: [Attr TestElement ElementEvent ElementEvent ()]
capture = [onEvent (\ev -> [OutMsg ev])]

spec :: Spec
spec = describe "Blink.Element" $ do
  describe "hover" $ do
    it "fires MouseEntered the first frame the mouse is over, not on later frames" $ do
      ctx1 <- startBoth (hoverAt onA)
      ctx2 <- runBoth (hoverAt onA) ctx1
      getMessages ctx1 `shouldBe` [(ElemA, MouseEntered)]
      getMessages ctx2 `shouldBe` []

    it "fires MouseExited the frame the mouse leaves after being over" $ do
      ctx1 <- startBoth (hoverAt onA)
      ctx2 <- runBoth (hoverAt offBoth) ctx1
      getMessages ctx2 `shouldBe` [(ElemA, MouseExited)]

    it "fires nothing for a disabled element, even while the mouse is over it" $ do
      ctx <- snd <$> runUI (disableWhen True $ withBounds rectA $ element ElemA capture)
                            (emptyUIContext testBounds (hoverAt onA) testTheme noOpTextMeasurer)
      getMessages ctx `shouldBe` []

  describe "mouse button" $ do
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

    it "fires Clicked alongside MouseUp when the press and release complete on the same element" $ do
      ctx1 <- startBoth (down onA)
      ctx2 <- runBoth (releasedAt onA) ctx1
      getMessages ctx2 `shouldBe` [(ElemA, MouseUp), (ElemA, Clicked)]

  describe "keyboard" $ do
    it "fires one KeyPressed per key event while the element holds focus" $ do
      ctx0 <- snd <$> runUI (setFocus ElemA) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx  <- runBoth (noInput { inputKeyEvents = [KeyEvent KeyReturn [], KeyEvent KeyTab []] }) ctx0
      getMessages ctx `shouldBe`
        [ (ElemA, KeyPressed (KeyEvent KeyReturn []))
        , (ElemA, KeyPressed (KeyEvent KeyTab []))
        ]

    it "fires nothing for an element that does not hold focus" $ do
      ctx0 <- snd <$> runUI (setFocus ElemB) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx  <- runBoth (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0
      [ ev | (ElemA, ev) <- getMessages ctx ] `shouldBe` []

    it "fires nothing for a disabled element, even while focused" $ do
      ctx0 <- snd <$> runUI (setFocus ElemA) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      let ctx1 = advance (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0
      ctx <- snd <$> runUI (disableWhen True $ element ElemA capture) ctx1
      getMessages ctx `shouldBe` []

  describe "focus" $ do
    it "reports FocusLost for the loser and FocusGained for the winner, one frame after a focus request" $ do
      ctx0 <- snd <$> runUI (setFocus ElemA >> requestFocus Nothing ElemB)
                            (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx1 <- runBoth noInput ctx0
      ctx2 <- runBoth noInput ctx1
      getMessages ctx1 `shouldBe` [(ElemA, FocusLost), (ElemB, FocusGained)]
      getMessages ctx2 `shouldBe` []

    it "reports FocusLost for the cleared element, with nothing gaining it" $ do
      ctx0 <- snd <$> runUI (setFocus ElemA >> requestClearFocus Nothing)
                            (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx1 <- runBoth noInput ctx0
      getMessages ctx1 `shouldBe` [(ElemA, FocusLost)]

  describe "reaction helpers" $ do
    it "onMouseEntered/onMouseExited react only to their own hover edge" $ do
      let attrs =
            [ onMouseEntered (const [OutMsg ("entered" :: String)])
            , onMouseExited  (const [OutMsg "exited"])
            ]
      ctx1 <- snd <$> runUI (element ElemA attrs) (emptyUIContext testBounds (hoverAt onA) testTheme noOpTextMeasurer)
      ctx2 <- snd <$> runUI (element ElemA attrs) (advance (hoverAt offBoth) ctx1)
      getMessages ctx1 `shouldBe` ["entered"]
      getMessages ctx2 `shouldBe` ["exited"]

    it "onMouseDown/onMouseUp react only to their own button edge" $ do
      let attrs =
            [ onMouseDown (const [OutMsg ("down" :: String)])
            , onMouseUp   (const [OutMsg "up"])
            ]
      ctx1 <- snd <$> runUI (element ElemA attrs) (emptyUIContext testBounds (down onA) testTheme noOpTextMeasurer)
      ctx2 <- snd <$> runUI (element ElemA attrs) (advance (releasedAt onA) ctx1)
      getMessages ctx1 `shouldBe` ["down"]
      getMessages ctx2 `shouldBe` ["up"]

    it "onClicked reacts only when the press and release complete on the same element" $ do
      let attrs = [onClicked (const [OutMsg ("clicked" :: String)])]
      ctx1 <- snd <$> runUI (element ElemA attrs) (emptyUIContext testBounds (down onA) testTheme noOpTextMeasurer)
      ctx2 <- snd <$> runUI (element ElemA attrs) (advance (releasedAt onA) ctx1)
      getMessages ctx2 `shouldBe` ["clicked"]

    it "onKeyPressed reacts with the triggering KeyEvent" $ do
      let attrs = [onKeyPressed (\k -> [OutMsg k])]
      ctx0 <- snd <$> runUI (setFocus ElemA) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx  <- snd <$> runUI (element ElemA attrs) (advance (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0)
      getMessages ctx `shouldBe` [KeyEvent KeyReturn []]

    it "onFocusGained/onFocusLost react only to their own side of a transfer" $ do
      let attrsA = [onFocusLost   (const [OutMsg ("A lost"   :: String)])]
          attrsB = [onFocusGained (const [OutMsg ("B gained" :: String)])]
          render = withBounds rectA (element ElemA attrsA) >> withBounds rectB (element ElemB attrsB)
      ctx0 <- snd <$> runUI (setFocus ElemA >> requestFocus Nothing ElemB)
                            (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx  <- snd <$> runUI render (advance noInput ctx0)
      getMessages ctx `shouldBe` ["A lost", "B gained"]
