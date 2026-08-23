module Blink.ElementSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Blink.Element
  ( ElementAttrs, ElementEvent (..)
  , element, onClicked, onFocusGained, onFocusLost, onKeyPressed, onMouseDown, onMouseEntered, onMouseExited, onMouseUp
  )
import Blink.ElementBehaviour (elementBehaviourSpec)
import Blink.Geometry (Point (..), Rectangle (..), noBorder, uniform)
import Blink.Input (InputState (..), Key (..), KeyEvent (..))
import Blink.Interaction (Interaction (DragTo, Wait), InteractionResult (..), runInteractions)
import qualified Blink.Interaction as Ixn (Interaction (MouseDown, MouseUp))
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

onA, offBoth, onB :: Point
onA     = Point 10 50
offBoth = Point 200 200
onB     = Point 60 50

-- | Every raw event a reaction built on 'element' can raise, tagged with
-- @e@ and which 'ElementEvent' it was -- since the new batched
-- 'Blink.Element.HasElementEvents' has one smart constructor per event
-- rather than a single catch-all reaction, this lists all eight instead of
-- reacting generically the way the old @onEvent@ escape hatch did.
tagAll :: TestElement -> [ElementAttrs TestElement (TestElement, ElementEvent)]
tagAll e =
  [ onMouseEntered (const [OutMsg (e, MouseEntered)])
  , onMouseExited  (const [OutMsg (e, MouseExited)])
  , onMouseDown    (const [OutMsg (e, MouseDown)])
  , onMouseUp      (const [OutMsg (e, MouseUp)])
  , onClicked      (const [OutMsg (e, Clicked)])
  , onKeyPressed   (\k -> [OutMsg (e, KeyPressed k)])
  , onFocusGained  (const [OutMsg (e, FocusGained)])
  , onFocusLost    (const [OutMsg (e, FocusLost)])
  ]

-- | Runs 'ElemA' at 'rectA' and 'ElemB' at 'rectB' together, each firing its
-- 'ElementEvent's as a message tagged with which element it came from.
both :: UI TestElement (TestElement, ElementEvent) ()
both = do
  withBounds rectA $ element ElemA (tagAll ElemA)
  withBounds rectB $ element ElemB (tagAll ElemB)

advance :: Ord e => InputState -> UIContext e msg -> UIContext e msg
advance input ctx = nextFrameContext testBounds input (contextTheme ctx) (contextAnimation ctx) ctx

seedCtx :: UIContext TestElement String
seedCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

seedBothCtx :: UIContext TestElement (TestElement, ElementEvent)
seedBothCtx = emptyUIContext testBounds noInput testTheme noOpTextMeasurer

spec :: Spec
spec = describe "Blink.Element" $ do
  elementBehaviourSpec testBounds seedCtx ElemA testBounds offBoth (element ElemA :: [ElementAttrs TestElement String] -> UI TestElement String ())

  describe "onKeyPressed" $
    it "reacts with the triggering KeyEvent" $ do
      let attrs :: [ElementAttrs TestElement KeyEvent]
          attrs = [onKeyPressed (\k -> [OutMsg k])]
      ctx0 <- snd <$> runUI (setFocus ElemA) (emptyUIContext testBounds noInput testTheme noOpTextMeasurer)
      ctx  <- snd <$> runUI (element ElemA attrs) (advance (noInput { inputKeyEvents = [KeyEvent KeyReturn []] }) ctx0)
      getMessages ctx `shouldBe` [KeyEvent KeyReturn []]

  describe "cross-element interaction" $ do
    it "reports MouseDown for the element the press started on, and MouseUp for whichever element the release happens over" $ do
      -- Mouse goes down over ElemA, is dragged (still held) onto ElemB, and
      -- released there. ElemA should only ever see MouseDown (it's not hit
      -- by the time the button comes up); ElemB should only ever see
      -- MouseUp (it wasn't hit when the button went down) and never
      -- Clicked -- confirming a drag begun on one element and released
      -- over another doesn't count as a click for the second. The mouse
      -- also crosses from A into B along the way, so both elements' hover
      -- edges fire too.
      result <- runInteractions testBounds seedBothCtx both []
        [Ixn.MouseDown onA, DragTo onB, Ixn.MouseUp onB]
      resultMessages result `shouldBe`
        [ (ElemA, MouseEntered), (ElemA, MouseDown)
        , (ElemA, MouseExited),  (ElemB, MouseEntered)
        , (ElemB, MouseUp)
        ]

    it "reports FocusLost for the loser and FocusGained for the winner, one frame after a focus request" $ do
      result <- runInteractions testBounds seedBothCtx
                  (setFocus ElemA >> requestFocus Nothing ElemB >> both) [Wait 1] []
      resultMessages result `shouldBe` [(ElemA, FocusLost), (ElemB, FocusGained)]
