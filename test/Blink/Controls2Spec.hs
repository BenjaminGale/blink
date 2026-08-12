module Blink.Controls2Spec (spec) where

import Test.Hspec

import Blink.Controls2 (Attr (..), configure, fire, onAny)
import Blink.ControlsTestSupport (TestElement (..), controlRect, noInput, testTheme)
import Blink.Input (InputState)
import Blink.UI

-- | 'Blink.ControlsTestSupport.mkCtx' fixes @msg ~ ()@; 'fire' is tested
-- against several different message types, so this stays polymorphic.
mkCtxFor :: InputState -> UIContext TestElement msg
mkCtxFor input = emptyUIContext controlRect input testTheme noOpTextMeasurer

data DummyEvent = Ping | Pong deriving (Eq, Show)

data DummyConfig = DummyConfig { dummyFlag :: Bool, dummyCount :: Int }
  deriving (Eq, Show)

defaultDummyConfig :: DummyConfig
defaultDummyConfig = DummyConfig { dummyFlag = False, dummyCount = 0 }

setFlag :: Bool -> Attr e ev msg DummyConfig
setFlag b = Config $ \cfg -> cfg { dummyFlag = b }

addCount :: Int -> Attr e ev msg DummyConfig
addCount n = Config $ \cfg -> cfg { dummyCount = dummyCount cfg + n }

onPing :: msg -> Attr e DummyEvent msg cfg
onPing msg = onAny $ \ev -> case ev of
  Ping -> [OutMsg msg]
  Pong -> []

runFire :: Ord e => [Attr e ev msg cfg] -> [ev] -> UIContext e msg -> IO (UIContext e msg)
runFire attrs evs ctx = snd <$> runUI (fire attrs evs) ctx

label :: DummyEvent -> String
label Ping = "Ping"
label Pong = "Pong"

spec :: Spec
spec = describe "Blink.Controls2" $ do
  describe "configure" $ do
    it "returns the base config unchanged when there are no attrs" $
      configure defaultDummyConfig ([] :: [Attr TestElement DummyEvent () DummyConfig])
        `shouldBe` defaultDummyConfig

    it "applies a single Config attr" $
      dummyFlag (configure defaultDummyConfig [setFlag True]) `shouldBe` True

    it "folds multiple Config attrs left to right" $
      dummyCount (configure defaultDummyConfig [addCount 3, addCount 4]) `shouldBe` 7

    it "a later Config attr overrides an earlier one for the same field" $
      dummyFlag (configure defaultDummyConfig [setFlag True, setFlag False]) `shouldBe` False

    it "ignores On attrs entirely" $
      dummyFlag (configure defaultDummyConfig [onPing (1 :: Int), setFlag True]) `shouldBe` True

  describe "fire" $ do
    it "emits a message when a handler matches the event" $ do
      ctx <- runFire [onPing (1 :: Int)] [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` [1]

    it "emits nothing when no handler matches the event" $ do
      ctx <- runFire [onPing (1 :: Int)] [Pong] (mkCtxFor noInput)
      getMessages ctx `shouldBe` []

    it "emits nothing when the attribute list is empty" $ do
      ctx <- runFire ([] :: [Attr TestElement DummyEvent Int DummyConfig]) [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` []

    it "runs in event order, then handler-list order within an event" $ do
      let attrs =
            [ onAny (\ev -> [OutMsg (label ev <> "-a")])
            , onAny (\ev -> [OutMsg (label ev <> "-b")])
            ]
      ctx <- runFire attrs [Ping, Pong] (mkCtxFor noInput)
      getMessages ctx `shouldBe` ["Ping-a", "Ping-b", "Pong-a", "Pong-b"]

    it "dispatches OutUi effects via emitUi rather than as messages" $ do
      let attrs = [onAny (const [OutUi (ScrollTo TestControl 0.5)])]
      ctx <- runFire attrs [Ping] (mkCtxFor noInput :: UIContext TestElement Int)
      getUiEffects ctx `shouldBe` [ScrollTo TestControl 0.5]
      getMessages ctx `shouldBe` []

    it "a single handler can fan out to both a message and a UiEffect" $ do
      let attrs = [onAny (const [OutMsg (1 :: Int), OutUi (ScrollTo TestControl 0.5)])]
      ctx <- runFire attrs [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` [1]
      getUiEffects ctx `shouldBe` [ScrollTo TestControl 0.5]

  describe "onAny" $
    it "builds a handler with full access to Out, usable like any other On attr" $ do
      ctx <- runFire [onPing (1 :: Int)] [Ping] (mkCtxFor noInput)
      getMessages ctx `shouldBe` [1]
