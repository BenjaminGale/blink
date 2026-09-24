{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The shared focus\/hit-region contract every 'Blink.Controls.Control.control'-based
-- control must satisfy, on top of the raw-event contract every element
-- already satisfies (see 'Blink.Controls.ElementBehaviour.elementBehaviourSpec').
-- 'Blink.Controls.ControlSpec' runs this against 'Blink.Controls.Control.control'
-- directly; any widget built on top reuses it to confirm the same
-- focus\/hit-region behaviour still holds through its own config type -- a
-- widget whose focus behaviour genuinely differs (e.g.
-- 'Blink.Controls.Label.label' never auto-claiming or taking focus on a
-- plain click) passes a 'ControlBehaviourConfig' reflecting that, rather
-- than skipping this contract altogether.
--
-- 'controlBehaviourSpec' runs every part. Each part is also exported on its
-- own, one per attribute capability, for a widget that exposes only some
-- of them.
module Blink.Controls.ControlBehaviour
  ( ControlBehaviourConfig (..)
  , defaultControlBehaviourConfig
  , controlBehaviourSpec
  , controlEventSpec
  , styleAttributeSpec
  , focusBehaviourSpec
  , focusPolicyAttributeSpec
  , enabledAttributeSpec
  ) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import Blink.Controls.Control (Attribute, FocusPolicy (..), HasControlConfig, isEnabled, focusPolicy, style)
import Blink.Controls.ElementBehaviour (elementBehaviourSpec, tagged)
import Blink.Controls.Fixtures (plainStyle, plainStyleSet)
import Blink.Generators (genPointIn)
import Blink.Geometry (Point, Rectangle)
import Blink.Interaction (Interaction (..), InteractionResult (..), runInteractions)
import Blink.Rendering (Colour (..), DrawCommand (..))
import Blink.Style (StyleKey (..), Theme (..))
import Blink.View

-- | The focus behaviour that varies between an ordinary control and a
-- special case like 'Blink.Controls.Label.label' or 'Blink.Controls.ProgressBar.progressBar'.
-- Defaults (via 'defaultControlBehaviourConfig') to what every plain
-- control does.
data ControlBehaviourConfig = ControlBehaviourConfig
  { cbcAutoClaims   :: Bool
    -- ^ Whether rendering first while nothing else is focused claims focus
    -- for it. 'False' for a control that's permanently not focusable.
  , cbcClickFocuses :: Bool
    -- ^ Whether clicking it grants it focus at all. 'False' for a control
    -- whose default click behaviour doesn't focus itself (e.g. a label
    -- with no 'Blink.Controls.Label.target').
  }

defaultControlBehaviourConfig :: ControlBehaviourConfig
defaultControlBehaviourConfig = ControlBehaviourConfig { cbcAutoClaims = True, cbcClickFocuses = True }

-- | The focus\/hit-region contract: given how to render the control under
-- test with a given attrs list, asserts it claims and gives up focus the
-- way @cfg@ says it should, and that its hit region respects its margin.
-- Every check that involves a point inside the control's hit area picks
-- one at random from @insideRect@ on each run -- see
-- 'Blink.Controls.ElementBehaviour.elementBehaviourSpec'.
controlBehaviourSpec
  :: (Ord e, Show e, HasControlConfig e String cfg)
  => ControlBehaviourConfig                      -- ^ how this control's focus behaviour deviates, if at all
  -> Rectangle                                   -- ^ bounds the control renders at
  -> ViewContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Point                                         -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attribute cfg] -> View e String ())                -- ^ render the control under test with these attrs
  -> Spec
controlBehaviourSpec cfg bounds ctx eid marginPoint insideRect outsidePoint render = do
  controlEventSpec bounds ctx eid marginPoint insideRect outsidePoint render
  styleAttributeSpec bounds ctx insideRect render
  focusBehaviourSpec cfg bounds ctx insideRect outsidePoint render
  focusPolicyAttributeSpec bounds ctx insideRect render
  enabledAttributeSpec bounds ctx insideRect render

-- | The raw events the event-handler attributes react to, and the margin
-- those events respect.
controlEventSpec
  :: (Ord e, HasControlConfig e String cfg)
  => Rectangle                                   -- ^ bounds the control renders at
  -> ViewContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> e                                             -- ^ element id under test
  -> Point                                         -- ^ a point inside its margin (not part of its hit area)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attribute cfg] -> View e String ())                -- ^ render the control under test with these attrs
  -> Spec
controlEventSpec bounds ctx eid marginPoint insideRect outsidePoint render = do
  -- A control auto-claims focus the moment nothing else holds it, which
  -- would otherwise leak an incidental focus-gained event into every one
  -- of these raw-fact checks.
  elementBehaviourSpec bounds ctx eid insideRect outsidePoint (\attrs -> render (focusPolicy NotFocusable : attrs))

  describe "hit region" $ do
    it "does not raise a click event for a press and release inside its margin" $ do
      result <- runInteractions bounds ctx (render tagged) [] [ClickAt marginPoint]
      resultMessages result `shouldNotContain` ["Clicked"]

    it "raises a click event for a press and release inside its margin-inset area" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [] [ClickAt p])
      assert ("Clicked" `elem` resultMessages result)

-- | Whether the control claims, keeps, and gives up focus the way @cfg@
-- says it should, with no focus attribute passed.
focusBehaviourSpec
  :: (Ord e, HasControlConfig e String cfg)
  => ControlBehaviourConfig                      -- ^ how this control's focus behaviour deviates, if at all
  -> Rectangle                                   -- ^ bounds the control renders at
  -> ViewContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> Point                                         -- ^ a point outside its bounds entirely
  -> ([Attribute cfg] -> View e String ())                -- ^ render the control under test with these attrs
  -> Spec
focusBehaviourSpec cfg bounds ctx insideRect outsidePoint render = do
  describe "focus claiming" $ do
    it "claims focus by rendering first when nothing else is focused, exactly when it auto-claims" $ do
      result <- runInteractions bounds ctx (render tagged) [] []
      resultMessages result `shouldBe` ["FocusGained" | cbcAutoClaims cfg]

    it "raises nothing while disabled, even with nothing else focused" $ do
      result <- runInteractions bounds ctx (disableWhen True (render tagged)) [] []
      resultMessages result `shouldBe` []

    it "raises no focus lost event across further interactions that don't move focus away" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render tagged) [Wait 1] [MoveTo outsidePoint, MoveTo p])
      assert (notElem "FocusLost" (resultMessages result))

  describe "click and keyboard focus" $ do
    it "raises no focus gained event from a click while disabled" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (disableWhen True (render tagged)) [] [ClickAt p, Wait 1])
      assert (notElem "FocusGained" (resultMessages result))

    -- Only meaningful for a control that can hold focus at all -- skipped
    -- entirely for one that neither auto-claims nor focuses on click (e.g.
    -- 'Blink.Controls.Label.label' with no target).
    when (cbcAutoClaims cfg || cbcClickFocuses cfg) $
      it "raises a focus lost event when Tab is pressed while focused" $ monadicIO $ do
        p <- pick (genPointIn insideRect)
        let primeFocus = if cbcAutoClaims cfg then [Wait 1] else [ClickAt p, Wait 1]
        result <- run (runInteractions bounds ctx (render tagged) primeFocus [Tab])
        assert (resultMessages result == ["FocusLost"])

-- | 'focusPolicy' 'NotFocusable' keeps the control from taking focus, both
-- by rendering first and by being clicked.
focusPolicyAttributeSpec
  :: (Ord e, HasControlConfig e String cfg)
  => Rectangle                                   -- ^ bounds the control renders at
  -> ViewContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> ([Attribute cfg] -> View e String ())                -- ^ render the control under test with these attrs
  -> Spec
focusPolicyAttributeSpec bounds ctx insideRect render =
  describe "focusPolicy attribute" $ do
    it "raises nothing when it isn't focusable, even with nothing else focused" $ do
      result <- runInteractions bounds ctx (render (focusPolicy NotFocusable : tagged)) [] []
      resultMessages result `shouldBe` []

    it "never claims focus when clicked while focusPolicy is NotFocusable" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render (focusPolicy NotFocusable : tagged)) [] [ClickAt p, Wait 1])
      assert (notElem "FocusGained" (resultMessages result))

-- | 'isEnabled' 'False' stops the control reacting, the same as an
-- enclosing 'disableWhen'.
enabledAttributeSpec
  :: (Ord e, HasControlConfig e String cfg)
  => Rectangle                                   -- ^ bounds the control renders at
  -> ViewContext e String                          -- ^ starting context (theme\/measurer already set up)
  -> Rectangle                                     -- ^ the region making up its margin-inset hit area
  -> ([Attribute cfg] -> View e String ())                -- ^ render the control under test with these attrs
  -> Spec
enabledAttributeSpec bounds ctx insideRect render =
  describe "enabled attribute" $ do
    it "raises nothing when disabled via the attribute, even with nothing else focused" $ do
      result <- runInteractions bounds ctx (render (isEnabled False : tagged)) [] []
      resultMessages result `shouldBe` []

    it "raises no focus gained event from a click when disabled via the attribute" $ monadicIO $ do
      p <- pick (genPointIn insideRect)
      result <- run (runInteractions bounds ctx (render (isEnabled False : tagged)) [] [ClickAt p, Wait 1])
      assert (notElem "FocusGained" (resultMessages result))

-- | Checks that the control draws its background from a style key passed
-- via 'style', so the key the caller chooses wins over the control's own
-- default. Run directly for a part a widget builds from caller-supplied
-- attributes, such as a menu bar's label.
styleAttributeSpec
  :: (Ord e, HasControlConfig e String cfg)
  => Rectangle                              -- ^ bounds the control renders at
  -> ViewContext e String                   -- ^ starting context (theme\/measurer already set up)
  -> Rectangle                              -- ^ the region making up its margin-inset hit area
  -> ([Attribute cfg] -> View e String ())  -- ^ render the control under test with these attrs
  -> Spec
styleAttributeSpec bounds ctx insideRect render =
  describe "style attribute" $
    it "fills its hit area with the background of the style key it is given" $ do
      result <- runInteractions bounds (withTheme themeWithKey ctx) (render [style key]) [] []
      resultDraws result `shouldContain` [FillRect insideRect keyColour]
  where
    key         = Class "styleAttributeSpec"
    keyColour   = RGBA 1 0 0 1
    thm         = contextTheme ctx
    -- The default metrics keep the fill on @insideRect@; only the colour
    -- distinguishes this key from the control's own.
    themeWithKey = thm
      { themeElementStyles = Map.insert key (fst (themeDefaultStyle thm), plainStyleSet (plainStyle keyColour))
          (themeElementStyles thm)
      }
