{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A button that keeps firing 'Blink.Controls.Button.onActivated' at a
-- steady interval for as long as it's held down with the mouse -- the core
-- building block behind e.g. a scrollbar's arrow buttons, or a stepper's
-- increment\/decrement. Built on 'Blink.Controls.Button.buttonBase' the
-- same way 'Blink.Controls.Button.button' is, configured with
-- 'Blink.Controls.Button.ActivateOnPress' -- see 'repeatButton'.
--
-- @
-- controlBase --> buttonBase --> repeatButton
-- @
--
-- 'repeatButton' keeps no timer of its own that survives between frames --
-- like 'Blink.Controls.Slider.slider' owning its value, the caller owns
-- two small numbers for the life of a press: the animation clock's
-- elapsed time when it began ('pressStartedAt'), and how many repeats have
-- fired so far ('firedCount'). Both are reported once each time they
-- change ('onPressStarted', 'onFiredCountChanged') and simply handed back
-- unchanged otherwise. From those two plus the animation clock's current
-- elapsed time, every frame recomputes /how many repeats are due by now/
-- from scratch -- a pure function of absolute elapsed time, never of how
-- many frames actually ran or how long any single frame took. That's
-- deliberate: a frame's own @dt@ is clamped (see 'Blink.UI.AnimationState')
-- to keep other animations numerically stable, which would silently drop
-- repeats after a long hitch if the cadence were derived from it instead.
module Blink.Controls.RepeatButton
  ( RepeatButtonConfig (..)
  , defaultRepeatButtonConfig
  , repeatButton
  , initialDelay
  , repeatInterval
  , pressStartedAt
  , firedCount
  , onPressStarted
  , onFiredCountChanged
  , onPressEnded
  ) where

import Control.Monad (replicateM_, void, when)

import Blink.Controls.Button
  (ButtonActivation (..), ButtonConfig (..), ButtonInteraction (..), HasButtonConfig (..), buttonBase, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label
  (HasLabelledConfig (..), captionElement, lcText, renderLabelledContent)
import Blink.Layout.Constraints (HasLayoutConfig (..))
import Blink.UI
import Blink.UI.Element (Element (..))

-- | Every capability 'repeatButton' resolves: the wrapped 'ButtonConfig'
-- (styling, caption, layout, and 'Blink.Controls.Button.onActivated'
-- reactions -- all reused unchanged), the repeat cadence, and the
-- two-number press handoff (see the module header).
data RepeatButtonConfig e msg = RepeatButtonConfig
  { rbButton             :: ButtonConfig e msg
  , rbInitialDelay       :: Double
    -- ^ Seconds held before the first repeat. Defaults to 0.4.
  , rbInterval           :: Double
    -- ^ Seconds between repeats thereafter. Defaults to 0.08.
  , rbPressStartedAt     :: Maybe Double
    -- ^ The animation clock's elapsed time when the current press began, as
    -- last reported via 'onPressStarted' -- 'Nothing' when not currently
    -- pressed. Owned entirely by the caller; see 'pressStartedAt'.
  , rbFiredCount         :: Int
    -- ^ How many repeats have fired so far during the current press, as
    -- last reported via 'onFiredCountChanged'. Owned entirely by the
    -- caller; see 'firedCount'.
  , rbOnPressStarted     :: Double -> [Out e msg]
  , rbOnFiredCountChanged :: Int -> [Out e msg]
  , rbOnPressEnded       :: [Out e msg]
  }

-- | 'defaultButtonConfig', a 0.4s initial delay, a 0.08s repeat interval,
-- no press anchored yet, a zero fired count, and no press\/repeat
-- reactions.
defaultRepeatButtonConfig :: RepeatButtonConfig e msg
defaultRepeatButtonConfig = RepeatButtonConfig
  { rbButton              = defaultButtonConfig
  , rbInitialDelay        = 0.4
  , rbInterval            = 0.08
  , rbPressStartedAt      = Nothing
  , rbFiredCount          = 0
  , rbOnPressStarted      = const []
  , rbOnFiredCountChanged = const []
  , rbOnPressEnded        = []
  }

instance HasElementConfig e msg (RepeatButtonConfig e msg) where
  overElement attr = Attribute (\rc -> rc { rbButton = runAttribute (overElement attr) (rbButton rc) })

instance HasControlConfig e msg (RepeatButtonConfig e msg) where
  overControl attr = Attribute (\rc -> rc { rbButton = runAttribute (overControl attr) (rbButton rc) })

instance HasButtonConfig e msg (RepeatButtonConfig e msg) where
  overButton attr = Attribute (\rc -> rc { rbButton = runAttribute attr (rbButton rc) })

instance HasLabelledConfig e msg (RepeatButtonConfig e msg) where
  overLabelled attr = Attribute (\rc -> rc { rbButton = runAttribute (overLabelled attr) (rbButton rc) })

instance HasLayoutConfig (RepeatButtonConfig e msg) where
  overLayout attr = Attribute (\rc -> rc { rbButton = runAttribute (overLayout attr) (rbButton rc) })

-- | How long the button must be held before it starts repeating. Defaults
-- to 0.4 seconds.
initialDelay :: Double -> Attribute (RepeatButtonConfig e msg)
initialDelay v = Attribute (\rc -> rc { rbInitialDelay = v })

-- | How often it repeats once past 'initialDelay'. Defaults to 0.08
-- seconds.
repeatInterval :: Double -> Attribute (RepeatButtonConfig e msg)
repeatInterval v = Attribute (\rc -> rc { rbInterval = v })

-- | Hands the current press's anchor timestamp back to 'repeatButton',
-- exactly as last reported via 'onPressStarted' -- pass 'Nothing' (the
-- default) once 'onPressEnded' fires. Without this, held-duration always
-- reads as zero and the button never repeats past its first press. Reset
-- 'firedCount' to 0 in the same reaction that stores a fresh value here --
-- a new press starting its own count over from zero.
pressStartedAt :: Maybe Double -> Attribute (RepeatButtonConfig e msg)
pressStartedAt v = Attribute (\rc -> rc { rbPressStartedAt = v })

-- | Hands the repeat count back, exactly as last reported via
-- 'onFiredCountChanged' -- 0 (the default) at the start of a fresh press.
-- Without this, 'repeatButton' can't tell repeats it has already fired from
-- ones still due, and re-fires everything due every single frame.
firedCount :: Int -> Attribute (RepeatButtonConfig e msg)
firedCount v = Attribute (\rc -> rc { rbFiredCount = v })

-- | Fires once, the frame the button is pressed, carrying the animation
-- clock's elapsed time at that instant. Store it and feed it back via
-- 'pressStartedAt' next frame, resetting whatever 'firedCount' tracks to 0
-- alongside it.
onPressStarted :: (Double -> [Out e msg]) -> Attribute (RepeatButtonConfig e msg)
onPressStarted f = Attribute (\rc -> rc { rbOnPressStarted = f })

-- | Fires each frame one or more repeats do, carrying the new total fired
-- so far this press. Store it and feed it back via 'firedCount' next
-- frame.
onFiredCountChanged :: (Int -> [Out e msg]) -> Attribute (RepeatButtonConfig e msg)
onFiredCountChanged f = Attribute (\rc -> rc { rbOnFiredCountChanged = f })

-- | Fires once, the frame the press ends (release, or the pointer leaving
-- while held -- anything that drops 'Blink.Controls.Element.eiHeld').
-- React by clearing whatever 'pressStartedAt' currently holds.
onPressEnded :: [Out e msg] -> Attribute (RepeatButtonConfig e msg)
onPressEnded hs = Attribute (\rc -> rc { rbOnPressEnded = hs })

-- | How many repeats should have fired by the time @heldFor@ seconds have
-- elapsed since the press began: none before 'rbInitialDelay', then one at
-- that instant and one more every 'rbInterval' after. A pure function of
-- @heldFor@ alone -- see the module header for why 'repeatButton' diffs
-- this against 'rbFiredCount' rather than against last frame's @heldFor@.
repeatsDueBy :: RepeatButtonConfig e msg -> Double -> Int
repeatsDueBy cfg heldFor
  | heldFor < rbInitialDelay cfg = 0
  | otherwise = floor ((heldFor - rbInitialDelay cfg) / rbInterval cfg) + 1

-- | A button that fires 'Blink.Controls.Button.onActivated' once
-- immediately on press (not on release, unlike
-- 'Blink.Controls.Button.button' -- holding is the whole point, so waiting
-- for a release first would miss the initial beat entirely), then again
-- after 'initialDelay', then every 'repeatInterval' for as long as it's
-- held. Releasing before 'initialDelay' elapses behaves just like a plain
-- button's single click.
--
-- Holding Enter while focused repeats it too (via
-- 'Blink.Controls.Button.ActivateOnPress', which this forces -- see
-- 'Blink.Controls.Button.ButtonActivation'), but at whatever cadence the
-- platform's own keyboard auto-repeat uses, not 'initialDelay'\/'repeatInterval':
-- unlike the mouse, there's no continuous "is this key still down" state
-- to compute our own cadence from, only a stream of discrete key events at
-- whatever rate the platform delivers them.
--
-- The mouse-driven repeat cadence is computed fresh every frame from
-- 'pressStartedAt', 'firedCount', and the animation clock's current
-- elapsed time -- see the module header for why, and
-- 'Blink.UI.requiresAnimation' for how it keeps getting frames to compute
-- it in while the mouse itself sits still.
repeatButton :: Ord e => e -> [Attribute (RepeatButtonConfig e msg)] -> Element e msg
repeatButton eid attrs = Element
  { elLayout  = bcLayout btn
  , elMeasure = measureChrome (ccStyleKey (bcControl btn)) (captionElement (lcText (bcLabelled btn)))
  , elRun     = void run
  }
  where
    cfg = resolve defaultRepeatButtonConfig attrs
    -- Always 'ActivateOnPress' -- fixed behaviour, not a default (same
    -- idiom as 'Blink.Controls.RadioButton.radioButton' forcing 'tgcNext').
    btn = (rbButton cfg) { bcActivation = ActivateOnPress }
    ctrl = (bcControl btn) { ccContent = renderLabelledContent (bcLabelled btn) }

    run = do
      -- 'buttonBase' itself fires 'onActivated' once already, off
      -- 'ActivateOnPress' (the press) or Enter-while-focused -- this only
      -- adds the repeats past that first activation.
      r <- buttonBase eid btn { bcControl = ctrl }
      let ei = ciElement (biControl r)

      when (eiMouseDown ei) $ do
        now <- realToFrac <$> getAnimElapsed
        runHandlers [rbOnPressStarted cfg] now

      -- Kept alive by 'eiHeld' alone, not by whether the anchor has
      -- arrived yet: the very frame a press starts, the caller hasn't had
      -- a chance to feed 'pressStartedAt' back in, but the ticker still
      -- needs to already be running so the *next* frame -- the first one
      -- with an anchor to work from -- is a real animation tick rather
      -- than an idle one.
      when (eiHeld ei) requiresAnimation

      case rbPressStartedAt cfg of
        Just started | eiHeld ei -> do
          now <- realToFrac <$> getAnimElapsed
          let heldFor = now - started
              due     = repeatsDueBy cfg heldFor
              toFire  = due - rbFiredCount cfg
          when (toFire > 0) $ do
            replicateM_ toFire (runHandlers (bcOnActivated btn) ())
            runHandlers [rbOnFiredCountChanged cfg] due
        Just _  -> runHandlers [const (rbOnPressEnded cfg)] ()
        Nothing -> pure ()
