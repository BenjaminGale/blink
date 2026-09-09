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
-- control --> buttonBase --> repeatButton
-- @
module Blink.Controls.RepeatButton
  ( RepeatButtonConfig (..)
  , defaultRepeatButtonConfig
  , repeatButton
  , initialDelay
  , repeatInterval
  ) where

import Control.Monad (replicateM_, void, when)

import Blink.Controls.Button
  (ButtonActivation (..), ButtonConfig (..), ButtonInteraction (..), HasButtonConfig (..), buttonBase, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label
  (HasLabelledConfig (..), captionElement, lcText, renderLabelledContent)
import Blink.View
import Blink.Element (Element (..), HasLayoutConfig (..))

-- | Every capability 'repeatButton' resolves: the wrapped 'ButtonConfig'
-- (styling, caption, layout, and 'Blink.Controls.Button.onActivated'
-- reactions -- all reused unchanged), and the repeat cadence.
data RepeatButtonConfig e msg = RepeatButtonConfig
  { rbButton       :: ButtonConfig e msg
  , rbInitialDelay :: Double
    -- ^ Seconds held before the first repeat. Defaults to 0.4.
  , rbInterval     :: Double
    -- ^ Seconds between repeats thereafter. Defaults to 0.08.
  }

-- | 'defaultButtonConfig', a 0.4s initial delay, and a 0.08s repeat
-- interval.
defaultRepeatButtonConfig :: RepeatButtonConfig e msg
defaultRepeatButtonConfig = RepeatButtonConfig
  { rbButton       = defaultButtonConfig
  , rbInitialDelay = 0.4
  , rbInterval     = 0.08
  }

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
    ctrl = (bcControl btn) { ccContent = const (renderLabelledContent (bcLabelled btn)) }

    run = do
      -- 'buttonBase' itself fires 'onActivated' once already, off
      -- 'ActivateOnPress' (the press) or Enter-while-focused -- this only
      -- adds the repeats past that first activation.
      r <- buttonBase eid btn { bcControl = ctrl }
      toFire <- resolveHoldRepeats eid (ciHeld (biControl r)) (rbInitialDelay cfg) (rbInterval cfg)
      when (toFire > 0) $ replicateM_ toFire (runHandlers (bcOnActivated btn) ())
