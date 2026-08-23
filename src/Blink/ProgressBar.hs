{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A progress indicator: a filled bar for a known 'Progress' value, or a
-- continuously animating band while 'Indeterminate'.
module Blink.ProgressBar
  ( ProgressBarAttributes
  , ProgressBarConfig
  , ProgressValue (..)
  , progressBar
  , progressBarStyleKey
  , progress
  , bandSpeed
  , isEnabled
  , style
  , StyleKey (..)
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onClicked
  , onKeyPressed
  , onFocusGained
  , onFocusLost
  ) where

import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Blink.Control
import Blink.Geometry (Rectangle (..))
import Blink.Input (KeyEvent)
import Blink.Style (Style (..))
import Blink.UI

-- | The value passed to 'progressBar' via 'progress'.
data ProgressValue
  = Progress Double
    -- ^ A determinate value in @[0, 1]@, clamped and rendered as a filled bar.
  | Indeterminate
    -- ^ Unknown progress: a band animates continuously across the bar.
  deriving (Eq, Show)

-- | 'Blink.ProgressBar.progressBar'\'s own closed attrs type: most of the
-- common capabilities every control has, plus 'progress' and 'bandSpeed'.
-- Doesn't expose 'isFocusable' -- a progress bar is never a tab stop; this
-- is fixed behaviour, not a default, so 'progressBar' has to still
-- implement a @ProgressBarIsFocusable@ constructor for its
-- 'HasControlConfig' instance to type-check, but simply never exports a
-- smart constructor that could build one.
data ProgressBarAttributes e msg
  = ProgressBarIsFocusable Bool
  | ProgressBarIsEnabled Bool
  | ProgressBarStyle (StyleKey e)
  | ProgressBarTabNavigation NavigationMode
  | ProgressBarIsArrowNavigationEnabled Bool
  | ProgressBarOnClicked (() -> [Out e msg])
  | ProgressBarOnFocusGained (() -> [Out e msg])
  | ProgressBarOnFocusLost (() -> [Out e msg])
  | ProgressBarOnMouseEntered (() -> [Out e msg])
  | ProgressBarOnMouseExited (() -> [Out e msg])
  | ProgressBarOnMouseDown (() -> [Out e msg])
  | ProgressBarOnMouseUp (() -> [Out e msg])
  | ProgressBarOnKeyPressed (KeyEvent -> [Out e msg])
  | ProgressBarProgress ProgressValue
  | ProgressBarBandSpeed Double

instance HasControlConfig e (ProgressBarAttributes e msg) where
  mkIsFocusable = ProgressBarIsFocusable
  matchIsFocusable (ProgressBarIsFocusable b) = Just b
  matchIsFocusable _ = Nothing
  mkIsEnabled = ProgressBarIsEnabled
  matchIsEnabled (ProgressBarIsEnabled b) = Just b
  matchIsEnabled _ = Nothing
  mkStyle = ProgressBarStyle
  matchStyle (ProgressBarStyle k) = Just k
  matchStyle _ = Nothing
  mkTabNavigation = ProgressBarTabNavigation
  matchTabNavigation (ProgressBarTabNavigation m) = Just m
  matchTabNavigation _ = Nothing
  mkIsArrowNavigationEnabled = ProgressBarIsArrowNavigationEnabled
  matchIsArrowNavigationEnabled (ProgressBarIsArrowNavigationEnabled b) = Just b
  matchIsArrowNavigationEnabled _ = Nothing

instance HasElementEvents e msg (ProgressBarAttributes e msg) where
  mkOnClicked = ProgressBarOnClicked
  matchOnClicked (ProgressBarOnClicked f) = Just f
  matchOnClicked _ = Nothing
  mkOnFocusGained = ProgressBarOnFocusGained
  matchOnFocusGained (ProgressBarOnFocusGained f) = Just f
  matchOnFocusGained _ = Nothing
  mkOnFocusLost = ProgressBarOnFocusLost
  matchOnFocusLost (ProgressBarOnFocusLost f) = Just f
  matchOnFocusLost _ = Nothing
  mkOnMouseEntered = ProgressBarOnMouseEntered
  matchOnMouseEntered (ProgressBarOnMouseEntered f) = Just f
  matchOnMouseEntered _ = Nothing
  mkOnMouseExited = ProgressBarOnMouseExited
  matchOnMouseExited (ProgressBarOnMouseExited f) = Just f
  matchOnMouseExited _ = Nothing
  mkOnMouseDown = ProgressBarOnMouseDown
  matchOnMouseDown (ProgressBarOnMouseDown f) = Just f
  matchOnMouseDown _ = Nothing
  mkOnMouseUp = ProgressBarOnMouseUp
  matchOnMouseUp (ProgressBarOnMouseUp f) = Just f
  matchOnMouseUp _ = Nothing
  mkOnKeyPressed = ProgressBarOnKeyPressed
  matchOnKeyPressed (ProgressBarOnKeyPressed f) = Just f
  matchOnKeyPressed _ = Nothing

-- | Sets the bar to 'Progress' (determinate) or 'Indeterminate'. Defaults
-- to @'Progress' 0@.
progress :: ProgressValue -> ProgressBarAttributes e msg
progress = ProgressBarProgress

-- | How fast the band sweeps across an 'Indeterminate' bar, in bar-widths
-- per second. Defaults to 0.5.
bandSpeed :: Double -> ProgressBarAttributes e msg
bandSpeed = ProgressBarBandSpeed

-- | Configuration for 'progressBar', resolved from a
-- @['ProgressBarAttributes' e msg]@.
data ProgressBarConfig = ProgressBarConfig
  { pbcfgValue     :: ProgressValue
  , pbcfgBandSpeed :: Double
  }

-- | The 'StyleKey' 'progressBar' resolves its style from unless overridden
-- via 'style'.
progressBarStyleKey :: StyleKey e
progressBarStyleKey = Class "progressBar"

defaultProgressBarConfig :: ProgressBarConfig
defaultProgressBarConfig = ProgressBarConfig { pbcfgValue = Progress 0, pbcfgBandSpeed = 0.5 }

resolveProgressBarConfig :: [ProgressBarAttributes e msg] -> ProgressBarConfig
resolveProgressBarConfig = foldl' apply defaultProgressBarConfig
  where
    apply cfg (ProgressBarProgress v)  = cfg { pbcfgValue = v }
    apply cfg (ProgressBarBandSpeed v) = cfg { pbcfgBandSpeed = v }
    apply cfg _                        = cfg

toProgressBarControlAttr :: ProgressBarAttributes e msg -> Maybe (ControlAttrs e msg)
toProgressBarControlAttr (ProgressBarProgress _)  = Nothing
toProgressBarControlAttr (ProgressBarBandSpeed _) = Nothing
toProgressBarControlAttr a                        = translateCommon a

-- | A progress indicator, set via 'progress' to 'Progress' for a
-- determinate bar or 'Indeterminate' for a continuously animating band. A
-- full 'control' like any other -- it reports hover\/click\/focus events
-- the same way every other control does, even though a caller has no real
-- reason to react to them. Never a tab stop, though: fixed behaviour, not
-- a default -- 'progressBar' simply never exposes 'isFocusable'.
progressBar :: Ord e => e -> [ProgressBarAttributes e msg] -> UI e msg ()
progressBar eid attrs = control eid (mapMaybe toProgressBarControlAttr attrs ++ [isFocusable False, focusOnClick NoFocus, content body])
  where
    cfg = resolveProgressBarConfig attrs
    body = do
      s <- currentStyle
      r <- getBounds
      case pbcfgValue cfg of
        Progress value -> do
          let clamped   = max 0 (min 1 value)
              fillRect' = r { rectWidth = rectWidth r * clamped }
          withBounds fillRect' $ fillRect (styleTextColour s)
        Indeterminate -> do
          requiresAnimation
          elapsed <- getAnimElapsed
          let speed = pbcfgBandSpeed cfg
              t     = realToFrac elapsed * speed
              phase = t - fromIntegral (floor t :: Int)
              bandW = rectWidth r * 0.3
              left  = rectX r - bandW + (rectWidth r + bandW) * phase
          withBounds (r { rectX = left, rectWidth = bandW }) $ fillRect (styleTextColour s)
