{- |
Module: Blink.Testing

Runs a view, or a single custom control, outside an application and
inspects what it did: what it drew, which messages it sent, and the state
Blink keeps for it between frames (focus, scroll position, selection).

@
ctx0 <- pure (emptyViewContext bounds input theme)
(_, ctx1) <- runView (runElement (myControl MyId [])) ctx0
getMessages ctx1 \`shouldBe\` [Clicked]
@

To simulate a second frame, build its context from the first with
'nextFrameContext', which also applies the effects the first frame
queued. An application never needs this module; "Blink.App" runs frames
itself.
-}
module Blink.Testing
  ( -- * Setting up
    ViewContext
  , emptyViewContext
  , withTheme
  , withMeasurers
  , Measurers (..)
  , TextMeasurer (..)
  , ImageMeasurer (..)
  , noOpMeasurers
  , noOpTextMeasurer
  , noOpImageMeasurer
    -- * Running frames
  , runView
  , nextFrameContext
  , rerenderContext
  , settleEffects
  , settleAndClearEffects
  , AnimationState (animDelta, animElapsed, animIsTick)
  , mkAnimationState
    -- * Inspecting the result
  , getDrawCommands
  , getMessages
  , getCursorShape
  , getPendingPopups
  , PendingPopup (..)
  , markPopupFloor
  , contextRequiresAnimation
  , contextInput
  , contextTheme
  , contextAnimation
  , contextCaptured
  , contextFocus
  , contextFocusChain
  , contextScrollState
  , contextExtentState
  , contextCursorIndex
  , contextSelection
  ) where

import Blink.Rendering
  ( TextMeasurer (..), noOpTextMeasurer, ImageMeasurer (..), noOpImageMeasurer, Measurers (..), noOpMeasurers )
import Blink.View.Animation
import Blink.View.Context
import Blink.View.Cursor
import Blink.View.Extent
import Blink.View.Focus
import Blink.View.Mouse
import Blink.View.Scroll
import Blink.View.Selection
