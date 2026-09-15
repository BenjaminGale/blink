{-# LANGUAGE OverloadedStrings #-}
-- | The dropdown item-list engine shared by "Blink.Controls.MenuButton" and
-- "Blink.Controls.MenuBar": a top-to-bottom list of buttons, one per item,
-- drawn on a panel background\/border, running in its own focus scope, with
-- Up\/Down moving the keyboard highlight between items (wrapping at either
-- end), Enter or a click on an item activating it and closing the menu,
-- Escape closing it without activating anything, a completed click outside
-- both the menu's own trigger area and this list closing it too, and so
-- does Tab or Shift-Tab.
module Blink.Controls.Menu
  ( menuList
  ) where

import Control.Monad (forM_, void, when)
import Data.List (find)

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), buttonBase, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label (captionElement, lcText, renderLabelledContent)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Input (InputState (inputKeyEvents), Key (KeyDown, KeyEscape, KeyTab, KeyUp), KeyEvent (key))
import Blink.Layout.Box (children, vBox)
import Blink.Layout.Constraints (Layout (..), fitContent)
import Blink.View
import Blink.Element (Element (..), height, width)

-- | A top-to-bottom list of buttons, one per item of @items@, sized to fit
-- its own content on both axes, drawn on a panel background\/border (per
-- @styleKey@) -- a plain, non-focusable 'Blink.Controls.Control.control'
-- wrapping it, purely for that chrome. @listId@ is this list's own element
-- id, used both for its hit-rect registration (so a click on the panel
-- background, not an item, never reaches through to whatever's behind the
-- popup) and its own focus scope. @itemId@ builds one item's own element id
-- from its data, so reordering items never disturbs another item's own
-- hover\/focus\/capture state.
--
-- @onOutsideTrigger@ is whether the click landed somewhere that, despite
-- not being this list itself, should still count as "not outside" -- e.g.
-- the trigger that opened it, or (for 'Blink.Controls.MenuBar.menuBar')
-- anywhere on the bar's own row, so switching to a different top-level menu
-- is never also treated as an outside click on this one.
menuList
  :: (Ord e, Ord b)
  => StyleKey e -> e -> (b -> e) -> [b] -> (b -> [Attribute (ButtonConfig e msg)]) -> View e msg () -> Bool
  -> Element e msg
menuList styleKey listId itemId items itemAttrsFor close onOutsideTrigger = Element
  { elLayout  = Layout fitContent fitContent TopLeft
  , elMeasure = measureChrome styleKey box
  , elRun     = void (control panelCfg)
  }
  where
    box = vBox [ width fitContent, height fitContent, children (map toItemElement items) ]

    -- ccElementId matters beyond styling: without one this never registers
    -- a hit-rect, so a click on the panel background (not an item) would
    -- reach straight through to whatever's behind the popup.
    panelCfg = defaultControlConfig
      { ccElementId   = Just listId
      , ccStyleKey    = styleKey
      , ccFocusPolicy = NotFocusable
      , ccContent     = const scopedRun
      }

    scopedRun = withFocusScope listId $ do
      handleEscape
      handleTabOut
      handleOutsideClick
      handleArrowKeys
      elRun box

    -- Closes on Escape whenever this list is open, regardless of which
    -- item (if any) currently holds focus within it -- matching a native
    -- menu, which closes on Escape without needing a specific item
    -- highlighted.
    handleEscape = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyEscape) . key) evs of
        Just e  -> consumeKey (key e) >> close
        Nothing -> pure ()

    -- Closes on Tab\/Shift-Tab too: this list renders after its trigger's
    -- own siblings, so a plain handoff can't reach them.
    handleTabOut = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyTab) . key) evs of
        Just e  -> consumeKey (key e) >> close
        Nothing -> pure ()

    -- Closes on a completed click (mirroring 'ActivateOnClick's own
    -- release-based timing, the only discrete click edge the public API
    -- exposes) that lands neither on @onOutsideTrigger@'s own region nor
    -- anywhere in this list -- an item click is itself within this list's
    -- own bounds, so it never reads as "outside" here; it closes via its
    -- own activation instead (see 'toItemElement').
    handleOutsideClick = do
      released <- isButtonReleased
      onList   <- isRegionHit
      when (released && not onOutsideTrigger && not onList) close

    -- Moves the highlight to the next/previous item on Down\/Up, wrapping
    -- from the last item to the first (and back) in a single keypress --
    -- unlike Tab\/Shift-Tab's own traversal, a menu's items are expected to
    -- cycle, matching every native menu\/dropdown. Redirects focus
    -- explicitly by position rather than reusing Tab\/Shift-Tab's own
    -- give-up-and-let-the-neighbour-auto-claim mechanism, which has no
    -- wraparound of its own.
    handleArrowKeys = case items of
      [] -> pure ()
      is -> do
        evs <- inputKeyEvents <$> getInput
        forM_ (find ((`elem` [KeyDown, KeyUp]) . key) evs) $ \e -> do
          consumeKey (key e)
          current <- getFocus
          let count        = length is
              currentIndex = current >>= (`lookup` zip (map itemId is) [0 ..])
              nextIndex = case (key e, currentIndex) of
                (KeyDown, Nothing) -> 0
                (KeyDown, Just i)  -> (i + 1) `mod` count
                (_,       Nothing) -> count - 1
                (_,       Just i)  -> (i - 1) `mod` count
          forM_ (itemAt is nextIndex) $ \item -> requestFocus (Just listId) (itemId item)

    itemAt xs idx = case drop idx xs of
      (x : _) -> Just x
      []      -> Nothing

    toItemElement item = Element
      { elLayout  = bcLayout itemCfg
      , elMeasure = measureChrome (ccStyleKey (bcControl itemCfg)) (captionElement (lcText (bcLabelled itemCfg)))
      , elRun     = do
          r <- buttonBase (itemId item) itemCfg { bcControl = itemCtrl }
          when (biActivated r) close
      }
      where
        itemCfg  = resolve defaultButtonConfig (width fitContent : height fitContent : itemAttrsFor item)
        itemCtrl = (bcControl itemCfg) { ccContent = const (renderLabelledContent (bcLabelled itemCfg)) }
