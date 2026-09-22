{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A button that opens a dropdown list of items when activated: the first
-- control built on "Blink.Popup"'s deferred overlay layer. Its own open\/
-- closed state is external, caller-owned state (see 'isOpen'), the same as
-- every other stateful control in "Blink.Controls" -- e.g.
-- 'Blink.Controls.ToggleButton.isSelected' -- rather than something
-- 'menuButton' tracks internally.
--
-- @
-- control --> buttonBase --> toggleBase --> menuButton
-- @
--
-- While open, Up\/Down move the keyboard highlight between items, wrapping
-- from the last item back to the first (and back), Enter or a click on an
-- item activates it and closes the menu, Escape closes it without
-- activating anything, a mouse press outside both the trigger and the item
-- list closes it too, and so does Tab or Shift-Tab. Closing returns focus
-- to the trigger, unless the press that closed it landed on a control that
-- takes focus itself.
module Blink.Controls.MenuButton
  ( MenuButtonConfig
  , MenuButtonPart (..)
  , defaultMenuButtonConfig
  , menuButton
  , items
  , itemAttrs
  , isOpen
  , onOpenChanged
  ) where

import Control.Monad (void, when)

import Blink.Controls.Button (ButtonConfig (..))
import Blink.Controls.Control
import Blink.Controls.Label (HasLabelledConfig (..), LabelledConfig (..), captionElement, lcText, renderLabelledContent)
import Blink.Controls.Menu (menuList)
import Blink.Controls.MenuButton.Style (menuButtonListStyleKey)
import Blink.Controls.ToggleButton
  (ToggleConfig (..), ToggleInteraction (..), defaultToggleButtonConfig, toggleBase)
import Blink.Popup (content, popup)
import Blink.View
import Blink.Element (Element (..), HasLayoutConfig (..))

-- | Identifies one part of a 'menuButton': the trigger button itself
-- ('MenuButtonTrigger'), the item list's own focus scope
-- ('MenuButtonList'), or one of its items, tagged by
-- the item's own data value rather than its position in the list -- the
-- same rationale as 'Blink.Controls.ToggleGroup.ToggleGroupPart'.
data MenuButtonPart a
  = MenuButtonTrigger
  | MenuButtonList
  | MenuButtonItem a
  deriving (Eq, Ord, Show)

-- | Every capability 'menuButton' resolves: the trigger button's own
-- config (wrapped as a 'ToggleConfig' -- see 'isOpen'), the data to build
-- each item from, and how to configure each item's own button.
data MenuButtonConfig e a msg = MenuButtonConfig
  { mbToggle    :: ToggleConfig e msg
  , mbItems     :: [a]
  , mbItemAttrs :: a -> [Attribute (ButtonConfig e msg)]
  }

-- | 'defaultToggleButtonConfig' (so a closed 'menuButton' looks like an
-- ordinary button, and an open one reads with the theme's toggle-checked
-- look -- a reasonable stand-in for "open" until a menu button gets its own
-- chrome), no items, and no per-item attrs.
defaultMenuButtonConfig :: MenuButtonConfig e a msg
defaultMenuButtonConfig = MenuButtonConfig
  { mbToggle    = defaultToggleButtonConfig
  , mbItems     = []
  , mbItemAttrs = const []
  }

instance HasControlConfig e msg (MenuButtonConfig e a msg) where
  overControl attr = Attribute (\c -> c { mbToggle = runAttribute (overControl attr) (mbToggle c) })

instance HasLabelledConfig e msg (MenuButtonConfig e a msg) where
  overLabelled attr = Attribute (\c -> c { mbToggle = runAttribute (overLabelled attr) (mbToggle c) })

instance HasLayoutConfig (MenuButtonConfig e a msg) where
  overLayout attr = Attribute (\c -> c { mbToggle = runAttribute (overLayout attr) (mbToggle c) })

-- | The data to build one item from, in order. Defaults to @[]@; a later
-- 'items' attribute replaces an earlier one rather than adding to it. Named
-- the same as 'Blink.Controls.ToggleGroup.items' -- import
-- "Blink.Controls.MenuButton" qualified if using both in the same module.
items :: [a] -> Attribute (MenuButtonConfig e a msg)
items xs = Attribute (\c -> c { mbItems = xs })

-- | Attributes for the button built from one item (e.g.
-- 'Blink.Controls.Label.text', 'Blink.Controls.Button.onActivated'),
-- computed once per item rather than written out by hand for each -- the
-- same shape as 'Blink.Controls.ToggleGroup.toggleAttributes'. An item's
-- own 'Blink.Controls.Button.onActivated' fires (if set) in addition to,
-- not instead of, 'menuButton' closing the list on that same activation.
itemAttrs :: (a -> [Attribute (ButtonConfig e msg)]) -> Attribute (MenuButtonConfig e a msg)
itemAttrs f = Attribute (\c -> c { mbItemAttrs = f })

-- | Whether the item list is currently open. External state the caller
-- owns and re-supplies every frame, the same as
-- 'Blink.Controls.ToggleButton.isSelected' for a plain toggle button.
-- Defaults to 'False'.
isOpen :: Bool -> Attribute (MenuButtonConfig e a msg)
isOpen b = Attribute (\c -> c { mbToggle = (mbToggle c) { tgcSelected = b } })

-- | Reacts when activating the trigger (a click, or Enter while focused)
-- would open or close the list, with the value it changed to. Also fires
-- (with 'False') when an item is activated or Escape is pressed while the
-- list is open, closing it the same way. It's up to the reaction to
-- actually store the new value and pass it back in via 'isOpen' next frame
-- -- the same contract as 'Blink.Controls.ToggleButton.onSelectedChanged'.
onOpenChanged :: (Bool -> [Effect e msg]) -> Attribute (MenuButtonConfig e a msg)
onOpenChanged f = Attribute (\c -> c
  { mbToggle = (mbToggle c) { tgcOnSelectedChanged = tgcOnSelectedChanged (mbToggle c) ++ [f] } })

-- | A button labelled via 'Blink.Controls.Label.text' that opens a dropdown
-- list of items, built from 'items', when activated -- the same activation
-- rule as 'Blink.Controls.Button.button' (a click, or Enter while focused).
-- While open, the list renders through 'Blink.Popup.popup', anchored to the
-- trigger's own bounds, below and left-aligned with it by default, and
-- keyboard focus moves into it. Defaults to filling
-- the width it's given and sizing its height to its own chrome-wrapped
-- caption, the same as 'Blink.Controls.Button.button'; override with
-- 'Blink.Element.width'\/'Blink.Element.height'\/'Blink.Element.align'.
--
-- @tag@ builds every part's element id from a 'MenuButtonPart': the
-- trigger's own id from 'MenuButtonTrigger', its item list's own focus
-- scope id from 'MenuButtonList', and each item's id from 'MenuButtonItem'
-- applied to the item's own data.
menuButton :: (Ord e, Ord a) => (MenuButtonPart a -> e) -> [Attribute (MenuButtonConfig e a msg)] -> Element e msg
menuButton tag attrs = Element
  { elLayout  = bcLayout btn
  , elMeasure = measureChrome (ccStyleKey (bcControl btn)) (captionElement (lcText (bcLabelled btn)))
  , elRun     = void (runMenuButton tag cfg)
  }
  where
    cfg = resolve defaultMenuButtonConfig attrs
    btn = tgcButton (mbToggle cfg)

-- | Runs the trigger as 'toggleBase' (its "selected" state standing in for
-- open\/closed). While open, queues the item list through
-- 'Blink.Popup.popup', anchored to the trigger's own just-rendered bounds;
-- the very frame it opens, moves focus into the item list's own scope (see
-- 'itemsElement') within whatever scope the trigger itself belongs to, so a
-- 'menuButton' nested inside another composite's focus scope still hands
-- off correctly, exactly as a click redirecting focus elsewhere already
-- does for any control.
--
-- While already open, the trigger's own click-to-focus is suppressed (see
-- @ctrl@ below): a mouse-down there is ambiguous between reopening and
-- closing the menu until the release resolves it, so focus only returns
-- to the trigger once a close actually happens (@justClosed@ below), the
-- same gated way every other closing path refocuses it (see 'itemsElement's
-- @close@).
runMenuButton :: (Ord e, Ord a) => (MenuButtonPart a -> e) -> MenuButtonConfig e a msg -> View e msg (ToggleInteraction e msg)
runMenuButton tag cfg = do
  enclosingScope <- getCurrentScope
  r <- toggleBase triggerId (mbToggle cfg) { tgcButton = btn { bcControl = ctrl } }
  onTrigger <- isRegionHit
  let wasOpen    = tgcSelected (mbToggle cfg)
      justOpened = tgiSelected r && not wasOpen
      justClosed = wasOpen && not (tgiSelected r)
      refocusTrigger = do
        alreadyClaimed <- hasQueuedFocus enclosingScope
        when (not alreadyClaimed) $ requestFocus enclosingScope triggerId
      close      = do
        runHandlers (tgcOnSelectedChanged (mbToggle cfg)) False
        refocusTrigger
  when justOpened $ requestFocus enclosingScope (tag MenuButtonList)
  when justClosed refocusTrigger
  when (tgiSelected r) $ popup triggerId [content (itemsElement tag cfg close onTrigger)]
  pure r
  where
    triggerId = tag MenuButtonTrigger
    btn       = tgcButton (mbToggle cfg)
    wasOpen'  = tgcSelected (mbToggle cfg)
    suppressClickToFocus policy = case policy of
      Focusable opts | wasOpen' -> Focusable opts { focusIsClickToFocus = False }
      _                         -> policy
    ctrl = (bcControl btn)
      { ccContent     = const (renderLabelledContent (bcLabelled btn))
      , ccFocusPolicy = suppressClickToFocus (ccFocusPolicy (bcControl btn))
      }

-- | The dropdown itself -- see 'Blink.Controls.Menu.menuList' for the
-- shared engine: Up\/Down move the keyboard highlight between items
-- (wrapping at either end), an item's own activation, Escape, or a click
-- completing outside both the trigger and this list, all run @close@.
-- @onTrigger@ is whether the trigger's own bounds were hit this frame,
-- captured by 'runMenuButton' before this list (a separate 'Element', run
-- later, at a different ambient bounds) is even queued.
itemsElement :: (Ord e, Ord a) => (MenuButtonPart a -> e) -> MenuButtonConfig e a msg -> View e msg () -> Bool -> Element e msg
itemsElement tag cfg close onTrigger =
  menuList menuButtonListStyleKey (tag MenuButtonList) (tag . MenuButtonItem) (mbItems cfg) (mbItemAttrs cfg) close onTrigger
