{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A button that opens a dropdown list of items when activated, drawn
-- above other controls via "Blink.Popup". Whether it's open is state the
-- caller owns (see 'isOpen'), like every other stateful control in
-- "Blink.Controls", such as 'Blink.Controls.ToggleButton.isSelected'.
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
-- ('MenuButtonList'), or one of its items. Items are tagged by their data,
-- so reordering keeps per-item state, as with
-- 'Blink.Controls.ToggleGroup.ToggleGroupPart'.
data MenuButtonPart a
  = MenuButtonTrigger
  | MenuButtonList
  | MenuButtonItem a
  deriving (Eq, Ord, Show)

-- | Every capability 'menuButton' resolves: the trigger's config (a
-- 'ToggleConfig', see 'isOpen'), the data to build each item from, and how
-- to configure each item's own button.
data MenuButtonConfig e a msg = MenuButtonConfig
  { mbToggle    :: ToggleConfig e msg
  , mbItems     :: [a]
  , mbItemAttrs :: a -> [Attribute (ButtonConfig e msg)]
  }

-- | 'defaultToggleButtonConfig', no items, and no per-item attrs. A closed
-- 'menuButton' looks like an ordinary button, and an open one uses the
-- theme's toggle-checked look.
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
-- 'items' attribute replaces an earlier one rather than adding to it.
-- Import "Blink.Controls.MenuButton" qualified if also using
-- 'Blink.Controls.ToggleGroup.items', which has the same name.
items :: [a] -> Attribute (MenuButtonConfig e a msg)
items xs = Attribute (\c -> c { mbItems = xs })

-- | Attributes for each item's button (e.g. 'Blink.Controls.Label.text',
-- 'Blink.Controls.Button.onActivated'), given the item. The same shape as
-- 'Blink.Controls.ToggleGroup.toggleAttributes'. An item's
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

-- | Reacts when the list should open or close, with the new value: from
-- activating the trigger, or 'False' whenever the open list closes for any
-- other reason. Store it and pass it back via 'isOpen'.
onOpenChanged :: (Bool -> [Effect e msg]) -> Attribute (MenuButtonConfig e a msg)
onOpenChanged f = Attribute (\c -> c
  { mbToggle = (mbToggle c) { tgcOnSelectedChanged = tgcOnSelectedChanged (mbToggle c) ++ [f] } })

-- | A button labelled via 'Blink.Controls.Label.text' that opens a dropdown
-- list of items, built from 'items', when activated by a click or Enter
-- while focused, as with 'Blink.Controls.Button.button'.
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

-- | The trigger and, while open, its item list. While open, the trigger
-- doesn't take focus on a press: the press may be closing the menu, and
-- the close refocuses it anyway.
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

-- | The open item list. @onTrigger@ is whether the pointer is on the
-- trigger, so a press there toggles the menu rather than counting as an
-- outside press.
itemsElement :: (Ord e, Ord a) => (MenuButtonPart a -> e) -> MenuButtonConfig e a msg -> View e msg () -> Bool -> Element e msg
itemsElement tag cfg close onTrigger =
  menuList menuButtonListStyleKey (tag MenuButtonList) (tag . MenuButtonItem) (mbItems cfg) (mbItemAttrs cfg) close onTrigger
