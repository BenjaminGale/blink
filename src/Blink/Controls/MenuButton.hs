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
    -- * Style
  , menuButtonListStyleKey
  , defaultStyleEntries
  ) where

import Control.Monad (void)

import Blink.Controls.Button (ButtonConfig (..), captionedButton)
import Blink.Controls.Control
import Blink.Controls.Label (HasLabelledConfig (..))
import Blink.Controls.Menu (MenuItems (..), menuList, menuListMetrics, menuTrigger)
import Blink.Controls.ToggleButton (ToggleConfig (..), ToggleInteraction, defaultToggleButtonConfig, isSelected, onSelectedChanged)
import Blink.View
import Blink.Element (Element (..), HasItemAttrs (..), HasItems (..), HasLayoutConfig (..))
import Blink.Style
import Blink.Controls.Style (containerStyle)

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
  overControl = nested mbToggle (\c x -> c { mbToggle = x }) . overControl

instance HasEventHandlers (MenuButtonConfig e a msg)

instance HasLabelledConfig e msg (MenuButtonConfig e a msg) where
  overLabelled = nested mbToggle (\c x -> c { mbToggle = x }) . overLabelled

instance HasLayoutConfig (MenuButtonConfig e a msg) where
  overLayout = nested mbToggle (\c x -> c { mbToggle = x }) . overLayout

-- | The data to build one item from, in order. Defaults to @[]@; a later
-- 'items' attribute replaces an earlier one rather than adding to it.
instance HasItems a (MenuButtonConfig e a msg) where
  items xs = Attribute (\c -> c { mbItems = xs })

-- | Attributes for each item's button (e.g. 'Blink.Controls.Label.text',
-- 'Blink.Controls.Button.onActivated'), given the item. An item's
-- own 'Blink.Controls.Button.onActivated' fires (if set) in addition to,
-- not instead of, 'menuButton' closing the list on that same activation.
instance HasItemAttrs (a -> [Attribute (ButtonConfig e msg)]) (MenuButtonConfig e a msg) where
  itemAttrs f = Attribute (\c -> c { mbItemAttrs = f })

-- | Whether the item list is currently open. External state the caller
-- owns and re-supplies every frame, the same as
-- 'Blink.Controls.ToggleButton.isSelected' for a plain toggle button.
-- Defaults to 'False'.
isOpen :: Bool -> Attribute (MenuButtonConfig e a msg)
isOpen = nested mbToggle (\c t -> c { mbToggle = t }) . isSelected

-- | Reacts when the list should open or close, with the new value: from
-- activating the trigger, or 'False' whenever the open list closes for any
-- other reason. Store it and pass it back via 'isOpen'.
onOpenChanged :: (Bool -> [Effect e msg]) -> Attribute (MenuButtonConfig e a msg)
onOpenChanged = nested mbToggle (\c t -> c { mbToggle = t }) . onSelectedChanged

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
menuButton tag attrs = captionedButton btn (void (runMenuButton tag cfg))
  where
    cfg = resolve defaultMenuButtonConfig attrs
    btn = tgcButton (mbToggle cfg)

runMenuButton :: (Ord e, Ord a) => (MenuButtonPart a -> e) -> MenuButtonConfig e a msg -> View e msg (ToggleInteraction e msg)
runMenuButton tag cfg = do
  onTrigger <- isRegionHit
  menuTrigger (tag MenuButtonTrigger) (tag MenuButtonList) (mbToggle cfg) (\close -> itemsElement tag cfg close onTrigger)

-- | The open item list. @onTrigger@ is whether the pointer is on the
-- trigger, so a press there toggles the menu rather than counting as an
-- outside press.
itemsElement :: (Ord e, Ord a) => (MenuButtonPart a -> e) -> MenuButtonConfig e a msg -> View e msg () -> Bool -> Element e msg
itemsElement tag cfg close onTrigger =
  menuList menuButtonListStyleKey menu close onTrigger
  where
    menu = MenuItems
      { miListId    = tag MenuButtonList
      , miItemId    = tag . MenuButtonItem
      , miItems     = mbItems cfg
      , miItemAttrs = mbItemAttrs cfg
      , miSubmenu   = const Nothing
      }

-- * Style

-- | The 'StyleKey' the item list resolves its own panel background\/border
-- from unless overridden via 'Blink.Controls.Control.style'. Uses
-- 'containerStyle' -- built for exactly this shape, a composite on
-- 'Blink.View.Focus.withFocusScope' that reads as focused whenever any item
-- inside it does.
menuButtonListStyleKey :: StyleKey e
menuButtonListStyleKey = Class "menuButtonList"

-- | This control's one entry in 'Blink.Style.Defaults.defaultTheme', for
-- the popup list. The trigger button itself resolves its look from
-- 'Blink.Controls.ToggleButton.toggleButtonStyleKey' instead -- see
-- 'defaultMenuButtonConfig'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (menuButtonListStyleKey, (menuListMetrics, containerStyle p)) ]
