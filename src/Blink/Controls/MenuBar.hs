{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A horizontal row of top-level menus, each opening a dropdown list of
-- items when clicked, with only one dropdown open at a time: the
-- multi-menu sibling of "Blink.Controls.MenuButton". Which menu (if any) is
-- open is external, caller-owned state (see 'openMenu'), the same as every
-- other stateful control in "Blink.Controls".
--
-- @
-- control --> menuBar
-- @
--
-- Clicking a label opens its dropdown; clicking it again, or clicking a
-- different label, closes it (switching to the other one in the latter
-- case). While a dropdown is open, the same navigation as
-- "Blink.Controls.MenuButton" applies within it: Up\/Down move the
-- keyboard highlight between items, wrapping at either end, Enter or a
-- click on an item activates it and closes the menu, Escape closes it
-- without activating anything, a mouse press outside both the open label
-- and its item list closes it too, and so does Tab or Shift-Tab. Closing
-- returns focus to the label that was open, unless the press that closed
-- it landed on a control that takes focus itself.
module Blink.Controls.MenuBar
  ( MenuBarConfig
  , MenuBarPart (..)
  , defaultMenuBarConfig
  , menuBar
  , menus
  , labelAttrs
  , menuItems
  , itemAttrs
  , submenuItems
  , openMenu
  , onOpenMenuChanged
  ) where

import Control.Monad (forM_, void, when)

import Data.Char (toUpper)
import Data.List (elemIndex, find)

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label (captionElement, lcMnemonic, lcText)
import Blink.Controls.Menu (menuListWithSubmenus, menuTrigger, submenuInPlay)
import Blink.Controls.MenuBar.Style (menuBarLabelStyleKey, menuBarListStyleKey, menuBarStyleKey)
import Blink.Controls.ToggleButton (ToggleConfig (..), ToggleInteraction (..), defaultToggleButtonConfig)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..))
import Blink.Input
  (InputState (inputKeyEvents), Key (KeyChar, KeyLeft, KeyRight), KeyEvent (key), mnemonicActivated)
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.View
import Blink.Element (Element (..), HasLayoutConfig (..), height, runElement, width)

-- | Identifies one part of a 'menuBar': its own container ('MenuBar'), one
-- top-level menu's own label ('MenuBarLabel'), its item list's own focus
-- scope ('MenuBarList'), one of its items ('MenuBarItem'), or an item's
-- submenu ('MenuBarSubmenu'). Each is tagged by the menu's data and, for an
-- item, the item's data, so reordering keeps per-part state, as with
-- 'Blink.Controls.ToggleGroup.ToggleGroupPart'.
data MenuBarPart a b
  = MenuBar
  | MenuBarLabel a
  | MenuBarList a
  | MenuBarItem a b
  | MenuBarSubmenu a b
  deriving (Eq, Ord, Show)

-- | Every capability 'menuBar' resolves: its own container control, the
-- top-level menus to build labels from (in order) and how to configure
-- each label's own button, the items to build one menu's dropdown from and
-- how to configure each item's own button, which menu (if any) is open,
-- and its reactions to that changing.
data MenuBarConfig e a b msg = MenuBarConfig
  { mbrControl       :: ControlConfig e msg
  , mbrLayout        :: Layout
  , mbrMenus         :: [a]
  , mbrLabelAttrs    :: a -> [Attribute (ButtonConfig e msg)]
  , mbrItemsFor      :: a -> [b]
  , mbrItemAttrs     :: a -> b -> [Attribute (ButtonConfig e msg)]
  , mbrSubmenuItemsFor :: a -> b -> [b]
  , mbrOpenMenu      :: Maybe a
  , mbrOnOpenChanged :: [Maybe a -> [Effect e msg]]
  }

-- | 'defaultControlConfig' (styled via 'menuBarStyleKey'), filling the
-- width it's given and sizing its height to its own labels' content, no
-- menus, no per-label or per-item attrs, nothing open, and no
-- 'onOpenMenuChanged' reactions.
defaultMenuBarConfig :: MenuBarConfig e a b msg
defaultMenuBarConfig = MenuBarConfig
  { mbrControl       = defaultControlConfig { ccStyleKey = menuBarStyleKey }
  , mbrLayout        = Layout fill fitContent TopLeft
  , mbrMenus         = []
  , mbrLabelAttrs    = const []
  , mbrItemsFor      = const []
  , mbrItemAttrs     = \_ _ -> []
  , mbrSubmenuItemsFor = \_ _ -> []
  , mbrOpenMenu      = Nothing
  , mbrOnOpenChanged = []
  }

instance HasControlConfig e msg (MenuBarConfig e a b msg) where
  overControl attr = Attribute (\c -> c { mbrControl = runAttribute (overControl attr) (mbrControl c) })

instance HasLayoutConfig (MenuBarConfig e a b msg) where
  overLayout attr = Attribute (\c -> c { mbrLayout = runAttribute attr (mbrLayout c) })

-- | The top-level menus to build a label from, in order. Defaults to
-- @[]@; a later 'menus' attribute replaces an earlier one rather than
-- adding to it.
menus :: [a] -> Attribute (MenuBarConfig e a b msg)
menus xs = Attribute (\c -> c { mbrMenus = xs })

-- | Attributes for each top-level menu's label button (e.g.
-- 'Blink.Controls.Label.text'), given the menu. The same shape as
-- 'Blink.Controls.ToggleGroup.toggleAttributes'.
labelAttrs :: (a -> [Attribute (ButtonConfig e msg)]) -> Attribute (MenuBarConfig e a b msg)
labelAttrs f = Attribute (\c -> c { mbrLabelAttrs = f })

-- | The data to build one menu's dropdown items from, given the menu.
-- Defaults to @const []@.
menuItems :: (a -> [b]) -> Attribute (MenuBarConfig e a b msg)
menuItems f = Attribute (\c -> c { mbrItemsFor = f })

-- | Attributes for the button built from one item, given its own menu and
-- data (e.g. 'Blink.Controls.Label.text', 'Blink.Controls.Button.onActivated').
-- An item's own 'Blink.Controls.Button.onActivated' fires (if set) in
-- addition to, not instead of, 'menuBar' closing the menu on that same
-- activation. Import "Blink.Controls.MenuBar" qualified if also using
-- 'Blink.Controls.MenuButton.itemAttrs', which has the same name.
itemAttrs :: (a -> b -> [Attribute (ButtonConfig e msg)]) -> Attribute (MenuBarConfig e a b msg)
itemAttrs f = Attribute (\c -> c { mbrItemAttrs = f })

-- | The data to build one item's own submenu from, given its menu and
-- itself. Defaults to @\\_ _ -> []@ (no submenu).
submenuItems :: (a -> b -> [b]) -> Attribute (MenuBarConfig e a b msg)
submenuItems f = Attribute (\c -> c { mbrSubmenuItemsFor = f })

-- | Which top-level menu, if any, currently has its dropdown open.
-- External state the caller owns and re-supplies every frame, the same as
-- 'Blink.Controls.ToggleGroup.selectedItem'. Defaults to 'Nothing'.
openMenu :: Maybe a -> Attribute (MenuBarConfig e a b msg)
openMenu m = Attribute (\c -> c { mbrOpenMenu = m })

-- | Reacts when the open menu should change, with the new value: from a
-- label click, or 'Nothing' whenever the open menu closes for any other
-- reason. Store it and pass it back via 'openMenu'.
onOpenMenuChanged :: (Maybe a -> [Effect e msg]) -> Attribute (MenuBarConfig e a b msg)
onOpenMenuChanged f = Attribute (\c -> c { mbrOnOpenChanged = mbrOnOpenChanged c ++ [f] })

-- | A row of labels, one per 'menus', each opening a dropdown list of
-- items (built from 'menuItems') when clicked. @tag@ builds every part's
-- element id from a 'MenuBarPart': the bar's own container id from
-- 'MenuBar', each label's own id from 'MenuBarLabel', each open dropdown's
-- own focus scope id from 'MenuBarList', each item's id from
-- 'MenuBarItem', and each submenu's own scope id from 'MenuBarSubmenu'.
menuBar :: (Ord e, Ord a, Ord b) => (MenuBarPart a b -> e) -> [Attribute (MenuBarConfig e a b msg)] -> Element e msg
menuBar tag attrs = Element
  { elLayout  = mbrLayout cfg
  -- Measuring never reads rowBounds, so a placeholder is safe here.
  , elMeasure = measureChrome (ccStyleKey (mbrControl cfg)) (rowBox (Rectangle 0 0 0 0))
  , elRun     = void (control ccfg)
  }
  where
    cfg = resolve defaultMenuBarConfig attrs
    rowBox rowBounds = hBox [ width fill, height fill, children (map (toLabel rowBounds) (mbrMenus cfg)) ]
    toLabel rowBounds menuKey = Element
      { elLayout  = bcLayout labelCfg
      , elMeasure = measureChrome menuBarLabelStyleKey (captionElement (lcText (bcLabelled labelCfg)))
      , elRun     = void (runMenuBarLabel tag cfg menuKey labelCfg rowBounds)
      }
      where labelCfg = resolve defaultButtonConfig (width fitContent : height fitContent : mbrLabelAttrs cfg menuKey)
    ccfg = (mbrControl cfg)
      { ccElementId   = Just (tag MenuBar)
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (handleMnemonics >> getBounds >>= runElement . rowBox)
      }

    -- Unlike a label click, never toggles an already-open menu closed.
    handleMnemonics = do
      scope <- getCurrentScope
      evs   <- inputKeyEvents <$> getInput
      forM_ (find (\m -> maybe False (`mnemonicActivated` evs) (labelMnemonic m)) (mbrMenus cfg)) $ \menuKey -> do
        forM_ (labelMnemonic menuKey) (consumeKey . KeyChar . toUpper)
        when (mbrOpenMenu cfg /= Just menuKey) (openMenuFor tag cfg scope menuKey)

    labelMnemonic m = lcMnemonic (bcLabelled (resolve defaultButtonConfig (mbrLabelAttrs cfg m)))

-- | One top-level label and, while its menu is open, its dropdown.
-- Hovering a label while another menu is open switches to it, as native
-- menu bars do.
runMenuBarLabel
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> ButtonConfig e msg -> Rectangle
  -> View e msg (ToggleInteraction e msg)
runMenuBarLabel tag cfg menuKey labelCfg rowBounds = do
  enclosingScope <- getCurrentScope
  onBar <- withBounds rowBounds isRegionHit
  let open            = openMenuFor tag cfg enclosingScope
      switchByKey dir = forM_ (adjacentMenu (mbrMenus cfg) menuKey dir) open
  r <- menuTrigger (tag (MenuBarLabel menuKey)) (tag (MenuBarList menuKey)) toggleCfg
         (\close -> itemsElement tag cfg menuKey close onBar switchByKey)
  let someOtherOpen = maybe False (/= menuKey) (mbrOpenMenu cfg)
      hoveredIn     = ciMouseEntered (biControl (tgiButton r))
  when (hoveredIn && someOtherOpen) (open menuKey)
  pure r
  where
    toggleCfg = defaultToggleButtonConfig
      { tgcButton            = labelCfg { bcControl = (bcControl labelCfg) { ccStyleKey = menuBarLabelStyleKey } }
      , tgcSelected          = mbrOpenMenu cfg == Just menuKey
      , tgcOnSelectedChanged =
          [ \opened -> concatMap ($ (if opened then Just menuKey else Nothing)) (mbrOnOpenChanged cfg) ]
      }

-- | Opens @newKey@'s dropdown and focuses its item list.
openMenuFor :: (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> Maybe e -> a -> View e msg ()
openMenuFor tag cfg enclosingScope newKey = do
  runHandlers (mbrOnOpenChanged cfg) (Just newKey)
  requestFocus enclosingScope (tag (MenuBarList newKey))

-- | The menu after @menuKey@ for 'KeyRight', or before it for any other
-- key, wrapping at either end. 'Nothing' if @menuKey@ isn't in @allMenus@.
adjacentMenu :: Eq a => [a] -> a -> Key -> Maybe a
adjacentMenu allMenus menuKey dir = do
  i <- elemIndex menuKey allMenus
  let count = length allMenus
      next  = case dir of
        KeyRight -> (i + 1) `mod` count
        _        -> (i - 1) `mod` count
  case drop next allMenus of
    (x : _) -> Just x
    []      -> Nothing

-- | The open dropdown. Left\/Right switch to the adjacent menu unless a
-- submenu is highlighted or open. @onBar@ is whether the pointer is on
-- the bar's row, so pressing another label switches menus rather than
-- also closing this one as an outside press.
itemsElement
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> View e msg () -> Bool -> (Key -> View e msg ())
  -> Element e msg
itemsElement tag cfg menuKey close onBar switchByKey = base { elRun = handleMenuSwitchKeys >> elRun base }
  where
    listId  = tag (MenuBarList menuKey)
    itemId  = tag . MenuBarItem menuKey
    items   = mbrItemsFor cfg menuKey
    submenuFor item = case mbrSubmenuItemsFor cfg menuKey item of
      [] -> Nothing
      xs -> Just (tag (MenuBarSubmenu menuKey item), xs)

    base = menuListWithSubmenus menuBarListStyleKey listId itemId items (mbrItemAttrs cfg menuKey) submenuFor
      close onBar

    -- Left\/Right belongs to a submenu the moment one is highlighted or
    -- open (see 'submenuInPlay'); only otherwise does it switch menus.
    handleMenuSwitchKeys = do
      inPlay <- submenuInPlay listId itemId items submenuFor
      evs    <- inputKeyEvents <$> getInput
      when (not inPlay) $ forM_ (find ((`elem` [KeyLeft, KeyRight]) . key) evs) $ \e -> do
        consumeKey (key e)
        switchByKey (key e)
