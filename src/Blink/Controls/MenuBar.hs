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
-- without activating anything, a completed click outside both the open
-- label and its item list closes it too, and so does Tab or Shift-Tab.
-- Every closing path returns focus to the label that was open.
module Blink.Controls.MenuBar
  ( MenuBarConfig
  , MenuBarPart (..)
  , defaultMenuBarConfig
  , menuBar
  , menus
  , labelAttrs
  , menuItems
  , itemAttrs
  , openMenu
  , onOpenMenuChanged
  ) where

import Control.Monad (forM_, void, when)

import Data.List (elemIndex, find)

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label (captionElement, lcText, renderLabelledContent)
import Blink.Controls.Menu (menuList)
import Blink.Controls.MenuBar.Style (menuBarListStyleKey, menuBarStyleKey)
import Blink.Controls.ToggleButton
  (ToggleConfig (..), ToggleInteraction (..), defaultToggleButtonConfig, toggleBase)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..))
import Blink.Input (InputState (inputKeyEvents), Key (KeyLeft, KeyRight), KeyEvent (key))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.Popup (content, popup)
import Blink.View
import Blink.Element (Element (..), HasLayoutConfig (..), height, runElement, width)

-- | Identifies one part of a 'menuBar': its own container ('MenuBar'), one
-- top-level menu's own label ('MenuBarLabel'), its item list's own focus
-- scope ('MenuBarList'), or one of its items ('MenuBarItem') -- each tagged
-- by the owning menu's own data value and, for an item, the item's own
-- data value, rather than either's position in its list -- the same
-- rationale as 'Blink.Controls.ToggleGroup.ToggleGroupPart'.
data MenuBarPart a b
  = MenuBar
  | MenuBarLabel a
  | MenuBarList a
  | MenuBarItem a b
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

-- | Attributes for the button built from one top-level menu's own label
-- (e.g. 'Blink.Controls.Label.text'), computed once per menu rather than
-- written out by hand for each -- the same shape as
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
-- activation. Named the same as 'Blink.Controls.MenuButton.itemAttrs' --
-- import "Blink.Controls.MenuBar" qualified if using both in the same
-- module.
itemAttrs :: (a -> b -> [Attribute (ButtonConfig e msg)]) -> Attribute (MenuBarConfig e a b msg)
itemAttrs f = Attribute (\c -> c { mbrItemAttrs = f })

-- | Which top-level menu, if any, currently has its dropdown open.
-- External state the caller owns and re-supplies every frame, the same as
-- 'Blink.Controls.ToggleGroup.selectedItem'. Defaults to 'Nothing'.
openMenu :: Maybe a -> Attribute (MenuBarConfig e a b msg)
openMenu m = Attribute (\c -> c { mbrOpenMenu = m })

-- | Reacts when clicking a label would open, close, or switch which menu
-- is open, with the menu (if any) it changed to. Also fires (with
-- 'Nothing') when an item is activated or Escape is pressed while a menu
-- is open, closing it the same way. It's up to the reaction to actually
-- store the new value and pass it back in via 'openMenu' next frame --
-- the same contract as 'Blink.Controls.MenuButton.onOpenChanged'.
onOpenMenuChanged :: (Maybe a -> [Effect e msg]) -> Attribute (MenuBarConfig e a b msg)
onOpenMenuChanged f = Attribute (\c -> c { mbrOnOpenChanged = mbrOnOpenChanged c ++ [f] })

-- | A row of labels, one per 'menus', each opening a dropdown list of
-- items (built from 'menuItems') when clicked. @tag@ builds every part's
-- element id from a 'MenuBarPart': the bar's own container id from
-- 'MenuBar', each label's own id from 'MenuBarLabel', each open dropdown's
-- own focus scope id from 'MenuBarList', and each item's id from
-- 'MenuBarItem' -- both applied to the owning menu's own data.
menuBar :: (Ord e, Ord a, Ord b) => (MenuBarPart a b -> e) -> [Attribute (MenuBarConfig e a b msg)] -> Element e msg
menuBar tag attrs = Element
  { elLayout  = mbrLayout cfg
  -- rowBounds only matters to a label's own 'elRun' (see 'runMenuBarLabel'),
  -- never its 'elMeasure' -- an unused placeholder here is never forced.
  , elMeasure = measureChrome (ccStyleKey (mbrControl cfg)) (rowBox (Rectangle 0 0 0 0))
  , elRun     = void (control ccfg)
  }
  where
    cfg = resolve defaultMenuBarConfig attrs
    rowBox rowBounds = hBox [ width fill, height fitContent, children (map (toLabel rowBounds) (mbrMenus cfg)) ]
    toLabel rowBounds menuKey = Element
      { elLayout  = bcLayout labelCfg
      , elMeasure = measureChrome (ccStyleKey (bcControl labelCfg)) (captionElement (lcText (bcLabelled labelCfg)))
      , elRun     = void (runMenuBarLabel tag cfg menuKey labelCfg rowBounds)
      }
      where labelCfg = resolve defaultButtonConfig (width fitContent : height fitContent : mbrLabelAttrs cfg menuKey)
    ccfg = (mbrControl cfg)
      { ccElementId   = Just (tag MenuBar)
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (getBounds >>= runElement . rowBox)
      }

-- | Runs one top-level label as 'toggleBase' (its "selected" state
-- standing in for its own dropdown being open), toggling whenever it's
-- activated regardless of what any other label is doing -- since
-- 'mbrOpenMenu' is a single shared value, opening this label's menu
-- naturally reads as closing whichever other one was open, with no extra
-- arbitration needed. While open, queues the item list through
-- 'Blink.Popup.popup', anchored to this label's own just-rendered bounds;
-- the very frame it opens, moves focus into the item list's own scope (see
-- 'itemsElement'). @rowBounds@ is the whole bar's own resolved rectangle
-- (captured once, before any label runs) -- see 'itemsElement's @onBar@.
--
-- Also switches to this label's own menu, without a click, the moment the
-- pointer enters it while some *other* menu is already open -- the
-- standard menu-bar convention of "sweeping" across the bar once one menu
-- has been opened. Never fires while nothing is open, so idle hovering
-- across the bar never opens anything.
runMenuBarLabel
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> ButtonConfig e msg -> Rectangle
  -> View e msg (ToggleInteraction e msg)
runMenuBarLabel tag cfg menuKey labelCfg rowBounds = do
  enclosingScope <- getCurrentScope
  r <- toggleBase labelId toggleCfg
  onBar <- withBounds rowBounds isRegionHit
  let wasOpen      = mbrOpenMenu cfg == Just menuKey
      justOpened   = tgiSelected r && not wasOpen
      someOtherOpen = maybe False (/= menuKey) (mbrOpenMenu cfg)
      hoveredIn    = ciMouseEntered (biControl (tgiButton r))
      open newKey  = do
        runHandlers (mbrOnOpenChanged cfg) (Just newKey)
        requestFocus enclosingScope (tag (MenuBarList newKey))
      close        = do
        runHandlers (mbrOnOpenChanged cfg) Nothing
        requestFocus enclosingScope labelId
      switchByKey dir = forM_ (adjacentMenu (mbrMenus cfg) menuKey dir) open
  when (hoveredIn && someOtherOpen) (open menuKey)
  when justOpened $ requestFocus enclosingScope (tag (MenuBarList menuKey))
  when (tgiSelected r) $ popup labelId [content (itemsElement tag cfg menuKey close onBar switchByKey)]
  pure r
  where
    labelId   = tag (MenuBarLabel menuKey)
    labelCtrl = (bcControl labelCfg) { ccContent = const (renderLabelledContent (bcLabelled labelCfg)) }
    toggleCfg = defaultToggleButtonConfig
      { tgcButton            = labelCfg { bcControl = labelCtrl }
      , tgcSelected          = mbrOpenMenu cfg == Just menuKey
      , tgcOnSelectedChanged =
          [ \opened -> concatMap ($ (if opened then Just menuKey else Nothing)) (mbrOnOpenChanged cfg) ]
      }

-- | The menu adjacent to @menuKey@ in @allMenus@, wrapping from the last
-- back to the first (and back) -- 'KeyRight' moves forward, any other key
-- (only ever 'KeyLeft', see 'itemsElement') moves backward. 'Nothing' if
-- @menuKey@ isn't in @allMenus@, which cannot happen in practice since
-- every 'runMenuBarLabel' call is for a menu drawn from 'mbrMenus'.
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

-- | The dropdown itself -- see 'Blink.Controls.Menu.menuList' for the
-- shared engine. @onBar@ is whether the click landed anywhere on the bar's
-- own row (any label, not just @menuKey@'s), captured by 'runMenuBarLabel'
-- before this list (a separate 'Element', run later, at a different
-- ambient bounds) is even queued -- so clicking a *different* label, which
-- already handles switching via its own activation, is never also treated
-- as "outside" here and closed a second time with a conflicting value.
--
-- Wraps 'menuList' with Left\/Right handling on top: since this list only
-- ever runs while its own menu is the open one, any Left\/Right pressed
-- this frame is unambiguously meant for switching to the adjacent menu
-- (see 'adjacentMenu'), the same reasoning 'menuList' already applies to
-- Escape and Tab.
itemsElement
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> View e msg () -> Bool -> (Key -> View e msg ())
  -> Element e msg
itemsElement tag cfg menuKey close onBar switchByKey = base { elRun = handleMenuSwitchKeys >> elRun base }
  where
    base = menuList menuBarListStyleKey (tag (MenuBarList menuKey)) (tag . MenuBarItem menuKey)
      (mbrItemsFor cfg menuKey) (mbrItemAttrs cfg menuKey) close onBar

    handleMenuSwitchKeys = do
      evs <- inputKeyEvents <$> getInput
      forM_ (find ((`elem` [KeyLeft, KeyRight]) . key) evs) $ \e -> do
        consumeKey (key e)
        switchByKey (key e)
