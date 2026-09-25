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
    -- * Style
  , menuBarStyleKey
  , menuBarLabelStyleKey
  , menuBarListStyleKey
  , defaultStyleEntries
  ) where

import Control.Monad (forM_, void, when)

import Data.List (elemIndex, find)
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Map.Strict as Map

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), captionedButton, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label (lcMnemonic)
import Blink.Controls.Menu (MenuItems (..), menuListMetrics, menuListWithSubmenus, menuTrigger, submenuInPlay)
import Blink.Controls.ToggleButton (ToggleConfig (..), ToggleInteraction (..), defaultToggleButtonConfig, toggleChecked)
import Blink.Geometry (Alignment (TopLeft), Insets (..), uniform)
import Blink.Input (InputState (inputKeyEvents), Key (KeyLeft, KeyRight), KeyEvent (key))
import Blink.Layout.Box (children, hBox)
import Blink.Layout.Constraints (Layout (..), fill, fitContent)
import Blink.View
import Blink.Element (Element (..), HasItemAttrs (..), HasLayoutConfig (..), height, runElement, width)
import Blink.Controls.Style (containerStyle, transparent)
import Blink.Rendering (TextAlign (..))
import Blink.Style

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
  { mbrControl           :: ControlConfig e msg
  , mbrLayout            :: Layout
  , mbrMenus             :: [a]
  , mbrLabelAttrs        :: a -> [Attribute (ButtonConfig e msg)]
  , mbrMenuItems         :: a -> [b]
  , mbrItemAttrs         :: a -> b -> [Attribute (ButtonConfig e msg)]
  , mbrSubmenuItems      :: a -> b -> [b]
  , mbrOpenMenu          :: Maybe a
  , mbrOnOpenMenuChanged :: [Maybe a -> [Effect e msg]]
  }

-- | 'defaultControlConfig' (styled via 'menuBarStyleKey'), filling the
-- width it's given and sizing its height to its own labels' content, no
-- menus, no per-label or per-item attrs, nothing open, and no
-- 'onOpenMenuChanged' reactions.
defaultMenuBarConfig :: MenuBarConfig e a b msg
defaultMenuBarConfig = MenuBarConfig
  { mbrControl           = defaultControlConfig { ccStyleKey = menuBarStyleKey }
  , mbrLayout            = Layout fill fitContent TopLeft
  , mbrMenus             = []
  , mbrLabelAttrs        = const []
  , mbrMenuItems         = const []
  , mbrItemAttrs         = \_ _ -> []
  , mbrSubmenuItems      = \_ _ -> []
  , mbrOpenMenu          = Nothing
  , mbrOnOpenMenuChanged = []
  }

instance HasControlConfig e msg (MenuBarConfig e a b msg) where
  overControl attr = Attribute (\c -> c { mbrControl = runAttribute (overControl attr) (mbrControl c) })

instance HasEventHandlers (MenuBarConfig e a b msg)

instance HasLayoutConfig (MenuBarConfig e a b msg) where
  overLayout attr = Attribute (\c -> c { mbrLayout = runAttribute attr (mbrLayout c) })

-- | The top-level menus to build a label from, in order. Defaults to
-- @[]@; a later 'menus' attribute replaces an earlier one rather than
-- adding to it.
menus :: [a] -> Attribute (MenuBarConfig e a b msg)
menus xs = Attribute (\c -> c { mbrMenus = xs })

-- | Attributes for each top-level menu's label button (e.g.
-- 'Blink.Controls.Label.text'), given the menu. The same shape as
-- 'Blink.Controls.ToggleGroup.itemAttrs'.
labelAttrs :: (a -> [Attribute (ButtonConfig e msg)]) -> Attribute (MenuBarConfig e a b msg)
labelAttrs f = Attribute (\c -> c { mbrLabelAttrs = f })

-- | The data to build one menu's dropdown items from, given the menu.
-- Defaults to @const []@.
menuItems :: (a -> [b]) -> Attribute (MenuBarConfig e a b msg)
menuItems f = Attribute (\c -> c { mbrMenuItems = f })

-- | Attributes for the button built from one item, given its own menu and
-- data (e.g. 'Blink.Controls.Label.text', 'Blink.Controls.Button.onActivated').
-- An item's own 'Blink.Controls.Button.onActivated' fires (if set) in
-- addition to, not instead of, 'menuBar' closing the menu on that same
-- activation.
instance HasItemAttrs (a -> b -> [Attribute (ButtonConfig e msg)]) (MenuBarConfig e a b msg) where
  itemAttrs f = Attribute (\c -> c { mbrItemAttrs = f })

-- | The data to build one item's own submenu from, given its menu and
-- itself. Defaults to @\\_ _ -> []@ (no submenu).
submenuItems :: (a -> b -> [b]) -> Attribute (MenuBarConfig e a b msg)
submenuItems f = Attribute (\c -> c { mbrSubmenuItems = f })

-- | Which top-level menu, if any, currently has its dropdown open.
-- External state the caller owns and re-supplies every frame, the same as
-- 'Blink.Controls.ToggleGroup.selection'. Defaults to 'Nothing'.
openMenu :: Maybe a -> Attribute (MenuBarConfig e a b msg)
openMenu m = Attribute (\c -> c { mbrOpenMenu = m })

-- | Reacts when the open menu should change, with the new value: from a
-- label click, or 'Nothing' whenever the open menu closes for any other
-- reason. Store it and pass it back via 'openMenu'.
onOpenMenuChanged :: (Maybe a -> [Effect e msg]) -> Attribute (MenuBarConfig e a b msg)
onOpenMenuChanged f = Attribute (\c -> c { mbrOnOpenMenuChanged = mbrOnOpenMenuChanged c ++ [f] })

-- | A row of labels, one per 'menus', each opening a dropdown list of
-- items (built from 'menuItems') when clicked. @tag@ builds every part's
-- element id from a 'MenuBarPart': the bar's own container id from
-- 'MenuBar', each label's own id from 'MenuBarLabel', each open dropdown's
-- own focus scope id from 'MenuBarList', each item's id from
-- 'MenuBarItem', and each submenu's own scope id from 'MenuBarSubmenu'.
menuBar :: (Ord e, Ord a, Ord b) => (MenuBarPart a b -> e) -> [Attribute (MenuBarConfig e a b msg)] -> Element e msg
menuBar tag attrs = controlElement (mbrLayout cfg) (rowBox False) ccfg
  where
    cfg = resolve defaultMenuBarConfig attrs
    rowBox onBar = hBox [ width fill, height fill, children (map (toLabel onBar) (mbrMenus cfg)) ]
    toLabel onBar menuKey = captionedButton labelCfg (void (runMenuBarLabel tag cfg menuKey labelCfg onBar))
      where labelCfg = labelConfigFor menuKey
    labelConfigFor m = resolve labelDefaults (width fitContent : height fitContent : mbrLabelAttrs cfg m)
    labelDefaults = defaultButtonConfig
      { bcControl = (bcControl defaultButtonConfig) { ccStyleKey = menuBarLabelStyleKey } }
    ccfg = (mbrControl cfg)
      { ccElementId   = Just (tag MenuBar)
      , ccFocusPolicy = NotFocusable
      , ccContent     = const (handleMnemonics >> isRegionHit >>= runElement . rowBox)
      }

    -- Unlike a label click, never toggles an already-open menu closed.
    handleMnemonics = do
      scope <- getCurrentScope
      takeMnemonic labelMnemonic (mbrMenus cfg) >>= mapM_ (\menuKey ->
        when (mbrOpenMenu cfg /= Just menuKey) (openMenuFor tag cfg scope menuKey))

    labelMnemonic = lcMnemonic . bcLabelled . labelConfigFor

-- | One top-level label and, while its menu is open, its dropdown.
-- Hovering a label while another menu is open switches to it, as native
-- menu bars do. @onBar@ is whether the pointer is on the bar's row.
runMenuBarLabel
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> ButtonConfig e msg -> Bool
  -> View e msg (ToggleInteraction e msg)
runMenuBarLabel tag cfg menuKey labelCfg onBar = do
  enclosingScope <- getCurrentScope
  let open            = openMenuFor tag cfg enclosingScope
      switchMenu step = forM_ (adjacentMenu (mbrMenus cfg) menuKey step) open
  r <- menuTrigger (tag (MenuBarLabel menuKey)) (tag (MenuBarList menuKey)) toggleCfg
         (\close -> itemsElement tag cfg menuKey close onBar switchMenu)
  let someOtherOpen = maybe False (/= menuKey) (mbrOpenMenu cfg)
      hoveredIn     = ciMouseEntered (biControl (tgiButton r))
  when (hoveredIn && someOtherOpen) (open menuKey)
  pure r
  where
    toggleCfg = defaultToggleButtonConfig
      { tgcButton            = labelCfg
      , tgcSelected          = mbrOpenMenu cfg == Just menuKey
      , tgcOnSelectedChanged = map (. toOpenMenu) (mbrOnOpenMenuChanged cfg)
      }
    toOpenMenu opened = if opened then Just menuKey else Nothing

-- | Opens @newKey@'s dropdown and focuses its item list.
openMenuFor :: (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> Maybe e -> a -> View e msg ()
openMenuFor tag cfg enclosingScope newKey = do
  runHandlers (mbrOnOpenMenuChanged cfg) (Just newKey)
  requestFocus enclosingScope (tag (MenuBarList newKey))

-- | The menu @step@ places after @menuKey@ (before it, if negative),
-- wrapping at either end. 'Nothing' if @menuKey@ isn't in @allMenus@.
adjacentMenu :: Eq a => [a] -> a -> Int -> Maybe a
adjacentMenu allMenus menuKey step = do
  i <- elemIndex menuKey allMenus
  listToMaybe (drop ((i + step) `mod` length allMenus) allMenus)

-- | The open dropdown. Left\/Right switch to the adjacent menu unless a
-- submenu is highlighted or open. @onBar@ is whether the pointer is on
-- the bar's row, so pressing another label switches menus rather than
-- also closing this one as an outside press.
itemsElement
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> View e msg () -> Bool -> (Int -> View e msg ())
  -> Element e msg
itemsElement tag cfg menuKey close onBar switchMenu = base { elRun = handleMenuSwitchKeys >> elRun base }
  where
    menu = MenuItems
      { miListId    = tag (MenuBarList menuKey)
      , miItemId    = tag . MenuBarItem menuKey
      , miItems     = mbrMenuItems cfg menuKey
      , miItemAttrs = mbrItemAttrs cfg menuKey
      , miSubmenu   = submenuFor
      }
    submenuFor item = case mbrSubmenuItems cfg menuKey item of
      [] -> Nothing
      xs -> Just (tag (MenuBarSubmenu menuKey item), xs)

    base = menuListWithSubmenus menuBarListStyleKey menu close onBar

    -- Left\/Right belongs to a submenu the moment one is highlighted or
    -- open (see 'submenuInPlay'); only otherwise does it switch menus.
    handleMenuSwitchKeys = do
      inPlay <- submenuInPlay menu
      evs    <- inputKeyEvents <$> getInput
      when (not inPlay) $ forM_ (find (isJust . menuStep . key) evs) $ \e -> do
        consumeKey (key e)
        forM_ (menuStep (key e)) switchMenu

    menuStep :: Key -> Maybe Int
    menuStep KeyLeft  = Just (-1)
    menuStep KeyRight = Just 1
    menuStep _        = Nothing

-- * Style

-- | The 'StyleKey' 'Blink.Controls.MenuBar.menuBar' resolves its own
-- container chrome from unless overridden via 'Blink.Controls.Control.style'.
-- A flat strip -- background plus a bottom rule only, the same
-- top\/bottom-only-border idea the sample app's own status bar uses --
-- rather than a fully bordered box, so it reads as a toolbar sitting above
-- the rest of the window instead of a boxed-in panel.
menuBarStyleKey :: StyleKey e
menuBarStyleKey = Class "menuBar"

-- | The 'StyleKey' each of 'Blink.Controls.MenuBar.menuBar''s own labels
-- resolves its look from -- flat at rest (no visible border\/fill), tinted
-- on hover and while its own dropdown is open ('toggleChecked'), the same
-- pseudo-state 'Blink.Controls.ToggleButton.toggleBase' already puts in
-- 'Blink.Controls.Control.ccActiveStates'. Deliberately its own key rather
-- than 'Blink.Controls.ToggleButton.toggleButtonStyleKey' -- a
-- top-level menu label reads as a plain menu-bar item, not a button.
menuBarLabelStyleKey :: StyleKey e
menuBarLabelStyleKey = Class "menuBarLabel"

-- | The 'StyleKey' an open dropdown list resolves its own panel
-- background\/border from unless overridden via 'Blink.Controls.Control.style'.
-- Uses 'containerStyle' -- the same shape
-- 'Blink.Controls.MenuButton.menuButtonListStyleKey' resolves to.
menuBarListStyleKey :: StyleKey e
menuBarListStyleKey = Class "menuBarList"

menuBarMetrics :: Metrics
menuBarMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = Insets { topInset = 0, rightInset = 4, bottomInset = 0, leftInset = 4 }
  }

menuBarLabelMetrics :: Metrics
menuBarLabelMetrics = Metrics
  { metricsMargin      = uniform 0
  , metricsPadding     = Insets { topInset = 3, rightInset = 8, bottomInset = 3, leftInset = 8 }
  }

-- | Only the bottom edge visible -- the flat-strip, rule-only look
-- described in the doc comment on 'menuBarStyleKey'.
bottomOnly :: EdgeVisibility
bottomOnly = allEdgesVisible { edgeTopVisible = False, edgeRightVisible = False, edgeLeftVisible = False }

menuBarStyle :: Palette -> StyleSet
menuBarStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = paletteSurface p
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignLeft
      , styleBorder       = map (\l -> l { layerVisible = bottomOnly }) (soloBorder (paletteBorder p) 1)
      }
  , styleOverrides = Map.empty
  }

menuBarLabelStyle :: Palette -> StyleSet
menuBarLabelStyle p = StyleSet
  { styleBase = Style
      { styleBackground   = transparent
      , styleTextColour   = paletteTextPrimary p
      , styleTextAlign    = AlignCenter
      , styleBorder       = soloBorder transparent 1
      }
  , styleOverrides = Map.fromList
      [ (CommonMouseOver, \s -> s { styleBackground = paletteSurfaceHover p })
      , (CommonPressed,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (toggleChecked,   \s -> s { styleBackground = paletteSurfaceHover p })
      , (FocusFocused,    \s -> s { styleBorder = withBorderColour (paletteFocusRing p) (styleBorder s) })
      ]
  }

-- | This control's entries in 'Blink.Style.Defaults.defaultTheme'.
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p =
  [ (menuBarStyleKey,      (menuBarMetrics, menuBarStyle p))
  , (menuBarLabelStyleKey, (menuBarLabelMetrics, menuBarLabelStyle p))
  , (menuBarListStyleKey,  (menuListMetrics, containerStyle p))
  ]
