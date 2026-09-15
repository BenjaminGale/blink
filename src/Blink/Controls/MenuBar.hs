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
import Data.List (find)

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), buttonBase, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label (captionElement, lcText, renderLabelledContent)
import Blink.Controls.MenuBar.Style (menuBarListStyleKey, menuBarStyleKey)
import Blink.Controls.ToggleButton
  (ToggleConfig (..), ToggleInteraction (..), defaultToggleButtonConfig, toggleBase)
import Blink.Geometry (Alignment (TopLeft), Rectangle (..))
import Blink.Input (InputState (inputKeyEvents), Key (KeyDown, KeyEscape, KeyTab, KeyUp), KeyEvent (key))
import Blink.Layout.Box (children, hBox, vBox)
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
runMenuBarLabel
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> ButtonConfig e msg -> Rectangle
  -> View e msg (ToggleInteraction e msg)
runMenuBarLabel tag cfg menuKey labelCfg rowBounds = do
  enclosingScope <- getCurrentScope
  r <- toggleBase labelId toggleCfg
  onBar <- withBounds rowBounds isRegionHit
  let wasOpen    = mbrOpenMenu cfg == Just menuKey
      justOpened = tgiSelected r && not wasOpen
      close      = do
        runHandlers (mbrOnOpenChanged cfg) Nothing
        requestFocus enclosingScope labelId
  when justOpened $ requestFocus enclosingScope (tag (MenuBarList menuKey))
  when (tgiSelected r) $ popup labelId [content (itemsElement tag cfg menuKey close onBar)]
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

-- | A top-to-bottom list of buttons, one per item of the menu @menuKey@,
-- sized to fit its own content on both axes, drawn on a panel
-- background\/border (see 'menuBarListStyleKey') -- a plain, non-focusable
-- 'Blink.Controls.Control.control' wrapping it, purely for that chrome,
-- exactly as 'Blink.Controls.MenuButton.itemsElement' does for a single
-- menu button. Runs in its own focus scope ('MenuBarList' for @menuKey@),
-- with the same Escape\/Tab\/outside-click\/arrow-key handling.
-- @onBar@ is whether the click landed anywhere on the bar's own row (any
-- label, not just @menuKey@'s), captured by 'runMenuBarLabel' before this
-- list (a separate 'Element', run later, at a different ambient bounds) is
-- even queued -- so clicking a *different* label, which already handles
-- switching via its own activation, is never also treated as "outside"
-- here and closed a second time with a conflicting value.
itemsElement
  :: (Ord e, Ord a, Ord b)
  => (MenuBarPart a b -> e) -> MenuBarConfig e a b msg -> a -> View e msg () -> Bool -> Element e msg
itemsElement tag cfg menuKey close onBar = Element
  { elLayout  = Layout fitContent fitContent TopLeft
  , elMeasure = measureChrome menuBarListStyleKey box
  , elRun     = void (control panelCfg)
  }
  where
    menuItemsHere = mbrItemsFor cfg menuKey
    box = vBox [ width fitContent, height fitContent, children (map toItemElement menuItemsHere) ]

    -- ccElementId matters beyond styling: without one this never registers
    -- a hit-rect, so a click on the panel background (not an item) would
    -- reach straight through to whatever's behind the popup.
    panelCfg = defaultControlConfig
      { ccElementId   = Just (tag (MenuBarList menuKey))
      , ccStyleKey    = menuBarListStyleKey
      , ccFocusPolicy = NotFocusable
      , ccContent     = const scopedRun
      }

    scopedRun = withFocusScope (tag (MenuBarList menuKey)) $ do
      handleEscape
      handleTabOut
      handleOutsideClick
      handleArrowKeys
      elRun box

    -- Closes on Escape whenever this list is open, regardless of which
    -- item (if any) currently holds focus within it.
    handleEscape = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyEscape) . key) evs of
        Just e  -> consumeKey (key e) >> close
        Nothing -> pure ()

    -- Closes on Tab\/Shift-Tab too: this list renders after the label's
    -- own siblings, so a plain handoff can't reach them.
    handleTabOut = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyTab) . key) evs of
        Just e  -> consumeKey (key e) >> close
        Nothing -> pure ()

    -- Closes on a completed click that lands neither on the bar's own row
    -- (any label) nor anywhere in this list -- an item click is itself
    -- within this list's own bounds, so it never reads as "outside" here;
    -- it closes via its own activation instead (see 'toItemElement').
    handleOutsideClick = do
      released <- isButtonReleased
      onList   <- isRegionHit
      when (released && not onBar && not onList) close

    -- Moves the highlight to the next/previous item on Down\/Up, wrapping
    -- from the last item to the first (and back) in a single keypress.
    handleArrowKeys = case menuItemsHere of
      [] -> pure ()
      is -> do
        evs <- inputKeyEvents <$> getInput
        forM_ (find ((`elem` [KeyDown, KeyUp]) . key) evs) $ \e -> do
          consumeKey (key e)
          current <- getFocus
          let count        = length is
              currentIndex = current >>= (`lookup` zip (map (tag . MenuBarItem menuKey) is) [0 ..])
              nextIndex = case (key e, currentIndex) of
                (KeyDown, Nothing) -> 0
                (KeyDown, Just i)  -> (i + 1) `mod` count
                (_,       Nothing) -> count - 1
                (_,       Just i)  -> (i - 1) `mod` count
          forM_ (itemAt is nextIndex) $ \item ->
            requestFocus (Just (tag (MenuBarList menuKey))) (tag (MenuBarItem menuKey item))

    itemAt xs idx = case drop idx xs of
      (x : _) -> Just x
      []      -> Nothing

    toItemElement item = Element
      { elLayout  = bcLayout itemCfg
      , elMeasure = measureChrome (ccStyleKey (bcControl itemCfg)) (captionElement (lcText (bcLabelled itemCfg)))
      , elRun     = do
          r <- buttonBase (tag (MenuBarItem menuKey item)) itemCfg { bcControl = itemCtrl }
          when (biActivated r) close
      }
      where
        itemCfg  = resolve defaultButtonConfig (width fitContent : height fitContent : mbrItemAttrs cfg menuKey item)
        itemCtrl = (bcControl itemCfg) { ccContent = const (renderLabelledContent (bcLabelled itemCfg)) }
