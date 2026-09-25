{-# LANGUAGE OverloadedStrings #-}
-- | The dropdown item-list engine shared by "Blink.Controls.MenuButton" and
-- "Blink.Controls.MenuBar": a top-to-bottom list of buttons, one per item,
-- drawn on a panel background\/border, running in its own focus scope, with
-- Up\/Down moving the keyboard highlight between items (wrapping at either
-- end), Enter or a click on an item activating it and closing the menu,
-- Escape closing it without activating anything, a mouse press outside
-- both the menu's own trigger area and this list closing it too, and so
-- does Tab or Shift-Tab.
--
-- 'menuListWithSubmenus' adds optional per-item submenus, opened on hover,
-- a click, or Right-arrow, closing back to their own parent (not the whole
-- menu) on Left-arrow or Escape, or once the pointer has been on another
-- part of the parent list for a short delay.
--
-- 'menuTrigger' is the toggle that opens such a list and returns focus to
-- itself when the list closes.
module Blink.Controls.Menu
  ( MenuItems (..)
  , menuTrigger
  , menuList
  , menuListWithSubmenus
  , submenuInPlay
    -- * Style
  , menuItemStyleKey
  , menuItemSubmenuOpen
  , menuListMetrics
  , defaultStyleEntries
  ) where

import Control.Monad (filterM, forM_, when)
import Data.List (find)
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), buttonBase, captionedButton, defaultButtonConfig, withCaptionContent)
import Blink.Controls.Control
import Blink.Controls.Label (lcMnemonic)
import Blink.Controls.ToggleButton (ToggleConfig (..), ToggleInteraction (..), toggleBase)
import Blink.Geometry (Alignment (TopLeft), Insets (..), uniform)
import Blink.Input
  ( InputState (inputKeyEvents), Key (KeyDown, KeyEscape, KeyLeft, KeyRight, KeyTab, KeyUp)
  , KeyEvent (key)
  )
import Blink.Layout.Box (children, vBox)
import Blink.Layout.Constraints (Layout (..), atLeast, fill, fitContent)
import Blink.Popup (Edge (Start), Side (SideRight), content, placement, popup)
import Blink.View
import Blink.Element (Element (..), height, width)
import Blink.Controls.Style (plainStyle)
import Blink.Style

-- | Runs @toggleCfg@ as a menu's trigger, with 'tgcSelected' as whether
-- the menu is open. While open, shows @listFor close@ in a popup anchored
-- to the trigger, where @close@ closes the menu. Opening focuses
-- @listId@; closing refocuses the trigger unless the press that closed it
-- landed on a control that takes focus itself.
menuTrigger
  :: Ord e
  => e -> e -> ToggleConfig e msg -> (View e msg () -> Element e msg)
  -> View e msg (ToggleInteraction e msg)
menuTrigger triggerId listId toggleCfg listFor = do
  enclosingScope <- getCurrentScope
  r <- toggleBase triggerId toggleCfg { tgcButton = btn { bcControl = ctrl } }
  let justOpened = tgiSelected r && not wasOpen
      justClosed = wasOpen && not (tgiSelected r)
      refocusTrigger = do
        alreadyClaimed <- hasQueuedFocus enclosingScope
        when (not alreadyClaimed) $ requestFocus enclosingScope triggerId
      close = do
        runHandlers (tgcOnSelectedChanged toggleCfg) False
        refocusTrigger
  when justOpened $ requestFocus enclosingScope listId
  when justClosed refocusTrigger
  when (tgiSelected r) $ popup triggerId [content (listFor close)]
  pure r
  where
    wasOpen = tgcSelected toggleCfg
    btn     = tgcButton toggleCfg
    -- The press may be closing the menu, and closing refocuses the trigger anyway.
    suppressClickToFocus policy = case policy of
      Focusable opts | wasOpen -> Focusable opts { focusIsClickToFocus = False }
      _                        -> policy
    ctrl = (bcControl (withCaptionContent btn))
      { ccFocusPolicy = suppressClickToFocus (ccFocusPolicy (bcControl btn))
      }

-- | One menu list's ids and items.
data MenuItems e b msg = MenuItems
  { miListId    :: e
    -- ^ The list's element id and focus scope.
  , miItemId    :: b -> e
    -- ^ Each item's id, built from its data so reordering keeps per-item state.
  , miItems     :: [b]
  , miItemAttrs :: b -> [Attribute (ButtonConfig e msg)]
  , miSubmenu   :: b -> Maybe (e, [b])
    -- ^ An item's own submenu, if it has one: its list id and its items,
    -- which may carry further submenus of their own. Ignored by 'menuList'.
  }

-- | A vertical list of buttons, one per item, on a panel styled by
-- @styleKey@. The list has a minimum width, grows if an item needs more,
-- and every item spans its full width.
--
-- @pressKeepsOpen@ is whether the pointer is somewhere outside this list
-- where a press shouldn't close it, such as the trigger that opened it, or
-- anywhere on the row of a 'Blink.Controls.MenuBar.menuBar', so switching
-- menus isn't also treated as an outside press.
menuList
  :: (Ord e, Ord b)
  => StyleKey e -> MenuItems e b msg -> View e msg () -> Bool -> Element e msg
menuList styleKey menu = menuListWithSubmenus styleKey menu { miSubmenu = const Nothing }

-- | 'menuList' with the optional per-item submenus from 'miSubmenu'.
menuListWithSubmenus
  :: (Ord e, Ord b)
  => StyleKey e -> MenuItems e b msg -> View e msg () -> Bool -> Element e msg
menuListWithSubmenus styleKey menu close pressKeepsOpen =
  menuListCore styleKey menu
    (TopLevel close)
    pressKeepsOpen

menuMinWidth :: Double
menuMinWidth = 160

-- | Seconds the pointer must spend on the rest of the parent list, while a
-- submenu is open, before the submenu closes and the item under the
-- pointer takes the highlight (opening its own submenu, if it has one).
submenuSwitchDelay :: Double
submenuSwitchDelay = 0.25

-- | How a list closes. A submenu can close back to its parent (on Escape
-- or Left-arrow) or close every level; a top-level list has one way.
data CloseBehaviour e msg
  = TopLevel (View e msg ())
  | Nested
      (View e msg ()) -- ^ Closes every level.
      (View e msg ()) -- ^ Closes back to the parent list.

closeAll :: CloseBehaviour e msg -> View e msg ()
closeAll (TopLevel close) = close
closeAll (Nested close _) = close

closeThis :: CloseBehaviour e msg -> View e msg ()
closeThis (TopLevel close)    = close
closeThis (Nested _ toParent) = toParent

-- | The shared engine behind both 'menuList' and 'menuListWithSubmenus'.
menuListCore
  :: (Ord e, Ord b)
  => StyleKey e -> MenuItems e b msg -> CloseBehaviour e msg -> Bool -> Element e msg
menuListCore styleKey menu closeBehaviour pressKeepsOpen =
  controlElement (Layout (atLeast menuMinWidth) fitContent TopLeft) (itemBox (map (toItemElement False) items)) panelCfg
  where
    MenuItems { miListId = listId, miItemId = itemId, miItems = items, miSubmenu = submenuFor } = menu

    itemBox kids = vBox [ width fitContent, height fitContent, children kids ]

    -- A 'fill' item measures as zero, so only stretch items when laying out.
    fillWidth el = el { elLayout = (elLayout el) { layoutWidth = fill } }

    -- ccElementId matters beyond styling: without one this never registers
    -- a hit-rect, so a click on the panel background (not an item) would
    -- reach straight through to whatever's behind the popup.
    panelCfg = defaultControlConfig
      { ccElementId   = Just listId
      , ccStyleKey    = styleKey
      , ccFocusPolicy = NotFocusable
      , ccContent     = const scopedRun
      }

    -- While the highlight points at an open submenu, this level's own
    -- handling is skipped; the submenu (its own deferred popup) owns it.
    scopedRun = withFocusScope listId $ do
      openSubmenu <- anySubmenuFocused menu
      onList      <- isRegionHit
      when (not openSubmenu) $ do
        handleEscape
        handleTabOut
        handleOutsideClick onList
        handleArrowKeys menu
        handleLeftArrow
        handleMnemonics menu closeBehaviour
      elRun (itemBox (map (fillWidth . toItemElement onList) items))

    onKey k act = takeKey k >>= (`when` act)

    handleEscape = onKey KeyEscape (closeThis closeBehaviour)

    handleLeftArrow = case closeBehaviour of
      Nested _ toParent -> onKey KeyLeft toParent
      TopLevel _        -> pure ()

    -- Closes on Tab\/Shift-Tab too: this list renders after its trigger's
    -- own siblings, so a plain handoff can't reach them.
    handleTabOut = onKey KeyTab (closeAll closeBehaviour)

    -- Closes on the press: the pressed control takes focus then, and waiting
    -- for the release would show this list without focus until it came.
    handleOutsideClick onList = do
      pressed <- isButtonPressed
      when (pressed && not pressKeepsOpen && not onList) (closeAll closeBehaviour)

    toItemElement onList item = captionedButton itemCfg $ do
      opened <- maybe (pure False) (isFocused . fst) (submenuFor item)
      let states = if opened then Set.singleton menuItemSubmenuOpen else Set.empty
      r <- buttonBase (itemId item) itemCfg { bcControl = itemCtrl { ccActiveStates = states } }
      case submenuFor item of
        Nothing                -> do
          highlightOnHover item r
          when (biActivated r) (closeAll closeBehaviour)
        Just (subId, subItems) ->
          runSubmenu menu item subId r $
            popup (itemId item)
              [ content (submenuElement item subId subItems (onList || pressKeepsOpen))
              , placement SideRight Start
              ]
      where
        itemCfg  = itemConfig menu item
        itemCtrl = bcControl (withCaptionContent itemCfg)

    -- Keeps a single highlight shared by mouse and keyboard.
    highlightOnHover item r = do
      pointed <- pointerMovedOver menu r
      when pointed $ requestFocus (Just listId) (itemId item)

    -- A press on this list keeps the submenu open too, so a click on
    -- another item (to activate it) doesn't close the whole menu first.
    submenuElement item subId subItems pressKeepsSubmenuOpen =
      menuListCore styleKey menu { miListId = subId, miItems = subItems }
        (Nested (closeAll closeBehaviour) (requestFocus (Just listId) (itemId item)))
        pressKeepsSubmenuOpen

anySubmenuFocused :: Eq e => MenuItems e b msg -> View e msg Bool
anySubmenuFocused menu = do
  cur <- getFocus
  pure $ any (submenuFocused menu cur) (miItems menu)

-- | Moves focus by index because Tab traversal doesn't wrap, and menu
-- items should.
handleArrowKeys :: Eq e => MenuItems e b msg -> View e msg ()
handleArrowKeys menu = case miItems menu of
  [] -> pure ()
  is -> do
    evs <- inputKeyEvents <$> getInput
    forM_ (find ((`elem` [KeyDown, KeyUp]) . key) evs) $ \e -> do
      consumeKey (key e)
      current <- getFocus
      let count        = length is
          currentIndex = current >>= (`lookup` zip (map (miItemId menu) is) [0 ..])
          nextIndex = case (key e, currentIndex) of
            (KeyDown, Nothing) -> 0
            (KeyDown, Just i)  -> (i + 1) `mod` count
            (_,       Nothing) -> count - 1
            (_,       Just i)  -> (i - 1) `mod` count
      forM_ (itemAt is nextIndex) $ \item -> requestFocus (Just (miListId menu)) (miItemId menu item)
  where
    itemAt xs idx = case drop idx xs of
      (x : _) -> Just x
      []      -> Nothing

handleMnemonics :: MenuItems e b msg -> CloseBehaviour e msg -> View e msg ()
handleMnemonics menu closeBehaviour =
  takeMnemonic itemMnemonic (miItems menu) >>= mapM_ (\item ->
    case miSubmenu menu item of
      Just (subId, _) -> requestFocus (Just (miListId menu)) subId
      Nothing         -> do
        runHandlers (bcOnActivated (itemConfig menu item)) ()
        closeAll closeBehaviour)
  where
    itemMnemonic = lcMnemonic . bcLabelled . itemConfig menu

itemConfig :: MenuItems e b msg -> b -> ButtonConfig e msg
itemConfig menu item =
  resolve defaultButtonConfig (style menuItemStyleKey : width fitContent : height fitContent : miItemAttrs menu item)

-- | Movement rather than entry, so the pointer takes the highlight back
-- from the keyboard without first leaving the item. While a sibling's
-- submenu is open, the delayed switch in 'runSubmenu' moves it instead.
pointerMovedOver :: Eq e => MenuItems e b msg -> ButtonInteraction e msg -> View e msg Bool
pointerMovedOver menu r = do
  moved       <- hasMouseMoved
  siblingOpen <- anySubmenuFocused menu
  pure (moved && ciHovered (biControl r) && not siblingOpen)

-- | Opens @item@'s submenu @subId@ and runs @showSubmenu@ while it's open.
-- While another item's submenu is open, hovering this one doesn't open
-- its submenu straight away; the delayed switch below does, so a
-- diagonal move towards the open submenu can cross this item safely.
runSubmenu
  :: Ord e
  => MenuItems e b msg -> b -> e -> ButtonInteraction e msg -> View e msg () -> View e msg ()
runSubmenu menu item subId r showSubmenu = do
  opened <- isFocused subId
  when (not opened) $ do
    pointed      <- pointerMovedOver menu r
    highlighted  <- isFocused (miItemId menu item)
    rightPressed <- if highlighted then takeKey KeyRight else pure False
    when (pointed || biActivated r || rightPressed) $
      requestFocus (Just (miListId menu)) subId
  leaving <- if opened then leavingSubmenu menu item subId else pure False
  heldFor <- resolveHeldFor subId leaving
  when (heldFor >= submenuSwitchDelay) $ do
    sibling <- hoveredSibling menu item
    forM_ sibling $ \it -> requestFocus (Just (miListId menu)) (maybe (miItemId menu it) fst (miSubmenu menu it))
  when opened showSubmenu

-- | Anywhere on this list other than @item@ counts, including the gaps
-- between items, so sweeping across several items doesn't restart the
-- delay. Uses last frame's hover because items later in the list
-- haven't run yet this frame.
leavingSubmenu :: Ord e => MenuItems e b msg -> b -> e -> View e msg Bool
leavingSubmenu menu item subId = do
  overList    <- wasMouseOverLastFrame (miListId menu)
  overItem    <- wasMouseOverLastFrame (miItemId menu item)
  overSubmenu <- wasMouseOverLastFrame subId
  pure (overList && not overItem && not overSubmenu)

hoveredSibling :: Ord e => MenuItems e b msg -> b -> View e msg (Maybe b)
hoveredSibling menu item = listToMaybe <$> filterM (wasMouseOverLastFrame . miItemId menu) siblings
  where siblings = filter ((/= miItemId menu item) . miItemId menu) (miItems menu)

-- | 'True' when @listId@'s highlight is on an item with a submenu, or in
-- that submenu. Left\/Right then belongs to this list rather than to an
-- enclosing control, such as 'Blink.Controls.MenuBar.menuBar' switching
-- menus.
submenuInPlay :: (Ord e, Ord b) => MenuItems e b msg -> View e msg Bool
submenuInPlay menu = withFocusScope (miListId menu) $ do
  cur <- getFocus
  let itemFocused it = isJust (miSubmenu menu it) && cur == Just (miItemId menu it)
  pure $ any (\it -> itemFocused it || submenuFocused menu cur it) (miItems menu)

-- | Whether @cur@ is @item@'s own submenu.
submenuFocused :: Eq e => MenuItems e b msg -> Maybe e -> b -> Bool
submenuFocused menu cur item = maybe False ((== cur) . Just . fst) (miSubmenu menu item)

-- * Style

-- | The 'StyleKey' each item in a dropdown menu resolves its look from,
-- unless overridden via 'Blink.Controls.Control.style' in its attributes.
menuItemStyleKey :: StyleKey e
menuItemStyleKey = Class "menuItem"

-- | Present in an item's 'Blink.Controls.Control.ccActiveStates' while its
-- submenu is open, so the item stays highlighted after the highlight moves
-- into that submenu.
menuItemSubmenuOpen :: VisualState
menuItemSubmenuOpen = Custom "MenuItem" "SubmenuOpen"

-- | The spacing of a dropdown menu's panel.
menuListMetrics :: Metrics
menuListMetrics = Metrics
  { metricsMargin  = uniform 0
  , metricsPadding = Insets { topInset = 4, rightInset = 0, bottomInset = 4, leftInset = 0 }
  }

menuItemMetrics :: Metrics
menuItemMetrics = Metrics
  { metricsMargin  = uniform 0
  , metricsPadding = Insets { topInset = 4, rightInset = 12, bottomInset = 4, leftInset = 12 }
  }

-- | No 'CommonMouseOver' look: hovering an item moves the highlight onto
-- it, so 'FocusFocused' alone marks the one highlighted item.
menuItemStyle :: Palette -> StyleSet
menuItemStyle p = (plainStyle p)
  { styleOverrides = Map.fromList
      [ (CommonDisabled,      \s -> s { styleTextColour = paletteTextMuted p })
      , (FocusFocused,        \s -> s { styleBackground = paletteSurfaceHover p })
      , (menuItemSubmenuOpen, \s -> s { styleBackground = paletteSurfaceHover p })
      ]
  }

-- | This module's entry in 'Blink.Style.Defaults.defaultTheme', for the
-- items in a dropdown menu. Shared by "Blink.Controls.MenuButton" and
-- "Blink.Controls.MenuBar".
defaultStyleEntries :: Ord e => Palette -> [(StyleKey e, (Metrics, StyleSet))]
defaultStyleEntries p = [ (menuItemStyleKey, (menuItemMetrics, menuItemStyle p)) ]
