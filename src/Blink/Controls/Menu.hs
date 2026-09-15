{-# LANGUAGE OverloadedStrings #-}
-- | The dropdown item-list engine shared by "Blink.Controls.MenuButton" and
-- "Blink.Controls.MenuBar": a top-to-bottom list of buttons, one per item,
-- drawn on a panel background\/border, running in its own focus scope, with
-- Up\/Down moving the keyboard highlight between items (wrapping at either
-- end), Enter or a click on an item activating it and closing the menu,
-- Escape closing it without activating anything, a completed click outside
-- both the menu's own trigger area and this list closing it too, and so
-- does Tab or Shift-Tab.
--
-- 'menuListWithSubmenus' adds optional per-item submenus, opened on hover,
-- a click, or Right-arrow, closing back to their own parent (not the whole
-- menu) on Left-arrow or Escape.
module Blink.Controls.Menu
  ( menuList
  , menuListWithSubmenus
  , submenuInPlay
  ) where

import Control.Monad (forM_, void, when)
import Data.List (find)

import Blink.Controls.Button (ButtonConfig (..), ButtonInteraction (..), buttonBase, defaultButtonConfig)
import Blink.Controls.Control
import Blink.Controls.Label (captionElement, lcText, renderLabelledContent)
import Blink.Geometry (Alignment (TopLeft))
import Blink.Input
  (InputState (inputKeyEvents), Key (KeyDown, KeyEscape, KeyLeft, KeyRight, KeyTab, KeyUp), KeyEvent (key))
import Blink.Layout.Box (children, vBox)
import Blink.Layout.Constraints (Layout (..), fitContent)
import Blink.Popup (Edge (Start), Side (SideRight), content, placement, popup)
import Blink.View
import Blink.Element (Element (..), height, width)

-- | A top-to-bottom list of buttons, one per item of @items@, sized to fit
-- its own content on both axes, drawn on a panel background\/border (per
-- @styleKey@) -- a plain, non-focusable 'Blink.Controls.Control.control'
-- wrapping it, purely for that chrome. No item carries a submenu; see
-- 'menuListWithSubmenus' for that. @listId@ is this list's own element id,
-- used both for its hit-rect registration (so a click on the panel
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
menuList styleKey listId itemId items itemAttrsFor close onOutsideTrigger =
  menuListCore styleKey listId itemId items itemAttrsFor (const Nothing)
    CloseBehaviour { cbCloseAll = close, cbCloseThis = close, cbNested = False }
    onOutsideTrigger

-- | 'menuList' with optional per-item submenus. @submenuFor@ gives an
-- item's own submenu, if it has one: its own list\/focus scope id (built
-- the same way as @itemId@) paired with its items, which may carry a
-- further submenu of their own via the same function.
menuListWithSubmenus
  :: (Ord e, Ord b)
  => StyleKey e -> e -> (b -> e) -> [b] -> (b -> [Attribute (ButtonConfig e msg)]) -> (b -> Maybe (e, [b]))
  -> View e msg () -> Bool
  -> Element e msg
menuListWithSubmenus styleKey listId itemId items itemAttrsFor submenuFor close onOutsideTrigger =
  menuListCore styleKey listId itemId items itemAttrsFor submenuFor
    CloseBehaviour { cbCloseAll = close, cbCloseThis = close, cbNested = False }
    onOutsideTrigger

-- | A flat, top-level list uses the same action for both fields; a
-- submenu, opened recursively, pops back to its own parent on
-- 'cbCloseThis' and only unwinds every level on 'cbCloseAll'.
data CloseBehaviour e msg = CloseBehaviour
  { cbCloseAll  :: View e msg ()
  , cbCloseThis :: View e msg ()
  , cbNested    :: Bool
    -- ^ Whether Left-arrow backs out a level -- only for a submenu.
  }

-- | The shared engine behind both 'menuList' and 'menuListWithSubmenus'.
menuListCore
  :: (Ord e, Ord b)
  => StyleKey e -> e -> (b -> e) -> [b] -> (b -> [Attribute (ButtonConfig e msg)]) -> (b -> Maybe (e, [b]))
  -> CloseBehaviour e msg -> Bool
  -> Element e msg
menuListCore styleKey listId itemId items itemAttrsFor submenuFor closeBehaviour onOutsideTrigger = Element
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

    -- While the highlight points at an open submenu, this level's own
    -- handling is skipped; the submenu (its own deferred popup) owns it.
    scopedRun = withFocusScope listId $ do
      openSubmenu <- anySubmenuFocused
      when (not openSubmenu) $ do
        handleEscape
        handleTabOut
        handleOutsideClick
        handleArrowKeys
        handleLeftArrow
      elRun box

    anySubmenuFocused = do
      cur <- getFocus
      pure $ any (\it -> case submenuFor it of
                            Just (subId, _) -> Just subId == cur
                            Nothing         -> False) items

    -- Closes on Escape whenever this list is open, regardless of which
    -- item (if any) currently holds focus within it -- matching a native
    -- menu, which closes on Escape without needing a specific item
    -- highlighted.
    handleEscape = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyEscape) . key) evs of
        Just e  -> consumeKey (key e) >> cbCloseThis closeBehaviour
        Nothing -> pure ()

    handleLeftArrow = when (cbNested closeBehaviour) $ do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyLeft) . key) evs of
        Just e  -> consumeKey (key e) >> cbCloseThis closeBehaviour
        Nothing -> pure ()

    -- Closes on Tab\/Shift-Tab too: this list renders after its trigger's
    -- own siblings, so a plain handoff can't reach them.
    handleTabOut = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyTab) . key) evs of
        Just e  -> consumeKey (key e) >> cbCloseAll closeBehaviour
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
      when (released && not onOutsideTrigger && not onList) (cbCloseAll closeBehaviour)

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
          case submenuFor item of
            Nothing                -> when (biActivated r) (cbCloseAll closeBehaviour)
            Just (subId, subItems) -> runSubmenu item subId subItems r
      }
      where
        itemCfg  = resolve defaultButtonConfig (width fitContent : height fitContent : itemAttrsFor item)
        itemCtrl = (bcControl itemCfg) { ccContent = const (renderLabelledContent (bcLabelled itemCfg)) }

    runSubmenu item subId subItems r = do
      opened <- isFocused subId
      when (not opened) $ do
        highlighted  <- isFocused (itemId item)
        rightPressed <- if highlighted then keyPressed KeyRight else pure False
        when (ciMouseEntered (biControl r) || biActivated r || rightPressed) $
          requestFocus (Just listId) subId
      when opened $ do
        onItem <- isRegionHit
        popup (itemId item)
          [ content (submenuElement item subId subItems onItem)
          , placement SideRight Start
          ]

    keyPressed k = do
      evs <- inputKeyEvents <$> getInput
      case find ((== k) . key) evs of
        Just e  -> consumeKey (key e) >> pure True
        Nothing -> pure False

    -- @onItem@ is whether the click landed on @item@'s own bounds, so a
    -- click back on it isn't treated as outside the submenu too.
    submenuElement item subId subItems onItem =
      menuListCore styleKey subId itemId subItems itemAttrsFor submenuFor
        CloseBehaviour
          { cbCloseAll  = cbCloseAll closeBehaviour
          , cbCloseThis = requestFocus (Just listId) (itemId item)
          , cbNested    = True
          }
        onItem

-- | 'True' when @listId@'s own current highlight is an item with a submenu,
-- or that item's own open submenu -- i.e. whether Left\/Right belongs to
-- this list rather than an enclosing composite (see
-- 'Blink.Controls.MenuBar.menuBar's own top-level Left\/Right switching).
submenuInPlay :: (Ord e, Ord b) => e -> (b -> e) -> [b] -> (b -> Maybe (e, [b])) -> View e msg Bool
submenuInPlay listId itemId items submenuFor = withFocusScope listId $ do
  cur <- getFocus
  pure $ any (\it -> case submenuFor it of
                        Just (subId, _) -> cur == Just (itemId it) || cur == Just subId
                        Nothing         -> False) items
