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
-- 'menuListWithSubmenus' extends the same engine with optional per-item
-- submenus: an item's own submenu opens, as a side-anchored popup, on
-- hover, a click, or Right-arrow while that item holds the keyboard
-- highlight, and closes back to its own parent list -- never the whole
-- menu -- on Left-arrow or Escape, with its own independent Up\/Down
-- wraparound. Nesting is unbounded: a submenu item can carry a further
-- submenu of its own, through the same mechanism.
module Blink.Controls.Menu
  ( menuList
  , menuListWithSubmenus
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

-- | 'menuList', extended with optional per-item submenus -- see the module
-- header. @submenuFor@ gives an item's own submenu, if it has one: the
-- element id its own nested list\/focus scope runs under (built the same
-- way as @itemId@, e.g. a sibling constructor of the same tag type), paired
-- with its own items -- themselves eligible for a further @submenuFor@ of
-- their own, the same function applying at every depth. 'Nothing' means the
-- item has no submenu.
menuListWithSubmenus
  :: (Ord e, Ord b)
  => StyleKey e -> e -> (b -> e) -> [b] -> (b -> [Attribute (ButtonConfig e msg)]) -> (b -> Maybe (e, [b]))
  -> View e msg () -> Bool
  -> Element e msg
menuListWithSubmenus styleKey listId itemId items itemAttrsFor submenuFor close onOutsideTrigger =
  menuListCore styleKey listId itemId items itemAttrsFor submenuFor
    CloseBehaviour { cbCloseAll = close, cbCloseThis = close, cbNested = False }
    onOutsideTrigger

-- | How a 'menuListCore' instance reacts to whatever closes it -- see
-- 'menuListWithSubmenus'. A flat, top-level list (from 'menuList', or the
-- outermost call under 'menuListWithSubmenus') uses the same action for
-- both fields, since there is no parent level to pop back to; a submenu,
-- opened recursively (see 'runSubmenu'), pops back to its own parent list
-- on 'cbCloseThis' and only unwinds every level on 'cbCloseAll'.
data CloseBehaviour e msg = CloseBehaviour
  { cbCloseAll  :: View e msg ()
    -- ^ Fires on an item's own activation, an outside click, or Tab\/
    -- Shift-Tab -- all of which, native menus agree, dismiss the whole
    -- cascade, not just the level the event happened to land on.
  , cbCloseThis :: View e msg ()
    -- ^ Fires on Escape, and (only when 'cbNested') Left-arrow -- both of
    -- which back out one level at a time.
  , cbNested    :: Bool
    -- ^ Whether Left-arrow should back out a level at all -- 'False' for a
    -- top-level list, which has nothing to back out to; native menus leave
    -- Left-arrow alone there rather than closing on it.
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

    -- Escape\/Tab\/outside-click\/arrow-key handling all read and react to
    -- this level's own highlight -- but while that highlight actually
    -- points at an open submenu's own id (see 'runSubmenu'), every one of
    -- those belongs to the submenu instead, which runs later this same
    -- frame as its own deferred popup (see "Blink.Popup") and handles them
    -- itself. Left alone here, they'd misfire against a highlight that no
    -- longer names any item in this list at all.
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
    -- highlighted. Only backs out one level (see 'cbCloseThis').
    handleEscape = do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyEscape) . key) evs of
        Just e  -> consumeKey (key e) >> cbCloseThis closeBehaviour
        Nothing -> pure ()

    -- Left-arrow is this list's own "back out" key when it's itself a
    -- submenu (see 'cbNested') -- a no-op for a top-level list, which has
    -- no parent level to back out to.
    handleLeftArrow = when (cbNested closeBehaviour) $ do
      evs <- inputKeyEvents <$> getInput
      case find ((== KeyLeft) . key) evs of
        Just e  -> consumeKey (key e) >> cbCloseThis closeBehaviour
        Nothing -> pure ()

    -- Closes on Tab\/Shift-Tab too: this list renders after its trigger's
    -- own siblings, so a plain handoff can't reach them. Unwinds every
    -- level at once, the same as an outside click -- Tab is leaving the
    -- whole menu behind for the rest of the page, not stepping back into a
    -- parent level of it.
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
    -- own activation instead (see 'toItemElement'). Unwinds every level,
    -- like Tab -- a click that misses everything is aimed at whatever's
    -- behind the whole cascade, not just this level of it.
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

    -- Opens @item@'s own submenu -- moving this list's highlight from the
    -- item's own id onto its submenu's id, the two mutually-exclusive
    -- values 'anySubmenuFocused' distinguishes -- on hover, a click, or
    -- Right-arrow while @item@ holds the highlight, and renders it (through
    -- this same engine, recursively) via 'Blink.Popup.popup' for as long as
    -- it stays open. Right-arrow is scoped to a highlighted item, same as
    -- Up\/Down; hover and a click are not, so sweeping the pointer across a
    -- row of submenu items -- or clicking one outright -- opens whichever
    -- one it lands on without needing it highlighted first, matching
    -- 'Blink.Controls.MenuBar.menuBar's own hover-sweep between top-level
    -- menus.
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

    -- The submenu itself: another 'menuListCore', anchored to its own
    -- triggering item's just-rendered bounds via the SideRight popup. Its
    -- own Escape\/Left-arrow pop back to @item@ (see 'CloseBehaviour'),
    -- refocusing it in this list, rather than unwinding this list too.
    -- @onItem@ -- whether the click landed on @item@'s own bounds, captured
    -- while they were still the ambient bounds in 'runSubmenu' -- keeps a
    -- click back on the triggering item from reading as outside the
    -- submenu and closing it a second time via a conflicting path.
    submenuElement item subId subItems onItem =
      menuListCore styleKey subId itemId subItems itemAttrsFor submenuFor
        CloseBehaviour
          { cbCloseAll  = cbCloseAll closeBehaviour
          , cbCloseThis = requestFocus (Just listId) (itemId item)
          , cbNested    = True
          }
        onItem
