{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The standard entry point for interactive controls: manages keyboard
-- focus, reports raw mouse\/keyboard\/focus events to the given attrs, and
-- draws styled chrome around the control's content.
--
-- Focus is claimed automatically when nothing else holds it (subject to
-- 'isFocusable'), reaffirmed each frame while held, and given up on Tab. A
-- click or Shift-Tab can also hand focus to a /different/ specific
-- element, per the 'FocusOnClick' passed in. That takes
-- effect the frame after it happens rather than immediately, so whichever
-- element is gaining or losing focus reports it consistently regardless
-- of render order -- see
-- 'Blink.Element.FocusGained'\/'Blink.Element.FocusLost'. A disabled
-- control (per 'isEnabled') neither claims focus nor reacts to clicks or Tab.
--
-- = Building a container
--
-- 'control' is also the direct way to build a control that manages a
-- group of other controls as children, via 'tabNavigation' and
-- 'isArrowNavigationEnabled' -- there is no separate "container" primitive; a
-- container is just a plain 'control' configured this way:
--
-- ['Flatten' (the default)] Not a navigation container: this control's own
--     slot (if it's a tab stop) and its children all fold into the same
--     Tab sequence as its siblings.
-- ['Contained'] Opens a focus scope for this control's children:
--     Tab\/Shift-Tab (and, with 'isArrowNavigationEnabled', the arrow keys too)
--     cycle within it forever. Ctrl+Tab\/Ctrl+Shift+Tab are always the way
--     out, moving this control's own slot to the next\/previous one at
--     the enclosing level, regardless of nesting depth.
--
-- Scope is otherwise always root -- only a 'Contained' control opens one.
--
-- = Styling
--
-- A control resolves its 'Blink.Style.Style' by looking up a
-- 'Blink.Style.StyleKey' in the active 'Blink.Style.Theme' -- normally
-- whatever 'Blink.Style.Class' a ready-made widget (e.g. 'Blink.Button.button')
-- defaults to, so every instance of that widget picks up the same theme
-- entry without registering each element id individually. 'style'
-- overrides the key a specific @control@ call resolves against, e.g. to
-- 'Blink.Style.ElementId' for a one-off, per-instance style.
module Blink.Control
  ( control
  , FocusOnClick (..)
  , ControlConfig
  , HasControlConfig (..)
  , defaultControlConfig
  , isFocusable
  , isEnabled
  , tabNavigation
  , isArrowNavigationEnabled
  , NavigationMode (..)
  , style
  , StyleKey (..)
  ) where

import Control.Monad (forM_, guard, unless, when)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromMaybe)

import Blink.Attributes (Attr, configAny, fire)
import Blink.Element (ElementEvent (..), HasElementEvent (..), element, onClicked)
import Blink.Geometry (Rectangle, insetRect, borderInsets)
import Blink.Input (Key (..), KeyEvent (..), Modifier (..), InputState (..))
import Blink.Style (Style (..), StyleSet (..), StyleKey (..))
import Blink.UI

-- | How a control's children navigate via Tab\/Shift-Tab -- set with
-- 'tabNavigation'.
data NavigationMode
  = Flatten
    -- ^ Not a navigation container: this control's own slot (if it's a tab
    -- stop) and its children all fold into the same Tab sequence as its
    -- siblings, entering and leaving without any special trapping. The
    -- default -- and, for a control with no navigable children at all,
    -- indistinguishable from plain leaf behaviour.
  | Contained
    -- ^ Opens a focus scope for this control's children: Tab\/Shift-Tab
    -- cycle within it forever, never escaping back out that way.
    -- Ctrl+Tab\/Ctrl+Shift+Tab are the only way out, moving this control's
    -- own slot to the next\/previous one at the enclosing level -- and, if
    -- @ccIsArrowNavigationEnabled@ is set, the arrow keys also cycle within the
    -- scope, same as Tab\/Shift-Tab.
  deriving (Eq, Show)

-- | Configuration shared by every control, regardless of that control's own
-- @cfg@: whether Tab lands on it (@ccIsFocusable@), whether it responds to
-- input at all (@ccIsEnabled@), how its children navigate via Tab
-- (@ccTabNavigation@), and (only meaningful when @ccTabNavigation@ is
-- 'Contained') whether the arrow keys also cycle within that same scope
-- (@ccIsArrowNavigationEnabled@). Every control's own @cfg@ carries one of
-- these, accessed uniformly via 'HasControlConfig'. Not exported: 'isFocusable'
-- \/ 'isEnabled' \/ 'tabNavigation' \/ 'isArrowNavigationEnabled' \/ 'style'
-- are the only way to build or change one.
data ControlConfig e = ControlConfig
  { ccIsFocusable          :: Bool
  , ccIsEnabled            :: Bool
  , ccTabNavigation        :: NavigationMode
  , ccIsArrowNavigationEnabled :: Bool
  , ccStyleKey             :: StyleKey e
  }

-- | The default 'ControlConfig': focusable, not a navigation container
-- ('Flatten'), styled via @key@ unless overridden by 'style' -- what a
-- plain interactive control wants unless it overrides one or more via
-- 'isFocusable' \/ 'isEnabled' \/ 'tabNavigation' \/
-- 'isArrowNavigationEnabled'. @key@ is normally a 'Class' named after the
-- control being built, e.g. @Class \"button\"@.
defaultControlConfig :: StyleKey e -> ControlConfig e
defaultControlConfig styleKey = ControlConfig
  { ccIsFocusable          = True
  , ccIsEnabled            = True
  , ccTabNavigation        = Flatten
  , ccIsArrowNavigationEnabled = False
  , ccStyleKey             = styleKey
  }

-- | Implemented by any control's own @cfg@ type to say how it carries a
-- 'ControlConfig' -- lets 'isFocusable' and friends work uniformly across
-- every control's differently-shaped @cfg@, the same pattern
-- 'Blink.Controls.HasTextConfig' already uses for 'Blink.Controls.text'.
class HasControlConfig e cfg | cfg -> e where
  controlConfig    :: cfg -> ControlConfig e
  setControlConfig :: ControlConfig e -> cfg -> cfg

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both.
isFocusable :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isFocusable b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsFocusable = b }) cfg

-- | Whether the control responds to input at all. A disabled control still
-- renders (in its disabled style) but ignores hover, clicks, key presses,
-- and focus, and is skipped by Tab\/Shift-Tab. Defaults to 'True'.
isEnabled :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isEnabled b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsEnabled = b }) cfg

-- | How this control's children navigate via Tab\/Shift-Tab (and, since
-- they ride along with the same scope, Ctrl+Tab\/Ctrl+Shift+Tab) -- see
-- 'NavigationMode'. Defaults to 'Flatten'.
tabNavigation :: HasControlConfig e cfg => NavigationMode -> Attr e ev msg cfg
tabNavigation m = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccTabNavigation = m }) cfg

-- | Whether the arrow keys also cycle within this control's focus scope,
-- the same way Tab\/Shift-Tab do. Only meaningful when 'tabNavigation' is
-- 'Contained'. Defaults to 'False'.
isArrowNavigationEnabled :: HasControlConfig e cfg => Bool -> Attr e ev msg cfg
isArrowNavigationEnabled b = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccIsArrowNavigationEnabled = b }) cfg

-- | Which 'StyleKey' this control resolves its style from. Defaults to a
-- 'Class' named after the control; pass 'ElementId' to theme this one
-- instance differently, or a different 'Class' to group it with others.
style :: HasControlConfig e cfg => StyleKey e -> Attr e ev msg cfg
style k = configAny $ \cfg -> setControlConfig ((controlConfig cfg) { ccStyleKey = k }) cfg

-- | The control-specific hit area: the current bounds inset by the
-- element's margin -- the margin itself is never part of the control, so a
-- mouse position within it counts as "outside" for hovering, clicking, and
-- focus-claiming alike.
--
-- Uses the element's /normal/ margin, not the margin of whichever style
-- variant is currently active, since real themes don't vary margin by
-- state and 'getStyle' would otherwise depend on its own result.
marginInsetBounds :: Ord e => StyleKey e -> UI e msg Rectangle
marginInsetBounds styleKey = do
  ss <- getStyleSet styleKey
  r  <- getBounds
  pure (insetRect (styleMargin (styleSetNormal ss)) r)

-- | 'True' when the mouse is over the element's background rectangle (see
-- @marginInsetBounds@) -- the control-specific hit area.
isMouseOver :: Ord e => StyleKey e -> UI e msg Bool
isMouseOver styleKey = do
  hitBounds <- marginInsetBounds styleKey
  withBounds hitBounds isRegionHit

-- | 'True' when nothing else holds mouse capture, or this element itself
-- does (a drag in progress on this element doesn't count as contention).
isMouseFreeFor :: Eq e => e -> UI e msg Bool
isMouseFreeFor eid = do
  capturedByMe <- isDragging eid
  (|| capturedByMe) <$> isMouseFree

-- | 'True' when the element is hovered (per 'isMouseOver'), enabled, and
-- the mouse isn't contested by another element's drag.
isMouseTarget :: Ord e => e -> StyleKey e -> UI e msg Bool
isMouseTarget eid styleKey = do
  disabled    <- isDisabled
  hit         <- isMouseOver styleKey
  uncontested <- isMouseFreeFor eid
  pure (not disabled && hit && uncontested)

-- | 'True' when the element is hovered, enabled, not contested by another
-- element's drag, and the left button is currently held down.
isPressed :: Ord e => e -> StyleKey e -> UI e msg Bool
isPressed eid styleKey = do
  isTarget <- isMouseTarget eid styleKey
  down     <- isButtonDown
  pure (isTarget && down)

-- | Resolves the active 'Style' for an element given its current
-- interaction state. Priority: disabled > pressed > hovered > focused >
-- normal.
getStyle :: Ord e => e -> StyleKey e -> UI e msg Style
getStyle eid styleKey = do
  styles <- getStyleSet styleKey
  isDis  <- isDisabled
  isHov  <- isMouseOver styleKey
  isFoc  <- isFocused eid
  isPrs  <- isPressed eid styleKey
  let candidates =
        [ guard isDis $> styleSetDisabled styles
        , guard isPrs $> styleSetPressed  styles
        , guard isHov $> styleSetHovered  styles
        , guard isFoc $> styleSetFocused  styles
        ]
  pure $ fromMaybe (styleSetNormal styles) (asum candidates)

-- | Draws an element's background and border from the resolved style, then
-- runs @content@ clipped to the remaining space inside the padding, with
-- that same style available via 'currentStyle' so content never needs to
-- resolve its own copy.
styledElement :: Ord e => e -> StyleKey e -> UI e msg () -> UI e msg ()
styledElement eid styleKey content = do
  s <- getStyle eid styleKey
  r <- getBounds
  let bg          = insetRect (styleMargin s) r
      borderRect  = case styleBorderColour s of
                      Just _  -> insetRect (borderInsets (styleBorderEdges s)) bg
                      Nothing -> bg
      contentRect = insetRect (stylePadding s) borderRect
      inner       = withBounds contentRect $ clipToCurrent (withStyle s content)
  withBounds bg $
    withBackground (styleBackground s) $
    case styleBorderColour s of
      Just c  -> withBorder c (styleBorderEdges s) inner
      Nothing -> inner

-- | What clicking a control does to focus -- passed directly to 'control',
-- not carried on @cfg@: every ready-made widget fixes its own
-- click-to-focus behaviour rather than leaving it caller-configurable.
data FocusOnClick e
  = FocusSelf
    -- ^ The control takes focus itself (the default for interactive controls).
  | FocusTarget e
    -- ^ The control hands focus to a different element instead of taking it
    -- itself — e.g. a checkbox's label redirecting focus onto the checkbox.
  | NoFocus
    -- ^ Clicking the control has no effect on focus at all.
  deriving (Eq, Show)

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all
-- (@ccIsFocusable@) and configured to take focus itself on click ('FocusSelf').
autoClaimsFocus :: Eq e => FocusOnClick e -> ControlConfig e -> Bool
autoClaimsFocus foc cc = ccIsFocusable cc && foc == FocusSelf

-- | 'True' when this element should take focus with nothing having asked
-- for it: opted into auto-claiming (per 'autoClaimsFocus'), nothing else is
-- currently focused, and the mouse isn't contested by another element's
-- drag.
canAutoClaim :: (Ord e, HasControlConfig e cfg) => e -> FocusOnClick e -> cfg -> UI e msg Bool
canAutoClaim eid foc cfg = do
  nothingIsFocused <- isNothingFocused <$> getFocus
  uncontested      <- isMouseFreeFor eid
  pure (autoClaimsFocus foc (controlConfig cfg) && nothingIsFocused && uncontested)

-- | Gives up focus immediately when @wasFocused@ and one of @advanceKeys@
-- was just pressed; hands focus to the previous tab stop, one frame later,
-- when @wasFocused@ and one of @retreatKeys@ was pressed instead (see the
-- module header for why that one is deferred). Consumes whichever specific
-- key matched, so nothing else reacts to the same press, and reports
-- whether it advanced (gave up focus) this frame, for a caller that needs
-- to prime 'withFocusScope's @blockFreshClaim@ with it. Disabled controls
-- never react.
advanceOrRetreat :: Bool -> [(Key, [Modifier])] -> [(Key, [Modifier])] -> UI e msg Bool
advanceOrRetreat wasFocused advanceKeys retreatKeys = do
  disabled <- isDisabled
  if disabled then pure False else do
    evs      <- inputKeyEvents <$> getInput
    prevCtrl <- getPreviousTabStop
    scopeId  <- getCurrentScope
    let advanceHit = find (\e -> (key e, modifiers e) `elem` advanceKeys) evs
        retreatHit = find (\e -> (key e, modifiers e) `elem` retreatKeys) evs
    case (wasFocused, advanceHit, retreatHit) of
      (True, Just e, _) -> clearFocus >> consumeKey (key e) $> True
      (True, _, Just e) -> forM_ prevCtrl (requestFocus scopeId) >> consumeKey (key e) $> False
      _                 -> pure False

-- | Manages this element's keyboard focus (see the module header), reports
-- its raw mouse\/keyboard\/focus events to @attrs@, and draws its chrome
-- around @content@. Hovering, clicking, and focus-claiming all respect the
-- same margin-inset hit area chrome resolution uses -- the margin itself
-- never counts as "on" the control.
--
-- When @ccTabNavigation@ is 'Contained' (and this control is itself a tab
-- stop -- see @withChildNavigation@), @content@ becomes a focus scope for
-- Tab\/Shift-Tab (and, if @ccIsArrowNavigationEnabled@, the arrow keys too);
-- Ctrl+Tab\/Ctrl+Shift+Tab always escape it, moving this control's own
-- slot to the next\/previous one at the enclosing level.
control :: (Ord e, HasControlConfig e cfg, HasElementEvent ev) => e -> FocusOnClick e -> cfg -> [Attr e ev msg cfg] -> UI e msg () -> UI e msg ()
control eid foc cfg attrs content = disableWhen (not (ccIsEnabled cc)) $ do
  wasFocused  <- isFocused eid
  currentScope <- getCurrentScope
  applySelfFocus
  unless opensScope (applyNavigationKeys wasFocused)
  midFocused <- isFocused eid
  fire attrs (focusEvents wasFocused midFocused)
  hitBounds <- marginInsetBounds styleKey
  withBounds hitBounds $
    (if opensScope then withoutKeyEvents reservedKeys else id) $
      element eid (attrs ++ clickFocusReaction currentScope)
  styledElement eid styleKey (withChildNavigation content)
  -- A nested container gets first claim on Ctrl+Tab\/Ctrl+Shift+Tab (it
  -- consumes the key while @content@ above is still running, before this
  -- check ever sees it) -- only if nothing inside claims it does it
  -- escape this control's own slot instead. Checked again afterward
  -- because this can change @eid@'s own focus after @midFocused@ was
  -- already computed, which a second, equally-real transition this same
  -- frame deserves its own 'FocusGained'\/'FocusLost' report for.
  when opensScope $ do
    _ <- applyScopeEscape wasFocused
    nowFocused <- isFocused eid
    fire attrs (focusEvents midFocused nowFocused)
  -- Recorded last, after this control's own retreat (if any) has already
  -- read whatever the previous tab stop was -- otherwise a control would
  -- see its own, just-written entry instead of the one before it.
  when (ccIsFocusable cc) $ setPreviousTabStop eid
  where
    cc = controlConfig cfg
    styleKey = ccStyleKey cc
    opensScope = ccIsFocusable cc && ccTabNavigation cc == Contained

    focusEvents was now = map liftElementEvent $ concat
      [ [FocusGained | not was && now]
      , [FocusLost   | was && not now]
      ]

    -- Immediate, not deferred: needed so that when several controls are
    -- simultaneously eligible, only the first one to render claims focus.
    applySelfFocus = whenEnabled $ do
      isRetaining <- isFocused eid
      auto        <- canAutoClaim eid foc cfg
      when (isRetaining || auto) $ setFocus eid

    -- This control's own reaction to whichever keys are currently ambient
    -- (plain Tab\/Shift-Tab, unless some enclosing container has
    -- redefined them) -- only when it isn't itself opening a scope, since
    -- otherwise this would consume the key before any child gets a chance
    -- to react to it (see @withChildNavigation@).
    applyNavigationKeys wasFocused = do
      keys <- getNavigationKeys
      () <$ advanceOrRetreat wasFocused (navAdvance keys) (navRetreat keys)

    -- Ctrl+Tab\/Ctrl+Shift+Tab always move this control's own slot to the
    -- next\/previous one at the enclosing level, escaping whatever scope
    -- it opened for its children -- regardless of whether Tab\/Shift-Tab
    -- themselves are trapped inside that scope.
    applyScopeEscape wasFocused = advanceOrRetreat wasFocused [(KeyTab, [Ctrl])] [(KeyTab, [Ctrl, Shift])]

    -- Tab\/Shift-Tab (plus the arrow keys, if @ccIsArrowNavigationEnabled@) --
    -- shared between the scope's own redefined navigation keys
    -- ('withChildNavigation') and 'reservedKeys' below.
    scopeAdvanceKeys, scopeRetreatKeys :: [(Key, [Modifier])]
    scopeAdvanceKeys = (KeyTab, []) : [(k, []) | ccIsArrowNavigationEnabled cc, k <- [KeyRight, KeyDown]]
    scopeRetreatKeys = (KeyTab, [Shift]) : [(k, []) | ccIsArrowNavigationEnabled cc, k <- [KeyLeft, KeyUp]]

    -- Every key this control reserves for navigation once it opens a
    -- scope -- hidden from its own raw key reporting (see the
    -- 'withoutKeyEvents' call above) so a child cycling through them, or
    -- an escape via Ctrl+Tab, never also leaks through as an ordinary
    -- 'Blink.Element.onKeyPressed' on this control itself.
    reservedKeys = scopeAdvanceKeys ++ scopeRetreatKeys ++ [(KeyTab, [Ctrl]), (KeyTab, [Ctrl, Shift])]

    -- A click hands focus to whichever element FocusOnClick names, taking
    -- effect one frame later (see the module header). Targets whichever
    -- scope was ambient when this control itself rendered (@currentScope@,
    -- read once up front), not always root, so this still works correctly
    -- from inside a 'Contained' container.
    clickFocusReaction currentScope = case foc of
      FocusSelf     | ccIsFocusable cc -> [onClicked (\() -> [OutUi (Focus currentScope eid)])]
                    | otherwise        -> []
      FocusTarget t -> [onClicked (\() -> [OutUi (Focus currentScope t)])]
      NoFocus       -> []

    -- Opens a focus scope for @content'@ exactly when this control does
    -- (see 'opensScope'), redefining the ambient navigation keys to Tab\/
    -- Shift-Tab (plus the arrow keys, if @ccIsArrowNavigationEnabled@) so
    -- containment and cycling fall out of ordinary focus behaviour for
    -- whatever's inside, unchanged. 'applyScopeEscape' runs after this
    -- returns (see above), never inside it, so it always targets the
    -- enclosing level, not the scope's own internal child pointer.
    withChildNavigation content'
      | opensScope = withFocusScope eid False (withNavigationKeys scopeKeys content')
      | otherwise  = content'
      where
        scopeKeys = NavigationKeys { navAdvance = scopeAdvanceKeys, navRetreat = scopeRetreatKeys }
