{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The standard entry point for interactive controls: manages keyboard
-- focus, reports raw mouse\/keyboard\/focus events, and draws styled chrome
-- around the control's content -- all configured via a plain list of
-- 'ControlAttrs', with no positional arguments beyond the element id.
--
-- Focus is claimed automatically when nothing else holds it (subject to
-- 'isFocusable'), reaffirmed each frame while held, and given up on Tab. A
-- click or Shift-Tab can also hand focus to a /different/ specific
-- element, per the 'FocusOnClick' passed via 'focusOnClick'. That takes
-- effect the frame after it happens rather than immediately, so whichever
-- element is gaining or losing focus reports it consistently regardless
-- of render order -- see
-- 'Blink.Element.FocusGained'\/'Blink.Element.FocusLost'. A disabled
-- control (per 'isEnabled') neither claims focus nor reacts to clicks or Tab.
--
-- = Building a widget
--
-- Every ready-made widget ('Blink.Button.button', 'Blink.Label.label', ...)
-- is built by resolving its own closed attrs type down to a
-- @['ControlAttrs' e msg]@ and calling 'control' directly -- see
-- 'HasControlConfig'\/'HasElementEvents' for the batched, mechanical way a
-- widget's own attrs type shares the common capabilities, and
-- 'translateCommon' for converting between them. 'focusOnClick' and
-- 'content' are deliberately /not/ part of either batched class: a widget
-- fixes both itself and never writes 'HasFocusOnClickConfig'\/
-- 'HasContentConfig' instances for its own attrs type, so e.g.
-- @button eid [content ...]@ simply fails to typecheck -- nothing stops a
-- more-derived control from writing those instances itself if it
-- deliberately wants to expose either.
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
  , ControlAttrs
  , ControlConfig
  , NavigationMode (..)
  , StyleKey (..)
  , EventHandler
  , KeyEventHandler
  , ElementEvents
  , ControlProperties
  , HasControlConfig (..)
  , HasElementEvents (..)
  , HasFocusOnClickConfig (..)
  , HasContentConfig (..)
  , HasTextConfig (..)
  , translateCommon
  , combineHandlers
  , runHandlers
  , isFocusable
  , isEnabled
  , style
  , tabNavigation
  , isArrowNavigationEnabled
  , onClicked
  , onFocusGained
  , onFocusLost
  , onMouseEntered
  , onMouseExited
  , onMouseDown
  , onMouseUp
  , onKeyPressed
  , fireOnClick
  , focusOnClick
  , content
  , text
  ) where

import Control.Monad (forM_, guard, unless, when)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find, foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

import Blink.Element
  ( ElementAttrs, ElementEvents, EventHandler, HasElementEvents (..), KeyEventHandler
  , element, fireClick, fireFocusChange, focusTransition, onClicked, onFocusGained, onFocusLost, onKeyPressed
  , onMouseDown, onMouseEntered, onMouseExited, onMouseUp
  )
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

-- | What clicking a control does to focus -- set with 'focusOnClick'.
data FocusOnClick e
  = FocusSelf
    -- ^ The control takes focus itself (the default for interactive controls).
  | FocusTarget e
    -- ^ The control hands focus to a different element instead of taking it
    -- itself — e.g. a checkbox's label redirecting focus onto the checkbox.
  | NoFocus
    -- ^ Clicking the control has no effect on focus at all.
  deriving (Eq, Show)

-- | One of the five capabilities shared by every control -- the single
-- payload every attrs type carries one of via 'HasControlConfig', instead
-- of each attrs type repeating its own parallel set of five constructors.
data ControlProperties e
  = ControlIsFocusable Bool
  | ControlIsEnabled Bool
  | ControlStyle (StyleKey e)
  | ControlTabNavigation NavigationMode
  | ControlIsArrowNavigationEnabled Bool

-- | Implemented by any attrs type that carries the capabilities shared by
-- every control -- one @configure@\/@extract@ pair over the shared
-- 'ControlProperties', rather than one pair per individual capability, so a
-- value can be both constructed /and/ generically inspected (see
-- 'translateCommon'). 'isFocusable'\/'isEnabled'\/'style'\/
-- 'tabNavigation'\/'isArrowNavigationEnabled' are the smart constructors
-- built on the @configure@ half; a widget's own module only re-exports the
-- ones it actually wants callers to be able to set.
class HasControlConfig e cfg | cfg -> e where
  configureControlCapability :: ControlProperties e -> cfg
  extractControlCapability :: cfg -> Maybe (ControlProperties e)

-- | Whether this control participates in keyboard focus at all: Tab\/
-- Shift-Tab cycling onto it, and auto-claiming focus by rendering first
-- while nothing else holds it. 'False' excludes it from both.
isFocusable :: HasControlConfig e cfg => Bool -> cfg
isFocusable = configureControlCapability . ControlIsFocusable

-- | Whether the control responds to input at all. A disabled control still
-- renders (in its disabled style) but ignores hover, clicks, key presses,
-- and focus, and is skipped by Tab\/Shift-Tab. Defaults to 'True'.
isEnabled :: HasControlConfig e cfg => Bool -> cfg
isEnabled = configureControlCapability . ControlIsEnabled

-- | Which 'StyleKey' this control resolves its style from. Defaults to a
-- 'Class' named after the control; pass 'ElementId' to theme this one
-- instance differently, or a different 'Class' to group it with others.
style :: HasControlConfig e cfg => StyleKey e -> cfg
style = configureControlCapability . ControlStyle

-- | How this control's children navigate via Tab\/Shift-Tab (and, since
-- they ride along with the same scope, Ctrl+Tab\/Ctrl+Shift+Tab) -- see
-- 'NavigationMode'. Defaults to 'Flatten'.
tabNavigation :: HasControlConfig e cfg => NavigationMode -> cfg
tabNavigation = configureControlCapability . ControlTabNavigation

-- | Whether the arrow keys also cycle within this control's focus scope,
-- the same way Tab\/Shift-Tab do. Only meaningful when 'tabNavigation' is
-- 'Contained'. Defaults to 'False'.
isArrowNavigationEnabled :: HasControlConfig e cfg => Bool -> cfg
isArrowNavigationEnabled = configureControlCapability . ControlIsArrowNavigationEnabled

-- | Converts the common subset of one attrs type onto another's, using the
-- @extract@ half of the source's instances and the @configure@ half of the
-- destination's -- 'Nothing' when @a@ doesn't carry either of these
-- capabilities (i.e. it's one of the destination type's own-specific
-- attrs). The mechanism every widget uses to translate its own closed
-- attrs type down to @['ControlAttrs' e msg]@ before calling 'control'.
translateCommon
  :: (HasControlConfig e a, HasControlConfig e b, HasElementEvents e msg a, HasElementEvents e msg b)
  => a -> Maybe b
translateCommon a = asum
  [ configureControlCapability <$> extractControlCapability a
  , configureElementEvent <$> extractElementEvent a
  ]

-- | Implemented only by 'ControlAttrs' in this migration: what clicking a
-- control does to focus (see 'FocusOnClick'). Deliberately /not/ part of
-- 'HasControlConfig' -- every widget fixes this itself, and simply never
-- writes this instance for its own attrs type, so callers can't override
-- it. A more-derived control that genuinely wants callers to set this can
-- still write the instance itself.
class HasFocusOnClickConfig e cfg | cfg -> e where
  configureFocusOnClick :: FocusOnClick e -> cfg
  extractFocusOnClick :: cfg -> Maybe (FocusOnClick e)

-- | Sets what clicking this control does to focus. Only ever called from
-- inside a widget's own module -- see 'HasFocusOnClickConfig'.
focusOnClick :: HasFocusOnClickConfig e cfg => FocusOnClick e -> cfg
focusOnClick = configureFocusOnClick

-- | Implemented only by 'ControlAttrs' in this migration: the content
-- rendered inside a control's chrome. Deliberately not part of
-- 'HasControlConfig' -- see 'HasFocusOnClickConfig', which this mirrors.
class HasContentConfig e msg cfg | cfg -> e msg where
  configureContent :: UI e msg () -> cfg
  extractContent :: cfg -> Maybe (UI e msg ())

-- | Sets the content rendered inside this control's chrome. Only ever
-- called from inside a widget's own module -- see 'HasContentConfig'.
content :: HasContentConfig e msg cfg => UI e msg () -> cfg
content = configureContent

-- | Implemented by any attrs type that carries displayed text, letting
-- 'text' work uniformly across every widget that has some (a caption for a
-- label\/button, the current value for a text input). Standalone -- not
-- folded into 'translateCommon', since "Blink.Control" itself has no text
-- capability to translate onto.
class HasTextConfig cfg where
  configureText :: Text -> cfg
  extractText :: cfg -> Maybe Text

-- | Sets the text a control displays. Defaults to @\"\"@ when not given.
text :: HasTextConfig cfg => Text -> cfg
text = configureText

-- | 'Blink.Control'\'s own closed attrs type -- the shared
-- 'ControlProperties' and 'ElementEvents', plus 'focusOnClick'\/'content'
-- (unlike every other widget's own attrs type, which never gets those two).
data ControlAttrs e msg
  = ControlCommon (ControlProperties e)
  | ControlEvent (ElementEvents e msg)
  | ControlFocusOnClick (FocusOnClick e)
  | ControlContent (UI e msg ())

instance HasControlConfig e (ControlAttrs e msg) where
  configureControlCapability = ControlCommon
  extractControlCapability (ControlCommon c) = Just c
  extractControlCapability _ = Nothing

instance HasElementEvents e msg (ControlAttrs e msg) where
  configureElementEvent = ControlEvent
  extractElementEvent (ControlEvent c) = Just c
  extractElementEvent _ = Nothing

instance HasFocusOnClickConfig e (ControlAttrs e msg) where
  configureFocusOnClick = ControlFocusOnClick
  extractFocusOnClick (ControlFocusOnClick f) = Just f
  extractFocusOnClick _ = Nothing

instance HasContentConfig e msg (ControlAttrs e msg) where
  configureContent = ControlContent
  extractContent (ControlContent c) = Just c
  extractContent _ = Nothing

-- | Translates a 'ControlAttrs' down to "Blink.Element"'s own closed
-- 'ElementAttrs' -- @Nothing@ for every capability 'Blink.Element' has no
-- concept of ('isFocusable', 'style', 'focusOnClick', 'content', ...). The
-- mechanism 'control' uses to call 'Blink.Element.element' with a concrete
-- attrs list of the type it actually expects, the same way every widget's
-- own @toXControlAttr@ calls 'control' with one of 'ControlAttrs'.
toElementAttr :: ControlAttrs e msg -> Maybe (ElementAttrs e msg)
toElementAttr (ControlEvent c) = Just c
toElementAttr _               = Nothing

-- | Fires every 'onClicked' handler in @attrs@ directly, the same way a
-- real mouse click would -- for a widget (e.g. 'Blink.Button.buttonBase')
-- that needs to re-fire the same reactions from a different trigger (Enter
-- while focused) using the same, already-translated attrs list it passes
-- to 'control', rather than resolving its own handler list on the side.
fireOnClick :: [ControlAttrs e msg] -> UI e msg ()
fireOnClick = fireClick . mapMaybe toElementAttr

-- | Concatenates the 'Out's every handler in @hs@ produces for @a@, without
-- dispatching any of them -- for composing a resolved handler list into a
-- single reaction, e.g. building the 'Blink.Element' attrs 'control' passes
-- down from a resolved 'ControlConfig', or a widget's derived reaction that
-- itself becomes one more handler in another resolved list (see
-- 'Blink.Button.toggleBase').
combineHandlers :: [a -> [Out e msg]] -> a -> [Out e msg]
combineHandlers hs a = concatMap ($ a) hs

-- | Runs every handler in @hs@ against @a@, dispatching the resulting
-- 'Out's -- for firing a resolved handler list directly, outside
-- 'Blink.Element.fire' (e.g. a button's Enter-key activation, which isn't
-- itself a raw 'Blink.Element.ElementEvent').
runHandlers :: [a -> [Out e msg]] -> a -> UI e msg ()
runHandlers hs a = mapM_ dispatch (combineHandlers hs a)
  where
    dispatch (OutMsg msg) = emit msg
    dispatch (OutUi eff)  = emitUi eff

-- | Configuration shared by every control, resolved from a
-- @['ControlAttrs' e msg]@ by @resolveControlConfig@. Doesn't carry the raw
-- event reactions (@onClicked@ and friends) -- those fire straight off the
-- attrs list itself, via 'Blink.Element.fire'\/'Blink.Element.element'
-- (see 'control'), rather than being resolved here first.
data ControlConfig e msg = ControlConfig
  { ccIsFocusable              :: Bool
  , ccIsEnabled                :: Bool
  , ccStyleKey                 :: StyleKey e
  , ccTabNavigation             :: NavigationMode
  , ccIsArrowNavigationEnabled :: Bool
  , ccFocusOnClick             :: FocusOnClick e
  , ccContent                  :: UI e msg ()
  }

-- | Every field at its default: focusable, enabled, not a navigation
-- container, styled via an arbitrary placeholder key (always overridden --
-- every real caller of 'control' supplies its own via 'style'), taking
-- focus itself on click, and rendering nothing.
defaultControlConfig :: ControlConfig e msg
defaultControlConfig = ControlConfig
  { ccIsFocusable              = True
  , ccIsEnabled                = True
  , ccStyleKey                 = Class ""
  , ccTabNavigation             = Flatten
  , ccIsArrowNavigationEnabled = False
  , ccFocusOnClick             = FocusSelf
  , ccContent                  = pure ()
  }

-- | Resolves a @['ControlAttrs' e msg]@ by folding every entry over
-- 'defaultControlConfig' left to right -- a later attr setting the same
-- field (e.g. 'style') overrides an earlier one. 'ControlEvent' is a no-op
-- here -- 'control' fires those directly off the original attrs list
-- instead (see 'ControlConfig').
resolveControlConfig :: [ControlAttrs e msg] -> ControlConfig e msg
resolveControlConfig = foldl' apply defaultControlConfig
  where
    apply cc (ControlCommon cap)     = applyCapability cc cap
    apply cc (ControlFocusOnClick f) = cc { ccFocusOnClick = f }
    apply cc (ControlContent c)      = cc { ccContent = c }
    apply cc (ControlEvent _)        = cc

    applyCapability cc (ControlIsFocusable b)              = cc { ccIsFocusable = b }
    applyCapability cc (ControlIsEnabled b)                = cc { ccIsEnabled = b }
    applyCapability cc (ControlStyle k)                    = cc { ccStyleKey = k }
    applyCapability cc (ControlTabNavigation m)            = cc { ccTabNavigation = m }
    applyCapability cc (ControlIsArrowNavigationEnabled b) = cc { ccIsArrowNavigationEnabled = b }

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
styledElement eid styleKey body = do
  s <- getStyle eid styleKey
  r <- getBounds
  let bg          = insetRect (styleMargin s) r
      borderRect  = case styleBorderColour s of
                      Just _  -> insetRect (borderInsets (styleBorderEdges s)) bg
                      Nothing -> bg
      contentRect = insetRect (stylePadding s) borderRect
      inner       = withBounds contentRect $ clipToCurrent (withStyle s body)
  withBounds bg $
    withBackground (styleBackground s) $
    case styleBorderColour s of
      Just c  -> withBorder c (styleBorderEdges s) inner
      Nothing -> inner

-- | Whether a control is eligible to claim focus purely by rendering first
-- while nothing else holds it: opted into keyboard focus at all
-- (@ccIsFocusable@) and configured to take focus itself on click ('FocusSelf').
autoClaimsFocus :: Eq e => FocusOnClick e -> ControlConfig e msg -> Bool
autoClaimsFocus foc cc = ccIsFocusable cc && foc == FocusSelf

-- | 'True' when this element should take focus with nothing having asked
-- for it: opted into auto-claiming (per 'autoClaimsFocus'), nothing else is
-- currently focused, and the mouse isn't contested by another element's
-- drag.
canAutoClaim :: Ord e => e -> FocusOnClick e -> ControlConfig e msg -> UI e msg Bool
canAutoClaim eid foc cc = do
  nothingIsFocused <- isNothingFocused <$> getFocus
  uncontested      <- isMouseFreeFor eid
  pure (autoClaimsFocus foc cc && nothingIsFocused && uncontested)

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
-- its raw mouse\/keyboard\/focus events, and draws its chrome around its
-- content -- all configured entirely by @attrs@, resolved once via
-- @resolveControlConfig@. Hovering, clicking, and focus-claiming all
-- respect the same margin-inset hit area chrome resolution uses -- the
-- margin itself never counts as "on" the control.
--
-- When @ccTabNavigation@ is 'Contained' (and this control is itself a tab
-- stop -- see @withChildNavigation@), the content becomes a focus scope for
-- Tab\/Shift-Tab (and, if @ccIsArrowNavigationEnabled@, the arrow keys too);
-- Ctrl+Tab\/Ctrl+Shift+Tab always escape it, moving this control's own
-- slot to the next\/previous one at the enclosing level.
control :: Ord e => e -> [ControlAttrs e msg] -> UI e msg ()
control eid attrs = disableWhen (not (ccIsEnabled cc)) $ do
  wasFocused  <- isFocused eid
  currentScope <- getCurrentScope
  applySelfFocus
  unless opensScope (applyNavigationKeys wasFocused)
  midFocused <- isFocused eid
  fireFocusChange elementAttrs (focusTransition wasFocused midFocused)
  hitBounds <- marginInsetBounds styleKey
  withBounds hitBounds $
    (if opensScope then withoutKeyEvents reservedKeys else id) $
      element eid (elementAttrs ++ clickFocusReaction currentScope)
  styledElement eid styleKey (withChildNavigation (ccContent cc))
  -- A nested container gets first claim on Ctrl+Tab\/Ctrl+Shift+Tab (it
  -- consumes the key while the content above is still running, before this
  -- check ever sees it) -- only if nothing inside claims it does it
  -- escape this control's own slot instead. Checked again afterward
  -- because this can change @eid@'s own focus after @midFocused@ was
  -- already computed, which a second, equally-real transition this same
  -- frame deserves its own 'FocusGained'\/'FocusLost' report for.
  when opensScope $ do
    _ <- applyScopeEscape wasFocused
    nowFocused <- isFocused eid
    fireFocusChange elementAttrs (focusTransition midFocused nowFocused)
  -- Recorded last, after this control's own retreat (if any) has already
  -- read whatever the previous tab stop was -- otherwise a control would
  -- see its own, just-written entry instead of the one before it.
  when (ccIsFocusable cc) $ setPreviousTabStop eid
  where
    cc = resolveControlConfig attrs
    styleKey = ccStyleKey cc
    foc = ccFocusOnClick cc
    opensScope = ccIsFocusable cc && ccTabNavigation cc == Contained
    elementAttrs = mapMaybe toElementAttr attrs

    -- Immediate, not deferred: needed so that when several controls are
    -- simultaneously eligible, only the first one to render claims focus.
    applySelfFocus = whenEnabled $ do
      isRetaining <- isFocused eid
      auto        <- canAutoClaim eid foc cc
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
    -- 'onKeyPressed' on this control itself.
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
