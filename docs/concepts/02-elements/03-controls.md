# The control primitive

Every ready-made widget in `Blink.View.Controls` — button, checkbox,
toggle, radio button, slider, text input — requires the same handling:
focus management, tab navigation, hover/press styling, and reporting of
mouse and keyboard activity. This is implemented once, in `control`, on
top of [`Element` and the attribute mechanism](01-introduction.md), rather
than being duplicated in each widget.

## The `control` function

`control` takes a `ControlConfig` — a widget's identity, style key, focus
policy, and its `onClicked`/`onFocusGained`/etc. handlers — and returns a
`ControlInteraction`, resolving hover, press, focus, and key-event state in
a single pass:

```haskell
data ControlInteraction e msg = ControlInteraction
  { ciHovered, ciHeld, ciFocused :: Bool
  , ciMouseEntered, ciMouseExited :: Bool
  , ciMouseDown, ciMouseUp, ciClicked :: Bool
  , ciFocusGained, ciFocusLost :: Bool
  , ciWasDragging :: Bool
  , ciKeysPressed :: [KeyEvent]
  , ciDisabled :: Bool
  }
```

`ciWasDragging` records whether the control already held mouse capture
before the current frame began, distinguishing a continuing drag from a
capture acquired this frame; this cannot be derived from a live read of
capture state within the same frame.

`ciClicked` fires according to the widget's `MouseActivation` setting.
`ClickActivated` (the default) requires the release to land within the
control's bounds. `CaptureActivated` fires on any release while the
control still holds mouse capture, regardless of pointer position —
required for a control such as a slider, whose value has already changed
by the time the button is released.

## Focus and chrome

`control` claims focus on render when nothing else holds it, releases it
on Tab, transfers it to the previous tab stop on Shift-Tab, and takes it on
mouse-down when `ccFocusPolicy` marks the control as focusable. Themed
chrome — background, border, padding — is resolved from `ccStyleKey` and
the control's current hover/press/focus state.

## Handler dispatch

Handlers report activity as `Out` values: `OutMsg msg`, an application
message dispatched via `emit`, or `OutUi eff`, a presentational effect
dispatched via `emitUi` (see [Effects](../01-immediate-mode-api/04-effects.md)).
`post`/`postWith` construct these — `onActivated (post AddItem)` reports
`OutMsg AddItem` on click.

Self-focus on a mouse-down click-to-focus is the one exception: it is not
routed through a handler, but applied as a direct `setFocus` call at the
moment of the click, for the reason given in
[Focus](../01-immediate-mode-api/05-focus.md) — a same-frame focus decision
must be visible to controls rendered later in the same tree walk, which a
queued handler dispatch cannot guarantee.

## Summary

Every ready-made control in `Blink.View.Controls` calls `control` — or, for
a control with no focus or chrome of its own, such as
`Blink.View.Controls.Label`, resolves hover/click state directly — plus its
own visual content and attributes. See each widget's module for what it
adds on top of this shared foundation.
