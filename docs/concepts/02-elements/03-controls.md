# 3. The control primitive

Every ready-made widget in `Blink.UI.Controls` — button, checkbox, toggle,
radio button, slider, text input — needs the same handful of things:
focus management, tab navigation, hover/press styling, and a consistent
way to report mouse and keyboard activity. Rather than each widget
reimplementing all of that, they're all built from two shared layers on
top of [`Element` and the attribute mechanism](01-introduction.md):
`elementBase` and `controlBase`.

## `elementBase`: one report, computed once

`elementBase` watches whether the pointer is over the element, whether a
mouse button is pressed or released on it, what keys are typed while it
holds focus, and whether focus moves onto or off of it — then returns all
of that as one `ElementInteraction` value, firing the matching handler
from the widget's `ElementConfig` for each event, once, in one place,
after every flag has been computed:

```haskell
data ElementInteraction = ElementInteraction
  { eiHovered, eiHeld, eiFocused :: Bool
  , eiMouseEntered, eiMouseExited :: Bool
  , eiMouseDown, eiMouseUp, eiClicked :: Bool
  , eiFocusGained, eiFocusLost :: Bool
  , eiKeysPressed :: [KeyEvent]
  }
```

`eiClicked`'s exact trigger is configurable per widget via
`MouseActivation`: `ClickActivated` (the default) fires it on a release
that lands back within the element's bounds — drag off before releasing
and nothing happens, the conventional way to back out of a click.
`CaptureActivated` fires it on *any* release while the element still holds
mouse capture, even outside its bounds — needed for something like a
slider, whose value has already changed by the time the button comes up,
so the release should count regardless of where the pointer ends up.

## `controlBase`: focus and chrome on top of that

`controlBase` adds two things `elementBase` doesn't have an opinion on:

* **Focus management** — claiming focus on render when nothing else holds
  it, giving it up on Tab, handing it to the previous tab stop on
  Shift-Tab, and taking it on a mouse-down when the control is focusable.
* **Themed chrome** — background, border, and padding resolved from a
  `Blink.Style.StyleKey` and the element's current hover/press/focus
  state, drawn around whatever content the control's own config carries.

## A layer fires only what it originates

Handlers report activity as a list of `Out` values —
`OutMsg msg` (an application message, dispatched via `emit`) or `OutUi
eff` (a presentational effect, dispatched via `emitUi`) — the same two
flavours covered in
[Effects](../01-immediate-mode-api/04-effects.md). `post`/`postWith` are
the usual way to build one: `onActivated (post AddItem)` reports
`OutMsg AddItem` when the button's own activation fires.

`controlBase` follows one rule strictly: it never dispatches an *element*
event itself — only the handlers a widget's own config attached do that.
The one exception is self-focus on a mouse-down click-to-focus, and even
that isn't fired through the handler system: it's issued as a direct
`setFocus` call, immediately, for exactly the reason
[Focus](../01-immediate-mode-api/05-focus.md) explains — a click deciding
who gets focus this frame needs to be visible to whatever else runs later
in the same tree walk, which a queued handler dispatch couldn't guarantee.

## Where this leaves you

Every ready-made control in `Blink.UI.Controls` is `controlBase` (or
`elementBase` directly, for something simpler like `Blink.UI.Controls.Label`)
plus its own visual content and attributes — see each widget's own module
for what it adds on top of this shared foundation.
