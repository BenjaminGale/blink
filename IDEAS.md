# Ideas

Running list of feature ideas and improvements for Blink. Not a roadmap or a
commitment — just a place to capture things before they're forgotten.

Entries are grouped by category. Add new categories as needed; add new
entries to the top of their category's list.

## Format

```
### Title
Depends on: Other idea title (optional, only if this idea builds on another)

Description — what it is, why it'd help, or what it'd take.
```

---

## Controls

## Text Input
Add a placeholderText attribute which is displayed (grayed out) when the control
has no input.

Support delete key

### Icon element
An `icon` control, built the same way `label` is, that draws a single glyph
from an icon font (e.g. Material Symbols) instead of body text — a `name`
attribute selects the icon via a named key (e.g. `"delete"`, `"settings"`)
looked up in a `Map Text Char` built from the icon font's codepoints file,
with a sensible fallback glyph (e.g. a "missing"/`question_mark`-style icon)
when a name isn't found rather than rendering nothing.

This needs multi-font support first: today `DrawText`/`TextMeasurer` and the
SDL2 backend (`app/Rendering.hs`, `app/Main.hs`) all assume a single global
`Font.Font` loaded once at startup, so drawing from a second font (body vs.
icon) means threading a `FontKey` (or similar) through `DrawCommand`,
`TextMeasurer`, and the backend's font map before `icon` can render anything.

Also open: the full Material Symbols variable font is ~10.6MB (vs. 407KB for
the current Inter body font), since it's the entire icon set in one
variable-axis file. Subsetting it down to just the icons actually used would
need `fonttools`/`pyftsubset`, which isn't installed and has no package
manager available in the dev sandbox — so this needs either accepting the
10.6MB asset as-is, installing `fonttools` (sudo/apt), or subsetting the font
outside this environment before it's added to `assets/fonts/`.

### Label mnemonics
A label could name a mnemonic key (e.g. Alt+F) that redirects focus to its
`target` the same way clicking it already does, without requiring the label
itself to hold focus. This needs a way to observe a key combination
regardless of which element currently holds focus, and modifier keys beyond
`Shift` (e.g. `Alt`) plus letter/digit keys, neither of which `Blink.Input`'s
`Key`/`Modifier` types support yet.

### Label ellipsis
When a label's text is wider than its available space, truncate it and
append an ellipsis rather than clipping or overflowing. This needs text
measurement (to know when truncation is needed and how much text fits)
wired into rendering, which the library doesn't do anywhere yet.

### Wrapping label
A label control that wraps text across multiple lines to fit whatever width
it's given, rather than clipping or overflowing:

- Wraps to available width instead of clipping or overflowing
- Self-sizes its height from the wrapped line count, rather than requiring
  a caller-supplied height
- Per-line text alignment (left/center/right), consistent with the
  existing single-line `label`
- Optional max-line cap with ellipsis truncation on the last visible line
- Same style/theme resolution as the existing `label`, so it's a drop-in
  upgrade rather than a separate visual system
- Re-wraps automatically on resize, since wrap width comes from ambient
  layout bounds each frame rather than being cached

### Disable-aware panel control
Building on the `enabled` attribute every widget in the `Blink.View.Controls.Control`
stack already has: a higher-level panel control that, when disabled,
automatically disables its content — every child control nested inside it
— rather than requiring each child to be disabled individually. This would
make it easy to disable a whole section of a UI (e.g. a form) in one place.

### Shrink the public disabling API
Depends on: Disable-aware panel control

Once a disable-aware panel control exists to disable whole sections at
once, `disableWhen` may no longer need to be part of the public API —
individual controls' own `enabled` attribute plus that panel could cover
the cases it exists for today. This is speculative until that design
settles and we can see whether anything still needs `disableWhen` directly.

### Default and cancel buttons
Buttons only support their label text today — there's no way to mark a
button as the one that responds to Enter or Escape when nothing else has
focus (the way dialogs conventionally have a default "OK" button and a
cancel "Cancel" button).

This needs a notion of scope, since only one default and one cancel button
can be active at a time — presumably per top-level view or per modal — and
that concept doesn't exist anywhere in the control library yet. It would
also need some visual indication that a button fired via keypress rather
than a click, since buttons don't currently distinguish the two for styling
purposes.

### Richer borders
Borders today are a single flat-coloured stroke per side, with no texture.
Two extensions come up:

- 9-slice (9-patch) borders, so a border can be drawn from a bitmap with
  fixed corners and stretchable edges instead of a flat colour.
- Box-drawing character borders for terminal-style rendering, plus a
  bordered container that reserves space for a header label mid-border (a
  titled group box).

### Standard glyph set
Some controls draw special characters for their marks — a checkbox's tick,
a radio button's dot — chosen ad hoc wherever that control is implemented.
The library could instead provide a standard, overridable set of glyphs for
this kind of thing, so every control drawing this style of mark draws from
the same consistent set rather than each picking its own.

### Generic control content
Controls that show text (buttons, checkboxes, labels, ...) can currently
only take plain text as their content. There's an idea to generalize this
so a control's content can be either plain text or arbitrary composed UI —
allowing, for example, a button with an icon next to its label — without
needing a separate icon-plus-label variant of the API for every control.
This is the same idea as WPF's content model or JavaFX's `Labeled` control:
one content slot that accepts either text or an arbitrary child, rather than
a bespoke text-only field per control.

## Styling

### Pseudo styles for control sub-structures
A control class styles a control as a whole, but composite controls are
made of several visually distinct parts — a checkbox has a container, a box,
and a label, each of which may need its own default look. This extends the
control class idea downward: instead of a control only exposing one class to
style, it would expose a class per named sub-part, so a theme can target
"the box inside a checkbox" the same way it can already target "a checkbox."

## Geometry

### Stronger geometry types
Points, sizes, and rectangles are currently plain records with no protection
against mixing up their components — e.g. accidentally using a size's width
where a point's x was meant. Wrapping them in stronger, distinct types was
raised as a way to catch that kind of mistake at compile time. Worth
revisiting once it's clear which of these values actually get passed around
interchangeably enough in practice to cause real bugs.

### Non-negative insets
Insets support a uniform convenience constructor but not a zero one, and
nothing stops a caller from constructing a negative inset, which would
invert a rectangle rather than raising an error. Needs a decision on whether
applying an inset to a rectangle should clamp the result, or whether insets
should be constrained to non-negative values when they're constructed.

## Architecture

### Three-tier test architecture: Infrastructure / Frame rendering / App
The test suite has two implicit tiers today with no explicit boundary
between them: pure-function tests (geometry, input state machines, layout
math) sit alongside `View`-level behaviour tests. Both are driven through
`Blink.Interaction`, which advances one `nextFrameContext` per simulated
frame. That matches continuous mode's loop; it has no equivalent of
event-driven mode's second render pass at all.

```
Layer 1 — Infrastructure (pure data types and functions, no View/ViewContext)
├─ Geometry: rects, insets, alignment, containment, intersection
├─ Input state machines: button down/held/released/up; hover entered/over/exited
├─ Update monad: modify/put/gets sequencing
├─ Layout constraint math: preferredSize for Exactly/Fill/AtLeast/AtMost/Between
├─ Selection helpers: low/high/collapse/extend/cursor
├─ Scroll position clamping
└─ Hold/repeat cadence arithmetic (repeatsDueBy)

Layer 2 — Frame rendering (single View, one simulated frame at a time,
                            via Blink.Interaction; matches continuous mode)
├─ Per-element raw event contract: hover, press/click, keyboard, focus claim/lost
├─ Per-family shared contracts: button activation, toggle activation, fixed-focus
├─ Individual control behavior: rendering, styling, value semantics
├─ Multi-control scenarios within one View, still one frame at a time:
│   FocusScope Continue/Contained navigation, click-to-focus,
│   cross-element capture/hover suppression, tab order
├─ View monad primitives: bounds, clip, drawing, disabled state,
│   animation state, mouse-over memory
├─ Box/border layout distribution (hBox/vBox/borderLayout)
└─ Theme/style resolution

Layer 3 — App (the real Blink.App orchestration loop, both modes) — DOESN'T EXIST YET
├─ A control's reaction reaches app state via the real update fold,
│   in both continuous and event-driven mode
├─ A deferred effect settling on event-driven's second pass still
│   reaches state, exactly once (not lost, not duplicated)
├─ A fresh click/press isn't reprocessed by the second pass
├─ Draws match each mode's own contract: continuous may be one frame
│   stale (by design); event-driven never is
├─ Animation-ticker start/stop decision fires correctly off real
│   per-frame state (not the ticker thread itself)
└─ Multi-control/composite scenarios agree between the two modes on
    messages/state, diverging only where draw-timing is documented to
```

Layer 3 can't be reached by Layer 2's harness by construction, since
`Blink.Interaction` never runs event-driven's second render pass.
`Blink.App`'s own second pass already had exactly the kind of bug this
tier exists to catch (silently dropping, then separately duplicating,
messages) — no amount of Layer 2 testing could have caught it, at any
scenario complexity.

Needs a harness (alongside `Blink.Interaction`) that wraps a `View` action
in a minimal message-accumulating `App` and drives it through both
`configureContinuous` and `configureEventDriven`, asserting messages/state
agree between the two. Draws aren't expected to match exactly: continuous
mode is one frame stale by design, so a draws comparison needs to account
for that offset rather than requiring byte-identical output.

### `scrollIntoView`
No such function currently exists. The idea is a helper that, given a
scrollable container and where a target region sits within its content,
computes and emits the minimal scroll needed to bring that region into
view — for use by keyboard navigation inside scrollable containers (list
boxes, viewports) so that moving focus or selection off-screen scrolls to
follow it.

### Row edit mode for composite controls
A composite control's rows can already render arbitrary content, including
real interactive controls (an inline text field for rename, a checkbox, a
delete button) — but there's no navigation/edit distinction: arrow keys
would need to move a "current" row without touching real keyboard focus,
and an explicit action (Enter, double-click) would switch that row into an
edit mode where focus can then legitimately move onto a control inside it.
This is the same two-layer model as JavaFX's cell model (a cell renders
itself in a plain, non-editing state until an explicit edit trigger swaps in
a live editing control for that cell) — plain navigation by default, an
explicit step in and out of editing.

Open questions: what triggers entry/exit, whether edit mode applies per-row
or to the whole list at once, and how it interacts with selection (editing
and selecting are different actions on the same row).

### General-purpose control activation handlers
Controls currently hard-code their own activation logic (a fixed
click-or-key-list check). The alternative raised is an attribute a control
could be given, holding an ordered list of condition/action pairs, either
running the first match or every match. Open questions: handler order,
whether a handler can consume an input event so later handlers don't also
see it, and how that interacts with hit-testing bubbling from child to
parent.

## Misc

### Animated text cursor
The text input's cursor is currently drawn static; it should blink, but
only while the field is focused. This should be straightforward — the
animation-frame machinery it needs already exists and is used by the
indeterminate progress bar.

### Mouse cursor shape
There's currently no way for a control to request a specific mouse cursor
icon (e.g. an I-beam over text input, a resize cursor over a splitter).
This would need a per-frame "requested cursor" output, similar to the
existing animation-request output, for the backend to apply.
