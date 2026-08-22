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

### Clamped (non-wrapping) arrow-key navigation for a future list widget
Depends on: Scope-aware container support (implemented; see `ccTabNavigation`/`ccArrowNavigation` on `Blink.Attributes.ControlConfig`)

`control`'s `Contained` arrow-key navigation always wraps (same mechanism
as Tab), since it works purely off render order with no notion of "am I
the first/last item" — deliberately, to avoid tracking extra state the
core mechanism doesn't otherwise need. The old `Blink.Controls.radioGroup`
convention was to clamp instead ("Down clamps at the last item instead of
wrapping"). A future data-driven list\/selection widget built on top of
`Contained` would have its own real item index to check against, so
clamp-vs-wrap belongs there as a property of that widget's own selection
logic, not as something the generic container mechanism needs to grow
position-awareness to support.

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
Depends on: Self-sizing layout via an Element type

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
Building on the `enabled` attribute every widget in the new `Blink.Control`
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

### Intrinsic control sizing
A control's on-screen size currently comes entirely from the layout slot
it's placed in, not from its own content — there's no general "size = margin
+ border + padding + content" calculation a control can opt into. This
matters most once controls can hold richer content (see above): text needs
to be measured to size a control around it, and non-text content needs its
own measurement approach entirely.

## Styling

### Control classes
Every control should belong to a class for its kind (e.g. all buttons share
the "Button" class), so the library can ship sensible default styling for
each kind of control without a theme having to style every element
individually. This is a prerequisite for style classes below — user-defined
classes only make sense once controls already have a notion of "what kind of
thing am I" to attach to.

### Style classes
Depends on: Control classes

Beyond the built-in per-kind styling above, there's no way for a theme to
apply a shared, user-defined style to a set of controls by class — reusing
one look across several elements means repeating that style for each one
individually. A style attribute, settable per control instance, would let a
theme define a style once and apply it wherever it's needed, overriding the
control's class-level default.

This gives three levels a control's style can come from, in order of
precedence: a style attribute set directly on that control instance, its
control class's default, and — for elements that aren't controls and so have
no class to fall back on — an explicit style keyed to that specific element.

### Built-in default styles per control
Depends on: Control classes

Once every control has a class to hang a default style off, the library
itself should ship a sensible default style for each one, so a theme that
sets nothing still gets a coherent, usable look out of the box rather than
unstyled controls. Today getting anything to look right requires a theme to
style every control explicitly.

### Pseudo styles for control sub-structures
Depends on: Control classes

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

## Layout

### Self-sizing layout via an Element type
Depends on: Intrinsic control sizing

Controls and panels currently only know the size they're given by their
parent — there's no way for either to report back how big they'd like to be,
so a caller who wants a panel to fit itself around its content, rather than
stretch to fill, has to size that content's slot by hand ahead of time (e.g.
measuring a button's chrome and text separately).

The idea is to have every control and panel return, alongside the action
that draws it, a description of how it wants to be sized:

```
data Element e msg = Element
  { elementLayout :: ...          -- how big this wants to be
  , elementAction :: UI e msg ()  -- how it draws
  }
```

Panels would then compute their own size from their children's declared
sizes — e.g. a vertical panel's height being the sum of its children's
heights — instead of requiring the caller to state it up front, and this
composes: a panel built from self-sizing children becomes self-sizing
itself.

One shape doesn't cover everything, though. Some content — wrapped text
being the obvious case — can't declare a fixed size ahead of time, because
its height depends on the width it ends up being given, which is only known
once layout is actually being resolved. Supporting that means letting
`elementLayout` be a measurement taken during layout rather than a value
stated upfront, and keeping that measurement strictly read-only (no drawing
or interaction side effects).

That points to `elementLayout` living in its own lightweight monad for
layout queries — something like `LayoutM e Layout`, offering only
bounds/text-measurement access rather than the full range of things a `UI`
action can do (drawing, emitting messages, mutating interaction state).
Kept as its own type, separate from `UI`, but with a way to run one inside
the other wherever a panel needs to resolve a child's size as part of its
own `UI` action.

## Architecture

### Split `Blink.UI` into topic modules; move control-building code under `Blink.Controls.*`
`Blink.UI` bundles several disjoint concerns that only live together because
they all need `UIContext`'s internals — the monad itself, mouse/button
accessors, focus/scope handling, scroll/selection, drawing primitives,
animation, text measurement. The size of the file is a symptom of that, not
just a big-file smell.

The idea: an internal `Blink.UI.Context` (or similar) holding `UI`/
`UIContext`/the raw `gets`/`modify`, with topic modules (`Blink.UI.Mouse`,
`Blink.UI.Focus`, `Blink.UI.Scroll`, `Blink.UI.Drawing`, `Blink.UI.Animation`,
...) importing it for context access, and `Blink.UI` itself becoming a thin
re-exporting shell — the same shape the top-level `Blink` module guide
already has.

Separately, move the control-building layer under a `Blink.Controls.*`
namespace: today's `Blink.Element` → `Blink.Controls.Element`,
`Blink.Attributes` → `Blink.Controls.Attributes`, `Blink.Control` →
`Blink.Controls.Control`. Once that shared core is cleanly factored out,
splitting the ready-made widgets one-per-module (`Blink.Controls.Button`,
`.Checkbox`, ...) becomes more justified than it would be today, since each
would be thin and self-contained rather than fighting over shared machinery.

Treat as its own dedicated, mostly-mechanical pass (bounded but real risk —
re-checking import cycles across the split) rather than interleaving with
feature work. New foundation modules are being kept flat for now (e.g.
`Blink.Control`, singular, not `Blink.Controls.Control`) to avoid a rename
thrash before this happens.

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
