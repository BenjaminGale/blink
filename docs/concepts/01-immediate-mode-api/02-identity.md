# 2. Element identity

A view is rebuilt from scratch every frame — there's no persistent
`Button` object for Blink to recognize as "the same button" across frames.
But some things genuinely need to be tracked *as* a specific control across
frames: which one has keyboard focus, where a scrollable list's scroll
position sits, whether the mouse was hovering it a moment ago.

Blink's answer is the element type, `e` — a value *you* define, typically
one constructor per interactive control:

```haskell
data MyElem = IncButton | DecButton | NameInput
  deriving (Eq, Ord)
```

Each frame, when a control renders, it says "I am `IncButton`" and Blink
uses that value as the key into its own bookkeeping — the capabilities
covered by the rest of this folder (bounds don't persist across frames,
but effects and focus do). Because `e` is just a value with `Eq`/`Ord`,
"is this the same control as last frame" is exactly value equality — no
object identity, no reconciliation, no diffing.

Next: [section 3](03-bounds.md) covers the first of those capabilities —
though unlike focus and effects, bounds don't persist across frames at
all, which is worth seeing as a contrast before the two that do.
