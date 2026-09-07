# Element identity

A view is rebuilt from scratch every frame, so there is no persistent
`Button` object for Blink to recognize as "the same button" across frames.
Some things still need to be tracked as a specific control across frames:
which one has keyboard focus, where a scrollable list's scroll position
sits, whether the mouse was hovering it a moment ago.

Blink's answer is the element type, `e` — a value *you* define, typically
one constructor per interactive control:

```haskell
data MyElem = IncButton | DecButton | NameInput
  deriving (Eq, Ord)
```

Each frame, when a control renders, it says "I am `IncButton`", and Blink
uses that value as the key into its own bookkeeping: the capabilities
covered by the rest of this folder. Bounds do not persist across frames;
effects and focus do. Because `e` is just a value with `Eq`/`Ord`,
determining whether this is the same control as last frame is exactly
value equality, with no object identity, reconciliation, or diffing
involved.

Next: [section 3](03-bounds.md) covers the first of those capabilities.
Bounds are the odd one out: unlike focus and effects, they don't persist
across frames at all, which is worth seeing before the two that do.
