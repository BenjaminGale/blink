# 5. Building a custom control

Everything so far has been building toward one thing: enough of a mental
model to write a control from scratch, rather than only composing the
ready-made ones in `Blink.Controls`. This is what you reach for when the
provided controls don't fit — a custom visualization that also needs to be
clickable, say.

Here's a minimal button, stripped down to bare primitives:

```haskell
miniButton :: Ord e => e -> Text -> UI e msg Bool
miniButton eid label = do
  isHit <- isRegionHit
  when isHit $ registerMouseOver eid
  fillRect (if isHit then RGBA 0.3 0.3 0.3 1 else RGBA 0.2 0.2 0.2 1)
  drawText (RGBA 1 1 1 1) AlignCenter label
  released <- isButtonReleased
  pure (isHit && released)
```

Walking through it against the previous four sections:

* **`eid :: e`** — the element identity from
  [section 3](03-elements-and-messages.md). This function doesn't own any
  persistent state itself; `eid` is how Blink's own bookkeeping (hover,
  focus, if this button used it) knows which control a given frame's
  hit-test result belongs to.
* **`isRegionHit`** reads the *current bounds* — the space this control was
  given by its layout parent (see `Blink.Layout`) — and checks it against
  this frame's mouse position. It's a pure query against this frame's
  context; nothing is written yet.
* **`registerMouseOver eid`** is the first write. Per
  [section 4](04-focus-and-timing.md)'s rule, this is a candidate for
  "does a sibling need to see this later in the same frame?" — and the
  answer is no, hover has no cross-element arbitration the way focus does
  — so it doesn't need to be immediate the way `setFocus` does. (It's
  still visible next frame via `wasMouseOverLastFrame`, which is enough
  for hover's purposes.)
* **`fillRect` / `drawText`** don't touch any persisted state at all —
  they just append draw commands for *this* frame, read back in step 3 of
  the frame loop ([section 2](02-the-frame-loop.md)).
* **`isButtonReleased`** reads this frame's input state — was the mouse
  button released this frame, full stop, with no element-specific
  targeting.
* **The return value**, `isHit && released`, is not itself a message —
  it's a plain `Bool` handed back to whatever composed `miniButton` into a
  larger view. Turning it into an actual state change is the caller's job:

  ```haskell
  clicked <- miniButton IncButton "+"
  when clicked (emit Incremented)
  ```

  This is [section 3](03-elements-and-messages.md)'s emit step, just
  deferred to the call site instead of baked into `miniButton` itself —
  which is what lets the same button shape be reused for any message type.

## Adding focus

None of the above touches focus. Adding "Enter activates this button when
it's focused" means pulling in the immediate primitives from
[section 4](04-focus-and-timing.md):

```haskell
miniButton :: Ord e => e -> Text -> UI e msg Bool
miniButton eid label = do
  isHit    <- isRegionHit
  released <- isButtonReleased
  when isHit $ do
    registerMouseOver eid
    when released (setFocus eid)          -- click to focus: immediate
  focused <- isFocused eid
  fillRect (borderColor focused isHit)
  drawText (RGBA 1 1 1 1) AlignCenter label
  enterPressed <- any (\e -> key e == KeyReturn && not (keyRepeat e))
                    . inputKeyEvents <$> getInput
  let activatedByKey = focused && enterPressed
  when activatedByKey (consumeKey KeyReturn)
  pure ((isHit && released) || activatedByKey)
```

`setFocus` is called immediately, not queued, for exactly the reason
worked through in section 4: if this button and a sibling both react to
the same click in the same frame, the sibling's `isFocused` check needs to
see this button's claim right away, not one frame late.

This is still far short of what `Blink.Controls.control` actually
provides — Tab/Shift-Tab navigation, themed style resolution, disabled
state — but it's built from the exact same primitives, just assembled by
hand instead of composed for you. Reach for `Blink.Controls.control` (see
its Haddocks) once you need those; reach for these primitives directly
only when a custom control's shape doesn't fit that abstraction.

## Where to go from here

That's the whole model: an immediate-mode view rebuilt every frame
([1](01-why-immediate-mode.md)), threaded through a small persistent
context across a three-step loop ([2](02-the-frame-loop.md)), reporting
change through messages rather than mutation
([3](03-elements-and-messages.md)), with a deliberate split between
immediate and queued state changes for anything contended between siblings
([4](04-focus-and-timing.md)).

From here, the Haddocks — starting from the `Blink` module — are reference
material for the exact primitives available in each area, now with the
model in hand to read them against.
