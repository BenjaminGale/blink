# Building a custom control

Everything so far has been building toward one thing: enough of a mental
model to write a control from scratch, rather than only composing the
ready-made ones in `Blink.Controls`. This is what you reach for when the
provided controls don't fit — a custom visualization that also needs to be
clickable, say.

Here's a minimal button, stripped down to bare primitives:

```haskell
miniButton :: Ord e => e -> Text -> View e msg Bool
miniButton eid label = do
  isHit <- isRegionHit
  when isHit $ registerMouseOver eid
  fillRect (if isHit then RGBA 0.3 0.3 0.3 1 else RGBA 0.2 0.2 0.2 1)
  drawText (RGBA 1 1 1 1) AlignCenter label
  released <- isButtonReleased
  pure (isHit && released)
```

Walking through it against the concepts covered so far:

* **`eid :: e`** — the element identity from
  [the identity concept](../concepts/01-immediate-mode-api/02-identity.md).
  This function doesn't own any persistent state itself; `eid` is how
  Blink's own bookkeeping (hover, focus, if this button used it) knows
  which control a given frame's hit-test result belongs to.
* **`isRegionHit`** reads the *current bounds* — the space this control was
  given by its layout parent (see
  [the bounds concept](../concepts/01-immediate-mode-api/03-bounds.md)) —
  and checks it against this frame's mouse position. It's a pure query
  against this frame's context; nothing is written yet.
* **`registerMouseOver eid`** is the first write. Per
  [the focus concept](../concepts/01-immediate-mode-api/05-focus.md)'s
  rule, this is a candidate for "does a sibling need to see this later in
  the same frame?" — and the answer is no, hover has no cross-element
  arbitration the way focus does — so it doesn't need to be immediate the
  way `setFocus` does. (It's still visible next frame via
  `wasMouseOverLastFrame`, which is enough for hover's purposes.)
* **`fillRect` / `drawText`** don't touch any persisted state at all —
  they just append draw commands for *this* frame, read back in step 3 of
  [the frame loop](../concepts/04-runtime/01-the-frame-loop.md).
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

  This is
  [the effects concept](../concepts/01-immediate-mode-api/04-effects.md)'s
  `emit` step, just deferred to the call site instead of baked into
  `miniButton` itself — which is what lets the same button shape be reused
  for any message type.

## Adding focus

None of the above touches focus. Adding "Enter activates this button when
it's focused" means pulling in the immediate primitives from
[the focus concept](../concepts/01-immediate-mode-api/05-focus.md):

```haskell
miniButton :: Ord e => e -> Text -> View e msg Bool
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
worked through in
[the focus concept](../concepts/01-immediate-mode-api/05-focus.md): if
this button and a sibling both react to the same click in the same frame,
the sibling's `isFocused` check needs to see this button's claim right
away, not one frame late.

This is still far short of what `Blink.Controls.control` actually
provides — Tab/Shift-Tab navigation, themed style resolution, disabled
state — see
[the control-primitive concept](../concepts/02-elements/03-controls.md)
for what that adds. Reach for `Blink.Controls.control` once you need
those; reach for these primitives directly only when a custom control's
shape doesn't fit that abstraction.

## Where to go from here

That draws on the whole conceptual model: an immediate-mode view rebuilt
every frame
([why immediate mode](../concepts/01-immediate-mode-api/01-introduction.md)),
identified so bookkeeping can be keyed by control
([identity](../concepts/01-immediate-mode-api/02-identity.md)), reporting
change through effects rather than mutation
([effects](../concepts/01-immediate-mode-api/04-effects.md)), with a
deliberate exception for anything contended between siblings
([focus](../concepts/01-immediate-mode-api/05-focus.md)), threaded through
a small persistent context across a three-step loop
([the frame loop](../concepts/04-runtime/01-the-frame-loop.md)). See
[the `Update` monad](../concepts/03-app-state/01-introduction.md) for how
those messages become a new application state, and
[`App` and the backend pipeline](../concepts/04-runtime/03-application-and-backend.md)
for how a real backend drives the whole thing.

From here, the Haddocks — starting from the `Blink` module — are reference
material for the exact primitives available in each area, now with the
model in hand to read them against.
