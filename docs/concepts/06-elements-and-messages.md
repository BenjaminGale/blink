# 6. Elements and messages

Section 1 established that Blink's view is a pure function of state, with
nothing retained to mutate. So how does clicking a button ever change
anything?

It doesn't — not directly. A control never touches your application state.
Instead, it **emits a message**: a value describing *what happened*, not
*what to do about it*. Your `update` function is the only thing that turns
a message into a new state, and it runs once per message, after the view
has finished for the frame.

```
+-----------------+       +-----------------+
|       view      |       |      update     |
|  (fn of state)  |-emit->| (msg -> s -> s) |
+-----------------+       +-----------------+
         ^                         |
         +-------------------------+
new state, folded in before next frame's view
```

A minimal counter's `msg` type and `update` might look like:

```haskell
data CounterMsg = Incremented | Decremented

update :: CounterMsg -> Update Int ()
update Incremented = modify (+1)
update Decremented = modify (subtract 1)
```

and the view emits one of those values on click:

```haskell
view :: Int -> Element MyElem CounterMsg
view count = vBox $ do
  label (Text.pack (show count))
  onClicked Incremented (button IncButton "+")
  onClicked Decremented (button DecButton "-")
```

Clicking `+` doesn't change `count`. It queues `Incremented`. `count`
changes on the *next* frame, when `update` runs against it and the view is
called again with the new value. This is why the view has no way to read
"the current value of a control" the way a retained-mode widget would —
there is no control object holding a value; there is only the state you
already have, and the messages that will produce the next state.

If more than one message is queued in a single frame (two clicks landed the
same frame, say), `update` runs once per message, in the order they were
emitted — never batched or reordered.

## Why controls need an identity you define

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
uses that value as the key into its own bookkeeping (focus, scroll,
hover) — the presentational state introduced in
[section 5](05-the-frame-loop.md) that survives across frames despite
nothing else doing so. Because `e` is just a value with `Eq`/`Ord`, "is
this the same control as last frame" is exactly value equality — no object
identity, no reconciliation, no diffing.

Next: [section 7](07-focus-and-timing.md) covers *when* changes to that
bookkeeping actually take effect — and why the answer isn't the same for
every kind of presentational state.
