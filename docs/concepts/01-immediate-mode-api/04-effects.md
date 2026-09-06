# 4. Effects

Section 1 established that Blink's view is a pure function of state, with
nothing retained to mutate. So how does clicking a button ever change
anything?

It doesn't — not directly. A control never mutates state itself. Instead,
it queues a description of a change — an **effect** — for something else
to apply once this frame's tree walk is finished. There are two flavours
of this in `Blink.UI`, aimed at two different owners:

* **`emit`** queues a `msg`: a value describing *what happened*, applied
  to your *application* state by the host's `update` function.
* **`emitUi`** queues a `UiEffect`: a change to *Blink's own*
  presentational state (scroll position, text selection), applied by
  `nextFrameContext`.

Both share the same shape — describe the change now, apply it later, never
mutate directly — just with different appliers and different timing for
when "later" is.

## `emit`: reporting application-level change

Your `update` function is the only thing that turns a `msg` into a new
application state, and it runs once per message, after the view has
finished for the frame:

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

If more than one message is queued in a single frame (two clicks landed
the same frame, say), `update` runs once per message, in the order they
were emitted — never batched or reordered. See
[`../03-runtime/02-application-and-backend.md`](../03-runtime/02-application-and-backend.md)
for the full `Update` monad this runs in.

## `emitUi`: reporting a presentational change

Scroll position and text selection work the same way, but the queue is
Blink's own, and the applier is `nextFrameContext` rather than your host's
`update`:

```haskell
emitUi :: UiEffect e -> UI e msg ()
```

A write made partway through a frame with `emitUi` is not visible to a
read later in that *same* frame — it only takes effect starting the
*next* one, once `nextFrameContext` has applied it. That's a strictly
easier model to reason about than trying to make a mid-frame write visible
immediately, and correctness doesn't depend on same-frame visibility here:
nothing else in the tree is contending for a particular list's own scroll
offset the way two sibling controls might contend for focus.

## The one thing that doesn't queue

Not every piece of Blink's own state works this way. Focus is the
exception — `setFocus`/`clearFocus` apply immediately, mid-frame, rather
than queuing like `emitUi` does. [Section 5](05-focus.md) is entirely
about why that exception exists, and why it can't be queued the way
everything else in this section is.
