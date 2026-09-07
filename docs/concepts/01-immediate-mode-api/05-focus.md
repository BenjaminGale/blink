# Focus

Keyboard focus is presentation state Blink tracks on the application's
behalf: which single element, if any, currently receives key events. It is
not a flat value shared by the whole view. Root has its own focus state,
and a composite control (a list, a tree — anything with its own
distinctly-identified children) can own a separate, nested focus state of
its own. At any point in the tree walk exactly one of these states is
*ambient*: the one `isFocused`, `setFocus`, and `clearFocus` read and write.

There are two distinct ways focus changes, depending on who is deciding:

* A control can claim or give up focus **for itself** — auto-claiming on
  render when nothing else holds it, giving it up on Tab. This uses the
  immediate primitives, `setFocus` and `clearFocus`.
* Something can redirect focus **to a different, specific element** — a
  click on a proxy that focuses another control, Tab handing off to a
  known next stop. This uses an effect, `requestFocus`/`requestClearFocus`,
  queued like any other change described in [Effects](04-effects.md).

The rest of this section covers each in turn, then how a composite gets
its own nested scope for its children.

## Claiming focus for yourself: an immediate change

Say two buttons render one after another, and pressing Tab on the first
should move focus to the second. Both run in the same frame, in the order
the tree is walked — `buttonA` first, then `buttonB`.

`buttonA`'s logic is roughly "if I'm focused and Tab was pressed, give up
focus." `buttonB`'s logic is roughly "if nothing is focused, take it."
Both checks run in the same frame, moments apart. If focus changes were
queued the way effects are — applied only on the next frame — `buttonB`'s
check would still see `buttonA` as focused, because the frame it reads
from is the one before `buttonA`'s change takes effect:

```
  buttonA (frame N)                          buttonB (frame N)

  |                                          |
  | Tab pressed                              |
  | calls clearFocus                         |
  | (queued, not applied                     |
  | until next frame)                        |
  |                                          |
  |-------------- same frame ---------------->
  |                                          | isFocused? reads
  |                                          | current focus: buttonA
  |                                          | (stale -- clearFocus
  |                                          | hasn't applied yet)
  |                                          | still focused, so
  |                                          | does nothing

------------------------ next frame boundary -------------------------

  queued clearFocus finally applies: focus := Nothing
  ...one frame too late for buttonB to have seen it
```

Tab silently does nothing. `buttonA` genuinely gave up focus, just one
frame later than `buttonB` needed to see it.

`setFocus` and `clearFocus` avoid this by mutating the ambient scope's
focus state the moment they are called. Because the tree walk is
sequential, `buttonB`'s check, running moments later in the same frame,
sees the result of `buttonA`'s change immediately:

```
  buttonA                                    buttonB

  |                                          |
  | Tab pressed                              |
  | calls clearFocus                         |
  | focus := Nothing                         |
  | (applied immediately,                    |
  | same frame)                              |
  |                                          |
  |---- tree walk continues, same frame ----->
  |                                          |
  |                                          | isFocused? reads
  |                                          | current focus: Nothing
  |                                          |
  |                                          | nothing focused,
  |                                          | so setFocus buttonB
  |                                          | focus := buttonB
  |                                          |
  v                                          v
```

`setFocus` is refused, leaving the current claim untouched, if a different
element already holds focus this frame — a control can never steal focus
away from whoever legitimately has it, regardless of render order. Mouse
capture works the same way and for the same reason: an element deciding
"should I take the mouse?" needs to see what the previous sibling in this
same frame just decided, or two overlapping elements could both believe
they have it.

## Redirecting focus to another element: a queued effect

Not every focus change is a control acting on itself. Sometimes something
decides that a *different, specific* element should become focused: a
click lands on a label whose target is a separate input; Tab, once it
reaches a boundary, needs to hand off to a specific known next stop. These
go through `requestFocus`/`requestClearFocus` instead, which queue a
change applied at the next frame boundary, the same way `emit` and
`emitUi` queue theirs.

This is deliberately not immediate, unlike a control claiming focus for
itself. The two situations differ in what has to happen for correctness:
a same-frame self-claim needs a sibling rendered moments later in the
*same* walk to see it, so a stale intermediate state is never observed
mid-frame. A redirect instead needs every affected element — the element
gaining focus and the one losing it — to observe the change *consistently*,
regardless of which one happens to render first. Queuing and applying the
change atomically at the frame boundary gives every element the same
answer; an element can check whether it was the one just granted focus, or
the one just displaced, with `hasGainedFocus`/`hasLostFocus`, both true for
the two frames following the change so no render order can miss it.

The element on the receiving end of a grant can also refuse it: `disclaimFocus`
rejects a redirect that just landed on the current element and restores
whoever held focus before, as if the grant had never happened — for a
control that has a reason, discovered only after the redirect was queued,
to reject becoming focused (having gone disabled in the meantime, say).

## Focus scopes: nested focus for composites

A composite control — a list, a tree, anything with its own
distinctly-identified children — owns a focus state of its own, separate
from whichever scope was ambient outside it. `withFocusScope` is what
swaps the ambient scope to a composite's own while its children render,
then folds the result back into the composite's persisted state once they
finish; without an enclosing `withFocusScope`, "ambient" is always root's.

This gives composites two things for free:

* Checking whether the composite's *own* id is focused doubles as
  checking whether *any descendant* is focused — CSS's `:focus-within` —
  because `withFocusScope` is exactly what makes the composite's own id
  read as ambiently focused whenever a descendant is. No separate
  child-by-child check is needed.
* The composite's children auto-claim, give up, and receive Tab/Shift-Tab
  exactly like any other control, because they are simply running against
  their own scope's ambient focus state the same way root-level controls
  run against root's.

A composite still decides what Tab does once focus reaches it: continue
straight through its children in render order, as if the composite itself
weren't there, or stay contained within them under the composite's own key
scheme until something deliberately leaves it. Either way, the scope keeps
track of which child was focused independently of the enclosing scope, so
returning to a composite (via Tab, or a click) can restore where focus was
left rather than always starting over.

A disabled composite can never appear focused, even vacuously (the
composite claimed, but no particular child selected): while disabled, its
children run against the ambient scope completely unaffected, so the
composite neither claims focus for itself nor leaves a stale claim from
before it became disabled.

## The practical rule

When writing a custom control:

* If a value being set needs to be visible to another element **later in
  the same frame** — anything involving arbitration between siblings, like
  focus claimed for yourself or mouse capture — use the immediate
  primitives (`setFocus`, `clearFocus`).
* If it is redirecting focus to a **different, specific** element rather
  than claiming it for the current one, use the queued effect
  (`requestFocus`/`requestClearFocus`).
* If it is local, single-owner state that nothing else reads — like a
  list's own scroll offset — queue it with `emitUi` instead.

That's everything `Blink.View` itself provides: identity, bounds, effects,
and focus. See
[`../02-elements/01-introduction.md`](../02-elements/01-introduction.md)
for what's built on top of these four capabilities, and
[`../../guides/building-a-custom-control.md`](../../guides/building-a-custom-control.md)
for a worked example that puts them together to hand-write a minimal
button.
