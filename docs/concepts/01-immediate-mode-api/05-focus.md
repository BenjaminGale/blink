# Focus

[Section 4](04-effects.md) covered Effects: describe a change now, let
something else apply it later. Focus is the one piece of Blink's own
state that breaks that rule — `setFocus`/`clearFocus` apply immediately,
mid-frame, rather than queuing. This section is about why.

## A race between two controls in the same frame

Say you have two buttons rendered one after another, and pressing Tab on
the first should move focus to the second. Both controls run in the same
frame, in the order the tree is walked — `buttonA` first, then `buttonB`.

`buttonA`'s logic is roughly "if I'm focused and Tab was pressed, give up
focus." `buttonB`'s logic is roughly "if nothing is focused, take it."
Both of those checks run **in the same frame**, moments apart. If focus
changes were queued the same way effects are — applied only on the *next*
frame — then `buttonB`'s check would still see `buttonA` as focused,
because the frame it's reading state from is the one *before* `buttonA`'s
change takes effect:

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

Tab silently does nothing. The keypress is consumed, `buttonA` genuinely
gave up focus — just one frame later than `buttonB` needed to see it.

## Focus changes apply immediately

`setFocus` and `clearFocus` mutate the ambient scope's focus state the
moment they're called, not on a queue. Because the tree walk is sequential,
`buttonB`'s check — running moments later in the *same* frame — sees the
result of `buttonA`'s change immediately:

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

Mouse capture works the same way and for the same reason: an element
deciding "should I take the mouse?" needs to see what the *previous*
sibling in this same frame just decided, or two overlapping elements could
both believe they have it.

## Effects do not have this problem

Contrast this with [Effects](04-effects.md): nothing else in the tree is
racing to claim "the" scroll offset of a particular list — only that
list's own state is at stake, and only *it* ever reads or writes it.
There's no sibling whose correctness this frame depends on seeing the
change, so there's nothing to race, which is exactly why effects can
afford to queue and focus can't.

## The practical rule

When writing a custom control:

* If a value you're setting needs to be visible to another element **later
  in the same frame** — anything involving arbitration between siblings,
  like focus or capture — use the immediate primitives (`setFocus`,
  `clearFocus`).
* If it's local, single-owner state that nothing else reads — like a
  list's own scroll offset — queue it with `emitUi` instead.

That's everything `Blink.View` itself provides: identity, bounds, effects,
and focus. See [`../02-elements/01-introduction.md`](../02-elements/01-introduction.md)
for what's built on top of these five capabilities, and
[`../../guides/building-a-custom-control.md`](../../guides/building-a-custom-control.md)
for a worked example that puts all of them together to hand-write a
minimal button.
