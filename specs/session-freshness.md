# Ticket: session freshness — durable route binds + env rehydration

**Priority: first item of the branching/rework phase 3.**
Found 2026-10-07 (the doasq face deploy, the 88f1364 cycle).

## The two halves, one root cause

Sessions are boot-snapshots plus per-session deltas. Two things break:

1. **Route binds are session state.** `(route ...)` writes a `view_bindings`
   row (the audit copy) and the *session's* dispatch map. New sessions start
   with empty maps: every restart, every user, every agent re-binds by hand.
   An agent's binds die with its session — agents cannot make durable binds.

2. **The env inherits the seed's holes.** The boot seed eval-binds defines
   *only if the name is absent from the store*. When a name exists (e.g. from
   an earlier hot-patch with a stale body), the seed skips it **including the
   env binding** — every new session has the name unbound even though the
   source row exists. Live example: `doasq/page` bound, `doasq/items` unbound,
   in the same boot.

## The fix

1. **Dispatch falls back to the store.** Route lookup: session map → miss →
   the branch's `view_bindings` rows. Session binds remain overrides. No
   migration; existing behavior unchanged for live sessions.
2. **The seed always eval-binds, persists-if-absent.** The persist check
   stays; the env bind becomes unconditional.
3. **Session create rehydrates from the store:** the branch's defines
   (re-eval of the source rows) + the routes (the binding rows). Then a
   restart is transparent: defines, routes, faces — everything just there.

## Acceptance

- Agent binds a route in a session; a *new* session (same user) resolves it.
- Deploy + restart: previously-bound routes still resolve, no re-bind step.
- A name with a stale store row re-binds its current source at session create.
- The delegation gate unchanged: the route prim still writes attributed rows;
  the approve-class prims stay roerick-only.

## Non-goals

Route sharing across branches (bindings stay per-branch); the watch/validate
tree ops; anything in the data-rows family.
