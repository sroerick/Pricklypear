# One substrate tree

Status: design (roerick thread, 2026-11-10 evening). Three decisions, but they
are one sequence — each step makes the next smaller, so the order is load
bearing.

## The problem

The town has five or six places where an agent turn can happen: PP
session/corridor, erde, the `loop.py` factory driver, autolith, pi ralph loops,
and the saguaro bridge. Every seam between two half-harnesses is a place where
the record can disagree with itself. The 2026-11-10 bug harvest is all seam:

- ledger on one box, board on another;
- run rows that are seat-dependent (the same turn yields a different row
  depending on who ran it);
- comment rows without an author or a timestamp;
- verdicts scraped from stderr, because no one owns the verdict as data.

None of these are bugs in a harness. They are bugs in the **gaps** between
harnesses.

## The sequence

1. **Shared LLM library** (pure refactor, no visible behavior change).
   Can start now; does not wait for "smooth".
2. **Turn contract** (the factory run record moves to erde).
   Only possible once there is one wire to call through.
3. **PP + erde + tuna monorepo** (one tree, one build, one seam).
   Only possible once the turn has one contract.

Doing (2) before (1) means defining the turn over two wire shapes at once.
Doing (3) before (2) means merging trees whose record layers still disagree.

## Step 1 — shared LLM library

Lift the stateless wire layer out of the harnesses into one library. This is a
pure refactor: no user-visible behavior change, no grammar change, no prompt
change. Both erde and `image/lib/llm_client.ml` become callers.

**IN (these five, and only these five):**

- `wire.ml` — both request/response shapes, plus the usage-accounting
  asymmetry made explicit instead of rediscovered per caller: Anthropic bills
  cache **writes** and excludes the cache counters from its total; OpenAI hides
  cache in `prompt_tokens`. The library reports one normalized usage record
  (`input`, `output`, `cache_read`, `cache_write`) and each provider adapter
  states how it maps onto that record.
- `cache_strategy.ml` — cache as a **provider property**, not a caller
  decision. The caller says "stable prefix"; the strategy says whether the
  provider can honor it, where the breakpoint goes, and whether a write is
  billed.
- `segments.ml` — the prompt as an **append-only list ordered by volatility**:
  `framework -> tools -> claims -> project -> task -> scratch`. One canonical
  render. One stable prefix hash (the hash covers the volatile-order prefix
  only, so a scratch change never invalidates the cacheable head).
- **Provider routing** — cache affinity first, then cost. Retirements are named
  (a retired model is a routing table entry with a date, not a comment in a
  caller).
- **Redaction** — secrets scrubbed from every string that leaves the process:
  prompts, responses, tool output, error text, logs.

**OUT, deliberately:**

- `journal` — the record of what happened is not the wire's business.
- `grants` — authority is caller/session state; the wire never decides who may
  call.
- `identity` — the library is **stateless about identity**: the caller passes
  an opaque identity handle and the library only forwards it into accounting
  and redaction context. It never mints, validates, or stores one.

**Streaming: decided IN, as an optional sink.** `wire.ml` already parses both
providers' streaming encodings, so carving it out would duplicate the parser in
every caller. The library exposes `stream : (delta -> unit) option`; `None`
means a buffered call (the default, and the only shape that guarantees a
complete usage record), `Some sink` means deltas plus a final usage record at
end-of-stream. Streaming is never a second code path for prompt construction —
same segments, same canonical render.

Statelessness is the acceptance test for step 1: the library must build and
pass its tests with no database handle, no board, and no session in scope.

## Step 2 — turn contract

Once there is one wire, there is one turn. Define it once, in erde, and make
the factory run record a first-class object rather than a side effect of the
driver:

- **claims** — what the turn asserts, as claim kinds (see the claim-kind
  card), not prose in a log line.
- **leases** — who holds the card/tree region while the turn runs, with a
  stated expiry, so two harnesses cannot both believe they own the work.
- **evidence** — the structured probe results behind each claim; adopted from
  step evidence entries, so a verdict has something to point at.
- **verdicts** — the outcome as data (accept / reject / blocked, with a reason
  and a reviewer handle), never scraped from stderr.

The move is the point: **the factory run record moves to erde.** `loop.py`
stops being the place where run state lives and becomes a client of the same
surface PP and autolith use. Every author and timestamp is structural
(assigned at the boundary), which is what kills the authorless comment row and
the seat-dependent run row.

Acceptance for step 2: a turn run from *any* of the six entry points produces
the same record shape, with the same author/timestamp fields filled the same
way, and a verdict readable without parsing output text.

## Step 3 — PP + erde + tuna monorepo

Only now merge the trees. One repo, one build, one seam: the shared LLM library
from step 1 and the turn contract from step 2 are already the boundary, so the
merge has no half-harness seams left to paper over. Concretely: one `dune
build`, one CI lane, one set of provider credentials, and no vendored copy of
the wire layer in any subtree.

## What each step buys

| Step | Kills |
|------|-------|
| 1 — shared LLM lib | five prompt builders, four usage-accounting interpretations, per-caller redaction |
| 2 — turn contract | seat-dependent run rows, authorless comments, stderr verdicts, split ledger/board |
| 3 — monorepo | the vendored-wire drift, the two-CI-lane seam, the cross-box record split |

## Non-goals

- Not a new agent framework. Step 1 is a lift, not a rewrite; step 2 is a move,
  not a new protocol.
- Not a DB migration in step 1. The library holds no handle.
- Not a grammar or prompt churn. Segment order is a *refactor* of existing
  prompts; visible prompt diffs are the bug.
