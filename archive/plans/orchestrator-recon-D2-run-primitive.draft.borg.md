;; DRAFT v2 - gregor review 2026-10-16: RATIFY (decisions folded below). Not yet registered in pricklypear.borg; phase 1 registration lands TOGETHER with the workstreams obituary supersession, in one commit.
;; Deliverable D2 of the 2026-10-16 recon handoff (roerick -> gregor).

(project pricklypear-run-primitive
  (doc "Pricklypear book chapter (DRAFT 2026-10-16): the run primitive - durable, chunked, resumable execution rows for the PP orchestrator. One run = one attempt to advance one card one gate, executed by a SUPERVISED SIDECAR RUNNER outside the PP runtime process, with state cheap enough to abandon and expensive enough to resume. The supervisor (a glochid app) owns claims, budgets, transitions, and evidence; PP rows are the queue; the LLM loop never runs inside the PP runtime process.")
  (status planned)
    (* agent note (|PROVENANCE: drafted 2026-10-16 from the recon handoff (docs/handoffs/orchestrator-recon-D1-findings.md). GREGOR REVIEW 2026-10-16: RATIFY - decisions folded below; review points map onto the sections. OBITUARY RULE (review point 1, binding): the 2026-09-26 sessions-as-runs note in borge/workstreams.borg ('no data.u.<user>.run.* rows ever exist') MUST be superseded for sidecar runs IN THE SAME COMMIT that registers this chapter - never ratified later; until then the book contradicts itself and a future obedient agent sides with the stale line (the silent-fail class). The session stays the TRANSCRIPT record (local JSONL, never tree rows); card-feed milestones + terminal markers remain the human-facing record; this supersedes the 09-26 stance FOR SIDECAR RUNS ONLY. ALSO RATIFIED (review points 2-4): storage = shared-space datum + single supervisor + ONE CAS on the claim transition; usage from executor terminal output is a SELF-REPORT - the budget gate is ADVISORY until provider-structured usage lands; POLICY-2 backoff = the jobs shape (2^n cap 60s); POLICY-3 keeps the NO-VERDICT fail-safe verbatim; concurrency default 2 until the overnight soak passes.|) *)

  (section run-row
      (doc "STORAGE (RATIFIED, gregor review 2026-10-16): a typed datum in a SHARED space (data.s.run.*) keyed by run id - every identity can read the queue, no branch lens can fork it. A dedicated kernel table (the jobs shape, migrations/015_jobs.sql) stays the recorded alternative (jobs.borg:64-91 records when that trade IS right). CLAIM ATOMICITY (review point 2, one line now, not when a second supervisor class appears): the queued -> claimed transition is a COMPARE-AND-SET guarded set - flip only a row still queued with no live claim; the reap-takeover in resume-semantics 3 is ALREADY a two-supervisor moment (a booting supervisor racing one that is stalled, not dead). Write-cost discipline: few bounded rows, text fields capped, transcripts NEVER in rows (2026-09-08 incident). CHEAP LEASE, EXPENSIVE STATE: a runner that dies on wall-clock timeout leaves resumable state in its row.

FIELDS (the contract gregor reviews):
  id          uuid. The run's identity everywhere (evidence, logs, archives).
  card_id     uuid. Linkage only; the card feed stays the human-facing record.
  list_id     uuid. The workstream the card rides.
  from_state  The column the run claimed.
  dest_state  The column this gate advances to (validated against the card's real columns, pp-flow.sh:406-415; empty = one-column advance).
  brief_hash  Hash of the rendered todo/flow-brief at claim time (reproducibility; re-render only on explicit re-brief).
  state       queued | claimed | running | succeeded | failed | timed_out | canceled | dead | parked
    claim       {supervisor_id, lease_ts, heartbeat_at, ttl_s}. ONE supervisor holds it; heartbeat_at bumps every tick while a child lives. Reap rule: heartbeat older than ttl means the supervisor died mid-run; a booting supervisor TAKES THE ROW OVER (the reap_dead pattern, image/lib/jobs.ml:335-347). The queued -> claimed flip is a guarded set (CAS): state must still be queued with no live claim at write time (review point 2).
  runner      {kind: pp-flow | native, instance_id, identity}. instance_id distinguishes sidecar processes; identity is the runner's OWN identity, never roerick's.
  steps       Ordered list. Each: {name, kind: provider | prim | hand, state: pending | running | done | failed | timed_out | skipped, attempts, max_attempts, wall_budget_s, usage: {requests, input_tokens, cached_tokens, output_tokens, wall_s}, evidence_ref, error}.
  budget      {max_total_tokens, max_provider_requests, max_run_wall_s}. Per-step wall lives in each step.
  usage       Accumulated totals, parsed ONLY from executor terminal output - the one place token counts exist today (pp-flow-autolith.sh:223-226 parses :INPUT-TOKENS/:OUTPUT-TOKENS/:PROVIDER-REQUESTS and then drops them; this row is where they land).
  evidence    List of {kind: observed | computed | cited | interpreted | asserted, ref, note}. Claim kinds per board card f526bace - the kind is STRUCTURAL, not asserted.
  error       Last failure message (machine-readable class + human text, the :CATEGORY discipline).
  timestamps  created_at, claimed_at, started_at, ended_at.
  acted_by    Per migrations/039_acted_by.sql PINNED semantics: author = owner, acted_by = the runner identity that performed each write.

  Transitions are owner-confined like jobs (jobs.borg:107-108): only the holding supervisor writes state; everyone else reads.")
    (status planned))

  (section lifecycle
    (doc "RUN: queued -> claimed (supervisor takes the lease) -> running (step 1 sidecar spawned) -> ... -> succeeded | failed | timed_out | canceled | dead | parked.
STEP: pending -> running -> done | failed | timed_out | skipped.
RULES:
  - failed WITH attempts remaining -> re-dispatch THE SAME STEP (never the whole run).
  - attempts exhausted -> dead. Terminal runs never auto-retry.
  - succeeded -> the supervisor performs the write-back: ONE batched habitat eval (card transition + evidence comment + lease release), the advance_batch/dest_batch discipline (scripts/pp-flow.sh:191-208).
  - parked -> a human gate or budget exhaustion. Parking posts a card comment; parked never auto-retries.
  - THE EXECUTOR NEVER TOUCHES THE BOARD. Sidecar output is scanned by the supervisor, which owns every card transition - this keeps the 2026-09-26 exclusive-ownership rule and the review-independence property (one gate = one run = one process; workstreams.borg:330-333).
  - A one-way door (prod flips, DNS, deletes) is never a runnable step: the run stages it, the run parks, the human gate is a real column.")
    (status planned))

  (section resume-semantics
    (doc "THE FAILURE PATHS - gregor's review leads here. Each names the mechanism and what review must ratify.

1. STEP DIES HALFWAY (sidecar crash): step -> failed with its partial evidence_ref; the supervisor re-dispatches THE SAME STEP. Idempotency contract per step kind: provider steps are naturally idempotent (a fresh LLM turn); prim/hand steps must be checkpoint-first or declared idempotent (the with-step rule: resume by checkpoint lookup, not code replay - jobs.borg:219). RATIFY: is per-step idempotency a declaration field or a convention?

2. RUNNER HANGS: wall budget per step enforced at SPAWN (the Io_eio.sh_capture pattern - child killed on timeout, image/lib/io_eio.ml:55-57); kill -> timed_out. TWO-LEVEL TIMEOUT SEMANTICS, deliberate: adapter-level :TIMED-OUT is terminal for THAT ATTEMPT (pp-flow-autolith.sh:147-148 - 'retrying it immediately is the same coin flip for the same cost'); the supervisor may still start a FRESH attempt later with a fresh wall, bounded by POLICY-2. RATIFY: max attempts per step, and whether supervisor-level timed_out is retryable.

3. MAYOR RESTARTS MID-PLAN: state lives in the row, not the mayor's memory (cheap lease, expensive state). A booting supervisor heartbeats its own lease, reaps rows whose claim.heartbeat_at expired (TAKES OVER, does not restart), and continues from the step pointer. Nothing replays from scratch. This takeover is exactly the two-supervisor moment the claim CAS guards (review point 2). RATIFY: supervisor lease TTL vs tick interval.

4. BUDGET EXHAUSTION: before dispatching any step the supervisor checks remaining budget vs the step's estimate; exhaustion -> failed(budget) + a card comment + parked. Visible, never silent (the no-verdict lesson generalized, pp-flow.sh:649-671). Usage accumulates ONLY from executor terminal summaries - a SELF-REPORT, so the budget gate is ADVISORY until provider-structured usage lands (review point 3: a crashed or lying executor reports zero). RATIFY: default budgets (needs pp-bench FINDINGS numbers - D4 Q7).

5. LEASE EXPIRY vs LONG STEPS: claim.heartbeat_at bumps every tick while the child process lives; ttl_s > max step wall_budget + tick interval + slack. A step that needs longer gets a bigger wall_budget - the lease rule is never ignored. Card-level visibility: the card's assignee mirrors the run claim (a run:<run-id> shape per the session-lease amendment, workstreams.borg:309-323) so humans see the claim on the board.

6. DETERMINISTIC vs TRANSIENT: the adapter's :CATEGORY rule becomes the step-level rule (pp-flow-autolith.sh:130-152): transient provider statuses (402/408/409/425/429/5xx) and crash-with-no-result retry; deterministic classes (:INVALID-*, :PROCESS-FAILURE, 400/401/403/404/422) fail ONCE - a retry can never burn a lease repeatedly on the same guaranteed failure.")
    (status planned))

  (section supervisor-tick
    (doc "WHAT THE SUPERVISOR DOES EACH TICK (the glochid app contract):
1. HEARTBEAT its own supervisor lease; reap run rows whose claim expired (take over or fail them).
2. REAP finished sidecars: parse the terminal output (status, :usage, verdict/destination/evidence), accumulate usage into the row, mark the step done/failed/timed_out.
3. ADVANCE: dispatch the next step; on run completion perform the write-back (ONE batched eval) or park.
4. FILL THE QUEUE: cards from todo/item/queue (ORDER owned by the prim, workstreams.borg:327-329) minus the eligibility filter (actionable states, excluded kinds, no live card lease, no open run row) -> create run rows (queued). [POLICY-6 hook: mayor ordering lives here, and nowhere else.]
5. DISPATCH: budget check -> claim (lease + heartbeat) -> spawn the sidecar (brief, per-step wall budget, identity env) under the concurrency cap.
6. PUBLISH: counts (runs by state, budget burn) for standup v2 to read; no new surface in phase 2.

The sidecar contract stays WHAT EXISTS at first: the supervisor invokes scripts/pp-flow.sh --run <card-id> as a child process with env; pp-flow's driver logic (claim/brief/write-back) moves INTO the supervisor; the adapter (model pinning, envelope, retries, evidence archive) stays the sidecar's body until the native runner replaces it (D3 phase 3).")
    (status planned))

  (section budget-accounting
      (doc "Budgets live on the run row (max_total_tokens, max_provider_requests, max_run_wall_s) and per step (wall_budget_s). Usage accumulates ONLY from executor terminal summaries - today that is the adapter's :usage parse (pp-flow-autolith.sh:223-226); phase 3's native runner reports prim/step counts. USAGE IS A SELF-REPORT (review point 3): a crashed or lying executor reports zero and the pre-dispatch check sails through - the budget gate is ADVISORY until provider-structured usage lands (Autolith already holds the real token counts in the provider response and drops them at the boundary). PP-side usage_events (docs/monitoring-spec.md:20-29) has NO token kinds and is unbuilt; if P1 lands, run-scoped kinds become a rollup target, not the enforcement source (D4 Q4). Enforcement is the supervisor's pre-dispatch check - no executor self-polices.")
    (status planned))

  (section evidence
    (doc "Evidence entries on the run row carry a CLAIM KIND (observed | computed | cited | interpreted | asserted) per board card f526bace, so a judge-sweep (the REVIEW gates) can dispatch a probe and say no for a stated reason. Phase 2: the kind is a CONVENTION (a required field on evidence entries). Phase 3+: enforcement at a claim verb boundary - 'a claim verb that REFUSES without a kind' (f526bace tier 2), never by overloading tree/put (f526bace). Card-feed comments remain the human-visible evidence (the EVIDENCE: contract, pp-flow.sh:417-433); the row is the machine-readable superset.")
    (status planned))

  (section identity
    (doc "ONE IDENTITY PER RUNNER, never roerick's. Phase 2: a revocable per-user Bearer minted via (issue-token ...) - the glochid bridge precedent (glochid/lib/habitat.ml:15-20: 'the laptop then holds a revocable credential, never the operator's password'). Phase 3+: tuna identities at bearer parity (libs/tuna/lib.pp:16, v1). Attribution follows migrations/039_acted_by.sql PINNED semantics (author = owner, acted_by = the actor; updates COALESCE-preserve) and the FED3 library law (the client never sets actor fields, libs/tuna/lib.pp:7-10). Allowlists default DENY-ALL; the runner's allowlist names exactly the prims it needs (todo read/write, comments) - the agent-native deny-list (checkout, forks/merges, promote, auth/session; agent-native.borg:44) is the floor. Keys and transcripts live in mode-600 env files, never in rows, logs, or chat.")
    (status planned))

  (section policy-todos
      (doc "NAMED POLICY DECISIONS - gregor review 2026-10-16 set three; the rest are roerick's at architecture review:
    POLICY-1 WORKER-LIFETIME: OPEN (roerick) - default per-step wall_budget_s and per-run max_run_wall_s.
    POLICY-2 RETRY-COUNTS: SET (gregor) - backoff = the jobs shape, 2^n capped 60s (image/lib/jobs.ml:274-290); per-step max_attempts still needs a number at make time.
    POLICY-3 REVIEW-GATES: SET (gregor) - the NO-VERDICT fail-safe toward the human survives VERBATIM (pp-flow.sh:649-671); which columns auto-run a review vs stop for a human stays roerick's call per stream.
    POLICY-4 CONCURRENCY-CAPS: default 2 parallel runs until the overnight soak passes (gregor, D4 Q9); per-workstream/per-identity caps roerick's.
    POLICY-5 IDENTITY-POLICY: OPEN (roerick) - per-runner vs per-run identities; rotation cadence; allowlist contents (tuna-as.sh = ~/bin/tuna-as.sh on vultr; rotation procedure on tuna-deriv/club).
    POLICY-6 PRIORITY: OPEN (roerick) - the mayor's rights over todo/item/queue order (reorder? propose-only?). The queue ORDER prim is already Nopales; the mayor's policy layer is the ONLY place ordering may be touched.")
    (status planned))

  (verify
    (test "a run row created queued, claimed, stepped, and parked survives a supervisor restart and is TAKEN OVER, not restarted"
          "a step killed at its wall budget leaves failed + evidence_ref and re-dispatches the SAME step"
          "budget exhaustion parks the run with a card comment and no further dispatch"
          "no run row ever contains a transcript or a credential"))
)