# D4: open questions for roerick + gregor

Gregor's review leads with the failure paths (F1-F5). Each states the D2 answer and what review must ratify.

## Review outcome (gregor, 2026-10-16: RATIFY)

Decided: Q1 storage = shared-space datum (data.s.run.*) + single supervisor + ONE CAS on queued->claimed (the reap-takeover is already a two-supervisor moment - a booting supervisor racing a stalled one). Q2 the workstreams.borg obituary supersedes IN THE SAME COMMIT that registers the run-row chapter - never later. Q4 usage from terminal output is a SELF-REPORT: the budget gate is advisory until provider-structured usage lands (Autolith holds real token counts in provider responses and drops them at the boundary). Q5 tuna-as.sh = ~/bin/tuna-as.sh on vultr; rotation procedure on tuna-deriv/club. Q8 obsd-sec is NOT retired - gregor's schedule in the .rocks image; the recon probe hit town (wrong instance), hence []. Q9 concurrency = 2 until the overnight soak passes. Policy: POLICY-2 backoff = jobs shape (2^n cap 60s); POLICY-3 NO-VERDICT fail-safe verbatim.

Still open (roerick): Q3 (f526bace scope), Q6 (supervisor residence), Q7 (budget defaults - needs pp-bench FINDINGS), POLICY-1 (worker lifetime), POLICY-5 (identity policy), POLICY-6 (priority rights).

Hygiene (gregor, non-blocking): the recon probed prod with the admin pp.token Bearer - read-only this time; per-runner revocable Bearers are the right next move, the admin corridor is the wide door (ff87a7c0, ffe72ad8).
## Failure paths (review agenda)

- **F1 - step dies halfway:** re-dispatch THE SAME STEP, never the whole run; idempotency contract per step kind (provider steps naturally idempotent; prim/hand steps checkpoint-first, the with-step rule, jobs.borg:219). Ratify: idempotency as declaration field or convention.
- **F2 - runner hangs:** per-step wall budget enforced at spawn (child killed on timeout, the sh_capture pattern); two-level timeout semantics (adapter-level :TIMED-OUT is attempt-terminal, pp-flow-autolith.sh:147-148; supervisor-level timed_out may start a FRESH attempt). Ratify: POLICY-2 numbers (max attempts, backoff shape).
- **F3 - mayor restarts mid-plan:** state lives in the row, not the mayor's memory; lease + heartbeat; the booting supervisor reaps expired claims and TAKES OVER from the step pointer. Ratify: supervisor lease TTL vs tick interval.
- **F4 - budget exhaustion:** pre-dispatch check; fail VISIBLE (failed(budget) + card comment) + park, never silent. Usage source = executor terminal summaries only. Ratify: default budgets (needs pp-bench FINDINGS - Q7).
- **F5 - lease expiry vs long steps:** heartbeat bumps every tick while the child lives; ttl > max step wall + tick + slack; a longer step gets a bigger wall budget - the lease rule is never ignored. Ratify: TTL defaults.

## Questions

1. **Storage for run rows:** shared-space datum (data.s.run.*) vs dedicated kernel table (the jobs shape). Recommendation: datum + single leased supervisor; SKIP LOCKED (kernel table) only if a second supervisor class must be structurally safe.
2. **Obituary ratification:** supersede the 2026-09-26 "no run datum" stance for SIDECAR runs only (workstreams.borg sessions-as-runs note); the session stays the transcript record (local JSONL, never tree rows).
3. **f526bace scope:** the card is about EPISTEMIC claim kinds on the tree substrate (verifier/probe/judge-sweep), not card leases. Confirm run EVIDENCE adopts claim kinds (phase 2 convention, tier-2 claim verb later). Is a lease-kind dimension ALSO wanted, or is the connection evidence-side only?
4. **Usage mechanism:** run rows carry usage now (parsed from executor terminal output - the counts exist there today and are currently dropped). If usage_events P1 lands (monitoring-spec.md:20-29), do we add run-scoped kinds as a rollup target, or keep exactly one mechanism?
5. **Identity:** per-runner revocable Bearer now ((issue-token) precedent, glochid/lib/habitat.ml:15-20); tuna identities at bearer parity (libs/tuna/lib.pp:16, v1); per-run minting deferred (provisioning story is a known gap, agent-native.borg:11). NEED from roerick/gregor: tuna-as.sh location + rotation procedure (off-repo: tuna-deriv on club - outside this recon's scope), and the per-identity cost of the deny-all allowlist config a runner needs.
6. **Supervisor residence for phase 2:** glochid on wyo (existing systemd service) vs the gregor box. Who may steal the supervisor lease?
7. **Budget defaults:** pp-bench FINDINGS needed (~/pp-bench is on the vultr/wyo box, not this laptop - could not read). The brief cites run 2: 36 provider requests / 1.75M input tokens / 89% cache hits at the 3000s wall.
8. **obsd-sec schedule row:** `(schedules)` on town returned [] as admin on 2026-10-16 - retired, branch-scoped visibility, or on another instance? Mechanism verified in code regardless (jobs.borg:231-241; boot.ml:257-282).
9. **Concurrency default for the phase-2 dogfood:** suggest 1-2 parallel runs until the overnight soak passes.
10. **Claim atomicity:** todo/item/assign is last-write-wins (no CAS). Acceptable under the single-supervisor lease; revisit only if a second supervisor class ever appears.

## Provenance

Recon was read-only: local clone @ 5f87708 (ground-truth commits b58a121/96b8d4e/e36dc00 verified present) + prod probes (whoami, card f526bace, schedules, card-id list). No prod writes; nothing deployed. Drafts committed and pushed to wyo at 7f2f15d (2026-10-16); this revision folds gregor's review.
