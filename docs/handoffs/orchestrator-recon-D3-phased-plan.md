# D3: phased plan - run primitive, then supervisor in glochid, then native runner

Each phase independently shippable. Sizes are rough agent-days (executor cadence already demonstrated by pp-flow: one card is ~5 minutes of supervised work).

## Phase 1 - Run primitive spec (book chapter)

- Input: D1 findings + D2 draft + gregor's review (failure paths F1-F5 first).
- Work: ratify storage (shared-space datum vs kernel table, D4 Q1); ratify the sessions-as-runs obituary for sidecar runs (D4 Q2); finalize field contracts + the six POLICY decisions; register the chapter in pricklypear.borg (inline order, after workstreams).
- Ships: a merged spec chapter. No code.
- Size: small - 1-2 agent-days including one review round.

## Phase 2 - Supervisor into glochid, pp-flow as the executor

- Work:
  - Run rows live (storage per phase 1) + minimal read/write helpers in libs/workstreams (the orchestration Do-NOT rule limits new Nopales surface to the todo/standup/workstreams families, workstreams.borg:298-300).
  - The glochid-resident tick loop: headless systemd service, NO filmstrip dependency (register-app! is the UI half and rides filmstrip phase 3; the boot gate is already fixed on master, 5b1ba59).
  - The supervisor spawns `scripts/pp-flow.sh --run <card>` UNCHANGED as the sidecar (proven executor); the adapter stays verbatim; pp-flow's driver logic (claim/brief/write-back/ledger) migrates INTO the supervisor.
  - Usage parsing from adapter terminal output (:INPUT-TOKENS/:OUTPUT-TOKENS/:PROVIDER-REQUESTS, pp-flow-autolith.sh:223-226) into run rows - the counts stop being dropped.
  - Lease/heartbeat/reap; failure-archive pointers into rows; runs.jsonl kept during transition as a derived artifact.
- Ship behind a flag; dogfood overnight on cheap cards before ANY default flips (workstreams.borg:508-509: "Do NOT flip the default until the loop has survived real overnight cards").
- Size: medium - 3-5 agent-days + an overnight dogfood.

## Phase 3 - Native dumb runner for PP-native work (last)

- Work: habitat-domain cards (branch_kind = habitat) execute steps as prim-only job fns via the EXISTING jobs runner (in-image, no LLM - the constraint holds, and jobs.borg:246-253 already requires long/LLM work out-of-process); with-step checkpoints give exactly-once (jobs.borg:199-229); evidence lands claim-kinded (f526bace tier-2 verb when it exists); one-way-door cards stay human-gated.
- Size: small-medium - 2-3 agent-days after phase 2 hardens.

## Cross-phase invariants

- The LLM loop NEVER runs inside the PP runtime process (sidecars only; tuna 10-04 precedent).
- Transcripts never in tree rows; the card feed keeps milestones + terminal marker.
- PP rows are the queue - no broker, no new service.
- One identity per runner, deny-all allowlists; acted_by per 039 PINNED semantics.
- Card transitions are supervisor-owned; review independence (one gate = one run = one process).

## Return

Plain files in docs/handoffs/ (roerick's "plain files" option): orchestrator-recon-D1-findings.md, orchestrator-recon-D2-run-primitive.draft.borg, orchestrator-recon-D3-phased-plan.md, orchestrator-recon-D4-open-questions.md. Uncommitted by design (.git read-only in this session); roerick routes to gregor.
