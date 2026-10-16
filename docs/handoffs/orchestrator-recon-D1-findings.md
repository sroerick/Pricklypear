# D1: PP orchestration recon findings (Q1-Q7)

**Handoff:** roerick, 2026-10-16 ("PP ORCHESTRATION - RECON + RUN-PRIMITIVE SPEC")
**Author:** AL (Autolith session, wyo laptop clone @ master 5f87708)
**Scope:** local clone + read-only prod probes via `/api/eval` Bearer (whoami, card reads, schedules). No writes anywhere; nothing deployed.
**Ground truth verified:** commits b58a121, 96b8d4e, e36dc00 present (`git log --all`); pp-flow.sh carries EVIDENCE:/DESTINATION:/CONTEXT_FILE (grep). pp_eval corridor live (`whoami` = admin via Bearer pp.token).

Verdict legend: **exists** / **partial** / **missing**

| Q | Capability | Verdict |
|---|---|---|
| Q1 | In-image job runner + schedules + step checkpoints | **exists** (implemented) |
| Q1 | Guest-visible subprocess spawn | **missing** (internal `sh_capture` only) |
| Q1 | Supervised-sidecar shape | **named, unbuilt** (jobs.borg hard edge pre-blesses it) |
| Q2 | Board claim/lease | **exists** (single lease, driver-enforced, no TTL primitive) |
| Q2 | Claim-kinds (card f526bace) | **design** (epistemic claims - evidence tie-in, not leases) |
| Q3 | Token/usage accounting in PP | **missing** (spec'd table unbuilt; usage exists Autolith-side, dropped at boundary) |
| Q4 | Agent-native loop reuse | **partial** |
| Q5 | Glochid app contract | **partial** |
| Q6 | pp-flow BOM | **exists** (split mapped) |
| Q7 | Identity ops | **partial** (laws pinned; tooling off-repo) |

## Q1. Execution primitives

**The job runner roerick half-remembers exists and is implemented**: borge/jobs.borg (status implemented, :3-4) + image/lib/jobs.ml (785 lines) + a boot-time runner fiber.

- "The async story for Nopales is job-shaped, not call-shaped" (jobs.borg:8). One image process, no broker: "the worker is an in-process host fiber, not a fleet" (:27-30); guests submit jobs and read rows; the runner fiber calls back into eval through the same Control rim an HTTP request uses (:39-42).
- Dedicated kernel tables (migrations/015_jobs.sql): jobs, job_steps, schedules - NOT tree nodes, for three recorded reasons (jobs.borg:64-91): SKIP LOCKED claim needs typed status/run_at columns + partial index `jobs_claim_idx ON jobs(run_at,id) WHERE status='pending'`; kernel authority; branching edge (no branch id on rows, so fork/merge/promote structurally cannot copy a job).
- Job row (jobs.borg:110-123; jobs.ml:25-38): id, fn (1-arity Nopales fn), args jsonb, **branch (code lens captured at submit)**, submitted_by (visibility key), status `pending|running|done|failed|dead`, run_at, attempts, max_attempts (default 3, exhausted = dead), result, error, runner_id.
- Runner loop (jobs.borg:164-196): CLAIM (SKIP LOCKED, oldest first, status->running + runner_id, committed before eval; jobs.ml:241-272), EVAL (fn on the recorded branch under a synthetic request context, whoami=submitted_by), RECORD (done+result | attempts+1 with backoff 2^attempts cap 60s (jobs.ml:274-290) | dead at max_attempts; jobs.ml:295-331). Fresh runner_id per boot ("boot-<pid>", jobs.ml:16-19); boot reaps running rows from dead runners (jobs.borg:191-196; jobs.ml:335-347).
- Step checkpoints (jobs.borg:199-229; jobs.ml:355-405): `(with-step "name" thunk)` memoized by (job-id, name); with-transaction composition = exactly-once for DB-writing steps; **"Resume is by CHECKPOINT LOOKUP, not code replay"** (jobs.borg:219). v1 limits: steps are RETRY-scoped - a crash restarts the job fn from its top, replaying checkpoints (:226-229); no mid-run suspension across boots.
- Budgets: **jobs run UNBUDGETED** (max_int steps; jobs.borg:289-293) - the runaway guard is attempts/backoff + checkpoints; the request path keeps its 100k budget.
- Schedules (jobs.borg:231-241, 310-318; jobs.ml:407-580; boot.ml:257-282 `fire_schedules`): cron is sugar over the same machinery - the due-scan submits a job per due schedule then advances next_run; missed fires collapse to ONE catch-up run at boot; per-schedule tz (migration 032), vixie DST semantics.
- Guest prims (jobs.borg:135-156): submit-job, submit-job-at, job-status, cancel-job, jobs, with-step, schedule, unschedule, schedules. **No await-job** - pull, not push (:158-161).

**How obsd-sec executes:** a schedule row fires via the runner's due-scan, which submits a job; the job's fn evals IN-IMAGE as the schedule's submitted_by identity on the recorded branch (jobs.borg:231-241 + boot.ml:257-282). No thread-per-schedule, no subprocess - it is an eval with the runner fiber's lifecycle. Prod probe 2026-10-16: `(schedules)` as admin on pricklypear.rocks returned `[]` - the obsd-sec row is not visible from system/main on town (branch-scoped visibility, another instance, or retired; see D4 Q8). The mechanism is what Q1 needs, and it is verified in code.

**Subprocess spawning exists internally, is not guest-visible.** `Io_eio.sh_capture ?timeout_s` (image/lib/io_eio.ml:53-76): `/bin/sh -c`, stdout captured (<=64 MB), child killed on timeout via Eio.Time.Timeout on a private switch. Used today by the LLM client (provider calls run through curl children: llm_client.ml:134 at 25s, 213/545 at 200s) and prims_net.ml:149. No Nopales prim exposes shell exec. glochid side: glochid/lib/habitat.ml:56-76 spawns sh for env reads; llm/lib/tools.ml:53-66 is the agent bash tool.

**Where a supervised sidecar fits - already named.** The jobs hard edge (jobs.borg:246-253) rules in-image exec out for long work: "genuinely long external work belongs to an external worker speaking the same SKIP LOCKED claim (film-edit's Blender bridge is the precedent shape)"; non-goals subsection: "out-of-process workers via shared claim protocol only" (:321). Two placements, both consistent with the book:
1. Supervisor-spawned sidecars (chosen for D2): the glochid supervisor owns claims/leases/state in PP rows and spawns runner processes per step (Eio child + wall-clock timeout, the sh_capture pattern at glochid scale).
2. Kernel jobs rows + external worker (the Blender-bridge shape) - the phase-3 complement for PP-native steps via the EXISTING in-image runner (no LLM in-process).

## Q2. Claims/leases

Board-level today - **exists: single lease, driver-enforced**:

- Claim = the assignee field, `todo/item/assign` (libs/todo/lib.pp:1420-1439). Protocol spec: borge/workstreams.borg:290-357 (claim-brief-protocol, implemented-live :349).
- Lease = `executor:<pid>@<epoch>` (pp-flow.sh:50-55); reclaimable when pid dead (`kill -0`) OR older than 12h (`STALE_SECS=43200`, pp-flow.sh:61; claim at 151-176, refusal rc 3, --force override); released on EVERY exit path (178-182) and batched INTO the write-back eval so a crash cannot strand it (191-208; the ac7ddff9 stale-lease regression note).
- SESSION-LEASE AMENDMENT (workstreams.borg:309-323, planned not built): lease becomes `glochid:<session-id>@<ts>`; staleness = claimed + no feed activity in N minutes; the session's terminal marker is the definitive "run over".
- Queue ORDER is already Nopales: `todo/item/queue` (priority, then position, then id - workstreams.borg:327-329). The eligibility filter is deliberately driver policy: "picking is a POLICY about the loop... todo/item/queue already owns the ORDER" (pp-flow.sh:230-233; drain_pick 234-271).
- Hard-won guards any port must preserve: double-write defense (pp-flow.sh:319-328), terminal no-resurrect (329-336 - `todo/cycle` WRAPS, so advancing a DONE card resurrects it), empty-state restore (354-365), DESTINATION validated against the card's real columns (395-415; workstreams.borg:341-347), NO-VERDICT fail-safe toward the human (649-671).

**What lease-with-TTL would need:** (1) a heartbeat/timestamp field plus ONE reclaim rule living server-side - today staleness is a driver-side constant, so two drivers = two staleness rules; (2) an atomic claim - `todo/item/assign` is a plain set-field (last-write-wins, no CAS; contrast the jobs SKIP LOCKED claim, jobs.ml:241-272): acceptable under a single leased supervisor, unsafe if two supervisors ever race; (3) the session-lease amendment's "feed inactivity" needs a defined activity signal.

**Card f526bace (read live; full UUID f526bace-f035-4755-a549-312a1df2ca6c; kind DESIGN, state TODO, Board list 0b21ffaa):** "DESIGN: claim kinds: enforce at the claim boundary, not via a typing role." It is about EPISTEMIC claim kinds - observed, computed, cited, interpreted, asserted, axiom (+ doctrinal/deontic for judges) - so a verifier can dispatch a probe and say no for a stated reason; typing must NOT be a role (post hoc; a single correlated point; its own output is itself a claim that would need typing - regresses forever); gate at the CLAIM BOUNDARY ("make illegal states unrepresentable"). Three enforcement tiers: 1. path convention (today - the kind is a path segment; checkable but unenforced); 2. tool layer - "a claim verb that REFUSES without a kind - real enforcement, cheap, do now"; 3. substrate - a claim-level surface distinct from the generic tree write, requiring kind + optionally a declared probe; "do NOT overload tree/put". Acceptance: a claim without a kind is rejected at the boundary; the kind is STRUCTURAL (enforced) not ASSERTED (attributed); judge-sweep can dispatch on kind.

**Scope note for roerick:** this card is about knowledge claims on the tree substrate, not board-card leases. The run primitive's tie-in is EVIDENCE-side: step evidence entries adopt claim kinds (tier-2 shape; judge-sweep = the REVIEW gates). See D4 Q3.

## Q3. Token accounting - **missing (PP-side)**

- `usage_events` is spec-only: docs/monitoring-spec.md:20-29 - append-only `id, kind, username, at, meta jsonb`; kinds are OPERATIONAL (signup, login_ok, login_fail, eval, write, media_put, media_denied, rate_denied, schedule_create) - **no LLM token kinds**. P1 (74-75) never landed; the 2026-10-06 revision (78-90) retired the digest and reduced alerts to tuna liveness + disk.
- No usage table in migrations (search "usage": only 002_auth.sql:76 "GRANT USAGE ON ALL SEQUENCES"); no prompt/completion/input/output-token capture anywhere in the repo (0 hits); llm/ records no usage.
- What exists instead: Tier_limits (request-rate + storage: http.ml:394-396 writes_per_min; auth_store.ml:274 instance user cap; media.ml:213/270 quota) and function_stat (migrations/031; eval tick/wall telemetry) - neither is token accounting.
- Token counts DO exist - Autolith-side only. The run-job terminal result carries `:usage`, and the adapter already parses `:INPUT-TOKENS`, `:OUTPUT-TOKENS`, `:PROVIDER-REQUESTS` (pp-flow-autolith.sh:223-226) and echoes them on stderr (249) - then drops them at the PP boundary. The per-run ledger (runs.jsonl, pp-flow.sh:292-294) records durations + rc + evidence + destination only, and lives OFF-PP.
- Enforcement today is wall-clock only: adapter TIMEOUT default 3000s (pp-flow-autolith.sh:60; raised 1800->3000 by 96b8d4e after run 2 hit the wall at 36 provider requests / 1.75M input tokens / 89% cache hits); `:TIMED-OUT` is terminal on purpose (147-148). The jobs runner has no wall or token budget (max_int, jobs.borg:290).
- D2 consequence: run rows carry budget + accumulate usage parsed from executor terminal summaries; the supervisor refuses dispatch when exhausted. (usage_events P1 could add run-scoped kinds later - D4 Q4.)

## Q4. Agent-native reuse - **partial**

What M1-M3 + the glochid M4 PoC already give a mayor:

- Identity pattern (agent-native.borg:7-15): a bot is a REAL auth-008 user with a token, ONE personal branch as code+memory, the same data plane and RLS as any human; attribution is free (data_rows.author = session username on every write); "the agent never auths as anyone else; there is no sudo on the data plane." Gaps (:11): "no provisioning story (create bot user + token + personal branch in one step), no token rotation, no per-agent tool namespace" - M1 was hand-created.
- Wake scheduling (29-37): a jobs cron schedule runs ON the agent's branch AS the agent; quiet tick makes NO provider call; loud tick = bounded observation + one LLM call (ai_store session via /api/agent, max_steps small).
- Safety decisions (44): idempotent cursor + wake-id dedup (a crash between post and advance replays quietly); /api/eval transactional (a failed wake rolls back clean; the runner records the error, next tick retries); tool gate (tools install on the agent's OWN branch; shared/system/main via HUMAN promote; server-only prims refused via register_server_only classification; default deny-list: checkout, branch forks/merges, promote, auth/session prims).
- Budgets (40): "max_steps from /api/agent caps provider turns; the eval budget (400k steps, 2026-09-03) caps any single eval; the request gate bounds wall time."
- Quarantine (43): 3 consecutive failures on an unadvanced cursor -> advance past the poison message + one ntfy + reset (born from 50+ identical failures on prod); parse floor surfaces provider error bodies as "provider error: <message>".
- ntfy push, M3 (55-63): triggers wired (room mention, wake failure); "Delivery failure logs and retries next wake; it never fails the wake itself"; the ntfy topic lives in a mode-600 env file, never in chat or git (59). Notable egress precedent: the jobs runner PASSES the outbound-http gate while a direct /api/eval http-post was denied for the bot (59) - identity-scoped egress is already a thing the kernel does.
- glochid-pairing M4/M5 (65-69): "one identity story from two residences." The M4 PoC (glochid.lisp SBCL service) built: append-only call/result rows keyed by call id (idempotent, restart-safe), a shell tool with a 30s in-sh watchdog ("timeout(1) is unusable spawned from SBCL, setpgid EPERM"), fs.read with 8KiB cap + secret-path deny, default-deny ACL tiers, an .armed file holding an expiry (disarmed = heartbeat only), @reboot cron, an fd-leak fix.

Missing for multi-run, per-identity work:
- Provisioning story, token rotation, per-agent tool namespace (agent-native.borg:11).
- Concurrency: the wake loop is one serial job per tick; a manual wake concurrent with the cron tick double-scans (wake-id dedupe catches replays, not races - :59).
- Run-scoped state: agent-state rows are per-room (kind=wake, sid/cursor), not per-run.
- Usage accounting: none (turn/step/wall budgets only).
- Session growth: the ai-session hit 200,705 tokens -> provider 400; rotation/compaction still open (:43, filed in log/issues).

## Q5. Glochid app contract - **partial**

Per docs/filmstrip-glochid-seam-map.md (model locked 2026-10-14, :4-17): FILMSTRIP = the desktop UI framework ("a desktop that apps load into"); GLOCHID = the agent harness, "an APP which loads inside filmstrip"; target = one authored libs/filmstrip, glochid consuming named seams, habitat /filmstrip and harness GET / rendering the same body.

What an app gets TODAY (:114-125): the harness surface (glochid/shell*, content-area, panes-region, rail-box, shell-doc), harness apps/trees (code-tree, fs-*, prims-tree, ui-tree, new, resume), chat templates (base-css, tg, css-text, ui/* + lookbook), keyboard content via the LANDED `register-keys!` (:140-144), and the LANDED storage seam (:129-136): `filmstrip/store-backend` is a read+write PAIR - habitat = tree prims/Postgres, glochid = fs prims/file.

Pending (:137-139, 151-169): the app registry verb `filmstrip/register-app!` rides filmstrip phase 3, WITH the load-order refusal guard (refuses until every framework verb it knows is bound, then idempotently binds). Boot gate: the standalone glochid bin crashed at boot from master because a filmstrip lib top-level called kernel-only prims (:80-109) - since fixed on current master (5b1ba59, 2026-10-14 triage: "glochid boots"), and the deprecation gate keeps the boot gate as checklist item #4 ("THE PREREQUISITE", :220-224).

What a mayor app needs that does not exist yet:
1. A lifecycle/tick seam - a recurring timer loop. Glochid has the sessions loop + on_msg sink but no tick source (the M4 PoC solved this with @reboot cron + a tick loop, agent-native.borg:69).
2. A supervised child-process registry - spawn + wall-clock timeout + reap + output capture. Glochid spawns subprocesses today only for env reads (habitat.ml:56-76); Eio provides the machinery (sh_capture precedent, io_eio.ml:53-76).
3. Run-row access via the bridge - mostly there: glochid's habitat bridge posts /api/eval (eval_timeout_s = 30, habitat.ml:29) and supports a REVOCABLE per-user Bearer via `(issue-token "glochid")` - "the laptop then holds a revocable credential, never the operator's password" (habitat.ml:15-20).

Order-of-work note: the supervisor loop itself is headless and does NOT need filmstrip phase 3; register-app! is the UI half and can land later.

## Q6. pp-flow bill of materials - **exists, split mapped**

pp-flow.sh = 706 lines (driver); pp-flow-autolith.sh = 373 lines (adapter). Three programs in two files:

**SUPERVISOR (moves into glochid as the native app):**
- Lease claim/release + staleness: pp-flow.sh:50-55, 61, 151-182.
- Batched habitat evals: 130-145 (eval plumbing, `(begin (load-library "todo") ...)` wrap), 184-220 (round trips cost ~1-2.5s - Basic-auth argon2 + wire - so reads/writes batch into ONE eval; scars: ac7ddff9 lease-in-batch, 88a92237 columns-ride-along).
- Queue pick: 222-271 (drain_pick = todo/item/queue + eligibility filter).
- Write-back: 300-367 (existence probe, double-write defense, terminal no-resurrect guard, batched write set, empty-state restore).
- Verdict contract: 607-622 (MANDATORY VERDICT/RESULT FIELDS appended to every brief - structural for every column and list).
- Extraction + safety: 369-433 (DESTINATION 4-tier extraction incl. JSON; EVIDENCE template-filtered), 645-671 (validation against real columns; NO-VERDICT fail-safe).
- Loops: drain 459-514 (separate subprocess per card = reviewer independence, :466-470; stop-on-failure default, :472-475; exit = failure count capped 125; creds forwarded to children, :494-498), walk 515-572 (visited-set ping-pong guard, :530-533).
- Ledger: 287-298 (runs.jsonl + RUN SUMMARY - becomes run rows).
- Creds: 63-75, 128.

**EXECUTOR (stays out-of-process as the sidecar):**
- The PI_RUNNER seam: pp-flow.sh:624-642 (`$PI_RUNNER "<brief>" > log 2>&1`; missing runner = config error rc 127).
- The adapter (pp-flow-autolith.sh): model pinning (54-58; executor role :models ("@auto") validated statically - autolith#76), reasoning-effort preflight (111-128: role's :medium vs glm-flash low|high|max -> fail fast, exit 2), job envelope (44-47, 154-184), output contract (156-184: schema with verdict enum PASS/FAIL/ESCALATE + required verdict/evidence, destination optional - "an answer with no verdict fails closed as :INVALID-OUTPUT"; measured across 3 models), run-job invocation + result-decides rule (186-206: "run-job exits 1 even when it wrote a well-formed job result... A written result sexp is definitive regardless of exit code"), retry policy driven by :CATEGORY (130-152, 229-262: RETRYABLE = :PROVIDER-FAILURE naming 402/408/409/425/429/5xx or a crash with no result; TERMINAL = everything else, incl. :TIMED-OUT "terminal on purpose"; 2 retries, 10s backoff doubling, cap 300), usage parsing (216-227), archive_failure (66-101: output + stderr + job envelope + the child's durable conversation via :TRACE-ID + README -> .ralph/failures/<ts>-<job-id>/), structured-result bridge (:OBJECT -> EVIDENCE:/DESTINATION: lines, 277-342).

**HANDS (stay box-local, invoked under identity):**
- .ralph logs/briefs/failure archives (pp-flow.sh:46, 436-438, 605, 625; adapter :29, 66-101), runs.jsonl (287-298), PP_FLOW_CONTEXT_FILE box-local repo/build truth (47-48, 590-604), autolith task store (79-86), creds file (63-75).

Debts the port fixes: runs ledger off-PP; usage dropped at the PP boundary; a killed driver leaves a stale claim lease needing manual unassign (the release-rides-the-batch is best-effort across exit paths); the staleness rule is duplicated per driver.

## Q7. Identity ops - **partial**

- FED3 law (libs/tuna/lib.pp:7-10): "the library NEVER sets actor fields. tuna derives the actor from the bearer token, so a bridge write is always attributed to the identity that owns the bearer. Nothing here can forge attribution, by construction."
- Tuna client surface (tuna/lib.pp, 129 lines): pure cfg {base, bearer} (20-26); envelope check raises on {error:...} incl. the 409 CAS conflict (28-47); tree-get RAISES on a missing path ("tuna journals the failed read - denials surface, never drop", 74-80); tree-cas version CAS (93-102); tree-log carries the actor per op ("every row's actor is the true caller", 111-116); ns-fork (124-129). v0 scope = tree surface + ns/fork; "Store-backed, identity-keyed config (bearer parity) lands in v1" (16).
- acted_by (migrations/039_acted_by.sql): data_rows.acted_by (:36) + tree_versions env stamp (:43-85, 87-125); PINNED semantics (:21-31): create -> author = session username (the OWNER on delegated tokens; the standing law), acted_by = actor or NULL; update -> delegated stamps acted_by, non-delegated PRESERVES (COALESCE); "acted_by answers 'has an agent's credential ever acted on this row'"; author + tree_versions carry per-write identity. The pre-registered failure (delegated write landing as author=admin, acted_by null) is what FED3/039 close - keep it closed in the design.
- Delegation producer (borge/auth.borg:1302+; smoke 1465-1466): owner mints a delegated token; the agent Bearer-writes; the row lands data.u.<owner>.* with author=<owner>, (whoami)=<owner>, (current-actor)=<agent>; "delegated (define) does NOT persist; after image restart (or fresh session) the name is unbound."
- Revocable per-user token precedent (glochid/lib/habitat.ml:15-20): `(issue-token "glochid")` replaces user/pass - "the laptop then holds a revocable credential, never the operator's password."
- Bot precedent (agent-native.borg:5, 11): token file mode 600, never committed; provisioning/rotation/tool-namespace MISSING.
- **tuna-as.sh is not in this repo** (0 hits). RESOLVED by gregor review 2026-10-16: it lives at `~/bin/tuna-as.sh` on vultr; the rotation procedure lives in tuna-deriv/club. Per-run identity cost: still unknown -> D4 Q5.
- Ground truth retained: allowlists default deny-all, parity identity-mapped (verified live 2026-10-15 on club, per the brief).

## Prod probes performed (all read-only)

- `(whoami)` via Bearer pp.token -> admin (corridor works; PP_URL=https://pricklypear.rocks).
- `(list/map car (todo/item/all))` -> card ids are full UUIDs (todo/item/get needs the full UUID, not the 8-char prefix).
- `(todo/item/get "f526bace-f035-4755-a549-312a1df2ca6c")` -> full card text captured (Q2 above).
- `(schedules)` -> [] on town as admin (Q1/D4 Q8). RESOLVED by gregor review 2026-10-16: obsd-sec is NOT retired - it is gregor's schedule running in the .rocks image; the probe hit town (wrong instance), hence [].
