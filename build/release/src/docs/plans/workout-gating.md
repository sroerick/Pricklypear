# workout-gating — programs as code, advancement as gates, sessions generated on demand

Draft spec. Target home: `borge/workout.borg` (new subsections + lib changes in
`libs/workout/lib.pp`). Follows the hybrid model in that chapter: sessions stay
fluid docs + fact rows; this chapter adds the missing piece — **cross-session
program state, outcome gates, and on-demand session generation**.

## Posture

Existing ksk1-style programs are clock-driven: a scheduler pre-materializes the
whole phase/week/day calendar, and advancement is unconditional. The
StrongFirst-family programs this chapter serves are **outcome-driven**: Sunday's
run ratio depends on two run sessions you haven't done yet; the C&J rep ladder
advances only when a session reaches 30 min; SCT phases unlock on benchmark
tests. You cannot pre-materialize those calendars — and you don't want to.

Decision: **gated programs generate the training session on demand.** "What is
today's session?" is a pure function of stored program state + date. The
calendar is not written, reconciled, or corrected. This deletes drift bugs
instead of healing them.

What we add (all additive, existing machinery untouched):

1. `workout-program-state` — the cross-session cursor (new deftype).
2. `workout-gate` — asserted gate records, the judgment ledger (new deftype).
3. Gate evaluation at capture time — derived gates read facts; asserted gates
   read the ledger.
4. `workout/suggest` — today's session generated from state + date, home-and-
   equipment filtered.
5. Four programs as plan/run/capture triplets over this machinery:
   `ksf` (C&J), `run5k`, `sct` (3-phase core), `stretch` (leapfrog cycles).

## Data model

Deftypes on load (fields are docs + form UI; payload trees in
data_rows convention):

    workout-program-state
      program    string   ; "ksf" | "run5k" | "sct" | "stretch"
      state      string   ; JSON tree: cursor + params + gate summary
      active     string   ; "true" | "" — one active row per program;
                          ; superseded rows keep history
      updated    string   ; YYYY-MM-DD (set at every gate eval)
      notes      string

    workout-gate
      program    string
      key        string   ; canonical gate name, e.g. "phase1-exit",
                          ; "repeat-week-sore", "own-the-range"
      passed     string   ; "true" | "false"
      session_id string   ; uuid of the session the assertion rode on (optional)
      date       string
      notes      string

Rules:

- **One active state row per program.** Writing a new active row supersedes the
  old (old rows become history; no delete). Same spirit as function_defs
  branching — cheap history, no mutable-in-place mysteries.
- **Derived gates are NEVER stored.** They are computed from facts at capture
  time (`session-complete` → capture → gate eval → new state row). If a derived
  gate is expressed as truth again later, it recomputes. The stored thing is the
  resulting state.
- **Asserted gates are ALWAYS stored** as `workout-gate` rows — user or agent
  assertion, with session link when applicable. Capture reads current gate truth
  from the ledger; state rows may cache a summary but the ledger is authority.
- `workout-gate` rows are shared like all journal rows. Gate names are
  package-canonical; unknown names are tolerated (logged under the program).

## Gate taxonomy (closed, by question answered)

- *Derived (did the meet-able thing happen?)* — the capture contract questions
  already answer these from facts: 30-min pattern-clear sessions, phase totals,
  session counts, time under work.
- *Asserted (did the human judge it?)* — checkpoints ("3×30 dead bugs,
  pain-free"), soreness flags, "own the range" (Quicksilver), day-quality at
  KSK-style talk tests when the meter isn't logged.
- *Clock (rhythm rails)* — deload-every-4th, med-wave spacing (heavy stretch
  3×/2wk), cycle boundaries. These are date arithmetic over state, not gates in
  the judgment sense — they parameterize suggest/capture, never block silently.

## On-demand session generation

    (workout/suggest program date) → doc-shaped suggestion
      ;; reads active state row + asserted gates since last capture
      ;; + weekday + equipment constraints in state
      ;; returns today's target: blocks + targets + day label
    (workout/log-suggested session-id suggestions) ; quick-log ticks → facts

- `ksf`: intent ("pattern-of-the-day" from rep ladder + cadence), targets,
  time cap. Session = one of the existing ladder/interval runners.
- `run5k`: ratio jog/walk, sets, warmup/flush walk, week # (advanced/repeated).
- `sct`: session-table targets — dead bugs 3×5 reps, hollow 6×10 sec, benchmark
  day marker, carry block (Sunday only) in Phase 3.
- `stretch`: Sunday/Wednesday slot content by cycle + "share the load" peers.

Suggestion is **pure and idempotent**: repeat reads with unchanged state return
the same session. Nothing is written until you start (or quick-log) an actual
session. When a session is completed and captured, gates evaluate and the state
row turns over — tomorrow's `suggest` sees the new cursor.

## Capture-time gate evaluation (the new hook)

At `session-complete` for a gated program:

1. capture projects facts (existing contract, unchanged)
2. gate eval (new, per program):
   - reads facts + gate ledger + state
   - computes derived gates
   - resolves rhythm rails (deload flag, waves)
   - emits the next state row (advance / repeat / deload / complete)
3. `workout/gate-status program` → printable cursor + unmet gates (agent + UI)

Agents get it all via pp_eval:

    pp_eval '(workout/suggest "ksf" (date-today))'
    pp_eval '(workout/gate-status "sct")'
    pp_eval '(workout/gate-assert "sct" "phase1-exit" true "3x30 all clean")'

## Equipment & location in state

State tree carries the constraint profile once:

    { "equipment": { "bells": [16, 20, 24, 28, 32], "unit": "kg",
                     "bar": true },
      "location": "home" }

Unlike the no-bar fallback case, the profile here is simple: bar present, 16–32
bells present → hanging + ground-based + all carry work selectable. The profile
exists so `suggest` filters without a second config system, and so variants stay
honest as equipment grows.

## The four programs (book as written, 2-day variant as parameter)

Each book program ships twice under one code path: the gate logic and runners
are identical; **weekly structure is a plan parameter** — `days` list (e.g.
`(fri sun)`), phase/session mapping, and deload cadence. Books compress
cleanly because their advancement is outcome-based, not calendar-based.

### ksf (Kettlebells StrongFirst C&J)

- Phase 0 technique practice → Plan A (LCC&J both days; sets every 30 s, weak
  arm on-the-minute) → ladder advance: 30-min pattern-clear ⇒ +clean, ⇒ +jerk,
  ⇒ stall test ⇒ Plan B (cleans day / jerks day) ⇒ (far) Plans 2A/2B doubles.
- Derived gates: pattern-clear duration ≥ 30 min; stall = no pattern-clear
  growth across W consecutive sessions. Clock rails: 4th week = deload bell.
- Facts: `LC C&J L/R` reps (L/R naming per chapter), session duration fact.

### run5k

- Doc = box of run/walk intervals; ratio ladder by phase; Day A (Fri) = ratio
  session, Day B (Sun) = long build. 5K day = calendar-free state target.
- Derived: two clean single steps in ratio ⇒ advance. Asserted: soreness
  (ledger) ⇒ repeat week. Clock: every 4th session = repeat-best (deload).

### sct (Systematic Core Training)

- Three phase sub-cursors (Stability → Strength → Power), sequential, gated:
  - Stability exit: benchmark session (3×30 dead bugs/bird dogs, 30 rocking,
    30 breaths/level, neck drills ×30) — derived from facts + asserted
    pain-free.
  - Strength exit: goal column reached (60 s hollow, 3×15 raises, 30 s L-sit,
    3×30 holds, 1×10 SL deadlifts) + worked-well-under-bells assertion.
  - Power: carries unlocked; Sunday-only; clock-marched (7→10 min, RPE 7→9).
- `days` parameter: full program says near-daily; our variant says Fri/Sun
  blocks + daily micro-doses that journal as facts on a day-session
  (dead bug rep totals etc.).

### stretch (Quicksilver)

- Two leapfrog goals (front split / loaded straddle); one advances per cycle,
  the other holds at medium. Ledgers, not facts, mostly: "own-the-range",
  "own-the-loaded-straddle".
- Rhythm rail: heavy isometric ≤ 3×/2 wk — Sunday heavy slot, Wednesday medium
  slot, everything else prying. Deload week = prying only, synced to the
  program-wide wave.
- Journaled sparsely: Sun/Wed slots + deload assert; daily prying is optional
  day-activity only.

## Ship order

1. Machinery slice in `libs/workout/lib.pp`: two deftypes + state upsert/read +
   `suggest` dispatch + gate eval hook in capture + `gate-status`. Prove it with
   a one-gate toy (advance after N complete sessions).
2. `ksf` — reuses KSK runner DNA; proves the full gating loop end-to-end
   fastest.
3. `run5k` — second gate species (asserted soreness, repeat logic).
4. `sct` — richest fact surface + the daily micro-dose journal pattern.
5. `stretch` — ledger-mostly program, least journal plumbing.

## Non-goals (this chapter)

- No calendar event generation/reconciliation for gated programs (books stay
  calendar-light by choice; ksk1 keeps its scheduler).
- No wearable/HR.
- No per-program fact columns; program variance stays inside docs/details.
- No automatic medical-style interpretation of asserted gates — the ledger
  records judgment, it doesn't launder it.
