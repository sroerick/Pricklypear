#!/bin/ksh
# pp-flow.sh — the LOCAL EXECUTOR for the claim/brief protocol
# (borge/workstreams.borg claim-brief-protocol; orchestration task 8).
#
# The habitat is the record + queue; the laptop is the executor. This script
# CLAIMs a card, renders its BRIEF (todo/flow-brief), hands the brief to a
# local pi session, and on completion WRITES BACK (advance one column + post a
# todo-comment with evidence). No agents run inside the prod image.
#
# Usage:
#   pp-flow.sh <card-id>                        # full run: claim + brief + pi + write-back
#   pp-flow.sh --run <card-id>                  # same as above
#   pp-flow.sh --dry-run <card-id>              # print the brief only (no side effects)
#   pp-flow.sh --claim <card-id>                # set the claim lease, print it
#   pp-flow.sh --write-back <card-id> [msg]     # advance one column + post evidence comment
#   pp-flow.sh --run <id> --supervised --brief-file F --claimed-state S \
#       --columns C --log-dir D  # SUPERVISED SIDECAR (run-primitive phase 2):
#                                # the glochid supervisor owns claim/brief/
#                                # write-back; this sidecar runs the adapter,
#                                # extracts the verdict, reports PPFLOW-* lines.
#   pp-flow.sh --drain [list-id] [N]            # UNATTENDED: pick + run N claimable cards
#                                               #   (default 1; --max N; --states "TODO,CODE")
#   pp-flow.sh --walk <card-id> [--walk-max N]  # drive ONE card through ALL its gates
#                                               #   (TODO->CODE->...->HUMAN-REVIEW/DONE),
#                                               #   each gate a separate run. Stops on a
#                                               #   revisited column (a FAIL bounce-back).
#   pp-flow.sh --help
#
# Automatable/loop knobs (--drain):
#   --states "TODO,CODE"     columns that count as actionable work
#   --exclude-kinds "BUG"    skip cards whose `kind` is listed
#   --max N                  at most N cards this invocation
#   PP_FLOW_DRAIN_CONTINUE=1 don't stop on the first failed card
# --walk budget:
#   --walk-max N             at most N gates this walk (default 6)
# The default runner for unattended loops is the autolith adapter:
#   PP_FLOW_PI=scripts/pp-flow-autolith.sh AUTOLITH_MODEL=syn:large:text \
#     scripts/pp-flow.sh --drain
#
# Options:
#   --url URL              habitat base (default PP_URL env, else https://pricklypear.rocks)
#   --user U --password P  creds (default PP_USER / PP_PASS env)
#   --force                override a LIVE non-stale claim lease
#   --dry-run              no side effects (overrides mode selection)
#
# Env / knobs:
#   PP_URL PP_USER PP_PASS (or PP_TOKEN for Bearer auth) - env vars, or
#   ~/.config/pricklypear/env; environment wins over the file
#   PP_FLOW_PI — the pi runner (default: `pi -p`); set to e.g. `true` to stub
#                the agent for tests:  PP_FLOW_PI=true ./scripts/pp-flow.sh --run <id>
#   PP_FLOW_LOG_DIR — where run logs + brief files go (default <repo>/.ralph)
#   PP_FLOW_CONTEXT_FILE — file whose lines REPLACE the brief's == CONTEXT ==
#     block (box-local repo/build/test truth, e.g. on vultr); unset = template
#
# Claim lease: the card's assignee is set to `executor:<pid>@<epoch>`. A lease
# is reclaimable when its pid is dead OR it is older than 12h (43200s);
# otherwise --run refuses to double-spawn unless --force. The lease is
# RELEASED (assignee cleared) on every exit path once the agent is done —
# success write-back, skipped/terminal write-back, or agent failure — so a
# finished card never lingers looking claimed.
set -u

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
DEFAULT_HOST="https://pricklypear.rocks"
STALE_SECS=43200   # 12h

# ---- resolve creds: environment wins over the secrets file; CLI wins over both ----
if [ -f "$HOME/.config/pricklypear/env" ]; then
  _pp_url="${PP_URL-}"; _pp_user="${PP_USER-}"; _pp_pass="${PP_PASS-}"
  set -a; . "$HOME/.config/pricklypear/env"; set +a
  [ -n "$_pp_url" ]  && PP_URL="$_pp_url"
  [ -n "$_pp_user" ] && PP_USER="$_pp_user"
  [ -n "$_pp_pass" ] && PP_PASS="$_pp_pass"
  unset _pp_url _pp_user _pp_pass
fi

URL="${PP_URL:-$DEFAULT_HOST}"
AUSER="${PP_USER:-admin}"
APASS="${PP_PASS:-}"
MODE="run"
FORCE=0
CARD=""
MSG=""
DRAIN_LIST=""      # --drain [list-id]: pick work automatically (queue-driven loop)
DRAIN_MAX=""       # --drain N: at most N cards this invocation
DRAIN_STATES=""    # --drain-states "A,B": actionable columns (default TODO,CODE)
DRAIN_EXCLUDE=""   # --drain-exclude "kind1,kind2"
WALK_MAX=""        # --walk-max N: gate budget for --walk (default 6)
SUPERVISED=0        # --supervised: sidecar mode (driver logic owned by the glochid supervisor)
BRIEF_SRC=""        # --brief-file F: the supervisor-rendered brief (supervised mode)
CLAIMED_STATE=""    # --claimed-state S: the card's state at claim time (supervised mode)
CARD_COLS=""        # --columns C: the card's column set for destination validation (supervised)
LOG_DIR_FLAG=""     # --log-dir D: per-run artifact dir (supervised mode)

while [ "$#" -gt 0 ]; do
  case "$1" in
    --run) MODE="run"; shift ;;
    --dry-run|--brief) MODE="dry-run"; shift ;;
    --claim) MODE="claim"; shift ;;
    --write-back) MODE="write-back"; shift ;;
    --drain) MODE="drain";
      # optional list-id / count as the next positional if it is not an option
      case "${2:-}" in
        ""|--*) : ;;
        *) case "$2" in
             *[!0-9]*) DRAIN_LIST="$2" ;;
             *) DRAIN_MAX="$2" ;;
           esac; shift ;;
      esac
      shift ;;
    --max) DRAIN_MAX="${2:-}"; shift 2 ;;
    --states) DRAIN_STATES="${2:-}"; shift 2 ;;
    --exclude-kinds) DRAIN_EXCLUDE="${2:-}"; shift 2 ;;
    --walk) MODE="walk"; shift ;;
    --walk-max) WALK_MAX="${2:-}"; shift 2 ;;
    --force) FORCE=1; shift ;;
    --supervised) SUPERVISED=1; shift ;;
    --brief-file) BRIEF_SRC="${2:-}"; shift 2 ;;
    --claimed-state) CLAIMED_STATE="${2:-}"; shift 2 ;;
    --columns) CARD_COLS="${2:-}"; shift 2 ;;
    --log-dir) LOG_DIR_FLAG="${2:-}"; shift 2 ;;
    --url) URL="${2:-$URL}"; shift 2 ;;
    --user) AUSER="${2:-$AUSER}"; shift 2 ;;
    --password|--pass) APASS="${2:-$APASS}"; shift 2 ;;
    --help|-h) sed -n '2,45p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    --*) echo "pp-flow: unknown option $1" >&2; exit 2 ;;
    *)
      if [ -z "$CARD" ]; then CARD="$1"
      else MSG="${MSG:-$1}"
      fi
      shift
      ;;
  esac
done

# --drain and --walk do not require a card; every other mode does.
# (--walk DOES: it walks exactly one named card through its gates.)
if [ -z "$CARD" ] && [ "$MODE" != "drain" ]; then
  echo "pp-flow: missing card id (see --help)" >&2
  exit 2
fi
[ -n "$APASS" ] || [ -n "${PP_TOKEN:-}" ] || { echo "pp-flow: no credentials (set PP_PASS, or PP_TOKEN for Bearer auth)" >&2; exit 2; }

# ---- eval plumbing -----------------------------------------------------------
# Every todo call runs inside (begin (load-library "todo") ...) so a fresh
# session still has the lib (and the executor badge is the usual load path).
pp_eval() { if [ -n "$APASS" ]; then "$SCRIPT_DIR/pp-eval.sh" -j --url "$URL" --user "$AUSER" --password "$APASS" -e "$1" 2>&1; else "$SCRIPT_DIR/pp-eval.sh" -j --url "$URL" -e "$1" 2>&1; fi; }
pp_todo_raw() { pp_eval "(begin (load-library \"todo\") $1)"; }
pp_todo_val() { if [ -n "$APASS" ]; then "$SCRIPT_DIR/pp-eval.sh" --url "$URL" --user "$AUSER" --password "$APASS" -e "(begin (load-library \"todo\") $1)" 2>/dev/null; else "$SCRIPT_DIR/pp-eval.sh" --url "$URL" -e "(begin (load-library \"todo\") $1)" 2>/dev/null; fi; }
# Nopales-safe string literal via POSIX awk (escape backslash, dquote, tab;
# join newline-separated records with \n). OpenBSD ksh has no ${v//pat/rep}.
# No interpreter one-liner, matching the repo's no-Python rule.
json_str() {
  # CR dropped; tab escaped; newline joins records so a single-line arg (card
  # ids!) never gains a trailing \n.
  printf '"%s"' "$(printf '%s' "$1" | tr -d '\r' | awk 'BEGIN { ORS = "" }
    NR > 1 { print "\\n" }
    { gsub(/\\/, "\\\\"); gsub(/"/, "\\\""); gsub(/\t/, "\\t"); print $0 }')"
}

card_assignee() { pp_todo_val "(todo/item/assignee (todo/item/get $1))"; }
brief_of()      { pp_todo_val "(todo/flow-brief $1)"; }
card_state()    { pp_todo_val "(todo/item/state (todo/item/get $1))"; }

# ---- claim lease ----------------------------------------------------------------
claim() {
  cur="$(card_assignee "$(json_str "$CARD")")"
  case "$cur" in
    executor:*@*)
      pid="${cur%%@*}"; pid="${pid#executor:}"
      ts="${cur#*@}"
      now="$(date +%s)"
      ago=$(( now - ts ))
      if [ "$FORCE" -eq 0 ] && kill -0 "$pid" 2>/dev/null && [ "$ago" -le "$STALE_SECS" ]; then
        echo "pp-flow: card $CARD already claimed by live run pid $pid (${ago}s old); use --force or wait" >&2
        return 3
      fi
      echo "pp-flow: reclaiming lease (pid $pid, ${ago}s old)" >&2
      ;;
    "") : ;;
    *)  echo "pp-flow: card $CARD has non-flow assignee \"$cur\"; claiming anyway" >&2 ;;
  esac
  lease="executor:$$@$(date +%s)"
    assigned="$(pp_todo_val "(todo/item/assign $(json_str "$CARD") $(json_str "$lease"))")"
    if [ -z "$assigned" ]; then
      echo "pp-flow: claim write failed (card missing or write denied)" >&2
      return 4
    fi
  echo "$lease"
}

# Release the claim lease (clear the assignee). The lease exists to prevent
# double-spawn WHILE the agent runs, not to mark ownership afterward: a run
# that ends but leaves executor:<pid>@<ts> on the card makes it look claimed
# until the 12h stale window passes. Called on every write-back exit path.
release_lease() { pp_todo_raw "(todo/item/unassign $1)" >/dev/null; }

# ---- batched eval helpers ---------------------------------------------------------
# Every habitat round trip costs ~1-2.5s (Basic-auth argon2 + wire), so the
# driver batches reads/writes into single (begin ...) evals wherever the
# decision logic stays in the shell. Field separator is "|"; every field is a
# uuid, a STATE token, or "ok"/"missing" — none can contain the separator.
# ONE eval: exists + current state of a card.
card_state_probe() { pp_todo_val "(begin (load-library \"todo\") (if (null? (todo/item/get $1)) (string-append \"missing\") (string-append \"ok|\" (as-str (todo/item/state (todo/item/get $1))))))"; }
# ONE eval: advance one column + evidence comment + release lease. Returns
# "event|comment|state" (state read AFTER the cycle, same eval). The lease
# release rides IN the batch: a write-back that forgets it resurrects the
# stale-lease bug this script already fixed once (card ac7ddff9, 09-27).
advance_batch() { pp_todo_val "(begin (load-library \"todo\") (string-append (as-str (todo/cycle $1)) \"|\" (as-str (todo/comment/add $1 $2)) \"|\" (as-str (todo/item/state (todo/item/get $1))) \"|\" (as-str (todo/item/unassign $1))))"; }
# ONE eval: set explicit destination + evidence comment + release lease.
# NOTE (autonomy hardening, 2026-09-27): the destination is mirrored back into
# the response with (as-str $3), NOT a literal "$3". A literal meant the shell
# passed an ALREADY json_str-quoted dest ("\"DONE\"") which interpolated into
# source as ""DONE"" — an unbound-symbol reader error. The batch therefore
# silently failed and fell back to single calls on EVERY explicit-destination
# write-back, which also meant dest_batch's in-batch lease release never ran
# (the exact stale-lease regression the fallback exists to prevent). Verified
# live on the AUTO lane: a LOGIC-REVIEW PASS with DESTINATION: DONE now goes
# LOGIC-REVIEW -> DONE in one round trip.
dest_batch() { pp_todo_val "(begin (load-library \"todo\") (string-append (as-str (todo/set-state $1 $3)) \"|\" (as-str (todo/comment/add $1 $2)) \"|\" (as-str $3) \"|\" (as-str (todo/item/unassign $1))))"; }
# ONE eval: evidence comment + release lease (card already left its claimed column).
comment_batch() { pp_todo_val "(begin (load-library \"todo\") (string-append (as-str (todo/comment/add $1 $2)) \"|\" (as-str (todo/item/unassign $1))))"; }
# ONE eval: state + brief + the card's valid columns together. The state is a
# short token; the run marker below is randomly generated so card content can
# never contain it. COLUMNS ride along because the destination validation needs
# them and a separate probe cost a whole extra round trip on every run that
# names a destination (found by the card 88a92237 AUTO-lane run: 6 -> 7 round
# trips). Same eval, same cost as before — the columns come free with the read.
# Output layout: <state>\n<MARK>\n<brief>\n<MARK>\n<cols>
COLSEP="PPCUTCOLS${RANDOM}${RANDOM}PPCUT"
state_brief_cols() {
  MARK="PPCUT${RANDOM}${RANDOM}PPCUT"
  pp_todo_val "(begin (load-library \"todo\") (let ((it (todo/item/get $1))) (string-append (as-str (todo/item/state it)) \"\\n$MARK\\n\" (as-str (todo/flow-brief $1)) \"\\n$COLSEP\\n\" (list/foldl (lambda (a c) (string-append a \",\" (as-str c))) \"\" (todo/state/columns-for-list (as-str (todo/item/list-id it)))))))"
}

# ONE eval: pick the next claimable card for an unattended drain loop.
# Returns "<card-id>|<state>|<title>" or empty when nothing is actionable.
# The picker is the QUEUE (todo/item/queue) filtered to: the requested list (or
# all lists), an ACTIONABLE column (default TODO,CODE — the columns whose
# recipes do actual work), no live lease, and not a card we have already
# terminalised. Claiming is left to the caller (claim()) so the lease/--force
# rules stay in ONE place.
#
# Why this lives here and not in Nopales: picking is a POLICY about the loop
# (what an unattended run is allowed to touch), the same reason the lease
# staleness rule lives in the driver. todo/item/queue already owns the ORDER
# (priority, then position, then id) — this is the eligibility filter on top.
drain_pick() { # $1 list-id-expr (or "")  $2 states-expr  $3 exclude-kinds-expr
  # The Lisp source is an unquoted heredoc so $1/$2/$3 interpolate directly.
  # DO NOT substitute via sed: sed's replacement text consumes backslashes, so
  # a JSON value like [\"TODO\"] silently became ["TODO"] — a reader error that
  # made the picker return NOTHING while actionable cards sat in the queue
  # (drain reported "no actionable card" against a board full of work). The
  # body interpolates ONLY the three numeric/JSON args below; it contains no
  # backticks and no bare $.
  src="$(cat <<LISP
(begin (load-library "todo")
  (let ((states (json-parse $2))
        (excl (json-parse $3))
        (want-list (as-str $1)))
    (let ((in? (lambda (x xs)
                 (list/foldl
                   (lambda (a c) (if (string-eq (as-str c) (as-str x)) #t a))
                   #f xs))))
      (let ((eligible
             (list/filter
               (lambda (row)
                 (let ((b (cdr row)))
                   (and (string-eq (todo/item/assignee b) "")
                        (or (string-eq want-list "")
                            (string-eq (todo/item/list-id b) want-list))
                        (in? (todo/item/state b) states)
                        (or (null? excl)
                            (not (in? (as-str (dict-get b "kind")) excl))))))
               (todo/item/queue ""))))
        (let ((first (car eligible)))
          (if (null? first)
              ""
              (string-append (as-str (car first))
                             "|" (todo/item/state (cdr first))
                             "|" (todo/item/title (cdr first)))))))))
LISP
)"
  pp_todo_val "$src"
}

# csv -> JSON array literal, for the --drain --states/--exclude-kinds options.
# Accepts a bare comma list ("TODO,CODE") or an already-JSON array; empty -> [].
csv_to_json() {
  case "$1" in
    '') printf '[]' ;;
    '['*) printf '%s' "$1" ;;   # caller passed JSON already
    *)
      printf '%s' "$1" | awk -F',' 'BEGIN{printf "["}
        { for (i=1;i<=NF;i++) { v=$i; gsub(/^[ \t]+|[ \t]+$/,"",v); if (v!="") { printf "%s\"%s\"", (n++?",":""), v } } }
        END{printf "]"}'
      ;;
  esac
}

# ---- run ledger + summary (observability) ---------------------------------------
# One JSON line per run-mode completion (success AND failure) in
# .ralph/runs.jsonl, plus a human RUN SUMMARY block — the efficiency/quality
# trend surface for scheduled automation (durations, evidence presence,
# destinations, failure rc).
ledger_line() { # $1 ts  $2 card  $3 runner  $4 total_s  $5 agent_s  $6 rc  $7 evidence  $8 dest  $9 from  $10 to
  printf '{"ts":"%s","card":"%s","runner":"%s","total_s":%s,"agent_s":%s,"agent_rc":%s,"evidence":"%s","destination":"%s","from_state":"%s","to_state":"%s"}\n' \
    "$1" "$2" "$(printf '%s' "$3" | tr -d '"\\')" "$4" "$5" "$6" "$7" "$8" "$9" "${10}" >> "$LOG_DIR/runs.jsonl"
}
run_summary() { # $1 card  $2 runner  $3 total_s  $4 agent_s  $5 rc  $6 evidence  $7 dest  $8 from  $9 to
  echo "pp-flow: RUN SUMMARY card=$1 runner='$2' total=${3}s agent=${4}s rc=$5 evidence=$6 destination=${7:-none} state=$8->${9:--}"
}

# ---- write-back: advance one column + post an evidence comment -------------------
write_back() {
  id="$1"
  msg="${2:-pp-flow: auto advanced the card to the next column}"
  dest="${3:-}"
  claimed_state="${4:-}"
  # ONE round trip: existence + current state. Existence first: a missing
  # card reports state TODO (default), and todo/comment/add happily orphans
  # a comment on it (verified live 09-25).
  probe="$(card_state_probe "$id")"
  case "$probe" in
    "missing")
      echo "pp-flow: card $id not found; not advancing, not commenting" >&2
      return 3 ;;
    "ok|"*) cur="$(printf '%s' "$probe" | cut -d'|' -f2-)" ;;  # ksh #strip: | is pattern alternation, ${probe#ok|} no-ops (turn-4 09-27)
    *)
      echo "pp-flow: unexpected state probe '$probe' for $id; not advancing" >&2
      return 3 ;;
  esac
  # Defense-in-depth: if the agent already moved the card off the state it was
  # claimed in (e.g. it wrote back itself despite the brief), do not advance
  # again — that was the double-write bug (TODO->DONE then DONE->PARKED).
  if [ -n "$claimed_state" ] && [ -n "$cur" ] && [ "$cur" != "$claimed_state" ]; then
    # Card already left the claimed column: only post the evidence comment.
    res="$(comment_batch "$id" "$(json_str "$msg")")"
    cmt="$(printf '%s' "$res" | awk -F'|' '{print $1}')"
    echo "pp-flow: card already advanced $claimed_state -> $cur; skipped transition, comment id=$cmt"
    return 0
  fi
  case "$cur" in
    DONE|PARKED|CANCELLED|ARCHIVED)
      # todo/cycle WRAPS at the end of the column set - advancing here would
      # resurrect a closed card back to TODO.
      release_lease "$id"
      echo "pp-flow: card $id is terminal ($cur); not advancing" >&2
      return 3 ;;
  esac
  # ONE round trip for the whole write set (transition + comment + lease
  # release); returns "event|comment|state|unassign".
  if [ -n "$dest" ]; then
    res="$(dest_batch "$id" "$(json_str "$msg")" "$(json_str "$dest")")"
  else
    res="$(advance_batch "$id" "$(json_str "$msg")")"
  fi
  if [ -z "$res" ]; then
    # Batch write failed mid-flight (card vanished, write denied, eval error):
    # fall back to the defensive single calls so the lease is never left behind.
    echo "pp-flow: batched write-back returned nothing; falling back to single calls" >&2
    release_lease "$id"
    return 4
  fi
  adv="$(printf '%s' "$res" | awk -F'|' '{print $1}')"
  cmt="$(printf '%s' "$res" | awk -F'|' '{print $2}')"
  newstate="$(printf '%s' "$res" | awk -F'|' '{print $3}')"
  if [ -z "$newstate" ]; then
    # 09-27 (gregor, run 3): a card on a list with no columns cycles to "" —
    # the state is ERASED, not wrapped. Restore the claimed state and refuse
    # the advance; the evidence comment above still posts (work happened).
    if [ -n "$claimed_state" ]; then
      pp_todo_val "(todo/set-state $id $(json_str "$claimed_state"))" >/dev/null
      echo "pp-flow: write emptied the state (list has no columns?); restored $claimed_state" >&2
    else
      echo "pp-flow: write emptied the state (list has no columns?); claimed_state unknown, card NOT restored - fix by hand" >&2
    fi
    newstate="$claimed_state"
  fi
  echo "pp-flow: write-back advanced to state=$newstate (event $adv), comment id=$cmt; claim released (in-batch)"
}

# extract an optional DESTINATION: <COL> from the agent transcript.
# Three-tier match (battle-tested 09-27, laptop + vultr): the anchored
# line-start form (OpenBSD sed: \{1,\}, not the GNU \+ extension) wins; then
# a trailing "... DESTINATION: <COL>" (executor-v2 folds it onto its one-line
# EVIDENCE paragraph); then the LAST mention anywhere (a reviewer wrote
# 'FAIL -> DESTINATION: CODE.' mid-sentence, card ac7ddff9) — in a terminal
# summary the final mention is the operative verdict.
agent_destination() {
  d="$(sed -n 's/^[[:space:]]*DESTINATION:[[:space:]]*\([A-Z0-9_-]\{1,\}\)[[:space:]]*$/\1/p' "$1" 2>/dev/null | tail -n1 | tr -d '[:space:]')"
  if [ -z "$d" ]; then
    d="$(sed -n 's/.*[^A-Z0-9_-]DESTINATION:[[:space:]]*\([A-Z0-9_-]\{1,\}\)[[:space:]]*$/\1/p' "$1" 2>/dev/null | tail -n1 | tr -d '[:space:]')"
  fi
  if [ -z "$d" ]; then
    d="$(grep -o 'DESTINATION:[[:space:]]*[A-Z][A-Z0-9_-]*' "$1" 2>/dev/null | tail -n1 | sed 's/.*:[[:space:]]*//')"
  fi
  # Tier 4 (autonomy hardening, 2026-09-27): structured-summary runners emit
  # the verdict as JSON ({"destination":"DONE"}) rather than the brief's literal
  # line. The autolith adapter bridges this, but pp-flow must not depend on a
  # particular runner normalizing for it. Same '[A-Z][A-Z0-9_-]*' shape as the
  # literal tiers; validated against the card's real columns before use.
  if [ -z "$d" ]; then
    d="$(tr '\n' ' ' < "$1" 2>/dev/null | grep -o '"destination"[[:space:]]*:[[:space:]]*"[A-Z][A-Z0-9_-]*"' | tail -n1 | sed 's/.*:[[:space:]]*"//; s/"$//')"
  fi
  printf '%s\n' "$d"
}

# Validate the agent's DESTINATION against the card's REAL column set before
# write_back honors it (autonomy hardening, 2026-09-27). The brief tells the
# agent to name the column it wants (an AUTO LOGIC-REVIEW PASS names DONE to
# skip the human gate), but that is agent-authored text: a hallucinated or
# typo'd column would be written verbatim by todo/set-state, silently moving a
# card into a state its list does not define. The columns were already read by
# state_brief_cols (folded into the claim-time eval), so this is PURE SHELL —
# no extra round trip. An earlier revision probed the habitat here, which cost
# a whole extra /api/eval on every run naming a destination (the 6 -> 7
# regression card 88a92237's CODE and LOGIC-REVIEW passes both caught).
# $1 candidate column  $2 the card's columns as ",A,B,C"
valid_destination() {
  cand="$1"
  cols="$2"
  [ -n "$cand" ] || return 0
  case ",$cols," in
    *",$cand,"*) printf '%s\n' "$cand"; return 0 ;;
  esac
  echo "pp-flow: agent DESTINATION '$cand' is not a column of this card's list ($cols); ignoring it and advancing one column" >&2
  return 0
}

# extract the last EVIDENCE: line from the agent transcript — the card comment
# IS this line, verbatim (workstreams claim-brief-protocol write-back step 4;
# the agent's EVIDENCE block is its contract). A transcript with no EVIDENCE:
# line degrades to a clear placeholder pointing at the log — never a
# mid-sentence tail scrape of the transcript.
agent_evidence() {
  # The flow-brief template embeds an 'EVIDENCE: <what you did ...>'
  # instruction line; a transcript that merely echoes the brief (or quotes the
  # template) must not be mistaken for the agent's evidence — template-shaped
  # lines (content starting with '<') are filtered before taking the last one.
  # shellcheck disable=SC2001
  ev="$(sed -n 's/^[[:space:]]*EVIDENCE:[[:space:]]*\(.*[^[:space:]]\)[[:space:]]*$/\1/p' "$1" 2>/dev/null | grep -v '^[[:space:]]*<' | tail -n1)"
  case "$ev" in
    "") echo "(no EVIDENCE: line in the agent transcript — full log: $LOG_FILE)" ;;
    *)  printf '%s\n' "$ev" | cut -c1-500 ;;
  esac
}

# ---- modes -----------------------------------------------------------------------
LOG_DIR="${LOG_DIR_FLAG:-${PP_FLOW_LOG_DIR:-$ROOT/.ralph}}"
mkdir -p "$LOG_DIR" 2>/dev/null || true
SHORT="$(echo "$CARD" | cut -c1-8)"

case "$MODE" in
  dry-run)
    b="$(brief_of "$(json_str "$CARD")")"
    if [ -z "$b" ]; then
      echo "pp-flow: no such card $CARD" >&2
      exit 1
    fi
    printf '%s\n' "$b"
    exit 0
    ;;
  claim)
    lease="$(claim)" || exit $?
    echo "$lease"
    exit 0
    ;;
  write-back)
    write_back "$(json_str "$CARD")" "$MSG" || exit $?
    exit 0
    ;;
  drain)
    # UNATTENDED DRAIN (autonomy hardening, 2026-09-27): the missing half of
    # the claim/brief loop. Until now every run needed a human to name a card,
    # so "let gregor/the habitat run it" meant a person in the loop. Drain
    # picks claimable cards itself: queue order (priority -> position -> id),
    # filtered to actionable columns and unleased cards.
    #
    # Each card is executed as a SEPARATE `pp-flow --run` subprocess on
    # purpose: that preserves the property the AUTO lane's safety story rests
    # on (borge/workstreams.borg stream-pipelines) — every column transition
    # is an independent run, so a LOGIC-REVIEW reviewer is never the session
    # that implemented the change. Sharing one process would collapse that.
    #
    # A FAILED card STOPS the drain (default): continuing would burn leases on
    # cards whose work may share the same root cause (a provider outage, a bad
    # build). Set PP_FLOW_DRAIN_CONTINUE=1 to press on instead. Exit code is
    # the count of failures seen (capped at 125) so a scheduler can alert.
    dlist="$(json_str "${DRAIN_LIST}")"
    dstates="$(json_str "$(csv_to_json "${DRAIN_STATES:-TODO,CODE}")")"
    dexcl="$(json_str "$(csv_to_json "${DRAIN_EXCLUDE:-}")")"
    dmax="${DRAIN_MAX:-1}"
    case "$dmax" in ''|*[!0-9]*) dmax=1 ;; esac
    echo "pp-flow: drain list=${DRAIN_LIST:-<all>} states=${DRAIN_STATES:-TODO,CODE} max=$dmax continue=${PP_FLOW_DRAIN_CONTINUE:-0}" >&2
    ran=0; failed=0; n=0
    while [ "$n" -lt "$dmax" ]; do
      n=$(( n + 1 ))
      pick="$(drain_pick "$dlist" "$dstates" "$dexcl")"
      case "$pick" in
        "") echo "pp-flow: drain: no actionable card" >&2; break ;;
      esac
      did="$(printf '%s' "$pick" | awk -F'|' '{print $1}')"
      dstate="$(printf '%s' "$pick" | awk -F'|' '{print $2}')"
      dtitle="$(printf '%s' "$pick" | cut -d'|' -f3-)"
      if [ -z "$did" ]; then echo "pp-flow: drain: unparseable pick '$pick'" >&2; break; fi
      echo "pp-flow: drain: picked $did [$dstate] $dtitle" >&2
      # separate process = separate run = fresh reviewer if this column is a review
      # Forward creds/URL explicitly: the child is a fresh process and a
      # caller who passed --url/--user/--password (rather than exporting them)
      # would otherwise have the child fall back to the secrets file/defaults
      # — reading a card from one habitat and writing back to another.
      "$0" --run "$did" --url "$URL" --user "$AUSER" --password "$APASS"
      drc=$?
      ran=$(( ran + 1 ))
      if [ "$drc" -ne 0 ]; then
        failed=$(( failed + 1 ))
        echo "pp-flow: drain: card $did failed (rc=$drc)" >&2
        if [ "${PP_FLOW_DRAIN_CONTINUE:-0}" != "1" ]; then
          echo "pp-flow: drain: stopping (set PP_FLOW_DRAIN_CONTINUE=1 to press on)" >&2
          break
        fi
      fi
    done
    echo "pp-flow: drain: ran=$ran failed=$failed" >&2
    [ "$failed" -gt 125 ] && failed=125
    exit "$failed"
    ;;
  walk)
    # --walk: drive ONE named card forward through ITS gates until it reaches
    # a terminal/human state, an already-visited column (a loop), or the gate
    # budget. This is the missing half of the card lifecycle: --drain picks
    # work ACROSS the board by state, so it cannot be aimed at one card — a
    # queued TODO would win the pick first. --walk is how you actually take a
    # card through TODO -> CODE -> LOGIC-REVIEW -> UI-REVIEW -> HUMAN-REVIEW
    # in one command.
    #
    # Each gate is a SEPARATE `--run` subprocess for the same reason the drain
    # does it (borge/workstreams.borg stream-pipelines): the LOGIC-REVIEW
    # reviewer is then never the session that implemented the change. Sharing
    # one process would collapse the structural independence the AUTO lane's
    # safety story rests on.
    #
    # The loop is bounded by BOTH a gate budget and a VISITED set. The visited
    # set is what catches a FAIL ping-pong (CODE -> LOGIC-REVIEW -> CODE -> ...):
    # without it, --walk-max would just burn N runs rediscovering the same
    # defect instead of handing the card to a human.
    wmax="${WALK_MAX:-6}"
    case "$wmax" in ''|*[!0-9]*) wmax=6 ;; esac
    echo "pp-flow: walk card=$CARD max=$wmax (each gate is a separate run)" >&2
    visited=""
    wg=0
    walk_failed=0
    while [ "$wg" -lt "$wmax" ]; do
      wg=$(( wg + 1 ))
      wstate="$(card_state "$(json_str "$CARD")")"
      if [ -z "$wstate" ]; then
        echo "pp-flow: walk: card $CARD not found" >&2
        walk_failed=1
        break
      fi
      case "$wstate" in
        DONE|PARKED|CANCELLED|ARCHIVED|HUMAN-REVIEW)
          echo "pp-flow: walk: reached $wstate — handing off (no further gates)" >&2
          break ;;
      esac
      case " $visited " in
        *" $wstate "*)
          echo "pp-flow: walk: $wstate already visited this walk — a review bounced back, stopping for a human (not burning more runs on the same defect)" >&2
          break ;;
      esac
      visited="$visited $wstate"
      echo "pp-flow: walk: gate $wg/$wmax at column $wstate" >&2
      "$0" --run "$CARD" --url "$URL" --user "$AUSER" --password "$APASS"
      wrc=$?
      if [ "$wrc" -ne 0 ]; then
        echo "pp-flow: walk: card $CARD failed at gate $wg (column $wstate, rc=$wrc)" >&2
        walk_failed=1
        break
      fi
    done
    wfinal="$(card_state "$(json_str "$CARD")")"
    echo "pp-flow: walk: done — card=$CARD gates_run=$wg final=$wfinal visited='$visited'" >&2
    [ "$walk_failed" -eq 0 ] || exit "$wrc"
    exit 0
    ;;
  run)
    T0=$(date +%s)
    if [ "$SUPERVISED" = "1" ]; then
      # SUPERVISED SIDECAR (run-primitive phase 2): the supervisor (the glochid
      # app) owns claims, the brief render, and every card write. This sidecar
      # shapes the supplied brief, runs the adapter, extracts the verdict, and
      # reports PPFLOW-* lines on stdout — it NEVER touches the board (the
      # executor-never-touches-the-board rule).
      echo "pp-flow: supervised run card=$CARD (claim/brief/write-back owned by the supervisor)"
      claimed_state="$CLAIMED_STATE"
      card_cols="$CARD_COLS"
      if [ -z "$BRIEF_SRC" ] || [ ! -r "$BRIEF_SRC" ]; then
        echo "pp-flow: supervised brief missing or unreadable: $BRIEF_SRC" >&2
        exit 2
      fi
      b="$(cat "$BRIEF_SRC")"
      if [ -z "$b" ]; then
        echo "pp-flow: supervised brief is empty: $BRIEF_SRC" >&2
        exit 2
      fi
    else
      lease="$(claim)" || exit $?
      echo "pp-flow: claimed $CARD lease=$lease"
      # ONE round trip for claimed state + brief + columns. The state rides
      # first so the double-write defense gets a real baseline (gregor run-3
      # fix); the columns ride last so valid_destination can check the agent's
      # named column WITHOUT a second habitat probe (see state_brief_cols).
      sb="$(state_brief_cols "$(json_str "$CARD")")"
      claimed_state="$(printf '%s\n' "$sb" | sed -n '1p')"
      # strip the trailing COLSEP + columns off the end, leaving the brief
      card_cols="$(printf '%s\n' "$sb" | awk -v sep="$COLSEP" 'f{print;next} $0==sep{f=1}')"
      b="$(printf '%s\n' "$sb" | awk -v sep="$COLSEP" '$0==sep{exit} {print}' | sed -n '3,$p')"
      if [ -z "$b" ]; then
        echo "pp-flow: no such card $CARD after claim" >&2
        exit 1
      fi
    fi
    # MACHINE-LOCAL CONTEXT OVERRIDE (vultr, 2026-09-28): the flow-brief
    # template hardcodes roerick's laptop for == CONTEXT == (repo path, opam
    # switch). An executor on THIS box cannot build there. PP_FLOW_CONTEXT_FILE
    # (set by the caller) replaces that block's lines with box-local truth;
    # unset renders the template verbatim, so the laptop path is unchanged.
    if [ -n "${PP_FLOW_CONTEXT_FILE:-}" ] && [ -r "$PP_FLOW_CONTEXT_FILE" ]; then
      # shellcheck disable=SC2016
      b="$(printf '%s\n' "$b" | awk -v ctxfile="$PP_FLOW_CONTEXT_FILE" '
        /^== CONTEXT ==$/ { print; inctx=1
          while ((getline line < ctxfile) > 0) print line
          close(ctxfile); next }
        inctx && /^== EVIDENCE / { inctx=0 }
        !inctx { print }')"
      echo "pp-flow: CONTEXT overridden from $PP_FLOW_CONTEXT_FILE" >&2
    fi
    BRIEF_FILE="$LOG_DIR/flow-$SHORT.brief.md"
    printf '%s\n' "$b" > "$BRIEF_FILE"
    # TERMINAL CONTRACT, appended by the DRIVER (autonomy hardening,
    # 2026-09-27): the per-column recipe already asks for a verdict, but a
    # reviewer answered the question and never rendered one (card 88a92237),
    # so the card advanced with no decision. The recipe is card CONTENT (lives
    # in the list datum, editable, and a fresh list may not have one at all);
    # putting the last word HERE makes the contract structural for every
    # column and every list, including ones with no recipe configured. It is
    # phrased as a hard requirement and names the exact machine-read shape.
    case "$claimed_state" in
      *REVIEW*)
        printf '\n== MANDATORY VERDICT (driver contract) ==\nYour run returns a STRUCTURED object: the fields `verdict` (= PASS | FAIL |\nESCALATE) and `evidence` are REQUIRED — a response without them is REJECTED\nby the provider and the review is treated as aborted.\n  verdict: PASS      = the work meets its acceptance criteria (name the target\n                       column in `destination` when the recipe calls for one,\n                       e.g. DONE on the AUTO lane or HUMAN-REVIEW elsewhere)\n  verdict: FAIL      = set destination: CODE and say exactly what to fix\n  verdict: ESCALATE  = repeat failure / needs a human: destination:\n                       HUMAN-REVIEW, explain why\n`evidence` = what you checked AND how (the command you ran and its result).\nDo NOT answer the review without rendering a verdict.\n' >> "$BRIEF_FILE"
        ;;
      *)
        printf '\n== MANDATORY RESULT FIELDS (driver contract) ==\nYour run returns a STRUCTURED object: `verdict` and `evidence` are REQUIRED\nfields (a response without them is REJECTED by the provider).\n  verdict:   PASS for completed work, FAIL when it does not meet the criteria,\n             ESCALATE when a human is needed.\n  destination: the target column, when this column recipe names one.\n  evidence:  what you did AND how you verified it (command + result).\n' >> "$BRIEF_FILE"
        ;;
    esac
    echo "pp-flow: brief -> $BRIEF_FILE"
    PI_RUNNER="${PP_FLOW_PI:-pi -p}"
    LOG_FILE="$LOG_DIR/flow-$SHORT.log"
    echo "pp-flow: running agent ($PI_RUNNER) on $CARD ..." >&2
    A0=$(date +%s)
    if [ "$PI_RUNNER" = "true" ]; then
      # explicit test stub: record the brief as the 'transcript', succeed
      cp "$BRIEF_FILE" "$LOG_FILE"
      rc=0
    elif ! command -v "${PI_RUNNER%% *}" >/dev/null 2>&1; then
      # missing runner is a CONFIG error, not a successful no-op
      echo "pp-flow: runner not found: $PI_RUNNER (set PP_FLOW_PI)" >&2
      rc=127
    else
      # shellcheck disable=SC2086
      $PI_RUNNER "$(cat "$BRIEF_FILE")" > "$LOG_FILE" 2>&1
      rc=$?
    fi
    A1=$(date +%s); AGENT_S=$(( A1 - A0 ))
    echo "pp-flow: agent rc=$rc (${AGENT_S}s); transcript -> $LOG_FILE" >&2
    evid="$(agent_evidence "$LOG_FILE")"
    dest="$(agent_destination "$LOG_FILE")"
    # Never obey an unvalidated column: the agent names the destination, but
    # the card's real pipeline has the final say (an AUTO PASS naming DONE is
    # honored; a hallucinated column falls back to the one-column advance).
    dest="$(valid_destination "$dest" "$card_cols")"
    # NO-VERDICT DETECTION (autonomy hardening, 2026-09-27): a REVIEW column's
    # whole contract is a VERDICT (PASS -> DESTINATION, FAIL -> DESTINATION),
    # but a structured-summary model can answer the review question and simply
    # never render one. Observed live on the AUTO lane (card 88a92237): the
    # LOGIC-REVIEW reviewer produced a thorough, correct analysis — and no
    # verdict — so the card advanced one column and parked in HUMAN-REVIEW,
    # indistinguishable from a deliberate escalation. Silent ambiguity is the
    # dangerous part in an unattended loop, so name it: a review that rendered
    # no destination gets a clearly-marked comment and a stderr warning. The
    # advance is still the conservative one-column step (for a stream WITH a
    # human gate that is the correct review->human handoff; for AUTO the card
    # lands in HUMAN-REVIEW, i.e. it fails SAFE toward a human rather than
    # closing a card on a verdict that was never given).
    noverdict=0
    case "$claimed_state" in
      *REVIEW*)
        if [ -z "$dest" ]; then
          noverdict=1
          echo "pp-flow: WARNING review column '$claimed_state' rendered NO verdict (no DESTINATION line); advancing one column conservatively rather than closing" >&2
          if [ "$SUPERVISED" != "1" ]; then
            evid="pp-flow: [NO VERDICT RENDERED — the reviewer did not name a DESTINATION. Advanced one column conservatively; read the summary below and re-run if a decision is needed.] $evid"
          fi
          # supervised: the RAW evidence rides PPFLOW-EVIDENCE; the supervisor
          # applies the POLICY-3 prefix itself when it writes back.
        fi
        ;;
    esac
    if [ "$SUPERVISED" = "1" ]; then
      # supervisor contract (run-primitive phase 2): verdict + adapter usage
      # ride stdout as single machine lines — the supervisor captures them
      # from driver.log and lands them on the run row (the usage pass-through).
      rep="$(tr -d '\n' < "$LOG_FILE" 2>/dev/null | sed -n 's/.*USAGE-REPORT //p' | tail -1)"
      u_status=""; u_cat=""; u_trace=""; u_req=0; u_in=0; u_cached=0; u_out=0; u_wall=0; u_msg=""
      for kv in $rep; do
        case "$kv" in
          status=*) u_status="${kv#status=}" ;;
          category=*) u_cat="${kv#category=}" ;;
          trace=*) u_trace="${kv#trace=}" ;;
          requests=*) u_req="${kv#requests=}" ;;
          input_tokens=*) u_in="${kv#input_tokens=}" ;;
          cached_tokens=*) u_cached="${kv#cached_tokens=}" ;;
          output_tokens=*) u_out="${kv#output_tokens=}" ;;
          wall_s=*) u_wall="${kv#wall_s=}" ;;
        esac
      done
      case "$rep" in
        *" message="*) u_msg="${rep##* message=}" ;;
      esac
      echo "pp-flow: PPFLOW-DEST $dest"
      echo "pp-flow: PPFLOW-EVIDENCE $evid"
      echo "pp-flow: PPFLOW-USAGE requests=$u_req input_tokens=$u_in cached_tokens=$u_cached output_tokens=$u_out wall_s=$u_wall trace=$u_trace"
      echo "pp-flow: PPFLOW-STATUS status=$u_status category=$u_cat trace=$u_trace message=$u_msg"
    fi
    if [ "$rc" -ne 0 ]; then
      # OBSERVABILITY: a failed run used to vanish (claim released, no card
      # trace). Post a failure comment so the card feed shows every run —
      # in supervised mode the supervisor owns this comment instead.
      if [ "$SUPERVISED" != "1" ]; then
        tailtxt="$(tail -c 300 "$LOG_FILE" 2>/dev/null | tr -d '\r' | tr '\n' ' ' | tr -d '\033' | tr -d '"\\')"
        fmsg="pp-flow: agent FAILED rc=$rc (runner: $PI_RUNNER, ${AGENT_S}s). Log: $LOG_FILE. Tail: $tailtxt"
        pp_todo_raw "(begin (load-library \"todo\") (todo/comment/add $(json_str "$CARD") $(json_str "$fmsg")) (todo/item/unassign $(json_str "$CARD")))" >/dev/null
      fi
      T1=$(date +%s)
      ledger_line "$(date -u +%FT%TZ)" "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "no" "" "$claimed_state" ""
      run_summary "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "no" "" "$claimed_state" ""
      if [ "$SUPERVISED" = "1" ]; then
        echo "pp-flow: agent failed (rc=$rc); verdict + usage reported to the supervisor; card writes owned by it" >&2
      else
        echo "pp-flow: agent failed (rc=$rc); card NOT advanced, failure comment posted, claim released — fix and re-run" >&2
      fi
      exit "$rc"
    fi
    case "$evid" in
      "("*")") EVFLAG=no ;;  # the placeholder, not real evidence
      *) EVFLAG=yes ;;
    esac
    if [ "$SUPERVISED" = "1" ]; then
      # supervised: the supervisor owns the write-back (transition + evidence
      # comment + lease release) and the run-row finish; this sidecar stops here.
      T1=$(date +%s)
      nodest="$dest"; [ "$noverdict" = 1 ] && nodest="(no-verdict)"
      ledger_line "$(date -u +%FT%TZ)" "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "$EVFLAG" "$nodest" "$claimed_state" ""
      run_summary "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "$EVFLAG" "$nodest" "$claimed_state" ""
      echo "pp-flow: supervised run complete for $CARD (write-back owned by the supervisor)"
      exit 0
    fi
    # verbatim: the posted comment IS the agent's EVIDENCE line. The claim
    # lease is released inside write_back on every exit path (in-batch), so
    # a successfully written-back card ends with an empty assignee.
    write_back "$(json_str "$CARD")" "$evid" "$dest" "$claimed_state"
    wbrc=$?
    final_state="$(card_state "$(json_str "$CARD")")"
    T1=$(date +%s)
    # NOTE: test the VALUE, not ${noverdict:+...} — noverdict is initialized to
    # 0, and "0" is a non-empty string, so a :+ expansion flagged EVERY run as
    # no-verdict (caught by the post-change regression run: a plain TODO->CODE
    # reported "destination= (no-verdict)").
    nodest="$dest"
    [ "$noverdict" = 1 ] && nodest="(no-verdict)"
    ledger_line "$(date -u +%FT%TZ)" "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "$EVFLAG" "$dest" "$claimed_state" "$final_state"
    run_summary "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "$EVFLAG" "$nodest" "$claimed_state" "$final_state"
    echo "pp-flow: write-back complete for $CARD"
    [ "$wbrc" -eq 0 ] || exit "$wbrc"
    ;;
esac
