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
#   pp-flow.sh --help
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

while [ "$#" -gt 0 ]; do
  case "$1" in
    --run) MODE="run"; shift ;;
    --dry-run|--brief) MODE="dry-run"; shift ;;
    --claim) MODE="claim"; shift ;;
    --write-back) MODE="write-back"; shift ;;
    --force) FORCE=1; shift ;;
    --url) URL="${2:-$URL}"; shift 2 ;;
    --user) AUSER="${2:-$AUSER}"; shift 2 ;;
    --password|--pass) APASS="${2:-$APASS}"; shift 2 ;;
    --help|-h) sed -n '2,33p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    --*) echo "pp-flow: unknown option $1" >&2; exit 2 ;;
    *)
      if [ -z "$CARD" ]; then CARD="$1"
      else MSG="${MSG:-$1}"
      fi
      shift
      ;;
  esac
done

if [ -z "$CARD" ]; then
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
dest_batch() { pp_todo_val "(begin (load-library \"todo\") (string-append (as-str (todo/set-state $1 $3)) \"|\" (as-str (todo/comment/add $1 $2)) \"|\" \"$3\" \"|\" (as-str (todo/item/unassign $1))))"; }
# ONE eval: evidence comment + release lease (card already left its claimed column).
comment_batch() { pp_todo_val "(begin (load-library \"todo\") (string-append (as-str (todo/comment/add $1 $2)) \"|\" (as-str (todo/item/unassign $1))))"; }
# ONE eval: state + brief together (state is a short token; the run marker
# below is randomly generated so card content can never contain it).
state_and_brief() {
  MARK="PPCUT${RANDOM}${RANDOM}PPCUT"
  pp_todo_val "(begin (load-library \"todo\") (string-append (as-str (todo/item/state (todo/item/get $1))) \"\\n$MARK\\n\" (as-str (todo/flow-brief $1))))" | awk -v m="$MARK" 'BEGIN{rs=1} rs==1{ if (index($0,m)==1) {rs=0; sub("^"m"\\n","")} else print; next } {print}'
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
    "ok|"*) cur="${probe#ok|}" ;;
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
  printf '%s\n' "$d"
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
LOG_DIR="${PP_FLOW_LOG_DIR:-$ROOT/.ralph}"
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
  run)
    T0=$(date +%s)
    lease="$(claim)" || exit $?
    echo "pp-flow: claimed $CARD lease=$lease"
    # ONE round trip for claimed state + brief (split on a random marker line
    # card content can never contain). The state rides first so the
    # double-write defense gets a real baseline (gregor run-3 fix).
    sb="$(state_and_brief "$(json_str "$CARD")")"
    claimed_state="$(printf '%s\n' "$sb" | sed -n '1p')"
    b="$(printf '%s\n' "$sb" | sed -n "2,\$p")"
    if [ -z "$b" ]; then
      echo "pp-flow: no such card $CARD after claim" >&2
      exit 1
    fi
    BRIEF_FILE="$LOG_DIR/flow-$SHORT.brief.md"
    printf '%s\n' "$b" > "$BRIEF_FILE"
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
    if [ "$rc" -ne 0 ]; then
      # OBSERVABILITY: a failed run used to vanish (claim released, no card
      # trace). Post a failure comment so the card feed shows every run.
      tailtxt="$(tail -c 300 "$LOG_FILE" 2>/dev/null | tr -d '\r' | tr '\n' ' ' | tr -d '\033' | tr -d '"\\')"
      fmsg="pp-flow: agent FAILED rc=$rc (runner: $PI_RUNNER, ${AGENT_S}s). Log: $LOG_FILE. Tail: $tailtxt"
      pp_todo_raw "(begin (load-library \"todo\") (todo/comment/add $(json_str "$CARD") $(json_str "$fmsg")) (todo/item/unassign $(json_str "$CARD")))" >/dev/null
      T1=$(date +%s)
      ledger_line "$(date -u +%FT%TZ)" "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "no" "" "$claimed_state" ""
      run_summary "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "no" "" "$claimed_state" ""
      echo "pp-flow: agent failed (rc=$rc); card NOT advanced, failure comment posted, claim released — fix and re-run" >&2
      exit "$rc"
    fi
    case "$evid" in
      "("*")") EVFLAG=no ;;  # the placeholder, not real evidence
      *) EVFLAG=yes ;;
    esac
    # verbatim: the posted comment IS the agent's EVIDENCE line. The claim
    # lease is released inside write_back on every exit path (in-batch), so
    # a successfully written-back card ends with an empty assignee.
    write_back "$(json_str "$CARD")" "$evid" "$dest" "$claimed_state"
    wbrc=$?
    final_state="$(card_state "$(json_str "$CARD")")"
    T1=$(date +%s)
    ledger_line "$(date -u +%FT%TZ)" "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "$EVFLAG" "$dest" "$claimed_state" "$final_state"
    run_summary "$CARD" "$PI_RUNNER" "$(( T1 - T0 ))" "$AGENT_S" "$rc" "$EVFLAG" "$dest" "$claimed_state" "$final_state"
    echo "pp-flow: write-back complete for $CARD"
    [ "$wbrc" -eq 0 ] || exit "$wbrc"
    ;;
esac
