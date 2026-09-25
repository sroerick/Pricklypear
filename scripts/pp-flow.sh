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
#   PP_URL PP_USER PP_PASS — set explicitly or in ~/.config/pricklypear/env
#   PP_FLOW_PI — the pi runner (default: `pi -p`); set to e.g. `true` to stub
#                the agent for tests:  PP_FLOW_PI=true ./scripts/pp-flow.sh --run <id>
#   PP_FLOW_LOG_DIR — where run logs + brief files go (default <repo>/.ralph)
#
# Claim lease: the card's assignee is set to `executor:<pid>@<epoch>`. A lease
# is reclaimable when its pid is dead OR it is older than 12h (43200s);
# otherwise --run refuses to double-spawn unless --force.
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
[ -n "$APASS" ] || { echo "pp-flow: no password (set PP_PASS or --password)" >&2; exit 2; }

# ---- eval plumbing -----------------------------------------------------------
# Every todo call runs inside (begin (load-library "todo") ...) so a fresh
# session still has the lib (and the executor badge is the usual load path).
pp_eval() { "$SCRIPT_DIR/pp-eval.sh" -j --url "$URL" --user "$AUSER" --password "$APASS" -e "$1" 2>&1; }
pp_todo_raw() { pp_eval "(begin (load-library \"todo\") $1)"; }
pp_todo_val() { pp_todo_raw "$1" | jq -r '.value // empty' 2>/dev/null; }

# JSON-encode a shell string into a Nopales string literal (quotes/newlines safe).
json_str() { python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1"; }

card_assignee() { pp_todo_val "(todo/item/assignee (todo/item/get $1))"; }
brief_of()      { pp_todo_val "(todo/flow-brief $1)"; }

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
  pp_todo_raw "(todo/item/assign $(json_str "$CARD") $(json_str "$lease"))" >/dev/null
  echo "$lease"
}

# ---- write-back: advance one column + post an evidence comment -------------------
write_back() {
  id="$1"
  msg="$2"
  dest="${3:-}"
  claimed_state="${4:-}"
  # Defense-in-depth: if the agent already moved the card off the state it was
  # claimed in (e.g. it wrote back itself despite the brief), do not advance
  # again — that was the double-write bug (TODO->DONE then DONE->PARKED).
  cur="$(pp_todo_val "(todo/item/state (todo/item/get $id))")"
  if [ -n "$claimed_state" ] && [ -n "$cur" ] && [ "$cur" != "$claimed_state" ]; then
    # Card already left the claimed column: only post the evidence comment.
    cmt="$(pp_todo_val "(todo/comment/add $id $(json_str "$msg"))")"
    echo "pp-flow: card already advanced $claimed_state -> $cur; skipped transition, comment id=$cmt"
    return 0
  fi
  if [ -n "$dest" ]; then
    adv="$(pp_todo_val "(todo/set-state $id $(json_str "$dest"))")"
    newstate="$dest"
  else
    adv="$(pp_todo_val "(todo/cycle $id)")"
    newstate="$(pp_todo_val "(todo/item/state (todo/item/get $id))")"
  fi
  cmt="$(pp_todo_val "(todo/comment/add $id $(json_str "$msg"))")"
  echo "pp-flow: write-back advanced to state=$newstate (event $adv), comment id=$cmt"
}

# extract an optional DESTINATION: <COL> line from the agent transcript
agent_destination() {
  # shellcheck disable=SC2001
  sed -n 's/^[[:space:]]*DESTINATION:[[:space:]]*\([A-Z0-9_-]\+\)[[:space:]]*$/\1/p' "$1" 2>/dev/null | tail -n1 | tr -d '[:space:]'
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
    write_back "$(json_str "$CARD")" "$MSG"
    exit 0
    ;;
  run)
    lease="$(claim)" || exit $?
    echo "pp-flow: claimed $CARD lease=$lease"
    claimed_state="$(pp_todo_val "(todo/item/state (todo/item/get $CARD))")"
    b="$(brief_of "$(json_str "$CARD")")"
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
    if [ "$PI_RUNNER" = "true" ] || ! command -v "${PI_RUNNER%% *}" >/dev/null 2>&1; then
      # stub / missing runner: record the brief as the 'transcript'
      cp "$BRIEF_FILE" "$LOG_FILE"
      rc=0
    else
      # shellcheck disable=SC2086
      $PI_RUNNER "$(cat "$BRIEF_FILE")" > "$LOG_FILE" 2>&1
      rc=$?
    fi
    echo "pp-flow: agent rc=$rc; transcript -> $LOG_FILE" >&2
    if [ "$rc" -ne 0 ]; then
      echo "pp-flow: agent failed (rc=$rc); NOT advancing the card — fix and re-run" >&2
      exit "$rc"
    fi
    evid="$(tail -c 1200 "$LOG_FILE" | tr '\n' ' ' | tr -s ' ' | cut -c1-500)"
    dest="$(agent_destination "$LOG_FILE")"
    write_back "$(json_str "$CARD")" "executor pid $$: $evid" "$dest" "$claimed_state"
    echo "pp-flow: write-back complete for $CARD"
    ;;
esac
