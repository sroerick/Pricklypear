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

# ---- write-back: advance one column + post an evidence comment -------------------
write_back() {
  id="$1"
  msg="${2:-pp-flow: auto advanced the card to the next column}"
  dest="${3:-}"
  claimed_state="${4:-}"
  # Existence first: a missing card reports state TODO (default), and
  # todo/comment/add happily orphans a comment on it (verified live 09-25).
  exists="$(pp_todo_val "(if (null? (todo/item/get $id)) \"missing\" \"ok\")")"
  if [ "$exists" != "ok" ]; then
    echo "pp-flow: card $id not found; not advancing, not commenting" >&2
    return 3
  fi
  cur="$(pp_todo_val "(todo/item/state (todo/item/get $id))")"
  # Defense-in-depth: if the agent already moved the card off the state it was
  # claimed in (e.g. it wrote back itself despite the brief), do not advance
  # again — that was the double-write bug (TODO->DONE then DONE->PARKED).
  if [ -n "$claimed_state" ] && [ -n "$cur" ] && [ "$cur" != "$claimed_state" ]; then
    # Card already left the claimed column: only post the evidence comment.
    cmt="$(pp_todo_val "(todo/comment/add $id $(json_str "$msg"))")"
    echo "pp-flow: card already advanced $claimed_state -> $cur; skipped transition, comment id=$cmt"
    return 0
  fi
  case "$cur" in
    DONE|PARKED|CANCELLED|ARCHIVED)
      # todo/cycle WRAPS at the end of the column set - advancing here would
      # resurrect a closed card back to TODO.
      echo "pp-flow: card $id is terminal ($cur); not advancing" >&2
      return 3 ;;
  esac
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
    echo "pp-flow: agent rc=$rc; transcript -> $LOG_FILE" >&2
    if [ "$rc" -ne 0 ]; then
      pp_todo_raw "(todo/item/unassign $(json_str "$CARD"))" >/dev/null
      echo "pp-flow: agent failed (rc=$rc); card NOT advanced, claim released — fix and re-run" >&2
      exit "$rc"
    fi
    evid="$(agent_evidence "$LOG_FILE")"
    dest="$(agent_destination "$LOG_FILE")"
    # verbatim: the posted comment IS the agent's EVIDENCE line (the claim
    # lease on the assignee field already carries the executor pid).
    write_back "$(json_str "$CARD")" "$evid" "$dest" "$claimed_state"
    echo "pp-flow: write-back complete for $CARD"
    ;;
esac
