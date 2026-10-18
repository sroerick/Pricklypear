#!/bin/ksh
# pp-runs.sh — read-only monitor for the PP run-row control plane
# (borge/run-primitive.borg). Answers: is the factory running, where is the
# queue, what is it burning runs on, which cards is it replaying, and which
# run ids can never resolve.
#
# The habitat is the record, so this is read-only evals plus jq: no claim, no
# dispatch, no write-back, no sidecar. The supervisor's own tick log lives on
# its host, not here:
#   ssh <host> 'tail -40 /home/al/glochid/glochid.log'
#   ssh <host> 'ls -lat ~/pricklypear/.ralph/supervisor | head'
#
# Usage: scripts/pp-runs.sh [board-list-id]
#
# Env: PP_URL / PP_USER / PP_PASS (~/.config/pricklypear/env is read, the
# environment wins), PP_EVAL for the client, and optionally
# PP_RUNS_SUPERVISOR_SSH=user@host [+ PP_RUNS_SUPERVISOR_LOG=/path/to/log]
# to append the supervisor's last tick lines.

set -u

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
BOARD="${1:-0b21ffaa-3a67-4bed-a9ac-658c855b4704}"

if [ -f "$HOME/.config/pricklypear/env" ]; then
  _pp_u="${PP_URL-}"; _pp_s="${PP_USER-}"; _pp_p="${PP_PASS-}"
  set -a; . "$HOME/.config/pricklypear/env"; set +a
  [ -n "$_pp_u" ] && PP_URL="$_pp_u"
  [ -n "$_pp_s" ] && PP_USER="$_pp_s"
  [ -n "$_pp_p" ] && PP_PASS="$_pp_p"
  unset _pp_u _pp_s _pp_p
fi

command -v jq >/dev/null 2>&1 || { print -u2 "pp-runs: jq is required"; exit 2; }
EVAL="$SCRIPT_DIR/pp-eval.sh"
[ -x "$EVAL" ] || { print -u2 "pp-runs: missing $EVAL"; exit 2; }

tmp=$(mktemp -d "${TMPDIR:-/tmp}/pp-runs.XXXXXX") || exit 1
trap 'rm -rf "$tmp"' EXIT

# ---- one read-only eval: every run row + every card the identity can see ----
# Heavy scans (the whole run space + the whole item pool) can outlast the
# client's own timeout on a busy habitat, so a failed pull is retried before
# the monitor gives up: a monitor that reports nothing is worse than a slow one.
RECORD_EXPR='(list (list "runs" (workstreams/run-all)) (list "cards" (list/map (lambda (r) (list (car r) (dict-get (cdr r) "state") (dict-get (cdr r) "list_id") (dict-get (cdr r) "kind") (dict-get (cdr r) "priority") (dict-get (cdr r) "assignee"))) (todo/item/all))))'
record=""
for attempt in 1 2 3; do
  record=$("$EVAL" -j "$RECORD_EXPR" 2>/dev/null || true)
  [ "$(print -r -- "$record" | jq -r '.ok // false' 2>/dev/null)" = "true" ] && break
  print -u2 "pp-runs: record pull attempt $attempt failed (habitat slow or timing out); retrying"
  sleep 3
done
if [ "$(print -r -- "$record" | jq -r '.ok // false' 2>/dev/null)" != "true" ]; then
  print -u2 "pp-runs: $(print -r -- "$record" | jq -r '.error // "no reply from the habitat"' 2>/dev/null)"; exit 1
fi
print -r -- "$record" | jq -r '.value' > "$tmp/record.json"

jq -r '.[] | select(.[0]=="runs")  | .[1] | to_entries | map(.value)' "$tmp/record.json" > "$tmp/runs.json"
jq -r '.[] | select(.[0]=="cards") | .[1]' "$tmp/record.json" > "$tmp/cards.json"

# ---- run ids whose card no longer resolves: dispatch can never succeed ----
idlist=$(jq -r '.[].card_id' "$tmp/runs.json" | sort -u |
  sed 's/^/"/; s/$/"/' | paste -sd, -)
# The unresolved set is the one signal that needs a second eval: those ids are
# the runs that can NEVER advance a card, so a heavy eval that times out must
# not be reported as an empty set.
: > "$tmp/unresolved.txt"
probe_status="ok"
if [ -n "$idlist" ]; then
  probe="(list/filter (lambda (id) (null? (todo/item/get id))) (list $idlist))"
  for attempt in 1 2; do
    out=$("$EVAL" -j "$probe" 2>/dev/null || true)
    if [ "$(print -r -- "$out" | jq -r '.ok // false' 2>/dev/null)" = "true" ]; then
      print -r -- "$out" | jq -r '.value | fromjson | .[]' > "$tmp/unresolved.txt"
      break
    fi
    probe_status="failed"
    sleep 2
  done
fi

jq -rn --slurpfile R "$tmp/runs.json" --slurpfile C "$tmp/cards.json" \
  --rawfile U "$tmp/unresolved.txt" --arg probe "$probe_status" \
  --arg board "$BOARD" --arg now "$(date -u +%FT%TZ)" '
  def ts: if . == "" or . == null then "-" else . end;
  def mins: if . == "-" then "-"
            else ((($now|fromdateiso8601) - (.|fromdateiso8601)) / 60 | floor | tostring) + "m" end;
  def row: "  \(.state)\t\(.card_id[0:8])\tlist=\(.list_id[0:8])\tdest=\(if .dest_state == "" then "-" else .dest_state end)\thb=\(.claim_heartbeat_at|ts|mins)\tcreated=\(.created_at[11:16])Z";
  ($R[0] // []) as $runs | ($C[0] // []) as $cards | ($now|fromdateiso8601) as $t |
  ($runs | map(select(.state=="queued" or .state=="claimed" or .state=="running"))) as $live |
  ($runs | group_by(.card_id) | map({id: .[0].card_id, n: length}) | sort_by(-.n)) as $bycard |
  ($runs | map(select((.created_at // "") != "" and (($t - (.created_at|fromdateiso8601)) < 3600))) | length) as $recent |
  ($cards | group_by(.[0]) | map(select(length > 1))) as $dups |
  ($cards | map(select((.[5] // "") != "")) | map("\(.[5])->\(.[0][0:8])")) as $leases |
  ("== PP runs  \($now)  board \($board[0:8]) =="),
  ("rows: \($runs|length)   " + ($runs | group_by(.state) | map("\(.[0].state)=\(length)") | join("  "))),
  ("newest row: \(($runs | map(.created_at) | max) // "-")   rows in the last hour: \($recent)" + (if $recent == 0 then "   ** no dispatch in an hour **" else "" end)),
  ("failure classes: " + ($runs | map(.error_class // "none") | group_by(.) | map("\(.[0])=\(length)") | sort_by(-(split("=")[1]|tonumber)) | join("  "))),
  ("in flight: \($live|length)"),
  ($live[] | row),
  ("top replay (runs per card): " + ($bycard[0:8] | map("\(.n)x \(.id[0:8])") | join("  "))),
  ("unresolved run ids (\($U | split("\n") | map(select(length>0)) | length)): " + ($U | split("\n") | map(select(length>0)) | join("  ")) + (if $probe == "ok" then "" else "  [probe eval FAILED — not a zero]" end)),
  ("cards seen: \($cards|length)   " + ($cards | map(.[1]) | group_by(.) | map("\(.[0])=\(length)") | join("  "))),
  ("duplicate card rows (\($dups|length)): " + ($dups | map("\(.[0][0][0:8])x\(length)") | join("  "))),
  ("leases (\($leases|length)): " + ($leases | join("  ")))
'

# ---- optional: the supervisor host own tick log ---------------------------
if [ -n "${PP_RUNS_SUPERVISOR_SSH-}" ]; then
  print -s ""
  print -s "== supervisor \$PP_RUNS_SUPERVISOR_SSH =="
  ssh -o BatchMode=yes -o ConnectTimeout=5 "$PP_RUNS_SUPERVISOR_SSH" \
    "tail -6 '${PP_RUNS_SUPERVISOR_LOG:-/home/al/glochid/glochid.log}'" ||
    print -s "(ssh failed)"
fi
