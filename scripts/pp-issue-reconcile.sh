#!/usr/bin/env bash
# pp-issue-reconcile.sh — READ-ONLY reconcile of the Pricklypear Board against
# shipped git commits (card a80bef58).
#
# The workstreams pipeline only advances a card when a flow RUN touches it, so
# fixes authored as direct commits (citing the card's issue_id) leave cards
# stuck in TODO/HUMAN-REVIEW long after the fix shipped. For every card NOT in
# DONE, grep git log for the card's issue_id and print:
#   card-id  state  matched-commits
#
# Heuristic: commits cite only the SHORT `i-<epoch>` prefix, not the full UUID,
# so match the short form first and the full id as a fallback. NO board writes.
#
# Usage: scripts/pp-issue-reconcile.sh [list-id]
# Env: PP_EVAL (default _build/.../pp_eval.exe); PP_URL/PP_USER/PP_PASS for the
#      board read (sourced from ~/.config/pricklypear/env if not already set).
set -euo pipefail
ROOT="$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)"
. "$ROOT/scripts/eval_json.sh"
LIST_ID="${1:-0b21ffaa-3a67-4bed-a9ac-658c855b4704}"
export PP_EVAL="${PP_EVAL:-$ROOT/_build/default/cli/bin/pp_eval.exe}"
if [ -z "${PP_USER:-}" ]; then
  set -a; . "$HOME/.config/pricklypear/env" 2>/dev/null || true; set +a
fi
items_json="$("$PP_EVAL" -j '(todo/item/all)' | pp_json_value)" || {
  echo "pp-issue-reconcile: could not read board (PP_URL/PP_USER/PP_PASS set?)" >&2
  exit 1
}
cd "$ROOT"
python3 - "$items_json" "$LIST_ID" <<'PY'
import json, subprocess, sys
items, list_id = json.loads(sys.argv[1]), sys.argv[2]

def commits(needles):
    seen, out = set(), []
    for n in filter(None, needles):
        r = subprocess.run(["git", "log", "--all", "--oneline",
                            "--fixed-strings", "--grep=" + n],
                           capture_output=True, text=True)
        for line in r.stdout.splitlines():
            if line not in seen:
                seen.add(line); out.append(line)
    return out

print("card-id   state         matched-commits")
print("--------  ------------  ------------------------------------------------")
rows = 0
for cid, v in sorted(items.items()):
    if v.get("state") == "DONE" or v.get("list_id") != list_id:
        continue
    iid = v.get("issue_id", "")
    if not iid:
        continue
    short = "-".join(iid.split("-")[:2])
    hits = commits([short, iid])
    if hits:
        rows += 1
    for i, h in enumerate(hits or ["(no commit match for %s)" % short]):
        print("%-8s  %-12s  %s" % (cid[:8] if i == 0 else "", v.get("state", ""), h))
print("\n%d card(s) with commit matches (READ-ONLY; human confirms closure)." % rows)
PY
