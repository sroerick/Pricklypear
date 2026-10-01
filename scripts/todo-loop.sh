#!/bin/ksh
# todo-loop.sh — the canonical ghuntley-Ralph loop for the todo-shared-spaces
# worktree, run from a SHELL so the child's working directory is this worktree
# by construction.
#
# WHY THIS EXISTS (not the pi `ralph` tool): the pi ralph extension spawns its
# child with `cwd: ctx.cwd` — the HOST session's directory. If the host pi was
# started in the main checkout, ralph children land THERE, and the main
# checkout is owned by a different agent (branch sec-cookie-hsts). This script
# `cd`s to the worktree and spawns each child with that cwd, so isolation is
# structural rather than a prompt instruction.
#
# Same contract as the ralph tool:
#   - the PLAN FILE is the prompt; a fresh child reads it, decides the next
#     task, does it, updates the plan
#   - child's final message ends with <promise>COMPLETE</promise> (stop) or
#     <promise>TASK_DONE</promise> (loop again)
#   - the plan file is the only shared state
#
# Usage:
#   scripts/todo-loop.sh                 # loop until COMPLETE or max loops
#   scripts/todo-loop.sh --max 3         # at most 3 child turns
#   scripts/todo-loop.sh --model synthetic/syn:large:text
#   scripts/todo-loop.sh --timeout 1800  # per-child seconds
#
# Run it in the background and watch:
#   nohup scripts/todo-loop.sh > /tmp/todo-loop.log 2>&1 &
#   tail -f /tmp/todo-loop.log
set -u

WORKTREE="/home/roerick/dev/wyo.tech/pricklypear-todo-shared"
PLAN="$WORKTREE/.ralph/todo-shared-spaces.md"
MODEL="${TODO_LOOP_MODEL:-synthetic/syn:large:text}"
THINKING="${TODO_LOOP_THINKING:-high}"
MAX="${TODO_LOOP_MAX:-50}"
TIMEOUT="${TODO_LOOP_TIMEOUT:-1800}"
TOOLS="${TODO_LOOP_TOOLS:-read,bash,edit,write}"

while [ $# -gt 0 ]; do
  case "$1" in
    --max)     MAX="$2"; shift 2 ;;
    --model)   MODEL="$2"; shift 2 ;;
    --timeout) TIMEOUT="$2"; shift 2 ;;
    --thinking) THINKING="$2"; shift 2 ;;
    -h|--help) sed -n '2,30p' "$0"; exit 0 ;;
    *) print -ru2 -- "unknown arg: $1"; exit 2 ;;
  esac
done

[ -f "$PLAN" ] || { print -ru2 -- "missing plan: $PLAN"; exit 2; }
cd "$WORKTREE" || exit 2

print -r -- "=== todo-loop: model=$MODEL max=$MAX timeout=${TIMEOUT}s ==="
print -r -- "=== worktree=$WORKTREE branch=$(git branch --show-current) ==="

loop=1
while [ "$loop" -le "$MAX" ]; do
  print -r -- ""
  print -r -- "=== child $loop/$MAX  $(date -Iseconds) ==="

  out="/tmp/todo-loop-child-$loop.json"
  # The child's cwd is the worktree because we cd'd above and `pi` inherits it.
  timeout "$TIMEOUT" pi -p --no-session --mode json \
      --model "$MODEL" --thinking "$THINKING" --tools "$TOOLS" \
      "You are a fresh worker in a ralph loop (loop #$loop). The plan is the file:
  $PLAN
Read it (and any files it points at).

Your job, non-deterministically:
1. Decide the single most important next task from the plan (check git log / progress notes / the plan file itself first — do NOT assume work is unimplemented).
2. Do it fully. Run the plan's verify/build/test commands.
3. Update the plan / progress file with what you did and what's still open (the plan is shared memory for future loops).
4. If the goal is now complete, end your FINAL message with exactly: <promise>COMPLETE</promise>
   Otherwise end it with exactly: <promise>TASK_DONE</promise>
Do not ask questions; act on the plan." > "$out" 2>&1
  rc=$?

  # The marker is only meaningful in the child's FINAL assistant text.
  final=$(python3 - "$out" <<'PYEOF'
import json,sys
last=""
try:
    for line in open(sys.argv[1]):
        line=line.strip()
        if not line: continue
        try: o=json.loads(line)
        except Exception: continue
        if o.get("type")=="message_end" and o.get("message",{}).get("role")=="assistant":
            for part in o["message"].get("content") or []:
                if part.get("type")=="text" and part.get("text"):
                    last=part["text"]
except Exception:
    pass
print(last[-4000:])
PYEOF
)

  # Surface a provider error plainly (a 402 must not look like a stall).
  err=$(python3 - "$out" <<'PYEOF'
import json,sys
for line in open(sys.argv[1]):
    line=line.strip()
    if not line: continue
    try: o=json.loads(line)
    except Exception: continue
    m=o.get("message",{}) if o.get("type")=="message_end" else {}
    if m.get("role")=="assistant" and m.get("errorMessage"):
        print(m["errorMessage"][:300]); break
PYEOF
)

  if print -r -- "$final" | grep -q '<promise>COMPLETE</promise>'; then
    print -r -- "=== COMPLETE after child $loop ($(date -Iseconds)) ==="
    print -r -- "$final" | tail -20
    exit 0
  fi

  if [ -n "$err" ]; then
    print -ru2 -- "!!! provider error on child $loop: $err"
    print -ru2 -- "!!! stopping; fix the provider/model and re-run."
    exit 1
  fi

  if print -r -- "$final" | grep -q '<promise>TASK_DONE</promise>'; then
    print -r -- "--- child $loop: TASK_DONE (rc=$rc) ---"
  else
    print -r -- "--- child $loop: no marker (rc=$rc) — looping again ---"
    print -r -- "$final" | tail -15
  fi

  loop=$((loop + 1))
  sleep 3
done

print -r -- "=== stopped after $MAX children without COMPLETE ==="
exit 0
