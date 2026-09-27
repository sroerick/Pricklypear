#!/bin/ksh
# pp-flow-autolith.sh — PP_FLOW_PI adapter: run one pp-flow executor brief
# through Autolith's headless run-job (the lambda-symbolics/autolith#76
# workaround: the role executor.sexp validates :models ("@auto") statically and
# the real model is pinned per-invocation via AUTOLITH_MODEL).
#
# Contract (matches what pp-flow.sh expects of PP_FLOW_PI):
#   invoked as: $PP_FLOW_PI "<brief text>"
#   on success prints the job's terminal summary to stdout as the transcript
#   (pp-flow scrapes EVIDENCE / DESTINATION: lines from it); exits 0 iff SUCCEEDED.
#
# Env:
#   AUTOLITH_MODEL   model id registered in ~/.config/autolith/init.lisp
#                    (default: neuralwatt/deepseek-v4-flash-class custom ids work;
#                     must be init.lisp-registered, NOT a builtin-only name —
#                     see autolith#76)
#   AUTOLITH_BIN     default: autolith
#   PP_FLOW_AUTOLITH_PERM  run-job permission mode (default: sandbox; use
#                    full only for cards that must touch the real repo)
#
# Job envelope (autolith native task contract, v1):
#   (:autolith-job :version 1 :id <str> :role "executor" :prompt <brief>
#                  :input nil :output-contract (:type :string)
#                  :timeout-seconds <n>)

set -u

BRIEF="${1:-}"
[ -n "$BRIEF" ] || { echo "pp-flow-autolith: no brief on argv" >&2; exit 2; }

AUTOLITH_BIN="${AUTOLITH_BIN:-autolith}"
PERM="${PP_FLOW_AUTOLITH_PERM:-sandbox}"
TIMEOUT="${PP_FLOW_AUTOLITH_TIMEOUT:-1800}"
JOB_ID="pp-flow-$(date +%s)-$$"
WORK="$(mktemp -t ppflow-al.XXXXXX)"
trap 'rm -f "$WORK.job" "$WORK.out" "$WORK.err"' EXIT

# POSIX awk Lisp-string escaper (same pattern as pp-flow.sh json_str, plus
# nothing job-specific): backslash, dquote, tab, newline joins with \n.
esc() {
  printf '%s' "$1" | tr -d '\r' | awk 'BEGIN { ORS = "" }
    NR > 1 { print "\\n" }
    { gsub(/\\/, "\\\\"); gsub(/"/, "\\\""); gsub(/\t/, "\\t"); print $0 }'
}

printf '(:autolith-job\n :version 1\n :id "%s"\n :role "executor"\n :prompt "%s"\n :input nil\n :output-contract (:type :string)\n :timeout-seconds %s)\n' \
  "$JOB_ID" "$(esc "$BRIEF")" "$TIMEOUT" > "$WORK.job"

# stderr (SBCL redefinition warnings) is captured, not passed through: pp-flow
# merges 2>&1 into the transcript it scrapes for evidence, and the warning spam
# would drown the executor summary. Diagnostics surface only on failure.
if ! "$AUTOLITH_BIN" run-job --immutable --input "$WORK.job" --output "$WORK.out" --permissions "$PERM" 2>"$WORK.err"; then
  echo "pp-flow-autolith: run-job crashed" >&2
  cat "$WORK.err" >&2
  exit 1
fi

# job result is a sexp; pull :STATUS and :RESULT with awk field-scan
status="$(tr -d '\n' < "$WORK.out" | sed -n 's/.*:STATUS \(:[A-Z-]*\).*/\1/p')"
if [ "$status" != ":SUCCEEDED" ]; then
  echo "pp-flow-autolith: job status $status" >&2
  cat "$WORK.out" >&2
  exit 1
fi

# terminal summary (the executor's output contract) on stdout as transcript.
# Awk escaped-string scanner: find :RESULT ", then walk to the closing
# unescaped quote, unescaping \" \\ \n as we go (pp-flow scrapes this for
# EVIDENCE / DESTINATION lines).
tr -d '\n' < "$WORK.out" | awk 'BEGIN{ORS=""}
  { s = s $0 }
  END{
    p = index(s, ":RESULT \"");
    if (!p || p > length(s)-9) exit 1;
    i = p + 9;
    while (i <= length(s)) {
      c = substr(s, i, 1);
      if (c == "\\") {
        d = substr(s, i+1, 1);
        if (d == "n") out = out "\n";
        else if (d == "\"") out = out "\"";
        else if (d == "\\") out = out "\\";
        else out = out d;
        i += 2; continue;
      }
      if (c == "\"") { print out "\n"; exit 0 }
      out = out c; i++;
    }
    exit 1;
  }'
rc=$?
[ $rc -eq 0 ] || { echo "pp-flow-autolith: could not extract :RESULT from job output" >&2; exit 1; }
exit 0
