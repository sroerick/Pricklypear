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
#                    (default: syn:large:text — the Synthetic provider, which is
#                     what autonomous/gregor runs use; overridable. The executor
#                     role's :models is ("@auto") and run-job validates it
#                     statically, so the real model MUST be pinned here —
#                     see autolith#76)
#   AUTOLITH_REASONING_EFFORT  NOT an override for this role: run-job honors
#                    the ROLE file's :reasoning-effort (verified live), so the
#                    executor role's pinned :medium governs. A model that
#                    rejects medium (Synthetic glm-flash: low|high|max) is
#                    refused up front with an actionable message rather than a
#                    cryptic 35s :PROCESS-FAILURE.
#   PP_FLOW_AUTOLITH_IGNORE_EFFORT  =1 to skip that preflight and attempt anyway
#   AUTOLITH_BIN     default: autolith
#   PP_FLOW_AUTOLITH_PERM  run-job permission mode (default: auto).
#                    sandbox CANNOT execute anything and is only useful for a
#                    read-only probe; see the measured note below.
#   PP_FLOW_AUTOLITH_FAILDIR  failure-archive dir (default: $PWD/.ralph/failures)
#   PP_FLOW_AUTOLITH_RETRIES  retries after a TRANSIENT provider fault
#                    (default 2). Deterministic failures never retry — see the
#                    retry policy block below.
#   PP_FLOW_AUTOLITH_RETRY_BASE  backoff seconds, doubled per attempt
#                    (default 10; capped at 300).
#
# This adapter is the DEFAULT executor runner for unattended pp-flow loops
# (see pp-flow.sh --drain): `pi -p` remains the interactive default, but a cron/
# habitat-driven loop runs headless through run-job. Keeping it robust for
# that use is the point of the guards below.
#
# Published defaults (2026-09-27, gregor/synthetic autonomy pass):
#   AUTOLITH_MODEL=syn:large:text
#
# Job envelope (autolith native task contract, v1):
#   (:autolith-job :version 1 :id <str> :role "executor" :prompt <brief>
#                  :input nil :output-contract (:type :string)
#                  :timeout-seconds <n>)

set -u

BRIEF="${1:-}"
[ -n "$BRIEF" ] || { echo "pp-flow-autolith: no brief on argv" >&2; exit 2; }

AUTOLITH_BIN="${AUTOLITH_BIN:-autolith}"
# Default the model to the Synthetic provider: unattended/gregor runs are
# provider-pinned to Synthetic (roerick), and a stale or absent AUTOLITH_MODEL
# must not silently fall back to whatever the laptop's durable preference is.
: "${AUTOLITH_MODEL:=syn:large:text}"
# PERMISSION MODE — MEASURED 2026-10-26 on gregor (autolith 0.62.1).
# The old default was `sandbox`, and it made every real card a source-only
# review: `shell.run` was denied on every attempt, so `dune build`/`runtest`
# and live smoke never ran. The evidence contract then still reported
# `rc=0 evidence=yes`, because the harness was grading itself, not the work.
#
#   sandbox: "echo PP_SHELL_OK" -> :CHILD-FAILURE, denied on every attempt,
#            279,342 input tokens, 151s, zero output.
#   auto:    same command -> :SUCCEEDED, 39,405 input tokens, 14s.
#   auto:    write a file + git init/add/commit -> :SUCCEEDED, 59,298 tokens.
#
# So `sandbox` is not a safe default, it is a broken one: it burns ~7x the
# tokens to produce nothing. `auto` is what actually executes work.
#
# Note both the IMPLEMENTER and the REVIEWER need this: a LOGIC-REVIEW that
# cannot run `dune build`/`dune runtest` is not a review, it is an opinion.
: "${PP_FLOW_AUTOLITH_PERM:=auto}"
TIMEOUT="${PP_FLOW_AUTOLITH_TIMEOUT:-3000}"
ROLE="${PP_FLOW_AUTOLITH_ROLE:-executor}"
# JOB_ID is assigned per attempt (see the retry loop); this is the initial value.
JOB_ID="pp-flow-$(date +%s)-$$-0"
WORK="$(mktemp -t ppflow-al.XXXXXX)"
trap 'rm -f "$WORK.job" "$WORK.out" "$WORK.err"' EXIT
# On failure, archive everything a post-mortem needs before the EXIT trap
# deletes the transient files, and copy the child's DURABLE conversation out
# of the task store (located via the result's :TRACE-ID). Run 2 (09-27) left
# a 0-byte transcript: the evidence survived only deep in
# ~/.local/share/autolith/tasks/<id>/ — auditability means surfacing it.
PP_FLOW_AUTOLITH_FAILDIR="${PP_FLOW_AUTOLITH_FAILDIR:-$PWD/.ralph/failures}"
archive_failure() {
  fstat="${1:-unknown}"
  fdir="$PP_FLOW_AUTOLITH_FAILDIR/$(date +%Y%m%dT%H%M%SZ)-$JOB_ID"
  mkdir -p "$fdir" 2>/dev/null || return 0
  cp -f "$WORK.out" "$fdir/output.sexp"       2>/dev/null
  cp -f "$WORK.err" "$fdir/runner-stderr.txt" 2>/dev/null
  cp -f "$WORK.job" "$fdir/job.sexp"          2>/dev/null
  ftrace="$(tr -d '\n' < "$WORK.out" 2>/dev/null | sed -n 's/.*:TRACE-ID "\([^"]*\)".*/\1/p')"
  if [ -n "$ftrace" ]; then
    fseg="$(ls -t ~/.local/share/autolith/tasks/*/conversation-picker/"$ftrace".sexp 2>/dev/null | head -1)"
    if [ -n "$fseg" ]; then
      ftasks="$(dirname "$(dirname "$fseg")")"
      find "$ftasks" -name '*.sexp' -exec cp -f {} "$fdir/" \; 2>/dev/null
    fi
  fi
  {
    echo "job:      $JOB_ID"
    echo "status:   $fstat"
    echo "trace:    ${ftrace:-none}"
    echo "archived: $(date -u '+%Y-%m-%dT%H:%M:%SZ')"
    echo
    echo "output.sexp        run-job terminal result (status, usage, :OUTPUT so far)"
    echo "runner-stderr.txt  SBCL/recovery stderr (2> capture)"
    echo "job.sexp           the job envelope as submitted"
    if [ -n "$ftrace" ]; then
      echo "*.sexp             the child's durable full transcript (copied from the task store)"
    fi
  } > "$fdir/README.txt"
  echo "pp-flow-autolith: failure evidence archived at $fdir" >&2
}

# POSIX awk Lisp-string escaper (same pattern as pp-flow.sh json_str, plus
# nothing job-specific): backslash, dquote, tab, newline joins with \n.
esc() {
  printf '%s' "$1" | tr -d '\r' | awk 'BEGIN { ORS = "" }
    NR > 1 { print "\\n" }
    { gsub(/\\/, "\\\\"); gsub(/"/, "\\\""); gsub(/\t/, "\\t"); print $0 }'
}

# REASONING-EFFORT PREFLIGHT (autonomy hardening, 2026-09-27): the executor
# role file (~/.config/autolith/agents/executor.sexp) pins
# :reasoning-effort :medium, and run-job validates the ROLE's effort against
# the pinned model BEFORE any provider call. The role value WINS over
# AUTOLITH_REASONING_EFFORT (verified live), so a model that rejects "medium"
# can never run through this role — it dies in ~35s with a cryptic
# :PROCESS-FAILURE and burns the card's lease. Known offenders: the Synthetic
# glm-flash family declares only low|high|max. Fail FAST and actionably here
# instead, so an unattended loop (and gregor) sees the real cause immediately.
case "$AUTOLITH_MODEL" in
  hf:zai-org/GLM-5.3-Flash|hf:zai-org/GLM-4.7-Flash)
    if [ "${PP_FLOW_AUTOLITH_IGNORE_EFFORT:-0}" != "1" ]; then
      echo "pp-flow-autolith: model '$AUTOLITH_MODEL' rejects the executor role's :reasoning-effort :medium (it accepts low|high|max). Use a compatible model (e.g. syn:large:text, the default) or set that role's effort to low/high in ~/.config/autolith/agents/executor.sexp. Set PP_FLOW_AUTOLITH_IGNORE_EFFORT=1 to attempt anyway." >&2
      exit 2
    fi
    ;;
  *) : ;;
esac

# ---- retry policy (autonomy hardening, 2026-09-27) --------------------- -
# A transient provider fault must not fail an unattended card: one run got an
# HTTP 402 from Synthetic ("Subscription access is currently blocked…") that
# cleared on the very next attempt, while direct curl to the same endpoint
# never failed. But a DETERMINISTIC failure (the executor role's
# :reasoning-effort disagreeing with the model, a malformed job envelope) must
# NEVER be retried — it would burn the card's lease N times over for the same
# guaranteed failure. So the retry is driven by the machine-readable :CATEGORY
# plus the HTTP status in the message, not by "it failed".
#
# RETRYABLE: :PROVIDER-FAILURE whose message names a transient HTTP status
#   (402/408/409/425/429/5xx), or a run-job crash that wrote no job result at
#   all (the recovery-image hiccup seen in early battle tests — that is an
#   autolith/infra fault, not an answer about the work).
# TERMINAL: everything else — :PROCESS-FAILURE, :INVALID-*, :UNKNOWN-ROLE,
#   :ARTIFACT-FAILURE, :RUNTIME-UNAVAILABLE, :TIMED-OUT, and any
#   :PROVIDER-FAILURE naming a non-transient status (400/401/403/404/422).
#   Note :TIMED-OUT is terminal on purpose: the job burned its whole wall
#   clock, so retrying it immediately is the same coin flip for the same cost.
RETRIES="${PP_FLOW_AUTOLITH_RETRIES:-2}"
RETRY_BASE="${PP_FLOW_AUTOLITH_RETRY_BASE:-10}"
case "$RETRIES" in ''|*[!0-9]*) RETRIES=2 ;; esac
case "$RETRY_BASE" in ''|*[!0-9]*) RETRY_BASE=10 ;; esac
# $1 = attempt number (distinguishes job ids across attempts)
attempt() {
  JOB_ID="pp-flow-$(date +%s)-$$-$1"
  # OUTPUT CONTRACT (2026-09-27): a STRING contract lets the model answer a
  # review analytically and never render a verdict (observed twice on card
  # 88a92237 — thorough analysis, no DESTINATION, card parked in HUMAN-REVIEW).
  # Appending a terminal instruction to the brief did NOT fix it. A JSON-SCHEMA
  # contract does, because the provider VALIDATES the object before returning it:
  # verdict + evidence are REQUIRED, so an answer with no verdict fails closed
  # as :INVALID-OUTPUT instead of silently becoming an un-actionable review.
  # PP_FLOW_AUTOLITH_CONTRACT=string restores the old free-text behavior for
  # runners/models without structured output support. MEASURED (2026-09-27, 2
  # runs each): syn:large:text, gpt-oss-120b and DeepSeek-V4.1-Flash all
  # SUCCEED with BOTH contracts; GLM-4.7-Flash returned no result in that
  # probe window. An earlier single-shot read suggested syn:large:text could
  # not do structured output — that was wrong: the failures were the same
  # intermittent upstream HTTP 402, which the retry policy now absorbs.
  if [ "${PP_FLOW_AUTOLITH_CONTRACT:-schema}" = "string" ]; then
    printf '(:autolith-job\n :version 1\n :id "%s"\n :role "%s"\n :prompt "%s"\n :input nil\n :output-contract (:type :string)\n :timeout-seconds %s)\n' \
      "$JOB_ID" "$ROLE" "$(esc "$BRIEF")" "$TIMEOUT" > "$WORK.job"
  else
    # NOTE: the native output-contract subset accepts ONLY :type, scalar :enum,
    # :properties/:required/:additional-properties and array bounds — a
    # :description is REJECTED as :INVALID-CONTRACT "The property is not part
    # of the native output contract" (verified live). Keep the semantic hints
    # in the PROMPT/brief instead; 'destination' is deliberately OPTIONAL (a
    # PASS on a list whose recipe means "advance one column" legitimately names
    # none), so an empty string is a valid answer and additional-properties
    # stays open for models that add fields.
    printf '(:autolith-job\n :version 1\n :id "%s"\n :role "%s"\n :prompt "%s"\n :input nil\n :output-contract (:type :object :properties (("verdict" (:type :string :enum ("PASS" "FAIL" "ESCALATE"))) ("destination" (:type :string)) ("evidence" (:type :string))) :required ("verdict" "evidence") :additional-properties t)\n :timeout-seconds %s)\n' \
      "$JOB_ID" "$ROLE" "$(esc "$BRIEF")" "$TIMEOUT" > "$WORK.job"
  fi

  : > "$WORK.out"; : > "$WORK.err"
  T0=$(date +%s)
  CRASHED=0
  # stderr (SBCL redefinition warnings) is captured, not passed through: pp-flow
  # merges 2>&1 into the transcript it scrapes for evidence, and the warning spam
  # would drown the executor summary. Diagnostics surface only on failure.
  # NOTE (battle-tested 09-27, both laptops): run-job exits 1 even when it wrote
  # a well-formed job result (e.g. :STATUS :TIMED-OUT — its recovery image then
  # also exits 1). A written result sexp is definitive regardless of exit code,
  # so the result file decides; only a missing/unparseable result is a crash.
  if ! "$AUTOLITH_BIN" run-job --immutable --input "$WORK.job" --output "$WORK.out" --permissions "$PERM" 2>"$WORK.err"; then
    if [ ! -s "$WORK.out" ] || ! tr -d '\n' < "$WORK.out" | grep -q ':STATUS'; then
      CRASHED=1
    else
      echo "pp-flow-autolith: run-job exited non-zero but wrote a job result; honoring it" >&2
    fi
  fi
  ELAPSED=$(( $(date +%s) - T0 ))

  status=""
  [ "$CRASHED" -eq 0 ] && status="$(tr -d '\n' < "$WORK.out" | sed -n 's/.*:STATUS \(:[A-Z-]*\).*/\1/p')"

  # Failure extraction is TWO sed passes, deliberately: the output is pretty-
  # printed and a newline can land between :CATEGORY and its keyword, so after
  # tr -d '\n' the single-pass pattern ':CATEGORY :X .* :MESSAGE "..."' is
  # brittle (verified live: it returned EMPTY for a real :PROCESS-FAILURE, so
  # every failure diagnostic said "failure=unknown"). Pull the category and the
  # message independently and concatenate. :MESSAGE may contain escaped quotes
  # ("model \"x\""); the [^"]* run stops at the first and is truncated, which
  # is fine — the message is a diagnostic, not data.
  fcat=""; fmsg=""
  if [ -s "$WORK.out" ]; then
    fcat="$(tr -d '\n' < "$WORK.out" | sed -n 's/.*:CATEGORY[[:space:]]*\(:[A-Za-z-]*\).*/\1/p' | head -1)"
    fmsg="$(tr -d '\n' < "$WORK.out" | sed -n 's/.*:MESSAGE[[:space:]]*"\([^"]*\)".*/\1/p' | head -1)"
  fi
  failure="${fcat}${fmsg:+ $fmsg}"

  trace="$(tr -d '\n' < "$WORK.out" 2>/dev/null | sed -n 's/.*:TRACE-ID[[:space:]]*"\([^"]*\)".*/\1/p')"
  intok="$(tr -d '\n' < "$WORK.out" 2>/dev/null | sed -n 's/.*:INPUT-TOKENS[[:space:]]*\([0-9]*\).*/\1/p')"
  outtok="$(tr -d '\n' < "$WORK.out" 2>/dev/null | sed -n 's/.*:OUTPUT-TOKENS[[:space:]]*\([0-9]*\).*/\1/p')"
  reqs="$(tr -d '\n' < "$WORK.out" 2>/dev/null | sed -n 's/.*:PROVIDER-REQUESTS[[:space:]]*\([0-9]*\).*/\1/p')"
  cached="$(tr -d '\n' < "$WORK.out" 2>/dev/null | sed -n 's/.*:CACHED-TOKENS[[:space:]]*\([0-9]*\).*/\1/p')"
  # Fleet analytics (autolith-analytics M3): one run-step event per attempt,
  # best-effort append; a failed write never affects the job outcome.
  ANALYTICS_EVENTS_DIR="${ANALYTICS_EVENTS_DIR:-$HOME/dev/wyo.tech/autolith-analytics/events}"
  ANALYTICS_ACTOR="${ANALYTICS_ACTOR:-${USER:-unknown}}"
  if mkdir -p "$ANALYTICS_EVENTS_DIR" 2>/dev/null; then
    printf '{"ts":"%s","schema_v":1,"source":"pp-flow","kind":"run-step","actor":"%s","run_id":"%s","requests":%s,"tokens_in":%s,"tokens_out":%s,"wall_s":%s}\n' \
      "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
      "$(printf '%s' "$ANALYTICS_ACTOR" | tr -d '"\\')" \
      "$(printf '%s' "$JOB_ID" | tr -d '"\\')" \
      "${reqs:-0}" "${intok:-0}" "${outtok:-0}" "${ELAPSED:-0}" \
      >> "$ANALYTICS_EVENTS_DIR/pp-flow.jsonl" 2>/dev/null || :
  fi
}

# Is the LAST attempt worth retrying? Echo "yes"/"no".
transient_failure() {
  if [ "$CRASHED" -eq 1 ]; then printf 'yes\n'; return; fi
  case "$fcat" in
    :PROVIDER-FAILURE)
      # transient only when the message names a status that can clear by itself
      case "$fmsg" in
        *"HTTP 402"*|*"HTTP 408"*|*"HTTP 409"*|*"HTTP 425"*|*"HTTP 429"*) printf 'yes\n' ;;
        *"HTTP 500"*|*"HTTP 502"*|*"HTTP 503"*|*"HTTP 504"*|*"HTTP 599"*)     printf 'yes\n' ;;
        *"HTTP 5"[0-9][0-9]*)                                                    printf 'yes\n' ;;
        *)                                                                        printf 'no\n' ;;
      esac ;;
    *) printf 'no\n' ;;
  esac
}

attempt=0
TOTAL_ELAPSED=0
while : ; do
  attempt=$(( attempt + 1 ))
  attempt "$attempt"
  TOTAL_ELAPSED=$(( TOTAL_ELAPSED + ${ELAPSED:-0} ))
  echo "pp-flow-autolith: job=$JOB_ID status=${status:-<crashed>} model=${AUTOLITH_MODEL:-default} role=$ROLE perm=$PERM elapsed=${ELAPSED}s trace=$trace tokens_in=$intok tokens_out=$outtok provider_requests=$reqs attempt=$attempt/$(( RETRIES + 1 ))" >&2
  [ "$status" = ":SUCCEEDED" ] && break
  retryable="$(transient_failure)"
  [ "$retryable" = "yes" ] || break
  if [ "$attempt" -gt "$RETRIES" ]; then
    echo "pp-flow-autolith: retry budget exhausted ($attempt attempt(s))" >&2
    break
  fi
  # exponential backoff, capped: 10s, 20s, 40s…
  delay=$(( RETRY_BASE * (1 << (attempt - 1)) ))
  [ "$delay" -gt 300 ] && delay=300
  echo "pp-flow-autolith: transient failure (${failure:-crashed}); retrying in ${delay}s (attempt $(( attempt + 1 ))/$(( RETRIES + 1 )))" >&2
  sleep "$delay"
done

  # USAGE PASS-THROUGH (run-primitive phase 2): the job result already
  # carries the provider counts (:INPUT-TOKENS / :OUTPUT-TOKENS /
  # :PROVIDER-REQUESTS parsed above) and they were dropped at this
  # boundary. Emit ONE machine line on stdout — it lands in the pp-flow
  # transcript — so the supervisor's run-usage-set / run-result finally
  # receive them. status/category/trace are LAST-attempt facts by the same
  # rule; wall_s accumulates across attempts.
  echo "pp-flow-autolith: USAGE-REPORT status=${status:-crashed} category=${fcat:-} trace=${trace:-} requests=${reqs:-0} input_tokens=${intok:-0} cached_tokens=${cached:-0} output_tokens=${outtok:-0} wall_s=${TOTAL_ELAPSED:-0} message=${failure:-none}"
if [ "$status" != ":SUCCEEDED" ]; then
  echo "pp-flow-autolith: job status ${status:-<crashed>} (elapsed=${ELAPSED}s, trace=$trace, failure=${failure:-unknown})" >&2
  archive_failure "${status:-crashed}"
  # bounded excerpt, not a full dump: enough to see WHY, never megabytes
  if [ "$CRASHED" -eq 1 ]; then
    head -c 2000 "$WORK.err" >&2
  else
    tr -d '\n' < "$WORK.out" | head -c 2000 >&2
  fi
  echo >&2
  exit 1
fi

# terminal summary (the executor's output contract) on stdout as transcript.
# Awk escaped-string scanner: find :RESULT ", then walk to the closing
# unescaped quote, unescaping \" \\ \n as we go (pp-flow scrapes this for
# EVIDENCE / DESTINATION lines).
result="$(tr -d '\n' < "$WORK.out" | awk 'BEGIN{ORS=""}
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
        else if (d == "t") out = out "\t";
        else if (d == "\"") out = out "\"";
        else if (d == "\\") out = out "\\";
        else out = out d;
        i += 2; continue;
      }
      if (c == "\"") { print out; exit 0 }
      out = out c; i++;
    }
    exit 1;
  }')"
rc=$?
# STRUCTURED RESULT (2026-09-27): with the default schema contract the :RESULT
# is a portable tagged object, e.g.
#   (:OBJECT ("evidence" "...") ("destination" "DONE") ("verdict" "PASS"))
# not a string. The string scanner above cannot read it (no quotes directly
# after :RESULT), so translate it into the literal EVIDENCE/DESTINATION lines
# pp-flow already parses — keeping ONE contract at the pp-flow boundary while
# the model gets a validated object. Mapping: evidence -> EVIDENCE:;
# destination (or the verdict's implied column) -> DESTINATION:. Verdict PASS
# with no destination falls back to the column recipe default by staying empty.
structured=0
if [ $rc -ne 0 ] || [ -z "$result" ]; then
  if tr -d '\n' < "$WORK.out" | grep -q ':RESULT[[:space:]]*(:OBJECT'; then
    flat="$(tr -d '\n' < "$WORK.out")"
    oev="$(printf '%s' "$flat" | sed -n 's/.*("evidence"[[:space:]]*"\([^"]*\)").*/\1/p')"
    odest="$(printf '%s' "$flat" | sed -n 's/.*("destination"[[:space:]]*"\([^"]*\)").*/\1/p')"
    overdict="$(printf '%s' "$flat" | sed -n 's/.*("verdict"[[:space:]]*"\([^"]*\)").*/\1/p')"
    result="$oev"
    rc=0
    structured=1
    echo "pp-flow-autolith: structured verdict=${overdict:-none} destination=${odest:-none}" >&2
  fi
fi
[ $rc -eq 0 ] || { echo "pp-flow-autolith: could not extract :RESULT from job output" >&2; exit 1; }
printf '%s\n' "$result"
# EVIDENCE CONTRACT BRIDGE. Structured path: the evidence came from the
# validated :OBJECT fields, so label it plainly (never the 'missing EVIDENCE'
# placeholder — card 0a0b995f showed the placeholder prefixing REAL evidence,
# and the glued literal \n leaked into the posted comment) and keep
# DESTINATION on its own line so pp-flow scrapes both. Literal path:
# flash-tier models do not reliably emit the brief's 'EVIDENCE:' line, so
# bridge it transparently, marked as ours, capped.
if [ "${structured:-0}" = 1 ]; then
  if [ -n "$result" ]; then
    printf 'EVIDENCE: %s\n' "$(printf '%b' "$result" | tr '\n' ' ' | sed 's/  */ /g' | cut -c1-400)"
  fi
  [ -n "$odest" ] && printf 'DESTINATION: %s\n' "$odest"
else
  case "$result" in
    *EVIDENCE:*) : ;;
    *) printf 'EVIDENCE: (autolith bridge: executor terminal summary had no EVIDENCE: line) %s\n' \
         "$(printf '%s' "$result" | tr '\n' ' ' | sed 's/  */ /g' | cut -c1-400)" ;;
  esac
fi
# DESTINATION CONTRACT BRIDGE (autonomy hardening, 2026-09-27): the brief asks
# for a literal 'DESTINATION: <COL>' line, but a structured-summary model puts
# the verdict in JSON instead ({"destination":"DONE"}). pp-flow's parser only
# understands the literal form, so the verdict is silently dropped and the card
# is advanced ONE column. That is fatal on the AUTO lane: a LOGIC-REVIEW PASS
# that should land the card in DONE (no HUMAN-REVIEW column) instead parks it
# in HUMAN-REVIEW, resurrecting exactly the human gate AUTO exists to remove
# (verified live: card 1f010ba9, JSON destination DONE, landed HUMAN-REVIEW).
# Bridge it: when the summary carries no literal DESTINATION: line but DOES
# carry a JSON-shaped destination, emit a literal line pp-flow will scrape.
# Only '[A-Z][A-Z0-9_-]*' destinations are emitted — the same shape pp-flow's
# own parser accepts — and pp-flow validates it against the card's real column
# set before acting, so a hallucinated column is refused there, not obeyed here.
if [ "${structured:-0}" = 0 ]; then
case "$result" in
  *DESTINATION:*) : ;;
  *)
    jsondest="$(printf '%s' "$result" | tr '\n' ' ' \
      | grep -o '"destination"[[:space:]]*:[[:space:]]*"[A-Z][A-Z0-9_-]*"' \
      | tail -1 | sed 's/.*:[[:space:]]*"//; s/"$//')"
    if [ -n "$jsondest" ]; then
      printf 'DESTINATION: %s\n' "$jsondest"
    fi
    ;;
esac
fi
exit 0
