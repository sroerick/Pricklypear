#!/bin/ksh
# pp-local — local-first pp development on this device (borge/local-first.borg).
#
# A thin wrapper over pp-remote that always wires the local lens
# (~/$REPO/lens): function defs and working-set reads (get, tree/get)
# resolve from the filesystem substrate FIRST; pure compute is local.
# No HTTP round trip on the dev-loop hot path.
#
#   export PP_URL=… PP_USER=… PP_PASS=…        (or ~/.config/pricklypear/env)
#   scripts/pp-local.sh -e '(+ 1 2)'           # one expr (lens-first, local)
#   scripts/pp-local.sh                        # stdin REPL (lens-first)
#   scripts/pp-local.sh pull-tree standup/run  # pull def + transitive deps
#   scripts/pp-local.sh pull-data user         # batched data.u.<you> pull
#   scripts/pp-local.sh pull-data shared       # batched data.shared pull
#   scripts/pp-local.sh sync                   # one data sync round
#   scripts/pp-local.sh sync-loop              # background rounds every 30s
#   scripts/pp-local.sh serve                  # local HTML surface (chat+eval)
#   scripts/pp-local.sh agent-context          # board digest from the lens (borge/agent.borg boot verb)
#   scripts/pp-local.sh memory-put TITLE --memory-content-file F [--tags a,b] [--id MID]  # stage-1 memory write (sync pushes it)
#   scripts/pp-local.sh memory-delete agent-memory <node-address>                          # remove + tombstone (drain deletes wire-side)
#
# Model (what is local vs wire):
#   - defs + (get …)/(tree/get …) reads: LENS FIRST, server on miss
#     (server miss degrades to "not found" when offline)
#   - pure compute: always local (placer keeps zero-prim exprs off the wire)
#   - (rows …)/(create …)/(update …)/(delete …)/with-transaction: wire
#     (the server is the index and the write authority — local-first.borg)
#   - local def edits: edit the envelope's body.src under lens/b/… (closest
#     branch wins over recorded ancestry). Code does NOT travel sync rounds;
#     landing an edit on the server is a separate promote (pp-eval define).
#   - data writes made server-side appear here after `sync`.
#
# Lens root: $PP_LENS (default: <repo>/lens, gitignored — the repo is the
# vault, the lens is this device's working copy; inspect with cat/jq/grep).

set -e
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
BUILD="$ROOT/_build/default/remote/bin/main.exe"

# habitat secrets (PP_URL / PP_USER / PP_PASS); never committed.
# Explicit environment WINS over the secrets file — an exported PP_URL
# (e.g. a dev-stack test) must not be silently clobbered with prod.
if [ -f "$HOME/.config/pricklypear/env" ]; then
  _pp_url="${PP_URL-}"; _pp_user="${PP_USER-}"; _pp_pass="${PP_PASS-}"; _pp_eval="${PP_EVAL-}"
  set -a; . "$HOME/.config/pricklypear/env"; set +a
  [ -n "$_pp_url" ]  && PP_URL="$_pp_url"
  [ -n "$_pp_user" ] && PP_USER="$_pp_user"
  [ -n "$_pp_pass" ] && PP_PASS="$_pp_pass"
  [ -n "$_pp_eval" ] && PP_EVAL="$_pp_eval"
  unset _pp_url _pp_user _pp_pass _pp_eval
fi

LENS="${PP_LENS:-$ROOT/lens}"
mkdir -p "$LENS"

is_python_script() {
  case "$1" in
    *.py) return 0 ;;
  esac
  return 1
}

# resolution order: $PP_REMOTE -> repo _build (fresh) -> PATH (may be a
# stale opam-installed pp-remote — prefer the repo build in a dev
# wrapper) -> dune exec
if [ -n "${PP_REMOTE:-}" ] && [ -x "$PP_REMOTE" ] && ! is_python_script "$PP_REMOTE"; then
  PP_REMOTE_BIN="$PP_REMOTE"
elif [ -x "$BUILD" ]; then
  PP_REMOTE_BIN="$BUILD"
elif command -v pp-remote >/dev/null 2>&1; then
  PP_REMOTE_BIN="pp-remote"
elif command -v dune >/dev/null 2>&1; then
  cd "$ROOT"
  if [ -n "${OPAM_SWITCH_PREFIX:-}" ] || command -v opam >/dev/null 2>&1; then
    eval "$(opam env --switch=poohstack --set-switch 2>/dev/null)" 2>/dev/null || true
  fi
  PP_REMOTE_BIN="dune exec remote/bin/main.exe --"
else
  echo "pp-local: no pp-remote found (build with: dune build remote/bin/main.exe)" >&2
  exit 1
fi

# pull/sync subcommands run-and-exit (pp-remote would otherwise drop
# into the REPL after the action; </dev/null ends it cleanly).
case "${1:-}" in
  pull)       shift; exec $PP_REMOTE_BIN --lens "$LENS" $(for n in "$@"; do printf -- '--lens-pull %s ' "$n"; done) </dev/null ;;
  pull-tree)  shift; exec $PP_REMOTE_BIN --lens "$LENS" $(for n in "$@"; do printf -- '--lens-pull-tree %s ' "$n"; done) </dev/null ;;
  pull-data)  shift; exec $PP_REMOTE_BIN --lens "$LENS" $(for n in "$@"; do printf -- '--lens-pull-data %s ' "$n"; done) </dev/null ;;
  sync)       shift; exec $PP_REMOTE_BIN --lens "$LENS" --sync "$@" </dev/null ;;
  sync-loop)  shift; exec $PP_REMOTE_BIN --lens "$LENS" --sync-loop "$@" ;;
  serve)      shift; exec $PP_REMOTE_BIN --lens "$LENS" --sync-loop --serve "$@" ;;
    agent-context) shift; exec $PP_REMOTE_BIN --lens "$LENS" --agent-context "$@" </dev/null ;;
    memory-put)    shift; exec $PP_REMOTE_BIN --lens "$LENS" --memory-put "$@" ;;
    memory-delete) shift; t="$1"; shift; exec $PP_REMOTE_BIN --lens "$LENS" --memory-type "$t" --memory-delete "$@" </dev/null ;;
    memory-get)     shift; exec $PP_REMOTE_BIN --lens "$LENS" --memory-get "$@" ;;
    memory-list)    shift; exec $PP_REMOTE_BIN --lens "$LENS" --memory-list "$@" ;;
  help|-h|--help)
    sed -n '2,40p' "$0" | sed 's/^# \{0,1\}//'
    exit 0 ;;
  *)          exec $PP_REMOTE_BIN --lens "$LENS" "$@" ;;
esac
