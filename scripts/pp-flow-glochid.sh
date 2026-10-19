#!/bin/ksh
# pp-flow-glochid.sh — PP_FLOW_PI adapter: run one pp-flow executor brief
# through glochid's headless piped-turn mode (--once).
#
# Contract (what pp-flow.sh expects of PP_FLOW_PI):
#   invoked as: $PP_FLOW_PI "<brief text>"
#   prints the terminal transcript to stdout (pp-flow scrapes the
#   EVIDENCE / DESTINATION lines from it); exits 0 iff the glochid turn
#   completed. glochid --once runs ONE full turn: model call -> tool
#   calls -> final reply, so a triage brief must be answerable in one
#   session of tool use.
#
# Env:
#   GLOCHID_BIN      default <repo>/_build/default/glochid/bin/main.exe
#   GLOCHID_MODEL    default hf:zai-org/GLM-4.7-Flash (glochid's own default)
#   GLOCHID_API_KEY  if unset, resolved from autolith's key store
#                    (~/.local/state/autolith/api-keys.sexp, synthetic)
#
# Known gap (10-19 vultr smoke): glochid ignores ~/.config/glochid/config.json
# here — config-file key had no effect, env key works — so the key travels
# through the environment until that is fixed.
set -u

BRIEF="${1:-}"
[ -n "$BRIEF" ] || { echo "pp-flow-glochid: no brief on argv" >&2; exit 2; }

DIR="${0%/*}"
BIN="${GLOCHID_BIN:-$DIR/../_build/default/glochid/bin/main.exe}"
[ -x "$BIN" ] || { echo "pp-flow-glochid: glochid binary not found: $BIN" >&2; exit 2; }

KEY="${GLOCHID_API_KEY:-}"
[ -n "$KEY" ] || KEY="$(grep -oE 'syn_[A-Za-z0-9]+' "$HOME/.local/state/autolith/api-keys.sexp" 2>/dev/null | head -1)"
[ -n "$KEY" ] || { echo "pp-flow-glochid: no synthetic api key found" >&2; exit 3; }

export GLOCHID_API_KEY="$KEY"
export GLOCHID_MODEL="${GLOCHID_MODEL:-hf:zai-org/GLM-4.7-Flash}"

# shellcheck disable=SC2086
echo "$BRIEF" | "$BIN" --once
exit $?
