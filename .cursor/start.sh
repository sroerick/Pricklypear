#!/usr/bin/env bash
# Cloud Agent start — bring up the dev stack every boot.
#
# Delegates to the repo's own orchestrator, scripts/dev.sh (ksh), which:
#   * initdb's + starts a throwaway Postgres 16 on :5433 (socket in /tmp),
#   * applies migrations/ (version-gated) and seeds system/main,
#   * launches the single image binary (htmx UI :18080, line REPL :17878).
# Dev Postgres data lives under /tmp and is re-initialised per boot — this
# is the kernel-lab stack, so ephemeral state is expected.
set -euo pipefail

cd "$(dirname "$0")/.."

eval "$(opam env --switch=poohstack --set-switch)"

# Postgres 16 server binaries (initdb/pg_ctl) are not on PATH on Ubuntu.
export PATH="/usr/lib/postgresql/16/bin:$PATH"

# The OCaml `argon2` binding resolves libargon2 symbols via dlsym at runtime.
# The system linker's --as-needed drops the unreferenced DT_NEEDED entry from
# the image binary, so preload the shared object into the image process.
export LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libargon2.so.1${LD_PRELOAD:+:$LD_PRELOAD}"

exec scripts/dev.sh start
