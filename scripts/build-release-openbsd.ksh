#!/bin/ksh
# Build an OpenBSD release archive for Pricklypear.
# Must run on OpenBSD 7.9 amd64 with the poohstack opam switch.
#
# Usage (from repo root):
#   /bin/ksh scripts/build-release-openbsd.ksh [out-dir]
#
# Produces: <out-dir>/pricklypear-openbsd.tar.gz

set -eu

PP_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT_DIR="${1:-$PP_ROOT/build}"
STAGE="$OUT_DIR/release"
ARCHIVE="$OUT_DIR/pricklypear-openbsd.tar.gz"

log() { print -ru2 -- "[release] $*"; }

cd "$PP_ROOT"

log "opam env (poohstack)"
eval "$(opam env --switch=poohstack --set-switch)"

log "dune build"
dune build \
  auth/bin/authd.exe \
  supervisor/bin/supervisor.exe \
  image/bin/unikernel.exe

AUTH_BIN="$PP_ROOT/_build/default/auth/bin/authd.exe"
SUP_BIN="$PP_ROOT/_build/default/supervisor/bin/supervisor.exe"
IMG_BIN="$PP_ROOT/_build/default/image/bin/unikernel.exe"

for b in "$AUTH_BIN" "$SUP_BIN" "$IMG_BIN"; do
  [ -f "$b" ] || { log "missing binary: $b"; exit 1; }
done

log "stage $STAGE"
rm -rf "$STAGE"
mkdir -p "$STAGE/bin" "$STAGE/migrations" "$STAGE/rc.d"

cp "$AUTH_BIN" "$STAGE/bin/authd"
cp "$SUP_BIN" "$STAGE/bin/supervisor"
cp "$IMG_BIN" "$STAGE/bin/image"
chmod 755 "$STAGE/bin/authd" "$STAGE/bin/supervisor" "$STAGE/bin/image"

cp migrations/*.sql "$STAGE/migrations/"
cp scripts/rc.d/pricklypear_authd "$STAGE/rc.d/pricklypear_authd"
cp scripts/rc.d/pricklypear_supervisor "$STAGE/rc.d/pricklypear_supervisor"
cp scripts/openbsd/run-authd.ksh "$STAGE/bin/run-authd.ksh"
cp scripts/openbsd/run-supervisor.ksh "$STAGE/bin/run-supervisor.ksh"
cp scripts/openbsd/pricklypear.env.example "$STAGE/pricklypear.env.example"
cp scripts/openbsd/pricklypear-promote.ksh "$STAGE/pricklypear-promote.ksh"
chmod 755 "$STAGE/bin/run-authd.ksh" "$STAGE/bin/run-supervisor.ksh" \
  "$STAGE/pricklypear-promote.ksh"

{
  sha="unknown"
  if git rev-parse HEAD >/dev/null 2>&1; then
    sha="$(git rev-parse HEAD)"
  elif [ -f .git/HEAD.sha ]; then
    sha="$(cat .git/HEAD.sha)"
  fi
  print -- "git_sha=${sha}"
  print -- "built_at=$(date -u +%Y-%m-%dT%H:%MZ)"
  print -- "built_on=$(hostname)"
  print -- "os=$(uname -srp)"
  print -- "ocaml=$(ocamlc -version 2>/dev/null || print unknown)"
} > "$STAGE/MANIFEST"

mkdir -p "$OUT_DIR"
tar -C "$STAGE" -czf "$ARCHIVE" .
log "wrote $ARCHIVE ($(ls -lh "$ARCHIVE" | awk '{print $5}'))"
print -- "$ARCHIVE"
