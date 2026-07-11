#!/bin/ksh
# Build an OpenBSD release archive for Pricklypear.
# Must run on OpenBSD 7.9 amd64 with the poohstack opam switch.
#
# Single-image model: one binary (image), one rc.d service, no supervisor/auth.
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
dune build image/bin/main.exe

IMG_BIN="$PP_ROOT/_build/default/image/bin/main.exe"
[ -f "$IMG_BIN" ] || { log "missing binary: $IMG_BIN"; exit 1; }

log "stage $STAGE"
rm -rf "$STAGE"
mkdir -p "$STAGE/bin" "$STAGE/migrations" "$STAGE/rc.d" "$STAGE/libs"

cp "$IMG_BIN" "$STAGE/bin/image"
chmod 755 "$STAGE/bin/image"

cp migrations/*.sql "$STAGE/migrations/"
# Lisp libraries (same text as in git) for load-library
if [ -d libs ]; then
  cp -R libs/. "$STAGE/libs/"
fi
cp scripts/rc.d/pricklypear "$STAGE/rc.d/pricklypear"
cp scripts/openbsd/run-image.ksh "$STAGE/bin/run-image.ksh"
cp scripts/openbsd/pricklypear.env.example "$STAGE/pricklypear.env.example"
cp scripts/openbsd/pricklypear-promote.ksh "$STAGE/pricklypear-promote.ksh"
chmod 755 "$STAGE/bin/run-image.ksh" "$STAGE/pricklypear-promote.ksh"
chmod 555 "$STAGE/rc.d/pricklypear"

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
