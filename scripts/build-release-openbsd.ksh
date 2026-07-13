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

# Prior releases stage a full source tree under build/release/src; if that
# tree remains, dune sees a second dune-project and aborts. Clear it first.
rm -rf "$STAGE"

log "opam env (poohstack)"
eval "$(opam env --switch=poohstack --set-switch)"

log "dune build"
dune build image/bin/main.exe

IMG_BIN="$PP_ROOT/_build/default/image/bin/main.exe"
[ -f "$IMG_BIN" ] || { log "missing binary: $IMG_BIN"; exit 1; }

log "stage $STAGE"
rm -rf "$STAGE"
mkdir -p "$STAGE/bin" "$STAGE/migrations" "$STAGE/rc.d" "$STAGE/libs" \
  "$STAGE/static" "$STAGE/src" "$STAGE/public"

cp "$IMG_BIN" "$STAGE/bin/image"
chmod 755 "$STAGE/bin/image"

cp migrations/*.sql "$STAGE/migrations/"
# Lisp libraries (same text as in git) for load-library
if [ -d libs ]; then
  cp -R libs/. "$STAGE/libs/"
fi
# Vendored browser assets (CodeMirror for /code)
if [ -d image/static ]; then
  cp -R image/static/. "$STAGE/static/"
fi
# Pruned source tree for public /code browser + /src.tgz
log "pack public source tree"
(
  cd "$PP_ROOT"
  tar -cf - \
    --exclude='_build' \
    --exclude='.git' \
    --exclude='.pi' \
    --exclude='.ralph' \
    --exclude='node_modules' \
    --exclude='_opam' \
    --exclude='.opamswitch' \
    --exclude='spikes' \
    --exclude='supervisor' \
    --exclude='.borge-bugs' \
    --exclude='public-artifacts' \
    --exclude='build' \
    --exclude='__pycache__' \
    --exclude='*.pyc' \
    --exclude='scripts/ship-to-vultr.ksh' \
    --exclude='scripts/post-receive-prod-hook.ksh' \
    --exclude='*.pdf' \
    --exclude='*.book.manifest' \
    . \
  | tar -C "$STAGE/src" -xf -
) 2>/dev/null || {
  # OpenBSD tar may lack GNU long options — copy key trees
  for d in lisp common image migrations libs scripts docs borge; do
    [ -d "$d" ] && cp -R "$d" "$STAGE/src/"
  done
  for f in *.borg README.md AGENTS.md dune-project; do
    [ -f "$f" ] && cp "$f" "$STAGE/src/"
  done
  # habitat-private deploy automation stays out of public source
  rm -f "$STAGE/src/scripts/ship-to-vultr.ksh" \
        "$STAGE/src/scripts/post-receive-prod-hook.ksh"
  find "$STAGE/src" -type d -name '__pycache__' -exec rm -rf {} +
  find "$STAGE/src" -type f -name '*.pyc' -delete
}
# Public artifacts (PDF book + tarball).
# Prefer regenerating via borge; falls back to docs/pricklypear-book.pdf
# (vendored) when borge/pdflatex are not on the build host.
if [ -x scripts/public_artifacts.sh ]; then
  log "public artifacts → stage/public"
  # Ensure vendored book is in the staged src so fallback can find it
  if [ -f docs/pricklypear-book.pdf ]; then
    mkdir -p "$STAGE/src/docs"
    cp -f docs/pricklypear-book.pdf "$STAGE/src/docs/pricklypear-book.pdf"
  fi
  PP_ROOT="$PP_ROOT" PP_SOURCE_ROOT="$STAGE/src" PP_PUBLIC_DIR="$STAGE/public" \
    /bin/ksh scripts/public_artifacts.sh "$STAGE/public" \
    || log "WARN: public_artifacts failed (book/tgz may be incomplete)"
  if [ ! -f "$STAGE/public/book.pdf" ] && [ -f docs/pricklypear-book.pdf ]; then
    cp -f docs/pricklypear-book.pdf "$STAGE/public/book.pdf"
    log "copied vendored docs/pricklypear-book.pdf → stage/public/book.pdf"
  fi
  if [ ! -f "$STAGE/public/book.pdf" ]; then
    log "WARN: release has no book.pdf"
  fi
else
  log "WARN: scripts/public_artifacts.sh missing"
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
