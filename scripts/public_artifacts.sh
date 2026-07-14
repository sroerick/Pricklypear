#!/bin/ksh
# Build public release artifacts: book.pdf + src.tgz
# Usage: scripts/public_artifacts.sh [OUT_DIR]
# Env: PP_ROOT (repo root), PP_SOURCE_ROOT (tree to pack; default PP_ROOT)

set -e
ROOT="${PP_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
SRC="${PP_SOURCE_ROOT:-$ROOT}"
OUT="${1:-${PP_PUBLIC_DIR:-$ROOT/public-artifacts}}"

mkdir -p "$OUT"

echo "[public] root=$ROOT src=$SRC out=$OUT"

# --- PDF book via borge, else vendored docs/pricklypear-book.pdf ---
# Build hosts may lack borge; regenerate the vendored PDF with:
#   borge book print . -o docs/pricklypear-book
BORGE=""
if command -v borge >/dev/null 2>&1; then
  BORGE="$(command -v borge)"
elif [ -x "${HOME}/.opam/poohstack/bin/borge" ]; then
  BORGE="${HOME}/.opam/poohstack/bin/borge"
fi

if [ -n "$BORGE" ]; then
  echo "[public] borge book print ($BORGE) …"
  (
    cd "$SRC"
    "$BORGE" book print . -o "$OUT/pricklypear-book" || {
      echo "[public] WARN: borge book print failed (is pdflatex installed?)" >&2
    }
  )
  if [ -f "$OUT/pricklypear-book.pdf" ]; then
    cp -f "$OUT/pricklypear-book.pdf" "$OUT/book.pdf"
    # keep vendored copy in sync when regenerating from full tree
    if [ "$SRC" = "$ROOT" ] && [ -d "$ROOT/docs" ]; then
      cp -f "$OUT/book.pdf" "$ROOT/docs/pricklypear-book.pdf"
    fi
    echo "[public] wrote $OUT/book.pdf"
  fi
fi

if [ ! -f "$OUT/book.pdf" ]; then
  VENDORED="$ROOT/docs/pricklypear-book.pdf"
  if [ -f "$VENDORED" ]; then
    cp -f "$VENDORED" "$OUT/book.pdf"
    echo "[public] used vendored $VENDORED → $OUT/book.pdf"
  elif [ -f "$SRC/docs/pricklypear-book.pdf" ]; then
    cp -f "$SRC/docs/pricklypear-book.pdf" "$OUT/book.pdf"
    echo "[public] used $SRC/docs/pricklypear-book.pdf → $OUT/book.pdf"
  else
    echo "[public] WARN: no book.pdf (install borge+pdflatex or commit docs/pricklypear-book.pdf)" >&2
  fi
fi

# --- source tarball (same allowlist spirit as /code) ---
TGZ="$OUT/src.tgz"
echo "[public] packing $TGZ"
# portable-ish: GNU/BSD tar with excludes
tar -C "$SRC" -czf "$TGZ" \
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
  --exclude='__pycache__' \
  --exclude='*.pyc' \
  --exclude='scripts/ship-to-vultr.ksh' \
  --exclude='scripts/post-receive-prod-hook.ksh' \
  --exclude='AGENTS.md' \
  --exclude='*.pdf' \
  --exclude='*.book.manifest' \
  . \
  2>/dev/null || {
    # OpenBSD tar may not like some long options — fallback
    (cd "$SRC" && tar -czf "$TGZ" \
      pricklypear.borg borge \
      lisp common image migrations libs scripts docs README.md LICENSE \
      dune-project 2>/dev/null) || true
    # strip habitat-private deploy scripts if fallback copied whole scripts/
    if [ -f "$TGZ" ]; then
      tmpdir="$(mktemp -d)"
      (cd "$tmpdir" && tar -xzf "$TGZ" 2>/dev/null) || true
      rm -f "$tmpdir/scripts/ship-to-vultr.ksh" \
            "$tmpdir/scripts/post-receive-prod-hook.ksh"
      find "$tmpdir" -type d -name '__pycache__' -exec rm -rf {} + 2>/dev/null || true
      (cd "$tmpdir" && tar -czf "$TGZ" .) || true
      rm -rf "$tmpdir"
    fi
  }

if [ -f "$TGZ" ]; then
  ls -la "$TGZ" "$OUT/book.pdf" 2>/dev/null || ls -la "$TGZ"
  echo "[public] done"
else
  echo "[public] ERROR: failed to write $TGZ" >&2
  exit 1
fi
