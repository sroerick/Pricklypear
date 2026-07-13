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

# --- PDF book via borge (optional if borge/pdflatex missing) ---
if command -v borge >/dev/null 2>&1; then
  echo "[public] borge book print …"
  (
    cd "$SRC"
    # stem writes STEM.pdf next to cwd; copy into OUT
    borge book print . -o "$OUT/pricklypear-book" || {
      echo "[public] WARN: borge book print failed (is pdflatex installed?)" >&2
    }
  )
  if [ -f "$OUT/pricklypear-book.pdf" ]; then
    cp -f "$OUT/pricklypear-book.pdf" "$OUT/book.pdf"
    echo "[public] wrote $OUT/book.pdf"
  fi
else
  echo "[public] WARN: borge not on PATH; skipping PDF" >&2
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
  --exclude='*.pdf' \
  --exclude='*.book.manifest' \
  . \
  2>/dev/null || {
    # OpenBSD tar may not like some long options — fallback
    (cd "$SRC" && tar -czf "$TGZ" \
      pricklypear.borg philosophy.borg architecture.borg single-image.borg \
      object-model.borg branching.borg lisp-interpreter.borg storage.borg \
      multi-user.borg auth.borg image.borg product-experience.borg \
      notes.borg calendar.borg flashcards.borg dabar.borg migrations.borg \
      workflow.borg ai-authoring.borg \
      lisp common image migrations libs scripts docs README.md AGENTS.md \
      dune-project 2>/dev/null) || true
  }

if [ -f "$TGZ" ]; then
  ls -la "$TGZ" "$OUT/book.pdf" 2>/dev/null || ls -la "$TGZ"
  echo "[public] done"
else
  echo "[public] ERROR: failed to write $TGZ" >&2
  exit 1
fi
