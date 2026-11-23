#!/bin/sh
# verify_plugins.sh — offline structural check for monorepo Nopales plugins.
#
# Layout: <wyo>/plugins/<pkg>/lib.pp, one package per directory. The image
# loads that directory as a package root in dev (PP_LIBS_PATH) and the
# release stages each package under vendor/ beside libs/.
#
# No image, no Postgres, no network.

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)
PLUGINS="$ROOT/plugins"
PP_ROOT="$ROOT/pp"

fail() { printf 'verify_plugins: FAIL: %s\n' "$*" >&2; exit 1; }
ok() { printf 'ok   %s\n' "$*"; }

[ -d "$PP_ROOT/libs" ] || fail "missing $PP_ROOT/libs (core package root)"
[ -d "$PLUGINS" ] || fail "missing $PLUGINS (monorepo plugin root)"

n=0
for pkgdir in "$PLUGINS"/*; do
  [ -d "$pkgdir" ] || continue
  pkg=$(basename "$pkgdir")
  lib="$pkgdir/lib.pp"
  [ -f "$lib" ] || fail "plugin $pkg has no lib.pp"
  # The package name is the directory name; the loader grammar is PP's.
  if ! grep -Eq "^[[:space:]]*\\(package[[:space:]]+$pkg([[:space:])]|$)" "$lib"; then
    fail "plugin $pkg: lib.pp does not declare (package $pkg ...)"
  fi
  n=$((n + 1))
  ok "plugin $pkg"
done

[ "$n" -gt 0 ] || fail "no plugin packages found under $PLUGINS"

# One package per directory: a nested lib.pp would be a second package the
# loader cannot address.
strays=$(find "$PLUGINS" -mindepth 3 -name lib.pp -type f 2>/dev/null || true)
[ -z "$strays" ] || fail "nested lib.pp under plugins/: $strays"

# The dev and release roots point at the monorepo plugins directory, never
# at an individual plugin directory (the loader wants <root>/<pkg>/lib.pp).
for script in "$PP_ROOT/scripts/dev.sh" "$PP_ROOT/scripts/measure-boot.sh"; do
  grep -q '\$PP_ROOT/\.\./plugins' "$script" \
    || fail "$(basename "$script") does not add \$PP_ROOT/../plugins to PP_LIBS_PATH"
done
grep -q 'PLUGINS_DIR="\$PP_ROOT/\.\./plugins"' "$PP_ROOT/scripts/build-release-openbsd.ksh" \
  || fail "build-release-openbsd.ksh does not stage \$PP_ROOT/../plugins"

# pp-borge ships a corpus importer; its path mapping/escaping is pure and
# must stay in lockstep with the Nopales pp-borge/claim-path.
if [ -f "$PLUGINS/pp-borge/import_corpus.py" ]; then
  command -v python3 >/dev/null 2>&1 \
    || fail "python3 not found (needed for the pp-borge self-test)"
  python3 "$PLUGINS/pp-borge/import_corpus.py" --self-test \
    || fail "pp-borge import_corpus.py --self-test failed"
  ok "pp-borge importer self-test"
fi

# pp-borge stages a pinned claim at .borg/cards/<cid>.borg; the host write is
# pure and must keep writing exact bytes (and never a file on a skip).
if [ -f "$PLUGINS/pp-borge/materialize.py" ]; then
  command -v python3 >/dev/null 2>&1 \
    || fail "python3 not found (needed for the pp-borge materialize test)"
  python3 "$PLUGINS/pp-borge/materialize.py" --self-test \
    || fail "pp-borge materialize.py --self-test failed"
  ok "pp-borge materialize self-test"
fi

# pp-borge's drift floor treats borge's error verdict as the review failure and
# never turns a crash or an empty run into a comment. The offline half uses a
# fake borge, so it needs no binary, image or network.
if [ -f "$PLUGINS/pp-borge/drift_card.py" ]; then
  command -v python3 >/dev/null 2>&1 \
    || fail "python3 not found (needed for the pp-borge drift test)"
  python3 "$PLUGINS/pp-borge/drift_card.py" --self-test \
    || fail "pp-borge drift_card.py --self-test failed"
  ok "pp-borge drift self-test"
fi

ok "plugins root: $PLUGINS ($n package(s))"
