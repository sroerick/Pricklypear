#!/bin/sh
# verify_vendor.sh — offline smoke for the vendor lock/materialize loop.
#
# Builds a throwaway git repo, pins its commit in a temp lock, runs the
# real scripts/vendor.sh, and checks the materialized package shape.
# No network, no PP image, no Postgres.

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/pp-vendor-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

SRC="$TMP/pkg-src"
VENDOR="$TMP/vendor"
mkdir -p "$SRC" "$VENDOR"

cat >"$SRC/lib.pp" <<'EOF'
(package zz-vendor-test (export zz-vendor-test/hello))
(define zz-vendor-test/hello (lambda () "hi"))
EOF

git -C "$SRC" -c init.defaultBranch=main init -q
git -C "$SRC" config user.email test@example.com
git -C "$SRC" config user.name test
git -C "$SRC" add lib.pp
git -C "$SRC" commit -qm fixture
SHA="$(git -C "$SRC" rev-parse HEAD)"

printf 'zz-vendor-test %s %s\n' "$SRC" "$SHA" >"$VENDOR/packages.lock"

run_vendor() {
  VENDOR_DIR="$VENDOR" \
  VENDOR_LOCK="$VENDOR/packages.lock" \
  VENDOR_SOURCES="$VENDOR/.sources" \
    sh "$ROOT/scripts/vendor.sh" "$@"
}

run_vendor fetch >/dev/null
test -f "$VENDOR/zz-vendor-test/lib.pp"
grep -q 'zz-vendor-test/hello' "$VENDOR/zz-vendor-test/lib.pp"
run_vendor status >/dev/null
run_vendor list | grep -q '^zz-vendor-test '
run_vendor clean >/dev/null
test ! -d "$VENDOR/zz-vendor-test"
test ! -d "$VENDOR/.sources"

printf '%s\n' "OK: vendor fetch/status/list/clean"
