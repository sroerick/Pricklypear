#!/bin/sh
# vendor.sh — fetch pinned external Nopales packages into vendor/.
#
# Lock format and workflow: vendor/README.md.
# Generated copies are gitignored; only vendor/packages.lock is source.
#
# Env overrides (used by the test and by unusual layouts):
#   VENDOR_DIR      default: <repo>/vendor
#   VENDOR_LOCK     default: <VENDOR_DIR>/packages.lock
#   VENDOR_SOURCES  default: <VENDOR_DIR>/.sources

set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
VENDOR_DIR="${VENDOR_DIR:-$ROOT/vendor}"
VENDOR_LOCK="${VENDOR_LOCK:-$VENDOR_DIR/packages.lock}"
VENDOR_SOURCES="${VENDOR_SOURCES:-$VENDOR_DIR/.sources}"

die() {
  printf 'vendor: %s\n' "$*" >&2
  exit 1
}

usage() {
  printf '%s\n' "usage: vendor.sh [fetch|status|clean|list]" >&2
  exit 2
}

valid_pkg() {
  case "$1" in
    "" | *[!a-z0-9_-]*) return 1 ;;
    *) return 0 ;;
  esac
}

# Read lock rows as "package repo revision" with # comments. The lock
# deliberately has no quoted/space-containing fields: git URLs and
# revisions never need them.
each_lock_row() {
  handler=$1
  [ -f "$VENDOR_LOCK" ] || die "missing lock: $VENDOR_LOCK"
  rc=0
  while IFS= read -r line || [ -n "$line" ]; do
    line="${line%%#*}"
    # shellcheck disable=SC2086
    set -- $line
    [ "$#" -eq 0 ] && continue
    [ "$#" -eq 3 ] || die "bad lock row (want: package repo revision): $line"
    pkg=$1
    repo=$2
    rev=$3
    valid_pkg "$pkg" || die "bad package name in lock: $pkg"
    "$handler" "$pkg" "$repo" "$rev" || rc=1
  done <"$VENDOR_LOCK"
  return "$rc"
}

fetch_one() {
  pkg=$1
  repo=$2
  rev=$3
  src="$VENDOR_SOURCES/$pkg"
  dest="$VENDOR_DIR/$pkg"

  mkdir -p "$VENDOR_SOURCES"
  if [ ! -d "$src/.git" ]; then
    git -c init.defaultBranch=main init -q "$src"
    git -C "$src" remote add origin "$repo"
  else
    git -C "$src" remote set-url origin "$repo"
  fi

  if ! git -C "$src" fetch -q --depth 1 origin "$rev"; then
    die "$pkg: cannot fetch '$rev' from $repo"
  fi
  got="$(git -C "$src" rev-parse FETCH_HEAD)"

  # A full or abbreviated hex revision must actually match; a tag or
  # branch name was resolved by fetch and needs no extra check.
  case "$rev" in
    *[!0-9a-f]* ) ;;
    *)
      case "$got" in
        "$rev"*) ;;
        *) die "$pkg: fetched $got, expected $rev" ;;
      esac
      ;;
  esac

  git -C "$src" checkout -q --detach FETCH_HEAD
  rm -rf "$dest"
  mkdir -p "$dest"
  git -C "$src" archive --format=tar FETCH_HEAD | tar -x -C "$dest"
  [ -f "$dest/lib.pp" ] || die "$pkg: no lib.pp at the repo root"
  printf 'vendor: %s %s -> %s\n' "$pkg" "$rev" "$dest" >&2
}

status_one() {
  pkg=$1
  dest="$VENDOR_DIR/$pkg/lib.pp"
  if [ -f "$dest" ]; then
    printf '%s\n' "ok      $pkg"
  else
    printf '%s\n' "missing $pkg"
    return 1
  fi
}

list_one() {
  printf '%s %s %s\n' "$1" "$2" "$3"
}

clean_all() {
  if [ -d "$VENDOR_DIR" ]; then
    find "$VENDOR_DIR" -mindepth 1 -maxdepth 1 -type d \
      ! -name .sources -exec rm -rf {} +
    rm -rf "$VENDOR_SOURCES"
  fi
  printf 'vendor: cleaned generated packages and %s\n' "$VENDOR_SOURCES" >&2
}

cmd="${1:-fetch}"
[ "$#" -le 1 ] || usage
case "$cmd" in
  fetch)
    each_lock_row fetch_one
    ;;
  status)
    if each_lock_row status_one; then
      exit 0
    else
      exit 1
    fi
    ;;
  list)
    each_lock_row list_one
    ;;
  clean)
    clean_all
    ;;
  *)
    usage
    ;;
esac
