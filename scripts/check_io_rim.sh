#!/bin/ksh
# scripts/check_io_rim.sh — H2.6 lint: domain must not call Pg execute directly.
set -e
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"
bad=$(rg -n 'Pg\.execute' image/lib --type ml \
  | rg -v 'image/lib/io_lwt\.ml|image/lib/store_db\.ml|image/lib/io_eio\.ml' \
  | rg -v 'must use|Io_lwt|Io_eio|H2\.6|domain SQL' \
  || true)
if [ -n "$bad" ]; then
  echo "H2.6 FAIL: domain Pg execute outside Io rim:" >&2
  echo "$bad" >&2
  exit 1
fi
echo "H2.6 OK: only io_lwt/store_db/io_eio call Pg.execute"
