#!/bin/sh
# sync_filmstrip_shared.sh -- keep libs/filmstrip/shared (the filmstrip-shared
# package) in step with the shared filmstrip branch, then verify that PP's
# adapter still delegates the strip rendering to it.
#
# The shared package is the single home of the strip renderer: tuna and
# pricklypear both consume it. This script is deliberately offline-only: it
# checks the vendored copy is present and that libs/filmstrip/page.ml still
# routes its rendering through Filmstrip_shared.render_strip. Use a real
# checkout of the filmstrip-share branch to refresh the files here.
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
shared="$root/libs/filmstrip/shared"

if ! test -f "$shared/filmstrip_shared.ml"; then
  echo "sync_filmstrip_shared: missing $shared/filmstrip_shared.ml" >&2
  exit 1
fi

if ! test -f "$shared/dune"; then
  echo "sync_filmstrip_shared: missing $shared/dune" >&2
  exit 1
fi

if ! /usr/bin/grep -q "Filmstrip_shared.render_strip" "$root/libs/filmstrip/page.ml"; then
  echo "sync_filmstrip_shared: libs/filmstrip/page.ml does not call Filmstrip_shared.render_strip" >&2
  exit 1
fi

echo "sync_filmstrip_shared: libs/filmstrip/shared present; page.ml delegates to Filmstrip_shared.render_strip"
