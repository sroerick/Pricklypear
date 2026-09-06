#!/usr/bin/env ksh
# verify_libs_layout.sh — the vault holds ONE lib.pp per package family.
#
# i-1788671331-d: libs/ carried 21 nested lib.pp dumps (libs/notes/capture,
# libs/todo/item, …) — stale pp-sync snapshots of sub-prefix binding
# groups, double-bookkeeping the family monoliths (libs/notes/lib.pp, …)
# and already drifted against them. borge/branching.borg LAYOUT CONVENTION:
#   libs/<pkg>/lib.pp     bootstrap + main vault (seed input)
#   backups/<ref>/...     non-main branch dumps (not seeded)
# borge/notes.borg PACKAGE FILE LAYOUT: subdirectory lib.pp files "are
# stale pp_sync snapshots and MUST NOT be maintained as a second source".
#
# Fails if any lib.pp exists deeper than libs/<pkg>/lib.pp, or if a
# package directory hides its lib.pp below the top level.

cd "$(dirname "$0")/.." || exit 1

strays=$(find libs -mindepth 3 -name lib.pp -type f 2>/dev/null)
if [ -n "$strays" ]; then
  print -u2 "FAIL: nested vault file(s) — stale pp_sync snapshots of a package family:"
  print -u2 "$strays" | sed 's/^/  /'
  print -u2 "The vault keeps ONE file per family: libs/<pkg>/lib.pp (boot seeds top-level only)."
  print -u2 "Non-main dumps belong in gitignored backups/ (pp-sync snapshot --branch)."
  exit 1
fi

print "OK: no nested lib.pp under libs/ (one file per package family)"
exit 0
