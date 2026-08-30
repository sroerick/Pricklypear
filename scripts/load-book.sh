#!/bin/ksh
# load-book.sh — seed a `borge book export` JSON into the one tree.
#
#   scripts/load-book.sh <export.book.json> [flags...]
#
# Thin wrapper over scripts/load_book.py (habitat-loader, P2.1). Env
# contract = pp-eval: PP_URL (default http://127.0.0.1:18080), PP_USER
# (default admin), PP_PASS / PP_PASS_FILE. Flags: --dry-run --out SEED.pp
# --prune --canary --url URL --user U --password …
#
# Additive seed discipline: writes ONLY typed book rows under data.book
# (type book-chapter); never touches other paths. Prod seeding is an
# explicit operator step (the promote gate, not this script, is the
# git->prod path for product code; this seeds the read-projection).

SCRIPT_DIR=$(dirname "$0")
exec python3 "$SCRIPT_DIR/load_book.py" "$@"
