#!/bin/ksh
# verify_session_split.sh - pin the session.ml split (i-1788671109-1).
#
# session.ml used to mix the interpreter driver, 498 lines of control
# prims, boot seeding, and the UI shell cache in one 2.5k-line module.
# The split:
#   - session_control_prims.ml owns the DB-touching control prims
#     (fork/merge/route/checkout/unbind/prune-synced/bench + private
#     __with_transaction__/__step__/with-public-write) and the
#     branch-ref helpers, parameterized by a hooks record (rehydrate +
#     checkout_to) so the module cycle stays broken.
#   - boot_seed.ml owns the additive libs seed, package discovery, the
#     agents-note seed, personal/agent branch bootstrap, and the
#     first-load recipes, parameterized by an ops record (load_library,
#     eval_one, rehydrate, checkout_to).
#   - session_types.ml groups the 25-field Session.t into ui_shell and
#     eval_state sub-records.
#
# This script pins:
#   1. The control prim registrations live ONLY in
#      session_control_prims.ml (no control prim re-registered or
#      dropped from session.ml).
#   2. The seeding entry points live ONLY in boot_seed.ml.
#   3. session.ml wires both modules in (hooks + ops) and re-exports
#      the moved names (Session.* API unchanged for http.ml / boot.ml /
#      attach_identity / tests).
#   4. Session.t has no top-level ui_*/env/load* fields left: all
#      shell/eval-state accesses in image/lib go through the
#      sub-records, and nothing replaces a sub-record wholesale.
#
# Negative-test: temporarily move a prim registration back into
# session.ml (or reintroduce `t.ui_nav_html`) and re-run - must FAIL.

set -eu
cd "$(dirname "$0")/.."

LIBDIR=image/lib
fail() { print -u2 "FAIL: $*"; exit 1; }
count() { grep -c "$1" "$2" 2>/dev/null || true; }

# --- 1. control prims: registered in session_control_prims, not session.ml
CONTROL_PRIMS="fork merge route public-route route-visibility checkout unbind prune-synced bench __with_transaction__ __step__ with-public-write"
for p in $CONTROL_PRIMS; do
  n=$(count "register.*tbl \"$p\"" $LIBDIR/session_control_prims.ml)
  [ "$n" -ge 1 ] || fail "control prim '$p' not registered in session_control_prims.ml"
  n=$(count "register.*tbl \"$p\"" $LIBDIR/session.ml)
  [ "$n" -eq 0 ] || fail "control prim '$p' is (also) registered in session.ml"
done
grep -q 'let register ~(hooks : hooks) (t : t) (tbl : Primitives.t)' \
  $LIBDIR/session_control_prims.ml \
  || fail "session_control_prims.register must take the hooks seam"
grep -q 'type hooks = {' $LIBDIR/session_control_prims.ml \
  || fail "hooks record missing from session_control_prims.ml"
grep -q 'Session_control_prims.register ~hooks:control_hooks' $LIBDIR/session.ml \
  || fail "session.ml must wire Session_control_prims.register with control_hooks"

# branch-ref helpers: defined once (control prims), re-exported by Session
for h in split_branch_ref branch_owner_authorized; do
  [ "$(count "^let $h " $LIBDIR/session_control_prims.ml)" -eq 1 ] \
    || fail "helper $h must be defined exactly once in session_control_prims.ml"
  grep -q "^let $h = Session_control_prims.$h" $LIBDIR/session.ml \
    || fail "session.ml must re-export $h"
done

# --- 2. seeding: entry points in boot_seed.ml, wired via ops
SEED_FNS="seed_one seed_agents_note seed_stdlib checkout_personal checkout_agent recipe_hello recipe_todo recipe_bible recipe_dabar recipe_notes"
for f in $SEED_FNS; do
  grep -q "^let $f ~(ops : ops)" $LIBDIR/boot_seed.ml \
    || fail "boot_seed.$f must take the ops seam"
  grep -q "^let $f (t : t)" $LIBDIR/session.ml \
    || fail "session.ml must re-export $f"
  n=$(count "^and $f " $LIBDIR/session.ml)
  [ "$n" -eq 0 ] || fail "$f still defined in session.ml's and-chain"
done
for f in seed_package_priority discover_seed_packages ensure_personal_branch ensure_agent_branch; do
  grep -q "^let $f" $LIBDIR/boot_seed.ml \
    || fail "boot_seed.ml must own $f"
  grep -q "Boot_seed.$f" $LIBDIR/session.ml \
    || fail "session.ml must reference Boot_seed.$f"
done
grep -q 'type ops = {' $LIBDIR/boot_seed.ml \
  || fail "ops record missing from boot_seed.ml"
grep -q 'Boot_seed.load_library = (fun t name -> load_library t name)' $LIBDIR/session.ml \
  || fail "session.ml must pass load_library via boot_ops"
grep -q 'Boot_seed.seed_stdlib ~ops:boot_ops' $LIBDIR/session.ml \
  || fail "boot_personal/make must call Boot_seed.seed_stdlib through the ops"

# --- 3. sub-records: Session.t field grouping
grep -q '^type ui_shell = {' $LIBDIR/session_types.ml \
  || fail "session_types.ml must define the ui_shell sub-record"
grep -q '^type eval_state = {' $LIBDIR/session_types.ml \
  || fail "session_types.ml must define the eval_state sub-record"
# no top-level ui_/eval-driver fields left on t (they moved into sub-records);
# check ONLY inside the `type t = { ... }` block (eval_state legitimately
# declares the same field names)
TBLOCK=$(sed -n '/^type t = {/,/^}/p' $LIBDIR/session_types.ml)
for fld in 'ui_nav_html' 'ui_css' 'ui_shell_reads' 'ui_shell_retry_at' \
           'load_stamp' 'def_journal_seq' 'thunk_bodies' 'load_skip_names' \
           'current_package' 'suppress_refresh_on_define' 'env'; do
  print "$TBLOCK" | grep -q "mutable $fld :" \
    && fail "field $fld must live in a sub-record, not on t"
done
# all accesses go through the sub-records: any dot access of a moved field
# must carry the eval_state./ui_shell. path OR be a module-path accessor
# call (receiver token starts uppercase, e.g. Session.ui_css s) - a bare
# lowercase receiver (t.env, session.prims, (!t).ui_nav_html) means a
# field escaped the regroup. request_context.ml's t is its own record type.
BAD=$(python3 - "$LIBDIR" <<'PYCHECK'
import re, glob, sys
libdir = sys.argv[1]
fields = (r'(env|prims|ui_nav_html|ui_css|ui_shell_reads|ui_shell_retry_at|'
          r'suppress_refresh_on_define|load_stamp|thunk_bodies|'
          r'def_journal_seq|load_skip_names|current_package)')
bad = []
for f in sorted(glob.glob(libdir + '/*.ml')):
    if 'request_context' in f:
        continue
    for i, line in enumerate(open(f, encoding='utf-8'), 1):
        if 'eval_state.' in line or 'ui_shell.' in line:
            continue
        for m in re.finditer(r'\.' + fields + r'\b', line):
            j = m.start() - 1
            while j >= 0 and (line[j].isalnum() or line[j] == '_'):
                j -= 1
            tok = line[j + 1:m.start()]
            if not tok or tok[0].isupper():
                continue  # module path / no receiver (".env" literal, Session.ui_css)
            bad.append(f"{f}:{i}:{line.rstrip()}")
if bad:
    print("\n".join(bad))
    sys.exit(1)
PYCHECK
) || fail "bare sub-record field access left behind:
$BAD"
# sub-records are mutated per field only - no wholesale replacement
BAD=$(grep -rnE '\.ui_shell *<-* *{|\.eval_state *<-* *{' $LIBDIR/*.ml || true)
[ -z "$BAD" ] || fail "sub-record replaced wholesale (must mutate per field):
$BAD"

print "OK: session split verified (control prims + boot seed + sub-records)"
