#!/usr/bin/env ksh
# verify_todo_exports.sh — the (package todo (export …)) manifest matches
# what other packages actually call.
#
# borge/workstreams.borg §lib-hygiene: "a bookkeeping check
# (scripts/verify_libs_layout.sh's sibling for exports) fails loudly when a
# lib calls an unexported todo symbol." This is that sibling.
#
# The todo family is one lib.pp (libs/todo/lib.pp). Cross-package callers
# (notes, kanban, workstreams, standup, filmstrip, calendar, ui, verify, and
# the pp-flow orchestration shell) may only reach symbols the manifest
# exports. Two failure modes are caught:
#   1. a lib/script calls a `todo/*` symbol that IS a define in the todo
#      package but is NOT exported (the bug T4 closes);
#   2. the manifest lists a `todo/*` name that is not a define at all
#      (stale export after a deletion).
#
# Usage:  scripts/verify_todo_exports.sh
# Exit 0 = clean, 1 = drift (with the offending names printed).

cd "$(dirname "$0")/.." || exit 1

TODO_LIB=libs/todo/lib.pp
if [ ! -f "$TODO_LIB" ]; then
  print -u2 "FAIL: $TODO_LIB not found (run from the repo root)"
  exit 1
fi

tmp=$(mktemp -d) || exit 1
trap 'rm -rf "$tmp"' EXIT

# --- strip `;` comments and "strings" so a name mentioned in prose or in a
#     UI label is not mistaken for a call. Line-oriented (pp has no
#     multi-line strings). ---
strip_pp() {
  awk '
    {
      line="";
      instr=0;
      n=length($0);
      for(i=1;i<=n;i++){
        c=substr($0,i,1);
        if(instr){
          if(c=="\\"){ i++; continue }
          if(c=="\""){ instr=0 }
          continue
        }
        if(c==";"){ break }
        if(c=="\""){ instr=1; line=line " "; continue }
        line=line c;
      }
      print line;
    }' "$1"
}

# --- exported names: the (export …) block that follows (package todo ---
sed -n '/^(package todo/,/^  (export/p' "$TODO_LIB" >/dev/null 2>&1
awk '
  /^\(package[ \t]+todo/ { inpkg=1 }
  inpkg && /^[ \t]*\(export/ { inex=1; next }
  inex && /^[ \t]*\)/ { exit }
  inex { print }
' "$TODO_LIB" | grep -oE 'todo/[A-Za-z0-9_/+*?!<>=.-]+' | sort -u > "$tmp/exported"

# --- every define name in the todo lib ---
grep -oE '^\(define[ \t]+todo/[A-Za-z0-9_/+*?!<>=.-]+' "$TODO_LIB" \
  | sed -E 's/^\(define[ \t]+//' | sort -u > "$tmp/defined"

# --- every todo/* token referenced from outside the todo lib ---
# Scope: libs/*.pp (the real cross-package callers) and scripts/*.sh|*.ksh
# (pp-flow evals the todo surface directly). image/*.ml is EXCLUDED on
# purpose: its todo/* mentions are doc-string examples, not calls, and OCaml
# cannot call a Nopales binding.
: > "$tmp/called"
for f in $(find libs scripts -type f \
             \( -name '*.pp' -o -name '*.sh' -o -name '*.ksh' \) 2>/dev/null); do
  case "$f" in
    libs/todo/lib.pp) continue ;;
  esac
  case "$f" in
    *.pp) strip_pp "$f" ;;
    *)    cat "$f" ;;
  esac | grep -oE 'todo/[A-Za-z0-9_/+*?!<>=.-]+' >> "$tmp/called" 2>/dev/null
done
sort -u "$tmp/called" > "$tmp/called.uniq"

# --- check 1: called + defined + not exported ---
# (restrict to tokens that ARE defines, so a typo'd/unknown name is a
#  different class of bug and does not spam this guard)
comm -12 "$tmp/called.uniq" "$tmp/defined" > "$tmp/called-defined"
comm -23 "$tmp/called-defined" "$tmp/exported" > "$tmp/unexported"

# --- check 2: exported but not defined ---
comm -23 "$tmp/exported" "$tmp/defined" > "$tmp/phantom"

fail=0
if [ -s "$tmp/unexported" ]; then
  fail=1
  print -u2 "FAIL: cross-package caller(s) use todo symbols that are NOT exported:"
  sed 's/^/  /' "$tmp/unexported" >&2
  print -u2 "Add them to (package todo (export …)) in $TODO_LIB, or stop calling them."
fi
if [ -s "$tmp/phantom" ]; then
  fail=1
  print -u2 "FAIL: (package todo (export …)) lists names that are not defines:"
  sed 's/^/  /' "$tmp/phantom" >&2
  print -u2 "Remove the stale entries from $TODO_LIB."
fi

if [ "$fail" -ne 0 ]; then
  exit 1
fi

print "OK: $(wc -l < "$tmp/exported" | tr -d ' ') todo exports cover every external caller ($(wc -l < "$tmp/called-defined" | tr -d ' ') distinct symbols called from outside)"
exit 0
