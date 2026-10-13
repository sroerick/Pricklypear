#!/bin/sh
# canonical copy: adopted from gregor pp-share 10-08; the live copy + crons stay ~/pp-share on vultr
# smoke-purge.sh - remove ws-flow-smoke junk rows from prod PP (gregor 10-01).
# Naive todo/list/delete trips the 400k-step /api/eval eval budget (the todo
# library's delete verbs are scan-shaped interpreted walks); this purges via
# raw tree prims by literal path: enumerate junk rows offline from the local
# lens, batch-probe existence on prod, batch-delete survivors, verify.
#
# v3: junk lists respawn hourly (see mem 1U54V6B). Deleted list ids are
# remembered in ~/pp-share/.smoke-purge-deleted.ids and every pass sweeps
# items/comments referencing them in hex AND dash form (item rows store
# dash list_ids; lens node files are hex-named). Enumerate/probe/delete
# cycles up to 3 passes with a sync between. Local lens copies of purged
# rows are removed so pp-local sync cannot re-push them (sync never
# propagates deletions).
set -eu
PP_LOCAL="${PP_LOCAL:-$HOME/.local/bin/pp-local}"
PP_BUILD_PAYLOAD="${PP_BUILD_PAYLOAD:-$HOME/pp-share/build-payload.lisp}"
"$PP_LOCAL" sync >/dev/null 2>&1 || echo "WARN: opening pp-local sync failed rc=$? (prod blip?); continuing with stale lens"
DATA="${PP_LENS_DATA:-$HOME/pp-lens/data}/u/Z3JlZ29y0"
L=dG9kby1saXN00; I=dG9kby1pdGVt0; C=dG9kby1jb21tZW500
STATE="${PP_PURGE_STATE:-$HOME/pp-share/.smoke-purge-deleted.ids}"
touch "$STATE"
T=$(cat ~/.config/pricklypear/pp.token)

dash() { printf '%s' "$1" | awk '{printf "%s-%s-%s-%s-%s", substr($0,1,8), substr($0,9,4), substr($0,13,4), substr($0,17,4), substr($0,21,12)}'; }

pass=0
while [ "$pass" -lt 3 ]; do
  pass=$((pass+1))
  : > /tmp/sp-rows.txt
  for f in $(grep -l '"ws-flow-smoke"' $DATA/$L/*.json 2>/dev/null || true); do
    echo "$(basename "$f" .json) $L" >> /tmp/sp-rows.txt
  done
  sort -u /tmp/sp-rows.txt -o /tmp/sp-rows.txt
  # all candidate junk list ids: current junk lists + everything ever purged
  cands=$(awk -v d="$L" '$2==d {print $1}' /tmp/sp-rows.txt; cat "$STATE")
  # items referencing any candidate id in hex or dash form
  for u in $cands; do
    [ -z "$u" ] && continue
    h=$(printf '%s' "$u" | tr -d '-')
    d=$(dash "$u")
    grep -l -e "$u" -e "$h" -e "$d" $DATA/$I/*.json 2>/dev/null || true
  done | while read f; do echo "$(basename "$f" .json) $I"; done >> /tmp/sp-rows.txt
  sort -u /tmp/sp-rows.txt -o /tmp/sp-rows.txt
  # comments referencing junk items (both forms) or junk lists
  jcands=$(awk -v d="$I" '$2==d {print $1}' /tmp/sp-rows.txt; printf '%s\n' $cands)
  for u in $jcands; do
    [ -z "$u" ] && continue
    h=$(printf '%s' "$u" | tr -d '-')
    d=$(dash "$u")
    grep -l -e "$u" -e "$h" -e "$d" $DATA/$C/*.json 2>/dev/null || true
  done | while read f; do echo "$(basename "$f" .json) $C"; done >> /tmp/sp-rows.txt
  sort -u /tmp/sp-rows.txt -o /tmp/sp-rows.txt
  n=$(wc -l < /tmp/sp-rows.txt | tr -d ' ')
  if [ "$n" = 0 ]; then
    echo "pass $pass: nothing to purge"
    break
  fi
  echo "pass $pass: $n junk rows in lens"
  { echo '(list'
    while read hex dir; do
      echo "  (if (null? (tree/get \"data.u.Z3JlZ29y0.$dir.$hex\")) 0 1)"
    done < /tmp/sp-rows.txt
    echo ')'; } > /tmp/sp-probe.expr
  /usr/local/bin/sbcl --script "${PP_BUILD_PAYLOAD:-$HOME/pp-share/build-payload.lisp}" /tmp/sp-probe.expr /tmp/sp-probe.json
  curl -s --max-time 150 -H "Authorization: Bearer $T" -H 'Content-Type: application/json' \
    -d @/tmp/sp-probe.json https://pricklypear.rocks/api/eval > /tmp/sp-probe.out
  sed -n 's/.*"value":\[\([^]]*\)\].*/\1/p' /tmp/sp-probe.out | tr -d ', \n' | fold -w1 > /tmp/sp-flags.txt
  paste -d' ' /tmp/sp-rows.txt /tmp/sp-flags.txt | awk '$3==1 {print $1, $2}' > /tmp/sp-exec.txt
  m=$(wc -l < /tmp/sp-exec.txt | tr -d ' ')
  if [ "$m" = 0 ]; then
    echo "all $n lens rows already gone from prod"
    while read hex dir; do [ -f "$DATA/$dir/$hex.json" ] && rm -f "$DATA/$dir/$hex.json"; done < /tmp/sp-rows.txt
    break
  fi
  echo "deleting $m live rows on prod (raw tree/delete batch)..."
  { echo '(list'
    while read hex dir; do
      printf '  (tree/delete "data.u.Z3JlZ29y0.%s.%s")\n' "$dir" "$hex"
    done < /tmp/sp-exec.txt
    echo ')'; } > /tmp/sp-del.expr
  /usr/local/bin/sbcl --script "${PP_BUILD_PAYLOAD:-$HOME/pp-share/build-payload.lisp}" /tmp/sp-del.expr /tmp/sp-del.json
  curl -s --max-time 150 -H "Authorization: Bearer $T" -H 'Content-Type: application/json' \
    -d @/tmp/sp-del.json https://pricklypear.rocks/api/eval > /tmp/sp-del.out -w 'http=%{http_code}\n'
  while read hex dir; do
    [ "$dir" = "$L" ] && printf '%s\n' "$hex" >> "$STATE"
    [ -f "$DATA/$dir/$hex.json" ] && rm -f "$DATA/$dir/$hex.json"
  done < /tmp/sp-exec.txt
  sort -u "$STATE" -o "$STATE"
  "$PP_LOCAL" sync >/dev/null 2>&1
done

echo 'verify - smoke lists left on prod:'
printf '(begin (load-library "todo") (define ls (todo/list/all)) (length (filter (lambda (p) (string=? (as-str (dict-get (cdr p) "title")) "ws-flow-smoke")) ls)))' > /tmp/sp-verify.expr
/usr/local/bin/sbcl --script "${PP_BUILD_PAYLOAD:-$HOME/pp-share/build-payload.lisp}" /tmp/sp-verify.expr /tmp/sp-verify.json
curl -s --max-time 90 -H "Authorization: Bearer $T" -H 'Content-Type: application/json' \
  -d @/tmp/sp-verify.json https://pricklypear.rocks/api/eval; echo
