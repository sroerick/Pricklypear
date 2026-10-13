# eval_json.sh — parse pp_eval -j / POST /api/eval JSON responses with
# python3 json, NOT `grep -q '"ok":true'` (which is whitespace/key-order
# fragile and hides the error body). Same approach as smoke_notes_ui.sh jval.
#
# Source it from a script living in scripts/:
#   . "$(dirname "$0")/eval_json.sh"
#
# Response shapes (cli/bin/pp_eval.ml, image /api/eval):
#   {"ok":true,"value":"..."} | {"ok":true,"value":null} | {"ok":false,"error":"..."}
#
# All functions read the JSON document on stdin; exit codes:
#   0 ok   1 parsed but condition false   2 unparseable/missing field
#
#   pp_json_ok           exit 0 iff ok is true; else one error line on stderr
#   pp_json_value        print decoded "value" (strings decoded, other types
#                        re-emitted as compact JSON; null -> exit 2, nothing)
#   pp_json_field NAME   same for an arbitrary top-level field

pp_json_field() {
  python3 -c '
import json, sys
field = sys.argv[1]
try:
    d = json.load(sys.stdin)
except Exception as e:
    print("eval_json: unparseable response: %s" % e, file=sys.stderr)
    sys.exit(2)
if not isinstance(d, dict) or field not in d:
    sys.exit(2)
v = d[field]
if isinstance(v, str):
    print(v)
elif v is None:
    sys.exit(2)
else:
    print(json.dumps(v), end="")
' "$1"
}

pp_json_value() {
  pp_json_field value
}

pp_json_ok() {
  python3 -c '
import json, sys
try:
    d = json.load(sys.stdin)
except Exception as e:
    print("eval_json: unparseable response: %s" % e, file=sys.stderr)
    sys.exit(2)
if isinstance(d, dict) and d.get("ok") is True:
    sys.exit(0)
print("eval error: %s" % (d.get("error", d) if isinstance(d, dict) else d), file=sys.stderr)
sys.exit(1)
'
}
