#!/usr/bin/env python3
"""Load libs/upsets/lib.pp onto a habitat branch via ONE persistent
pp_eval -i session (checkout once; defines bind that session's env and
persist to the branch's function_defs)."""
import subprocess
import sys

sys.path.insert(0, "scripts")
from load_upsets_lib import split_forms  # noqa: E402

SRC = sys.argv[1] if len(sys.argv) > 1 else "libs/upsets/lib.pp"
# Home = admin/personal (fresh sessions attach there; jobs submitted
# without an explicit branch run there). Avoid begin-checkout splits:
# defines go to the session default branch, period.
BRANCH = sys.argv[2] if len(sys.argv) > 2 else "admin/personal"

forms = split_forms(open(SRC).read())
print(f"{len(forms)} top-level forms from {SRC}", file=sys.stderr)

lines = [" ".join(f.split()) for f in forms]
lines += [
    "(string-append \"loaded: \" (as-str (length (list))))",
]
inp = "\n".join(lines) + "\n:quit\n"

p = subprocess.run(
    ["_build/default/cli/bin/pp_eval.exe", "-i"],
    input=inp, capture_output=True, text=True, timeout=600,
)
out = p.stdout
errs = [ln for ln in out.splitlines() if "error" in ln.lower() or "unbound" in ln.lower()]
print(out[-2500:])
if errs:
    print(f"\n!! {len(errs)} suspicious lines:", file=sys.stderr)
    for e in errs[:10]:
        print("  " + e[:200], file=sys.stderr)
    sys.exit(1)
print("session closed cleanly")
