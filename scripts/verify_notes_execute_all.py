#!/usr/bin/env python3
"""Deprecated — use scripts/verify_habitat.sh (OCaml pp-eval + libs/verify).

Thin wrapper kept for old docs/CI.
"""
import os
import subprocess
import sys

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
print("verify_notes_execute_all.py → verify_habitat.sh", file=sys.stderr)
rc = subprocess.call(
    ["ksh", os.path.join(root, "scripts", "verify_habitat.sh")],
    cwd=root,
    env=os.environ,
)
if rc != 0:
    raise SystemExit(rc)
# optional BI
pp = os.environ.get("PP_EVAL") or os.path.join(
    root, "_build", "default", "cli", "bin", "pp_eval.exe"
)
if not (os.path.isfile(pp) and os.access(pp, os.X_OK)):
    pp = os.path.join(root, "scripts", "pp-eval.sh")
for expr, label in [
    ('(load-library "bi")', "load bi"),
    ("(bi/seed-sample-data)", "seed"),
    ("(bi/metrics-snapshot)", "metrics"),
]:
    r = subprocess.run([pp, "-j", expr], capture_output=True, text=True, env=os.environ)
    out = (r.stdout or "") + (r.stderr or "")
    if r.returncode != 0 or '"ok":false' in out.replace(" ", "").lower() or '"ok": false' in out:
        # also handle compact JSON ok:false
        if '"ok":false' in out.replace(" ", "") or '"ok": false' in out:
            print(f"FAIL BI {label}: {out}")
            raise SystemExit(1)
        if r.returncode != 0:
            print(f"FAIL BI {label}: {out}")
            raise SystemExit(1)
    print(f"PASS BI {label}")
print("ALL PASSED")
raise SystemExit(0)
