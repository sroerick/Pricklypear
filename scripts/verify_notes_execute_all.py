#!/usr/bin/env python3
"""Deprecated wrapper — core checks moved to libs/verify + verify_habitat.sh.

Still runs a few BI evals after the habitat suite (kept thin).
Prefer: scripts/verify_habitat.sh
"""
from __future__ import annotations

import json
import os
import subprocess
import sys

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PP_EVAL = os.environ.get("PP_EVAL", os.path.join(root, "scripts", "pp_eval.py"))
PP_URL = os.environ.get("PP_URL", "http://127.0.0.1:18080")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def eval_j(expr: str) -> dict:
    r = subprocess.run(
        ["python3", PP_EVAL, "-j", expr],
        capture_output=True,
        text=True,
        env={**os.environ, "PP_URL": PP_URL, "PP_USER": USER, "PP_PASS": PASS, "PP_EVAL": PP_EVAL},
    )
    for line in reversed((r.stdout or "").splitlines()):
        if line.strip().startswith("{"):
            return json.loads(line.strip())
    return {"ok": False, "error": r.stdout[:300] or r.stderr[:300]}


def main() -> int:
    print(
        "verify_notes_execute_all.py: habitat suite + thin BI checks",
        file=sys.stderr,
    )
    rc = subprocess.call(
        ["ksh", os.path.join(root, "scripts", "verify_habitat.sh")],
        cwd=root,
        env=os.environ,
    )
    if rc != 0:
        return rc
    # BI MVP smoke (Lisp)
    for expr, label in [
        ('(load-library "bi")', "load bi"),
        ("(bi/seed-sample-data)", "seed"),
        ("(bi/metrics-snapshot)", "metrics"),
    ]:
        d = eval_j(expr)
        if not d.get("ok"):
            print(f"FAIL BI {label}: {d.get('error')}")
            return 1
        print(f"PASS BI {label}")
    print("ALL PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
