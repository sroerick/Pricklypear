#!/usr/bin/env python3
"""Deprecated wrapper — product budget stress lives on-image (libs/verify).

Prefer: scripts/verify_habitat.sh
Kept so old docs/CI invocations still work.
"""
import os
import subprocess
import sys

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
script = os.path.join(root, "scripts", "verify_habitat.sh")
print(
    "verify_budget_stress.py: thin wrapper → verify_habitat.sh "
    "(libs/verify on image)",
    file=sys.stderr,
)
raise SystemExit(subprocess.call(["ksh", script], cwd=root, env=os.environ))
