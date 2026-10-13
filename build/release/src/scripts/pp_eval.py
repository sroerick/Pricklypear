#!/usr/bin/env python3
"""Deprecated shim — use the OCaml CLI.

  dune build cli/bin/pp_eval.exe
  export PP_EVAL=$PWD/_build/default/cli/bin/pp_eval.exe
  # or: scripts/pp-eval.sh …

This file only execs the OCaml binary (or scripts/pp-eval.sh).
"""
from __future__ import annotations

import os
import sys


def main() -> int:
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    candidates = [
        os.environ.get("PP_EVAL_BIN", ""),
        os.path.join(root, "_build", "default", "cli", "bin", "pp_eval.exe"),
        "pp-eval",
    ]
    for c in candidates:
        if not c:
            continue
        if c == "pp-eval" or (os.path.isfile(c) and os.access(c, os.X_OK)):
            if c.endswith(".py"):
                continue
            os.execvp(c if c == "pp-eval" else c, [c, *sys.argv[1:]])
    sh = os.path.join(root, "scripts", "pp-eval.sh")
    if os.path.isfile(sh):
        os.execv("/bin/ksh", ["ksh", sh, *sys.argv[1:]])
    print(
        "pp_eval.py: OCaml pp-eval not found. Build:\n"
        "  eval $(opam env --switch=poohstack --set-switch)\n"
        "  dune build cli/bin/pp_eval.exe\n"
        f"  export PP_EVAL={root}/_build/default/cli/bin/pp_eval.exe",
        file=sys.stderr,
    )
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
