#!/usr/bin/env python3
"""Deprecated shim — use OCaml pp-sync / scripts/pp-sync.sh."""
import os, sys
root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
bin_ = os.path.join(root, "_build", "default", "cli", "bin", "pp_sync.exe")
sh = os.path.join(root, "scripts", "pp-sync.sh")
if os.path.isfile(bin_) and os.access(bin_, os.X_OK):
    os.execv(bin_, [bin_, *sys.argv[1:]])
if os.path.isfile(sh):
    os.execv("/bin/ksh", ["ksh", sh, *sys.argv[1:]])
print("pp_sync.py: build dune cli/bin/pp_sync.exe", file=sys.stderr)
sys.exit(2)
