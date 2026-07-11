#!/usr/bin/env python3
"""Sync libs/*.pp with live Pricklypear function_defs (language state).

  git  libs/ui/lib.pp  ◄── pull / push ──►  live image (export-package / load-library)

Uses the same auth as pp_eval (PP_URL, PP_USER, PP_PASS).

Commands:
  status [pkg]   compare local define names vs live package-bindings
  pull <pkg>     write libs/<pkg>/lib.pp from live (export-package)
  push <pkg>     load-library <pkg> on live (file → DB)
  pull-all       pull every package that exists live or under libs/

Examples:
  set -a; . ~/.config/pricklypear/env; set +a
  python3 scripts/pp_sync.py status ui
  python3 scripts/pp_sync.py pull ui
  python3 scripts/pp_sync.py push todo
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
from pathlib import Path

# Reuse eval client
sys.path.insert(0, str(Path(__file__).resolve().parent))
from pp_eval import eval_expr, env_or, load_password  # type: ignore


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def libs_dir() -> Path:
    return repo_root() / "libs"


def pkg_path(pkg: str) -> Path:
    if pkg in ("", "(root)", "root"):
        return libs_dir() / "_root" / "lib.pp"
    return libs_dir() / pkg / "lib.pp"


def parse_local_defines(text: str) -> set[str]:
    """Extract top-level (define name …) names from .pp text (heuristic)."""
    names: set[str] = set()
    # strip line comments
    lines = []
    for line in text.splitlines():
        if ";" in line:
            # crude: drop from ; outside strings — good enough for our libs
            in_str = False
            out = []
            i = 0
            while i < len(line):
                c = line[i]
                if c == '"' and (i == 0 or line[i - 1] != "\\"):
                    in_str = not in_str
                    out.append(c)
                elif c == ";" and not in_str:
                    break
                else:
                    out.append(c)
                i += 1
            line = "".join(out)
        lines.append(line)
    blob = "\n".join(lines)
    for m in re.finditer(r"\(\s*define\s+([^\s()]+)", blob):
        names.add(m.group(1))
    return names


def unwrap_value(result: dict) -> str | None:
    if not result.get("ok"):
        return None
    v = result.get("value", "")
    # pp_value of VStr adds outer quotes and escapes
    if isinstance(v, str) and len(v) >= 2 and v[0] == '"' and v[-1] == '"':
        try:
            return json.loads(v)
        except Exception:
            return v[1:-1].encode("utf-8").decode("unicode_escape")
    return str(v)


def unwrap_list(result: dict) -> list[str]:
    raw = unwrap_value(result)
    if raw is None:
        return []
    # Lisp list print: ("a" "b") or (a b)
    names = re.findall(r'"([^"]+)"', raw)
    if names:
        return names
    inner = raw.strip()
    if inner.startswith("(") and inner.endswith(")"):
        inner = inner[1:-1].strip()
    if not inner:
        return []
    return [p for p in inner.split() if p]


def live_eval(args, expr: str) -> dict:
    return eval_expr(
        base_url=args.url,
        user=args.user,
        password=args.password,
        expr=expr,
        timeout=args.timeout,
    )


def cmd_status(args) -> int:
    pkgs = [args.package] if args.package else discover_packages(args)
    rc = 0
    for pkg in pkgs:
        path = pkg_path(pkg)
        local: set[str] = set()
        if path.is_file():
            local = parse_local_defines(path.read_text(encoding="utf-8"))
        r = live_eval(args, f'(package-bindings "{pkg}")')
        if not r.get("ok"):
            print(f"{pkg}: live error: {r.get('error')}", file=sys.stderr)
            rc = 1
            continue
        live = set(unwrap_list(r))
        only_local = sorted(local - live)
        only_live = sorted(live - local)
        both = sorted(local & live)
        label = pkg if pkg else "(root)"
        print(f"== {label} ==")
        print(f"  local file: {path if path.is_file() else '(missing)'}")
        print(f"  both: {len(both)}  only-local: {len(only_local)}  only-live: {len(only_live)}")
        if only_local:
            print("  only-local:", ", ".join(only_local))
        if only_live:
            print("  only-live:", ", ".join(only_live))
        if both and args.verbose:
            print("  both:", ", ".join(both))
    return rc


def cmd_pull(args) -> int:
    pkg = args.package
    if not pkg:
        print("pull: need package name", file=sys.stderr)
        return 2
    r = live_eval(args, f'(export-package "{pkg}")')
    if not r.get("ok"):
        print(r.get("error", "export failed"), file=sys.stderr)
        return 1
    text = unwrap_value(r)
    if text is None:
        print("pull: empty export", file=sys.stderr)
        return 1
    path = pkg_path(pkg)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text if text.endswith("\n") else text + "\n", encoding="utf-8")
    print(f"wrote {path} ({len(text)} bytes)")
    return 0


def cmd_push(args) -> int:
    pkg = args.package
    if not pkg:
        print("push: need package name", file=sys.stderr)
        return 2
    path = pkg_path(pkg)
    if not path.is_file():
        print(f"push: missing {path}", file=sys.stderr)
        return 1
    # Prefer load-library (reads release/CWD libs). For laptop push of
    # working tree, use load-string of file contents so git tree wins.
    src = path.read_text(encoding="utf-8")
    # Escape for Lisp string: use JSON string encoding
    lit = json.dumps(src)
    r = live_eval(args, f"(load-string {lit})")
    if not r.get("ok"):
        print(r.get("error", "push failed"), file=sys.stderr)
        return 1
    print(unwrap_value(r) or r.get("value"))
    print(f"pushed {path} → live via load-string")
    return 0


def discover_packages(args) -> list[str]:
    pkgs: set[str] = set()
    # from libs/
    ld = libs_dir()
    if ld.is_dir():
        for p in ld.iterdir():
            if p.is_dir() and (p / "lib.pp").is_file():
                if p.name != "_root":
                    pkgs.add(p.name)
    # from live: list-bindings and package_of_name
    r = live_eval(args, "(list-bindings)")
    if r.get("ok"):
        for name in unwrap_list(r):
            if "/" in name:
                pkgs.add(name.rsplit("/", 1)[0])
            # skip bare root for pull-all noise unless requested
    return sorted(pkgs)


def cmd_pull_all(args) -> int:
    pkgs = discover_packages(args)
    rc = 0
    for pkg in pkgs:
        args.package = pkg
        print(f"--- pull {pkg} ---")
        code = cmd_pull(args)
        if code != 0:
            rc = code
    return rc


def main() -> int:
    ap = argparse.ArgumentParser(description="Sync libs/*.pp with live image")
    ap.add_argument(
        "command",
        choices=["status", "pull", "push", "pull-all"],
        help="status | pull | push | pull-all",
    )
    ap.add_argument("package", nargs="?", default="", help="package name (ui, todo, …)")
    ap.add_argument("-v", "--verbose", action="store_true")
    ap.add_argument("--url", default=env_or("PP_URL", "http://127.0.0.1:18080"))
    ap.add_argument("--user", default=env_or("PP_USER", "pricklypear"))
    ap.add_argument("--password", default="")
    ap.add_argument("--password-file", default="")
    ap.add_argument("--timeout", type=float, default=float(env_or("PP_EVAL_TIMEOUT", "180")))
    args = ap.parse_args()
    # load_password expects namespace with password / password_file
    args.password = load_password(args)
    if not args.password:
        print("pp_sync: set PP_PASS or --password", file=sys.stderr)
        return 2

    if args.command == "status":
        return cmd_status(args)
    if args.command == "pull":
        return cmd_pull(args)
    if args.command == "push":
        return cmd_push(args)
    if args.command == "pull-all":
        return cmd_pull_all(args)
    return 2


if __name__ == "__main__":
    sys.exit(main())
