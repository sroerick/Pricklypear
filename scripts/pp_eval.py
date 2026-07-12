#!/usr/bin/env python3
"""Eval Lisp on a live Pricklypear image — the image is the API.

From a laptop (or any agent host), call functions/libraries on the running
system without building per-app REST endpoints:

  (todo/list)
  (bible/verse "John" 3 16)
  (load-library "ui")
  (demo/double 21)

Auth: HTTP basic against the image credentials table (or a session cookie).
Nginx edge basic auth on :80 is not required.

Usage:
  export PP_URL=http://155.138.208.45
  export PP_USER=admin
  export PP_PASS='…'          # or PP_PASS_FILE=/path/to/file

  python3 scripts/pp_eval.py '(+ 1 2)'
  python3 scripts/pp_eval.py -e '(load-library "todo")' -e '(count (todo/list))'
  python3 scripts/pp_eval.py -i                 # interactive multi-turn
  python3 scripts/pp_eval.py -j '(list-bindings)'   # JSON only (agents)
  echo '(bible/verse "John" 3 16)' | python3 scripts/pp_eval.py

Exit codes: 0 ok, 1 eval/HTTP error, 2 usage/config.
"""
from __future__ import annotations

import argparse
import base64
import json
import os
import sys
import urllib.error
import urllib.request
from pathlib import Path


def env_or(name: str, default: str = "") -> str:
    v = os.environ.get(name, default)
    return v if v is not None else default


def load_password(args: argparse.Namespace) -> str:
    if args.password:
        return args.password
    if args.password_file:
        return Path(args.password_file).read_text(encoding="utf-8").strip()
    pf = env_or("PP_PASS_FILE")
    if pf:
        return Path(pf).read_text(encoding="utf-8").strip()
    return env_or("PP_PASS")


def eval_expr(
    *,
    base_url: str,
    user: str,
    password: str,
    expr: str,
    timeout: float,
) -> dict:
    url = base_url.rstrip("/") + "/api/eval"
    body = json.dumps({"expr": expr}).encode("utf-8")
    token = base64.b64encode(f"{user}:{password}".encode()).decode("ascii")
    req = urllib.request.Request(
        url,
        data=body,
        method="POST",
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Basic {token}",
            "Accept": "application/json",
        },
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            raw = resp.read().decode("utf-8", errors="replace")
            return json.loads(raw)
    except urllib.error.HTTPError as e:
        detail = e.read().decode("utf-8", errors="replace")
        try:
            return json.loads(detail)
        except Exception:
            return {"ok": False, "error": f"HTTP {e.code}: {detail[:500]}"}
    except Exception as e:
        return {"ok": False, "error": str(e)}


def print_result(result: dict, *, as_json: bool) -> int:
    if as_json:
        print(json.dumps(result, ensure_ascii=False))
        return 0 if result.get("ok") else 1
    if result.get("ok"):
        print(result.get("value", ""))
        return 0
    print(result.get("error", "unknown error"), file=sys.stderr)
    return 1


def interactive(args: argparse.Namespace, password: str) -> int:
    print(f"[pp_eval] {args.url}  user={args.user}", file=sys.stderr)
    print(
        "[pp_eval] enter Lisp forms; :quit to exit; blank line ignored",
        file=sys.stderr,
    )
    rc = 0
    while True:
        try:
            line = input("pp> ")
        except (EOFError, KeyboardInterrupt):
            print(file=sys.stderr)
            break
        line = line.strip()
        if not line:
            continue
        if line in (":quit", ":q", "quit", "exit"):
            break
        result = eval_expr(
            base_url=args.url,
            user=args.user,
            password=password,
            expr=line,
            timeout=args.timeout,
        )
        code = print_result(result, as_json=args.json)
        if code != 0:
            rc = code
    return rc


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Eval Lisp on a live Pricklypear image (laptop/agent client)"
    )
    ap.add_argument(
        "expr",
        nargs="?",
        help="single expression (or pass via stdin / -e)",
    )
    ap.add_argument(
        "-e",
        "--eval",
        action="append",
        default=[],
        metavar="EXPR",
        help="expression to eval (repeatable, in order)",
    )
    ap.add_argument(
        "-i",
        "--interactive",
        action="store_true",
        help="interactive multi-turn session",
    )
    ap.add_argument(
        "-j",
        "--json",
        action="store_true",
        help="print full JSON result (agent-friendly)",
    )
    ap.add_argument(
        "--url",
        default=env_or("PP_URL", "http://127.0.0.1:18080"),
        help="image base URL (default PP_URL or http://127.0.0.1:18080)",
    )
    ap.add_argument(
        "--user",
        default=env_or("PP_USER", "admin"),
        help="image basic-auth user (default PP_USER or admin)",
    )
    ap.add_argument(
        "--password",
        default="",
        help="basic-auth password (or PP_PASS / PP_PASS_FILE)",
    )
    ap.add_argument(
        "--password-file",
        default="",
        help="read password from file",
    )
    ap.add_argument(
        "--timeout",
        type=float,
        default=float(env_or("PP_EVAL_TIMEOUT", "120")),
        help="HTTP timeout seconds (default 120)",
    )
    args = ap.parse_args()
    password = load_password(args)
    if not password:
        print(
            "pp_eval: set PP_PASS or PP_PASS_FILE or --password",
            file=sys.stderr,
        )
        return 2

    if args.interactive:
        return interactive(args, password)

    exprs: list[str] = list(args.eval)
    if args.expr:
        exprs.append(args.expr)
    if not exprs and not sys.stdin.isatty():
        for line in sys.stdin:
            line = line.strip()
            if line and not line.startswith(";"):
                exprs.append(line)
    if not exprs:
        ap.print_help()
        return 2

    rc = 0
    for expr in exprs:
        result = eval_expr(
            base_url=args.url,
            user=args.user,
            password=password,
            expr=expr,
            timeout=args.timeout,
        )
        code = print_result(result, as_json=args.json)
        if code != 0:
            rc = code
    return rc


if __name__ == "__main__":
    sys.exit(main())
