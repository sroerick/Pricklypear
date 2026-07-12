#!/usr/bin/env python3
"""Smoke: product theme Path B (image.product-theme / BORGE-1783837363).

Requires local or configured image (PP_URL / PP_USER / PP_PASS).
"""
from __future__ import annotations

import base64
import json
import os
import re
import subprocess
import sys
import urllib.request

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EVAL = os.path.join(ROOT, "scripts", "pp_eval.py")
URL = os.environ.get("PP_URL", "http://127.0.0.1:18080").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def eval_j(expr: str) -> dict:
    env = os.environ.copy()
    env.setdefault("PP_URL", URL)
    env.setdefault("PP_USER", USER)
    env.setdefault("PP_PASS", PASS)
    out = subprocess.check_output(
        [sys.executable, EVAL, "-j", expr], env=env, text=True
    )
    return json.loads(out)


def http_get(path: str, auth: bool = True) -> str:
    req = urllib.request.Request(f"{URL}{path}")
    if auth:
        token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
        req.add_header("Authorization", f"Basic {token}")
    with urllib.request.urlopen(req) as r:
        return r.read().decode()


def main() -> int:
    fails: list[str] = []

    def check(name: str, cond: bool, detail: str = "") -> None:
        if cond:
            print(f"PASS {name}")
        else:
            print(f"FAIL {name} {detail}")
            fails.append(name)

    r = eval_j(
        '(css/render (css/sheet (list (css/rule "body" '
        '(list "margin" 0 "background" "#d4d0c8")))))'
    )
    check(
        "css/render",
        r.get("ok")
        and "body{" in r.get("value", "")
        and "margin:0" in r.get("value", ""),
        str(r),
    )

    r = eval_j('(function-source "ui/css")')
    check(
        "function-source ui/css",
        r.get("ok") and "css/render" in r.get("value", ""),
        str(r),
    )

    r = eval_j('(export-package "ui")')
    v = r.get("value", "")
    check(
        "export-package ui theme",
        r.get("ok")
        and ("ui/theme" in v or "theme/bg" in v)
        and "ui/css" in v,
        f"len={len(v)}",
    )

    html = http_get("/")
    head_style = html.split("</style>")[0] if "</style>" in html else ""
    check(
        "GET / ui/css marker",
        "/* ui/css */" in html
        and "#d4d0c8" in html
        and "/* emergency-css */" not in head_style,
    )

    r = eval_j('(define theme/bg "#ff00aa")')
    check("define theme/bg", bool(r.get("ok")), str(r))
    html = http_get("/")
    m = re.search(r"body\{[^}]+\}", html)
    check(
        "personal theme/bg in page",
        bool(m and "#ff00aa" in m.group(0)),
        m.group(0) if m else "",
    )
    eval_j('(define theme/bg "#d4d0c8")')

    eval_j("(define ui/css (lambda () (car 1)))")
    html = http_get("/")
    style_m = re.search(r"<style>(.*?)</style>", html, re.S)
    style = style_m.group(1) if style_m else ""
    check(
        "emergency on ui/css fail",
        style.strip().startswith("/* emergency-css */"),
        style[:60],
    )
    eval_j('(load-library "ui")')
    html = http_get("/")
    check("restore after load-library ui", "/* ui/css */" in html)

    login = http_get("/login", auth=False)
    check(
        "login page loads",
        "Sign in" in login or "password" in login.lower(),
    )

    print("---")
    if fails:
        print("FAILED", len(fails), fails)
        return 1
    print("ALL PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
