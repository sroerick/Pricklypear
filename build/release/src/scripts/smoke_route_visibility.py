#!/usr/bin/env python3
"""Smoke: auth.route-visibility — public/private user routes.

  scripts/dev.sh start   # applies migration 006
  python3 scripts/smoke_route_visibility.py

Defaults: http://127.0.0.1:18080 admin/admin (local bootstrap).
Public routes are registered on system/main (anonymous branch).
"""
from __future__ import annotations

import base64
import json
import os
import sys
import urllib.error
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def auth_h(extra=None):
    token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}"}
    if extra:
        h.update(extra)
    return h


def eval_lisp(expr: str) -> dict:
    body = json.dumps({"expr": expr}).encode()
    req = urllib.request.Request(
        BASE + "/api/eval",
        data=body,
        method="POST",
        headers=auth_h(
            {"Content-Type": "application/json", "Accept": "application/json"}
        ),
    )
    with urllib.request.urlopen(req, timeout=15) as r:
        return json.loads(r.read().decode())


def get(path: str, auth: bool = False, follow: bool = True):
    headers = auth_h() if auth else {}

    class NoRedir(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, req, fp, code, msg, headers, newurl):
            if follow:
                return urllib.request.HTTPRedirectHandler.redirect_request(
                    self, req, fp, code, msg, headers, newurl
                )
            return None

    opener = urllib.request.build_opener(NoRedir)
    req = urllib.request.Request(BASE + path, headers=headers)
    try:
        with opener.open(req, timeout=15) as resp:
            return resp.status, resp.geturl(), resp.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        loc = e.headers.get("Location") if e.headers else None
        return e.code, loc or "", e.read().decode("utf-8", "replace")


def must(cond, msg):
    if not cond:
        print("FAIL:", msg)
        sys.exit(1)
    print("OK:", msg)


def main():
    must(eval_lisp('(checkout "system/main")').get("ok"), "checkout system/main")
    must(
        eval_lisp(
            '(define vis-pub-page (lambda () (render-html "<h1>public-ok</h1>")))'
        ).get("ok"),
        "define public page",
    )
    must(
        eval_lisp('(route "/vis-pub" "vis-pub-page" "public")').get("ok"),
        "route public",
    )
    must(
        "public" in str(eval_lisp('(route-visibility "/vis-pub")').get("value")),
        "route-visibility public",
    )
    must(
        eval_lisp(
            '(define vis-priv-page (lambda () (render-html "<h1>private-ok</h1>")))'
        ).get("ok"),
        "define private page",
    )
    must(eval_lisp('(route "/vis-priv" "vis-priv-page")').get("ok"), "route private")
    must(
        "private" in str(eval_lisp('(route-visibility "/vis-priv")').get("value")),
        "route-visibility private",
    )

    code, _, html = get("/vis-pub", auth=False)
    must(code == 200 and "public-ok" in html, f"unauth GET public -> {code}")
    must("user=anonymous" in html, "anonymous chrome")

    code, loc, _ = get("/vis-priv", auth=False, follow=False)
    must(
        code in (303, 302, 301) and "login" in (loc or "").lower(),
        f"unauth private -> {code} {loc}",
    )

    code, loc, _ = get("/data", auth=False, follow=False)
    must(
        code in (303, 302, 301) and "login" in (loc or "").lower(),
        f"unauth /data -> {code} {loc}",
    )

    req = urllib.request.Request(
        BASE + "/api/eval",
        data=b'{"expr":"1"}',
        method="POST",
        headers={"Content-Type": "application/json"},
    )
    try:
        urllib.request.urlopen(req, timeout=10)
        print("FAIL: unauth /api/eval allowed")
        sys.exit(1)
    except urllib.error.HTTPError as e:
        must(e.code in (401, 303, 302), f"unauth /api/eval -> {e.code}")

    code, _, html = get("/vis-priv", auth=True)
    must(code == 200 and "private-ok" in html, "auth private")

    code, _, html = get("/vis-pub", auth=True)
    must(
        code == 200 and "public-ok" in html and f"user={USER}" in html,
        "optional auth on public",
    )

    code, _, html = get("/pages", auth=True)
    must("visibility" in html.lower() and "public" in html, "/pages visibility col")

    must(eval_lisp('(route "/vis-pub" "vis-pub-page")').get("ok"), "handler-only re-route")
    must(
        "public" in str(eval_lisp('(route-visibility "/vis-pub")').get("value")),
        "keep public on handler-only update",
    )

    must(eval_lisp(f'(checkout "{USER}/personal")').get("ok"), "checkout personal")
    must(
        "public" in str(eval_lisp('(route-visibility "/vis-pub")').get("value")),
        "personal inherits public from main",
    )

    print("ALL PASS")


if __name__ == "__main__":
    main()
