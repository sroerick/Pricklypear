#!/usr/bin/env python3
"""Smoke: image.data-browser full CRUD over /data (local dev stack).

  scripts/dev.sh start
  python3 scripts/smoke_data_browser.py

Env: PP_HTTP_PORT (default 18080), PP_USER/PP_PASS (default admin/admin).
"""
from __future__ import annotations

import base64
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")
TYPE = "smoke_widget"


def auth_headers(extra=None):
    token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}", "Accept": "text/html"}
    if extra:
        h.update(extra)
    return h


def eval_lisp(expr: str) -> dict:
    body = json.dumps({"expr": expr}).encode()
    req = urllib.request.Request(
        BASE + "/api/eval",
        data=body,
        method="POST",
        headers=auth_headers(
            {"Content-Type": "application/json", "Accept": "application/json"}
        ),
    )
    with urllib.request.urlopen(req, timeout=15) as r:
        return json.loads(r.read().decode())


def req(method: str, path: str, data=None, follow=True):
    url = BASE + path
    body = None
    headers = auth_headers()
    if data is not None:
        body = urllib.parse.urlencode(data).encode()
        headers["Content-Type"] = "application/x-www-form-urlencoded"

    class NoRedir(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, req, fp, code, msg, headers, newurl):
            if follow:
                return urllib.request.HTTPRedirectHandler.redirect_request(
                    self, req, fp, code, msg, headers, newurl
                )
            return None

    opener = urllib.request.build_opener(NoRedir)
    r = urllib.request.Request(url, data=body, headers=headers, method=method)
    try:
        with opener.open(r, timeout=15) as resp:
            return resp.status, resp.geturl(), resp.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        loc = e.headers.get("Location") if e.headers else None
        return e.code, loc or getattr(e, "url", url), e.read().decode(
            "utf-8", "replace"
        )


def must(cond, msg):
    if not cond:
        print("FAIL:", msg)
        sys.exit(1)
    print("OK:", msg)


def main():
    out = eval_lisp(f"(deftype '{TYPE} 'title 'color)")
    must(out.get("ok"), f"deftype {out}")

    code, _, html = req("GET", "/data")
    must(code == 200, f"GET /data {code}")
    must("shared across branches" in html, "banner")
    must(TYPE in html, "type on index")

    code, _, html = req("GET", f"/data/{TYPE}")
    must(code == 200 and "New row" in html, "list page")

    code, _, html = req("GET", f"/data/{TYPE}/new")
    must(code == 200 and 'name="title"' in html, "new form")

    code, final, html = req(
        "POST", f"/data/{TYPE}", {"title": "Widget A", "color": "red"}
    )
    must(code == 200, f"create status {code}")
    m = re.search(rf"/data/{TYPE}/([0-9a-f-]{{36}})", final)
    must(m, f"uuid {final}")
    rid = m.group(1)
    must("Widget A" in html or "red" in html, "create values")

    code, _, html = req("GET", f"/data/{TYPE}")
    must(rid[:8] in html, "list short id")

    code, final, html = req(
        "POST", f"/data/{TYPE}/{rid}", {"title": "Widget B", "color": "blue"}
    )
    must("Widget B" in html or "blue" in html, "update")

    code, final, html = req(
        "POST",
        f"/data/{TYPE}",
        {
            "fields_json": json.dumps(
                {"title": "X", "color": "g", "extra": "keepme"}
            )
        },
    )
    m2 = re.search(rf"/data/{TYPE}/([0-9a-f-]{{36}})", final)
    must(m2, "create2")
    rid2 = m2.group(1)
    must("keepme" in html or "extra" in html, "extra shown")
    code, final, html = req(
        "POST",
        f"/data/{TYPE}/{rid2}",
        {"title": "Y", "color": "g", "extra": "keepme"},
    )
    must("keepme" in html, "extra preserved")

    code, loc, _ = req("GET", f"/inspect/row/{TYPE}/{rid}", follow=False)
    must(
        code in (303, 302, 301) and loc and f"/data/{TYPE}/{rid}" in loc,
        f"inspect redir {code} {loc}",
    )

    code, _, html = req("GET", "/types")
    must(f"/data/{TYPE}" in html, "types link")

    code, final, _ = req("POST", f"/data/{TYPE}/{rid}/delete", {})
    must(f"/data/{TYPE}" in final, f"delete land {final}")
    code, _, html = req("GET", f"/data/{TYPE}/{rid}")
    must(code == 404 or "not found" in html.lower(), "404 after delete")
    req("POST", f"/data/{TYPE}/{rid2}/delete", {})
    print("ALL PASS")


if __name__ == "__main__":
    main()
