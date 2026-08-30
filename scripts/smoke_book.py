#!/usr/bin/env python3
"""Smoke: habitat-book /book surface + book/verify (local dev stack).

  scripts/dev.sh start
  scripts/load-book.sh <export.book.json>      # once, to seed chapters
  python3 scripts/smoke_book.py

Env: PP_HTTP_PORT (default 18080), PP_SMOKE_URL, PP_USER/PP_PASS
(default admin/admin).

Covers the habitat-book verify smokes:
  - /book index: chapters in export (inline) order, verify link
  - chapter render: headings/prose from seeded tree rows
  - reader vs workshop register (status badges workshop-only)
  - example eval click-path: POST /book/eval-example
    (value + elapsed, refused, captured-not-live, 400/404)
  - book/verify: pass/fail/refusal holes via /api/eval + the
    GET /book/verify report page; fix-forward transition

Seeds throwaway rows under data.book.ch.smoke_verify and deletes them
in a finally (branch-free data.* namespace; the real book rows stay
untouched). Requires the book rows seeded (data.book.ch nonempty).
"""
from __future__ import annotations

import base64
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")

CH = "data.book.ch.smoke_verify"
X1 = CH + ".x901"  # (+ 40 2) expect 42      -> PASS
X2 = CH + ".x902"  # (+ 1 1)  expect 3       -> FAIL (hole)
X3 = CH + ".x903"  # (define x 1) expect 1   -> REFUSED (hole)
X4 = CH + ".x904"  # ocaml, no expect        -> skipped by verify


def auth_headers(extra=None):
    token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}", "Accept": "text/html"}
    if extra:
        h.update(extra)
    return h


def eval_lisp(expr: str, timeout: int = 60) -> dict:
    body = json.dumps({"expr": expr}).encode()
    req = urllib.request.Request(
        BASE + "/api/eval",
        data=body,
        method="POST",
        headers=auth_headers(
            {"Content-Type": "application/json", "Accept": "application/json"}
        ),
    )
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.loads(r.read().decode())


def value(expr: str, timeout: int = 60):
    r = eval_lisp(expr, timeout)
    if not r.get("ok"):
        raise RuntimeError(f"eval failed: {expr!r} -> {r.get('error')}")
    return r.get("value")


def http_get(path: str):
    req = urllib.request.Request(BASE + path, headers=auth_headers())
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return r.status, r.read().decode()
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode()


def http_post_form(path: str, data: dict):
    body = urllib.parse.urlencode(data).encode()
    req = urllib.request.Request(
        BASE + path,
        data=body,
        method="POST",
        headers=auth_headers({"Content-Type": "application/x-www-form-urlencoded"}),
    )
    try:
        with urllib.request.urlopen(req, timeout=60) as r:
            return r.status, r.read().decode()
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode()


def must(cond, msg):
    if not cond:
        print(f"FAIL: {msg}")
        sys.exit(1)
    print(f"ok: {msg}")


def nop_quote(s: str) -> str:
    """Escape a str as a Nopales string literal (port of the reader's
    escape_string: quote/backslash/named controls, \\ddd for the rest)."""
    out = []
    for ch in s:
        o = ord(ch)
        if ch == '"':
            out.append('\\"')
        elif ch == "\\":
            out.append("\\\\")
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\t":
            out.append("\\t")
        elif ch == "\r":
            out.append("\\r")
        elif o < 0x20:
            out.append("\\%03d" % o)
        else:
            out.append(ch)
    return '"' + "".join(out) + '"'


def upsert_row(path: str, body: dict, kind: str = "note"):
    """tree/upsert reports failures as STRINGS; wrap so they raise."""
    j = json.dumps(body, ensure_ascii=False, separators=(",", ":"))
    expr = (
        "(let ((r (tree/upsert %s %s \"book-chapter\" %s)))"
        " (if (string-eq r %s) r"
        ' (error (string-append "smoke_book: upsert failed: " r))))'
        % (nop_quote(path), nop_quote(kind), nop_quote(j), nop_quote(path))
    )
    value(expr)


def delete_row(path: str, recursive: bool = True):
    try:
        eval_lisp(
            "(tree/delete %s %s)" % (nop_quote(path), "#t" if recursive else "#f")
        )
    except (urllib.error.URLError, RuntimeError):
        pass  # cleanup must never mask the real failure


def seed():
    upsert_row(
        CH,
        {
            "node": "chapter",
            "chapter": "smoke_verify",
            "title": "smoke-verify",
            "status": "planned",
            "prose": "Throwaway smoke chapter — deleted by smoke_book.py.",
        },
    )
    upsert_row(X1, {"node": "example", "lang": "nopales", "src": "(+ 40 2)", "expected": "42", "order": 0})
    upsert_row(X2, {"node": "example", "lang": "nopales", "src": "(+ 1 1)", "expected": "3", "order": 1})
    upsert_row(X3, {"node": "example", "lang": "nopales", "src": "(define x 1)", "expected": "1", "order": 2})
    upsert_row(X4, {"node": "example", "lang": "ocaml", "src": "let double x = x * 2", "expected": None, "order": 3})


def cleanup():
    for p in (X1, X2, X3, X4):
        delete_row(p)
    delete_row(CH)


def main():
    # -- preconditions ------------------------------------------------
    nch = value('(count (tree/children-of "data.book.ch"))')
    must(nch and int(nch) > 0, f"book rows seeded (data.book.ch children = {nch})")
    bound = value('(null? (function-source "book/verify-page"))')
    if bound is True:
        print("book/verify-page unbound — (load-library \"book\") …")
        r = eval_lisp('(load-library "book")')
        must(r.get("ok"), f"load-library book: {r}")
    must(
        value('(null? (function-source "book/verify-page"))') is False,
        "book/verify-page bound",
    )

    try:
        seed()
        must(True, "smoke rows seeded (smoke_verify chapter + 4 examples)")

        # -- 1. index ---------------------------------------------------
        st, body = http_get("/book")
        must(st == 200, f"GET /book -> {st}")
        must("The Pricklypear Book" in body, "index shows the book title")
        must('"/book/verify"' in body, "index links the verify report")
        i_phil = body.find('href="/book/chapter/borge_philosophy"')
        i_single = body.find('href="/book/chapter/borge_single_image"')
        must(i_phil != -1 and i_single != -1, "index lists philosophy + single-image")
        must(i_phil < i_single, "chapters render in export (inline) order")
        n_links = body.count('href="/book/chapter/')
        must(n_links >= 30, f"index lists the borg chapters ({n_links} links)")

        # -- 2. chapter render -------------------------------------------
        st, ch = http_get("/book/chapter/borge_philosophy")
        must(st == 200, f"GET /book/chapter/philosophy -> {st}")
        must("<h1" in ch and "seeded rows (git is authoritative" in ch,
             "chapter renders headings/prose from tree rows")
        must("<h2" in ch or "<h3" in ch, "chapter renders sections")

        # -- 3. reader vs workshop ---------------------------------------
        must('class="book-status"' not in ch, "reader (default) strips status badges")
        must('class="book-verify"' not in ch, "reader strips verify blocks")
        st, ws = http_get("/book/chapter/borge_philosophy?mode=workshop")
        must(st == 200, f"workshop view -> {st}")
        must('class="book-status"' in ws, "workshop shows status badges")
        must('class="book-verify"' in ws,
             "workshop shows verify scaffolding")

        # -- 4. example eval click-path ----------------------------------
        st, r = http_post_form("/book/eval-example", {"path": X1})
        must(st == 200 and "⇒" in r and "42" in r and "book-result" in r,
             f"run button evals nopales example -> {st}")
        must("expect =&gt; 42" in r, "run result echoes the expect trailer")
        st, r = http_post_form("/book/eval-example", {"path": X3})
        must(st == 200 and "refused" in r, "define in an example is refused")
        st, r = http_post_form("/book/eval-example", {"path": X4})
        # ocaml blocks render no run button (span instead); the POST is
        # a hand-crafted edge and the kernel answers 400 + explanation.
        must(st == 400 and "captured, not live" in r,
             f"ocaml example is captured, not live (400) -> {st}")
        st, r = http_post_form("/book/eval-example", {"path": ""})
        must(st == 400, f"eval-example with no path -> {st}")
        st, r = http_post_form("/book/eval-example", {"path": "data.book.ch.nope.x999"})
        must(st == 404, f"eval-example unknown row -> {st}")

        # -- 5. book/verify (fn) ------------------------------------------
        holes = value(f'(book/verify "smoke_verify")')
        must(isinstance(holes, list) and len(holes) == 2,
             f"verify finds 2 holes (fail + refused), got {holes!r}")
        h_fail = next(h for h in holes if h[0] == X2)
        must(h_fail[1] == "(+ 1 1)",
             f"hole carries the source: {h_fail[1]!r}")
        must(h_fail[2] == "3" and h_fail[3] == "2",
             f"hole records expected 3 vs got 2: {h_fail[2:]!r}")
        h_ref = next(h for h in holes if h[0] == X3)
        must("refused:" in str(h_ref[3]),
             f"refusal lands as a hole: {h_ref[3]!r}")
        must(value(f'(count (book/verify "smoke_verify"))') == 2,
             "verify count = 2 pre-fix")
        must(value(f'(null? (book/verify "smoke_verify"))') is False,
             "whole-chapter verify is non-empty")

        # -- 6. book/verify (page) ----------------------------------------
        st, page = http_get("/book/verify?chapter=smoke_verify")
        must(st == 200, f"GET /book/verify?chapter=smoke_verify -> {st}")
        must(page.count('class="book-hole-row"') == 2,
             "report renders exactly 2 hole rows")
        must("checked 3 examples" in page and "holes 2" in page,
             "report counts candidates + holes")
        must(X2 in page and X3 in page, "report names the hole paths")
        st, page = http_get("/book/verify")
        must(st == 200 and 'class="book-hole-row"' in page,
             "whole-book report includes the smoke holes")

        # -- 7. fix-forward: expect repaired -> hole closes ----------------
        upsert_row(X2, {"node": "example", "lang": "nopales", "src": "(+ 1 1)",
                        "expected": "2", "order": 1})
        must(value(f'(count (book/verify "smoke_verify"))') == 1,
             "repaired expect closes its hole (1 hole left)")
        st, page = http_get("/book/verify?chapter=smoke_verify")
        must(page.count('class="book-hole-row"') == 1 and "holes 1" in page,
             "report reflects the repair")

    finally:
        cleanup()
        left = value(f'(null? (tree/get "{CH}"))')
        must(left is True, "smoke rows cleaned up")

    # real book has no example fences yet -> whole-book verify is green
    must(value("(count (book/verify))") == 0,
         "whole-book verify green after cleanup (real book: 0 examples)")

    print("\nSMOKE OK: habitat-book /book surface + book/verify")


if __name__ == "__main__":
    main()
