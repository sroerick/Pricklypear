#!/usr/bin/env python3
"""Monitor-rerunnable verification for notes-execute-all work queue.

Usage (local dev stack):
  scripts/dev.sh start   # if not already up
  export PP_URL=http://127.0.0.1:18080
  export PP_USER=admin
  export PP_PASS=admin
  export PP_EVAL=$PWD/scripts/pp_eval.py
  python3 scripts/verify_notes_execute_all.py

Exit 0 on all pass, 1 on any failure.
"""
from __future__ import annotations

import base64
import json
import os
import subprocess
import sys
import urllib.error
import urllib.request

PP_EVAL = os.environ.get("PP_EVAL", "scripts/pp_eval.py")
PP_URL = os.environ.get("PP_URL", "http://127.0.0.1:18080")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def eval_j(expr: str) -> dict:
    r = subprocess.run(
        ["python3", PP_EVAL, "-j", expr],
        capture_output=True,
        text=True,
        env={
            **os.environ,
            "PP_URL": PP_URL,
            "PP_USER": USER,
            "PP_PASS": PASS,
            "PP_EVAL": PP_EVAL,
        },
    )
    out = (r.stdout or "").strip()
    for line in reversed(out.splitlines()):
        line = line.strip()
        if line.startswith("{"):
            return json.loads(line)
    return {"ok": False, "error": f"no json: {out[:200]} {r.stderr[:200]}"}


def must_ok(name: str, expr: str) -> str:
    d = eval_j(expr)
    if not d.get("ok"):
        raise AssertionError(f"{name}: {d.get('error') or d}")
    return d.get("value", "")


def must_err(name: str, expr: str, substr: str) -> None:
    d = eval_j(expr)
    if d.get("ok"):
        raise AssertionError(
            f"{name}: expected error containing {substr!r}, got ok {d.get('value')}"
        )
    err = d.get("error") or ""
    if substr not in err:
        raise AssertionError(f"{name}: error {err!r} missing {substr!r}")


def http_get(path: str) -> tuple[int, str]:
    req = urllib.request.Request(f"{PP_URL.rstrip('/')}{path}")
    tok = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    req.add_header("Authorization", f"Basic {tok}")
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            return resp.status, resp.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")


def main() -> int:
    fails: list[str] = []

    def check(name: str, fn) -> None:
        try:
            fn()
            print(f"PASS  {name}")
        except Exception as e:
            print(f"FAIL  {name}: {e}")
            fails.append(name)

    def load_libs() -> None:
        must_ok("outline", '(load-library "outline")')
        must_ok("notes", '(load-library "notes")')
        must_ok("bi", '(load-library "bi")')

    def a1() -> None:
        v = must_ok(
            "src",
            '(string-contains? (function-source "outline/html-node") "outline/render-body")',
        )
        if v != "#t":
            raise AssertionError(f"hook missing: {v}")

    def a2() -> None:
        h = must_ok(
            "html",
            '(outline/html "t" (list "root" "T" "hi [[wiki|W]] [[media:abc|z]]" "" (list)) #f)',
        )
        if "/notes?slug=wiki" not in h or "<img" not in h or "note-media" not in h:
            raise AssertionError(f"render missing pieces in {h[:300]}")

    def a3() -> None:
        must_err(
            "sfu",
            '(select-for-update (quote bi-expense) "00000000-0000-0000-0000-000000000001")',
            "with-transaction",
        )

    def a4() -> None:
        must_ok("seed", "(bi/seed-sample-data)")
        v = must_ok(
            "sfu-tx",
            """(let ((id (car (car (rows (quote bi-expense) 1)))))
                 (with-transaction (lambda ()
                   (let ((row (select-for-update (quote bi-expense) id)))
                     (if (null? row) "nil" "locked")))))""",
        )
        if "locked" not in v:
            raise AssertionError(v)

    def b_list() -> None:
        if must_ok("map", '(list/map (lambda (x) (* x 2)) (list 1 2 3))') != "(2 4 6)":
            raise AssertionError("map")
        if "10" not in must_ok(
            "fold", '(list/foldl (lambda (a x) (+ a x)) 0 (list 1 2 3 4))'
        ):
            raise AssertionError("fold")
        if (
            must_ok("filt", '(list/filter (lambda (x) (= x 2)) (list 1 2 3 2))')
            != "(2 2)"
        ):
            raise AssertionError("filter")

    def c_links() -> None:
        v = must_ok(
            "el",
            '(extract-links "see [[a]] and [[b|Bee]] [[media:u|x]] [[]] text [[oops")',
        )
        if '"a"' not in v or "Bee" not in v or "media:u" not in v:
            raise AssertionError(v)

    def c_large() -> None:
        v = must_ok(
            "big",
            """(let ((body (list/foldl (lambda (a i)
                    (string-append a "lorem [[p" (as-str i) "]] x "))
                  "" (list/range 500))))
               (list (string-length body) (count (roam/parse-links body))
                     (budget-remaining)))""",
        )
        if "500" not in v:
            raise AssertionError(v)

    def c_e2e() -> None:
        v = must_ok(
            "upsert",
            """(let ((pad (list/foldl (lambda (a i)
                    (string-append a "abcdefghijklmnopqrstuvwxyz0123456789 "))
                  "" (list/range 100))))
               (let ((mk (lambda (i)
                       (list (string-append "n" (as-str i)) "t"
                             (string-append pad " [[L" (as-str i) "]] " pad)
                             "" (list)))))
                 (let ((tree (list "root" "R" "" ""
                               (list (mk 0) (mk 1) (mk 2) (mk 3) (mk 4)
                                     (mk 5) (mk 6) (mk 7) (mk 8) (mk 9)))))
                   (list
                     (notes/doc/upsert "verify-budget-upsert" "Verify" tree
                       "test" "note" "")
                     (count (find-rows (quote note-link) "from_slug"
                              "verify-budget-upsert" 100))
                     (budget-remaining)))))""",
        )
        if "verify-budget-upsert" not in v or "10" not in v:
            raise AssertionError(v)

    def c_budget() -> None:
        v = must_ok("br", "(budget-remaining)")
        n = int(v.strip('"'))
        if n <= 0:
            raise AssertionError(v)

    def e_bench() -> None:
        v = must_ok("bench", '(bench "(+ 1 2)" 100)')
        if float(v.strip('"')) <= 0.0:
            raise AssertionError(v)

    def f_bi() -> None:
        must_ok("seed", "(bi/seed-sample-data)")
        snap = must_ok("snap", "(bi/metrics-snapshot)")
        if "revenue" not in snap or "expenses" not in snap:
            raise AssertionError(snap)
        code, html = http_get("/bi")
        if code != 200:
            raise AssertionError(f"HTTP {code}")
        if "bi-card" not in html or "vega" not in html or "Revenue" not in html:
            raise AssertionError(html[:400])

    def f_agent() -> None:
        v = must_ok("read", "(bi/dashboard/read)")
        if "/bi" not in v or "metrics" not in v:
            raise AssertionError(v)

    check("load outline/notes/bi", load_libs)
    check("A1 outline/html-node render-body hook", a1)
    check("A2 wiki+media in outline/html", a2)
    check("A3 select-for-update outside tx", a3)
    check("A4 select-for-update inside tx", a4)
    check("B list/map filter foldl", b_list)
    check("C extract-links correctness", c_links)
    check("C large parse-links budget", c_large)
    check("C e2e upsert + reindex", c_e2e)
    check("C6 budget-remaining", c_budget)
    check("E bench", e_bench)
    check("F BI metrics + dashboard HTTP", f_bi)
    check("F agent dashboard read", f_agent)

    if fails:
        print(f"\n{len(fails)} FAILED: {', '.join(fails)}")
        return 1
    print("\nALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
