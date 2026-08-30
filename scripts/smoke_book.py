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
  - courses (habitat-book.course-documents): /book/course/<slug>
    renders the ordered reading list with chapter status badges; a
    second course referencing the same chapter duplicates nothing in
    the tree; /book index lists course groupings

Seeds throwaway rows under data.book.ch.smoke_verify and deletes them
in a finally (branch-free data.* namespace; the real book rows stay
untouched). Requires the book rows seeded (data.book.ch nonempty).
The real course doc (notes slug course/cs-interpreters, kind course)
is seeded ONLY when absent — content discipline: the smoke never
clobbers manual edits. The throwaway second course doc is deleted in
a finally.
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

COURSE = "course/cs-interpreters"  # the real course doc (seed if absent)
SECOND = "course/smoke-second"  # throwaway second course (deleted)
CH1 = "borge_lisp_interpreter"  # real chapter labels (loader slug_label)
CH2 = "borge_structural_editing"
CH3 = "borge_branching"


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


def upsert_doc(slug, title, root_body, kids, tags, day):
    """notes/doc/upsert of one course doc. kids: (id, title, body, kids)
    tuples; a reading node's body starts with its `chapter: <label>`
    line. Upserts report failures as strings — wrapped to raise."""

    def node(nid, ntitle, nbody, nkids):
        # kids entries are (id, title, body, kids) tuples — recurse to
        # s-expression strings before joining (draft joined raw tuples).
        kid_exprs = [node(*k) for k in nkids]
        kids = ("(list " + " ".join(kid_exprs) + ")") if kid_exprs else "(list)"
        return (
            "(outline/node* %s %s %s \"\" %s)"
            % (
                nop_quote(nid),
                nop_quote(ntitle),
                nop_quote(nbody),
                kids,
            )
        )

    root = node("root", title, root_body, kids)
    expr = (
        "(notes/doc/upsert %s %s %s %s \"course\" %s)"
        % (nop_quote(slug), nop_quote(title), root, nop_quote(tags), nop_quote(day))
    )
    r = eval_lisp(expr)
    if not r.get("ok"):
        raise RuntimeError(f"course doc upsert failed: {slug} -> {r.get('error')}")


def doc_path(slug):
    # notes/doc/row is a (uuid . body) pair — the kernel's row-path
    # accessor reads (cdr row) and returns "" when the row is absent
    # (dict-get on the raw pair errors: "dict-get needs 2 strings").
    return value(
        f'(notes/doc/row-path (notes/doc/row {nop_quote(slug)}))'
    )


def seed_course_doc():
    """Seed the REAL course doc only when absent (content discipline:
    the smoke must never clobber manual edits)."""
    absent = value(f'(null? (notes/doc/row {nop_quote(COURSE)}))')
    if absent is True:
        upsert_doc(
            COURSE,
            "CS: interpreters and code as data",
            "Build a Lisp, then fork, diff, and merge programs as trees — on the "
            "habitat you are reading this in. Every reading is a spec chapter of "
            "the very interpreter under your session; examples evaluate live in "
            "YOUR session (habitat-first eval), and (book/verify) re-checks every "
            "worked example. The paper edition is the same chapters: borge book "
            "print --root course/cs-interpreters.borg.",
            [
                (
                    "front-matter",
                    "Front matter",
                    "WHO: anyone who wants to know what a programming language IS — "
                    "by building one that runs a real multi-user operating system.\n\n"
                    "OUTCOMES: build a Lisp (reader, AST, evaluator, environments, "
                    "closures, tail calls, lazy thunks — (delay)/(force)); extend the "
                    "language (add a special form or primitive to Nopales behind a "
                    "conformance test in lisp/conformance/); treat programs as data "
                    "(read a function's AST, fork a branch, patch, merge, diff).\n\n"
                    "HOW: read in course order below; run the examples as you read — "
                    "they evaluate in your session. Labs are habitat objects, not "
                    "worksheets. Report problems in log/issues — issue hygiene is "
                    "part of the coursework.",
                    [],
                ),
                (
                    "readings",
                    "Readings",
                    "Three chapters, in course order — each is the spec of a layer "
                    "you then extend in the lab.",
                    [
                        (
                            "r1",
                            "1 — build a Lisp (the language layer)",
                            f"chapter: {CH1}\nThe reader, the AST, the evaluator, "
                            "environments, closures, tail calls, lazy thunks. Read "
                            "lisp/lib/eval.ml as the primary source — it is this "
                            "chapter's running code.",
                            [],
                        ),
                        (
                            "r2",
                            "2 — programs as trees (structural editing)",
                            f"chapter: {CH2}\nOne shared tree-op protocol (slurp, "
                            "barf, wrap, splice, raise) over outlines AND code "
                            "ASTs — you never type structure.",
                            [],
                        ),
                        (
                            "r3",
                            "3 — code as institutional data (branching)",
                            f"chapter: {CH3}\nBranches as fork/merge over program "
                            "text, AST diffing, inheritance of definitions on the "
                            "one tree — what version control becomes when programs "
                            "are data.",
                            [],
                        ),
                    ],
                ),
                (
                    "labs",
                    "Labs",
                    "LAB 1 — extend the language (after reading 1): add one special "
                    "form or primitive to Nopales behind a conformance test. Bench: "
                    "/repl, /code, and the /binding pages.\n\n"
                    "LAB 2 — operate on a program as data (after readings 2-3): fork "
                    "a branch, patch a function's AST, merge back, diff two "
                    "generations of a binding. Bench: /branches, /bindings, and the "
                    "/book chapter pages.\n\n"
                    "Report problems in log/issues — the habitat's own register.",
                    [],
                ),
            ],
            "course,interpreters,nopales",
            "2026-08-30",
        )
        print("ok: course doc seeded")
    else:
        print("ok: course doc already seeded (leaving manual edits alone)")


def seed_second_course():
    """Throwaway second course referencing the SAME chapter — proves
    composition duplicates nothing in the tree."""
    upsert_doc(
        SECOND,
        "smoke: second course (throwaway)",
        "Throwaway smoke course — deleted by smoke_book.py.",
        [
            (
                "only",
                "the shared chapter",
                f"chapter: {CH1}\nSame chapter, second course — the tree must "
                "not grow a copy.",
                [],
            )
        ],
        "course,smoke",
        "2026-08-30",
    )


def cleanup_second_course():
    try:
        delete_row(doc_path(SECOND))
    except Exception:
        pass  # cleanup must never mask the real failure


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

    # -- 8. courses (habitat-book.course-documents) --------------------
    seed_course_doc()  # real doc; seeded only when absent
    st, page = http_get("/book/course/cs-interpreters")
    must(st == 200, f"GET /book/course/cs-interpreters -> {st}")
    must("CS: interpreters and code as data" in page,
         "course page carries the doc title")
    must(f"/book/chapter/{CH1}" in page
         and f"/book/chapter/{CH2}" in page
         and f"/book/chapter/{CH3}" in page,
         "reading list links the three chapters in course order")
    must(page.find(f"/book/chapter/{CH1}")
         < page.find(f"/book/chapter/{CH2}")
         < page.find(f"/book/chapter/{CH3}"),
         "reading order is document order (compose, never reorder)")
    must("<ol" in page, "readings render as one ordered list")
    must(page.count('class="book-status"') >= 3,
         "chapter status badges on the reading list")
    must("Front matter" in page and "Labs" in page,
         "front matter + labs sections render")
    st, page = http_get("/book/course/cs-interpreters?mode=workshop")
    must(st == 200 and 'class="book-status"' in page,
         "workshop register renders the course too")
    n_before = value('(count (tree/children-of "data.book.ch"))')
    try:
        seed_second_course()
        n_after = value('(count (tree/children-of "data.book.ch"))')
        must(n_before == n_after,
             f"second course duplicates nothing in the tree ({n_before} chapters)")
        st, page = http_get("/book/course/smoke-second")
        must(st == 200 and f"/book/chapter/{CH1}" in page,
             "second course renders the SAME chapter by reference")
        st, idx = http_get("/book")
        must("/book/course/cs-interpreters" in idx
             and "/book/course/smoke-second" in idx,
             "index lists course groupings")
    finally:
        cleanup_second_course()
        gone = value(f'(null? (notes/doc/row {nop_quote(SECOND)}))')
        must(gone is True, "second course doc cleaned up")
    st, _ = http_get("/book/course/nope")
    must(st == 404, "unknown course 404s")
    st, _ = http_get("/book/course/Not_A_Slug")
    must(st == 404, "unsafe course suffix 404s")

    # real book has no example fences yet -> whole-book verify is green
    must(value("(count (book/verify))") == 0,
         "whole-book verify green after cleanup (real book: 0 examples)")

    print("\nSMOKE OK: habitat-book /book surface + book/verify + courses")


if __name__ == "__main__":
    main()
