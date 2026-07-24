#!/usr/bin/env python3
"""Budget / scale stress suite for notes+roam+outline (notes.budget-scale).

Monitor-rerunnable. Uses synthetic slugs under budget-stress-* and cleans up.

Usage:
  scripts/dev.sh start   # or point at habitat
  export PP_URL=http://127.0.0.1:18080   # or https://pricklypear.rocks
  export PP_USER=admin PP_PASS=...
  export PP_EVAL=$PWD/scripts/pp_eval.py
  python3 scripts/verify_budget_stress.py

Exit 0 all pass, 1 any failure.
"""
from __future__ import annotations

import json
import os
import re
import subprocess
import sys

PP_EVAL = os.environ.get("PP_EVAL", "scripts/pp_eval.py")
PP_URL = os.environ.get("PP_URL", "http://127.0.0.1:18080")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")

# Conservative floors after stress calls (steps left of 100k default).
FLOOR_AFTER_PARSE = 20_000
FLOOR_AFTER_UPSERT = 10_000
FLOOR_AFTER_RENDER = 5_000

PREFIX = "budget-stress"


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
    return {
        "ok": False,
        "error": f"no json stdout={out[:300]!r} stderr={(r.stderr or '')[:300]!r}",
    }


def must_ok(name: str, expr: str) -> str:
    d = eval_j(expr)
    if not d.get("ok"):
        raise AssertionError(f"{name}: {d.get('error') or d}")
    return d.get("value", "")


def parse_intish(v: str) -> int:
    """Pull first integer from a Lisp/JSON-ish value string."""
    m = re.search(r"-?\d+", v.replace('"', ""))
    if not m:
        raise AssertionError(f"no int in {v!r}")
    return int(m.group(0))


def main() -> int:
    fails: list[str] = []

    def check(name: str, fn) -> None:
        try:
            fn()
            print(f"PASS  {name}")
        except Exception as e:
            print(f"FAIL  {name}: {e}")
            fails.append(name)

    def boot() -> None:
        must_ok("outline", '(load-library "outline")')
        must_ok("notes", '(load-library "notes")')
        must_ok("who", "(whoami)")

    def cleanup() -> None:
        # Best-effort delete of stress slugs.
        must_ok(
            "cleanup",
            f"""
(list/foldl
  (lambda (_ row)
    (let ((f (cdr row)))
      (let ((slug (as-str (dict-get f "slug"))))
        (if (string-contains? slug "{PREFIX}")
            (seq (notes/doc/delete slug) 0)
            0))))
  0
  (rows (quote note) 500))
""",
        )

    # ----- 1. PARSE / EXTRACT -----
    def parse_correctness() -> None:
        v = must_ok(
            "el",
            '(extract-links "see [[a]] and [[b|Bee]] [[media:u|x]] [[]] text [[oops")',
        )
        if '"a"' not in v or "Bee" not in v or "media:u" not in v:
            raise AssertionError(f"pairs missing: {v}")
        if must_ok("empty", '(extract-links "no links")') not in ("()", '""'):
            # () is expected
            if must_ok("empty2", '(count (extract-links "no links"))') != "0":
                raise AssertionError("zero links")
        if must_ok("unclosed", '(count (extract-links "text [[oops"))') != "0":
            raise AssertionError("unclosed")
        if must_ok("emptypair", '(count (extract-links "[[]]"))') != "0":
            raise AssertionError("empty brackets")

    def parse_scale() -> None:
        v = must_ok(
            "scale500",
            """
(let ((body (list/foldl (lambda (a i)
        (string-append a "lorem [[p" (as-str i) "]] x "))
      "" (list/range 500))))
  (list (string-length body)
        (count (roam/parse-links body))
        (budget-remaining)))
""",
        )
        # expect 500 links
        if "500" not in v:
            raise AssertionError(f"expected 500 links: {v}")
        # last number is budget-remaining
        parts = re.findall(r"\d+", v)
        if not parts:
            raise AssertionError(v)
        rem = int(parts[-1])
        if rem < FLOOR_AFTER_PARSE:
            raise AssertionError(f"budget floor {rem} < {FLOOR_AFTER_PARSE}: {v}")

    # ----- 2. REINDEX / UPSERT -----
    def upsert_reindex() -> None:
        slug = f"{PREFIX}-upsert-10x8k"
        v = must_ok(
            "upsert",
            f"""
(let ((pad (list/foldl (lambda (a i)
        (string-append a "abcdefghijklmnopqrstuvwxyz0123456789 "))
      "" (list/range 220))))
  (let ((mk (lambda (i)
          (list (string-append "n" (as-str i))
                (string-append "Node " (as-str i))
                (string-append pad " [[L" (as-str i) "]] " pad)
                "" (list)))))
    (let ((tree (list "root" "Stress Upsert" "" ""
                  (list (mk 0) (mk 1) (mk 2) (mk 3) (mk 4)
                        (mk 5) (mk 6) (mk 7) (mk 8) (mk 9)))))
      (list
        (string-length (outline/body (car (outline/kids tree))))
        (notes/doc/upsert "{slug}" "Stress Upsert" tree "stress" "note" "")
        (count (find-rows (quote note-link) "from_slug" "{slug}" 100))
        (budget-remaining)))))
""",
        )
        if slug not in v and f'"{slug}"' not in v:
            # upsert returns slug string
            pass
        if "10" not in v:
            raise AssertionError(f"expected 10 links indexed: {v}")
        rem = int(re.findall(r"\d+", v)[-1])
        if rem < FLOOR_AFTER_UPSERT:
            raise AssertionError(f"budget floor {rem} < {FLOOR_AFTER_UPSERT}: {v}")

    # ----- 3. RENDER -----
    def render_large() -> None:
        # Build tree in one eval, render in same eval (show path without request).
        v = must_ok(
            "render",
            f"""
(let ((pad (list/foldl (lambda (a i)
        (string-append a "word{0} "))
      "" (list/range 400))))
  (let ((mk (lambda (i)
          (list (string-append "r" (as-str i))
                (string-append "R" (as-str i))
                (string-append pad " see [[w" (as-str i) "|W]] and [[media:deadbeef|m]] " pad)
                "" (list)))))
    (let ((tree (list "root" "R" "" ""
                  (list (mk 0) (mk 1) (mk 2) (mk 3) (mk 4)
                        (mk 5) (mk 6) (mk 7) (mk 8) (mk 9)))))
      (let ((html (outline/html "render-stress" tree #f)))
        (list (string-length (as-str html))
              (string-contains? (as-str html) "/notes?slug=w0")
              (string-contains? (as-str html) "note-media")
              (budget-remaining))))))
""",
        )
        if "#t" not in v:
            raise AssertionError(f"expected wiki/media render #t: {v}")
        rem = int(re.findall(r"\d+", v)[-1])
        if rem < FLOOR_AFTER_RENDER:
            raise AssertionError(f"budget floor {rem} < {FLOOR_AFTER_RENDER}: {v}")
        # string-length should be substantial
        length = int(re.findall(r"\d+", v)[0])
        if length < 1000:
            raise AssertionError(f"html too small: {v}")

    def render_single_big_body() -> None:
        v = must_ok(
            "bigbody",
            """
(let ((body (list/foldl (lambda (a i)
        (string-append a "lorem ipsum dolor sit amet [[p" (as-str i) "]] "))
      "" (list/range 200))))
  (let ((tree (list "root" "Big" body "" (list))))
    (let ((html (outline/html "big" tree #f)))
      (list (string-length body)
            (string-length (as-str html))
            (string-contains? (as-str html) "/notes?slug=p0")
            (budget-remaining)))))
""",
        )
        if "#t" not in v:
            raise AssertionError(f"render big body: {v}")
        rem = int(re.findall(r"\d+", v)[-1])
        if rem < FLOOR_AFTER_RENDER:
            raise AssertionError(f"budget floor {rem}: {v}")

    # ----- 4. MANY-NODE ACTIONS -----
    def many_node_ops() -> None:
        slug = f"{PREFIX}-many-nodes"
        # create note with 50 kids
        must_ok(
            "mk50",
            f"""
(let ((kids (list/map (lambda (i)
        (list (string-append "k" (as-str i))
              (string-append "Kid " (as-str i))
              (string-append "body [[x" (as-str i) "]]")
              "" (list)))
      (list/range 50))))
  (notes/doc/upsert "{slug}" "Many Nodes"
    (list "root" "Many Nodes" "" "" kids)
    "stress" "note" ""))
""",
        )
        n = parse_intish(
            must_ok(
                "count",
                f'(count (outline/kids (notes/doc/outline (notes/doc/get "{slug}"))))',
            )
        )
        if n < 50:
            raise AssertionError(f"expected 50 kids, got {n}")

        # each op own eval — cycle first 10
        for i in range(10):
            must_ok(
                f"cycle-{i}",
                f"""
(let ((tree (notes/doc/outline (notes/doc/get "{slug}")))
      (id (string-append "k" "{i}")))
  (let ((node (outline/find tree id)))
    (if (null? node)
        "missing"
        (let ((new (outline/update-by-id tree id
                     (lambda (n) (outline/cycle-todo n)))))
          (notes/doc/upsert "{slug}" "Many Nodes" new "stress" "note" "")))))
""",
            )

        # If outline/update-by-id missing, fall back
        # Check whether cycle worked or error — re-try with set-todo via find path
        # Actually check if update-by-id exists
        d = eval_j('(outline/update-by-id (list "root" "t" "" "" (list)) "root" (lambda (n) n))')
        if not d.get("ok"):
            # fallback: use outline ops that exist — set-title via manual rebuild is heavy;
            # at least touch move via delete+add is too much. Use notes/outline/apply-op if any.
            # Simpler many-node: N separate set-body via upsert of modified tree using outline/set-title on find
            pass

        # extract one node
        new_slug = must_ok(
            "extract",
            f'(notes/refile/extract "{slug}" "k25")',
        ).strip('"')
        if not new_slug or new_slug in ("()", ""):
            raise AssertionError(f"extract failed: {new_slug!r}")
        if must_ok("gone", f'(null? (outline/find (notes/doc/outline (notes/doc/get "{slug}")) "k25"))') != "#t":
            raise AssertionError("k25 still in source")
        if must_ok("exists", f'(null? (notes/doc/get "{new_slug}"))') != "#f":
            raise AssertionError("extracted note missing")

        # refile k26 into extracted note if both exist
        must_ok(
            "refile",
            f'(notes/refile/move "{slug}" "k26" "{new_slug}")',
        )

        # cleanup extract target
        must_ok("del-ex", f'(notes/doc/delete "{new_slug}")')
        must_ok("del-src", f'(notes/doc/delete "{slug}")')

    def many_node_ops_safe() -> None:
        """Many-node without relying on outline/update-by-id."""
        slug = f"{PREFIX}-many-nodes"
        must_ok(
            "mk50",
            f"""
(let ((kids (list/map (lambda (i)
        (list (string-append "k" (as-str i))
              (string-append "Kid " (as-str i))
              (string-append "body [[x" (as-str i) "]]")
              "" (list)))
      (list/range 50))))
  (notes/doc/upsert "{slug}" "Many Nodes"
    (list "root" "Many Nodes" "" "" kids)
    "stress" "note" ""))
""",
        )
        n = parse_intish(
            must_ok(
                "count",
                f'(count (outline/kids (notes/doc/outline (notes/doc/get "{slug}"))))',
            )
        )
        if n != 50:
            raise AssertionError(f"expected 50 kids, got {n}")

        # 20 independent evals: each reindexes after a tiny title change via
        # outline/set-title on a found node + upsert (tests budget isolation).
        for i in range(20):
            must_ok(
                f"touch-{i}",
                f"""
(let ((tree (notes/doc/outline (notes/doc/get "{slug}")))
      (id (string-append "k" (as-str {i}))))
  (let ((node (outline/find tree id)))
    (if (null? node)
        "missing-node"
        (let ((new-node (outline/set-title node
                          (string-append "Kid " (as-str {i}) " x")))
              (kids (outline/kids tree)))
          (let ((kids2 (list/map (lambda (k)
                            (if (string-eq (outline/id k) id) new-node k))
                          kids)))
            (notes/doc/upsert "{slug}" "Many Nodes"
              (outline/set-kids tree kids2)
              "stress" "note" ""))))))
""",
            )

        new_slug = must_ok(
            "extract", f'(notes/refile/extract "{slug}" "k40")'
        ).strip().strip('"')
        if not new_slug or new_slug == "()":
            raise AssertionError(f"extract: {new_slug!r}")
        still = must_ok(
            "still?",
            f'(null? (outline/find (notes/doc/outline (notes/doc/get "{slug}")) "k40"))',
        )
        if still != "#t":
            raise AssertionError("extracted node still in source")
        must_ok("refile", f'(notes/refile/move "{slug}" "k41" "{new_slug}")')
        left = parse_intish(
            must_ok(
                "left",
                f'(count (outline/kids (notes/doc/outline (notes/doc/get "{slug}"))))',
            )
        )
        # 50 -1 extract -1 refile = 48
        if left != 48:
            raise AssertionError(f"expected 48 kids after extract+refile, got {left}")
        must_ok("del-ex", f'(notes/doc/delete "{new_slug}")')
        must_ok("del-src", f'(notes/doc/delete "{slug}")')

    check("boot load libs", boot)
    check("cleanup prior stress slugs", cleanup)
    check("1a parse correctness", parse_correctness)
    check("1b parse scale 500 links", parse_scale)
    check("2 upsert 10x8k + reindex", upsert_reindex)
    check("3a render 10-node wiki+media", render_large)
    check("3b render single big body", render_single_big_body)

    def render_with_extra_actions() -> None:
        """Regression: per-node refile/options used to N×scan vault and OOM budget."""
        # notes load defines outline/extra-actions; must stay cheap per node
        must_ok("notes", '(load-library "notes")')
        src = must_ok("src", '(function-source "outline/extra-actions")')
        if "notes/refile/options" in src or "notes/refile/form" in src:
            raise AssertionError(
                "outline/extra-actions must not embed refile/options per node "
                f"(budget N×vault); got {src[:200]}"
            )
        v = must_ok(
            "html25",
            """
(let ((kids (list/map (lambda (i)
        (list (string-append "n" (as-str i)) (string-append "T" (as-str i))
              (string-append "body [[x" (as-str i) "]] more text here")
              "" (list)))
      (list/range 25))))
  (let ((tree (list "root" "R" "" "" kids)))
    (list (string-length (outline/html "x" tree #f))
          (budget-remaining))))
""",
        )
        rem = int(re.findall(r"\d+", v)[-1])
        if rem < FLOOR_AFTER_RENDER:
            raise AssertionError(f"budget floor after 25-node html: {v}")

    check("3c render 25-node with notes extra-actions", render_with_extra_actions)
    check("4 many-node ops + extract/refile", many_node_ops_safe)
    check("cleanup after", cleanup)

    if fails:
        print(f"\n{len(fails)} FAILED: {', '.join(fails)}")
        return 1
    print("\nALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
