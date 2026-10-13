#!/usr/bin/env python3
"""Smoke: pads hello dual-render spike (archive/plans/pads-hello-dualrender.md).

Proves V1 (byte-identity, legacy vs pads) against a running dev image:

  1. boot check (/health)
  2. GET /hello serves the hello card (route -> hello/page)
  3. FIXTURE DUEL: one in-image eval renders a fixture pad tree via
     pads/render-html AND the same tree hand-written with libs/html
     calls; both fragments are returned and compared byte-for-byte
     (in-image string-eq AND python-side equality).
  4. HELLO DUEL: (pads/render-html (hello/view)) vs (hello/page)
     (stripping the render-html __html__ raw sentinel).
  5. SCENE DIALECT v0: (hello/scene) is valid JSON, version 0, root
     column with stable path ids, size grammar fixed:240 / grow:1;
     two evals are byte-identical (determinism).

Exit non-zero on any mismatch.

  scripts/dev.sh start
  python3 scripts/smoke_pads_hello.py

Env: PP_HTTP_PORT (default 18080), PP_USER/PP_PASS (default admin/admin).
"""
from __future__ import annotations

import base64
import json
import os
import sys
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")
SENTINEL = "__html__:"


def auth_headers(extra=None):
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
        headers=auth_headers(
            {"Content-Type": "application/json", "Accept": "application/json"}
        ),
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.loads(r.read().decode())


def must(cond, msg):
    if not cond:
        print("FAIL:", msg)
        sys.exit(1)
    print("OK:", msg)


# The fixture duel: SAME tree authored twice, inside one eval so nothing
# touches branch state. Escaping-sensitive strings prove html-escape
# parity; the splice list + nil kid exercise pads/flatten-kids; sizes
# exercise the canonical style order.
FIXTURE_DUEL = r"""(let ((fixture (lambda ()
        (pad 'column (list (cons 'gap "8"))
          (pad 'heading (list (cons 'level "2")) "Head & <b>gone</b>")
          (pad 'text (list (cons 'class "c") (cons 'size (list 'grow 1))) "body \"q\" <i>")
          (pad 'link (list (cons 'href "/x")) "go")
          (pad 'raw (list (cons 'tag "details") (cons 'open "open")) "raw body")
          (pad "span" (list (cons 'class "x")) "span kid")
          (pad 'row (list)
            (list (pad 'text (list) "spliced-a")
                  (pad 'text (list (cons 'size (list 'fixed 120))) "spliced-b"))
            (quote ())
            "tail"))))
      (legacy (lambda ()
        (html/tag "div" (html/attr "style" "display:flex;flex-direction:column;gap:8px")
          (html/join (list
            (html/tag "h2" "" (html/text "Head & <b>gone</b>"))
            (html/tag "div" (html/attrs (list (html/attr "class" "c") (html/attr "style" "flex:1"))) (html/text "body \"q\" <i>"))
            (html/tag "a" (html/attr "href" "/x") (html/text "go"))
            (html/tag "details" (html/attr "open" "open") (html/text "raw body"))
            (html/tag "span" (html/attr "class" "x") (html/text "span kid"))
            (html/tag "div" (html/attr "style" "display:flex;flex-direction:row")
              (html/join (list
                (html/text "spliced-a")
                (html/tag "div" (html/attr "style" "flex:0 0 120px") (html/text "spliced-b"))
                (html/text "tail"))))))))))
  (let ((pads-out (pads/render-html (fixture)))
        (legacy-out (legacy)))
    (list pads-out legacy-out (string-eq pads-out legacy-out))))"""


def main():
    # 1. boot check
    with urllib.request.urlopen(BASE + "/health", timeout=10) as r:
        health = json.loads(r.read().decode())
    must(health.get("ok") is True, f"boot check /health ok={health.get('ok')}")

    # 2. /hello serves the card
    req = urllib.request.Request(BASE + "/hello", headers=auth_headers())
    with urllib.request.urlopen(req, timeout=15) as r:
        page = r.read().decode("utf-8", "replace")
    must("display:flex;flex-direction:column;gap:12px;padding:16px" in page,
         "/hello column style")
    must("Hello, pads" in page, "/hello greeting")
    must("flex:0 0 240px" in page and "flex:1" in page, "/hello row boxes")

    # 3. fixture duel (byte-identity over a fixture tree)
    out = eval_lisp(FIXTURE_DUEL)
    must(out.get("ok"), f"fixture duel eval ok: {out}")
    pads_out, legacy_out, in_image_eq = out["value"]
    must(in_image_eq is True, "fixture duel in-image string-eq")
    must(pads_out == legacy_out, "fixture duel python byte-equality")
    must("&amp;" in pads_out and "&lt;i&gt;" in pads_out, "fixture escaping applied")

    # 4. hello duel (the shipped card, both pipelines)
    out = eval_lisp("(pads/render-html (hello/view))")
    must(out.get("ok"), f"hello/view render ok: {out}")
    pads_hello = out["value"]
    out = eval_lisp("(hello/page)")
    must(out.get("ok"), f"hello/page eval ok: {out}")
    legacy_hello = out["value"]
    must(legacy_hello.startswith(SENTINEL), "hello/page carries raw sentinel")
    legacy_body = legacy_hello[len(SENTINEL):]
    must(pads_hello == legacy_body, "hello duel byte-equality (pads vs legacy)")

    # 5. scene dialect v0: parse + shape + determinism. NOTE: the image's
    # /api/eval transport re-parses JSON-lookalike string values into real
    # JSON (i-fields-as-string-2026-08-12), so the scene may arrive as a
    # dict, not the raw string pads/render-scene built in-image.
    out = eval_lisp("(hello/scene)")
    must(out.get("ok"), f"hello/scene eval ok: {out}")
    scene_val = out["value"]
    scene = json.loads(scene_val) if isinstance(scene_val, str) else scene_val
    must(scene.get("version") == 0, "scene version 0")
    root = scene.get("root", {})
    must(root.get("kind") == "column", "scene root kind column")
    must(root.get("id") == "0", "scene root path id 0")
    must(root.get("attrs", {}).get("gap") == "12"
         and root.get("attrs", {}).get("padding") == "16",
         "scene root gap/padding string attrs")
    kids = root.get("kids", [])
    must(len(kids) == 3, "scene root has 3 kids")
    must(kids[0]["kind"] == "text" and kids[0]["attrs"].get("level") == "1"
         and kids[0]["text"] == "Hello, pads", "scene heading -> text w/ level")
    must(kids[1]["kind"] == "text" and kids[1]["id"] == "0/1", "scene subline path id")
    row = kids[2]
    must(row["kind"] == "row" and row["id"] == "0/2", "scene row path id")
    boxes = row.get("kids", [])
    must(len(boxes) == 2
         and boxes[0]["attrs"].get("size") == "fixed:240"
         and boxes[1]["attrs"].get("size") == "grow:1",
         "scene size grammar fixed:240 / grow:1")
    out2 = eval_lisp("(hello/scene)")
    must(out2.get("ok") and out2["value"] == scene_val,
         "scene render deterministic (two evals identical)")

    print("ALL PASS")


if __name__ == "__main__":
    main()
