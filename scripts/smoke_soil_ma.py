#!/usr/bin/env python3
"""Smoke: soil M-A — filmstrip kiosk scene (docs/plans/soil-m-a.md).

Proves the filmstrip shell lowers to scene-dialect v0 and renders on a
real Wayland toplevel:

  1. boot check (/health)
  2. empty-state scene: valid v0 shape (kinds, stable path ids, size
     grammar), dim empty hint, no panes
  3. workspace mutations reflected in the scene:
     open /todos + /notes -> two pane columns at niri weights
     focus left           -> exactly one pane title wears sel (the right one)
     width cycle          -> todos pane weight 2 -> 3
     cascade d=0 t=todo   -> app column sel on "todo" + lists column
  4. shim: soil_hello --frames 12 --eval "(filmstrip/scene)" exits 0
     (only when WAYLAND_DISPLAY is set; SKIP otherwise)

Exit non-zero on any mismatch.

  scripts/dev.sh start
  python3 scripts/smoke_soil_ma.py

Env: PP_HTTP_PORT (default 18080), PP_USER/PP_PASS (default admin/admin).
"""
from __future__ import annotations

import base64
import json
import os
import re
import subprocess
import sys
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SIZE_RE = re.compile(r"^(fit|grow:\d+|fixed:\d+)$")


def auth_headers(extra=None):
    token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}"}
    if extra:
        h.update(extra)
    return h


def http(method: str, path: str, form: dict | None = None) -> str:
    data = "&".join(f"{k}={urllib.parse.quote(str(v))}" for k, v in (form or {}).items()).encode()
    extra = {"HX-Request": "true"}
    if data:
        extra["Content-Type"] = "application/x-www-form-urlencoded"
    req = urllib.request.Request(BASE + path, data=data or None, method=method,
                                 headers=auth_headers(extra))
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.read().decode("utf-8", "replace")


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


def scene() -> dict:
    out = eval_lisp("(filmstrip/scene)")
    must(out.get("ok"), f"filmstrip/scene eval ok: {out}")
    v = out["value"]
    return json.loads(v) if isinstance(v, str) else v


def validate_v0(node: dict, parent_id: str, idx: int | None, seen: set):
    """Scene dialect v0: kinds, stable path ids, string attrs, size grammar."""
    kind = node.get("kind")
    must(kind in ("row", "column", "text"), f"kind v0: {kind}")
    nid = node.get("id", "")
    want = parent_id if idx is None else f"{parent_id}/{idx}"
    must(nid == want, f"stable path id {nid} (want {want})")
    must(nid not in seen, f"id unique: {nid}")
    seen.add(nid)
    for k, v in (node.get("attrs") or {}).items():
        must(isinstance(v, str), f"attr {k} string-valued")
        if k == "size":
            must(SIZE_RE.match(v), f"size grammar: {v}")
    if kind == "text":
        must(isinstance(node.get("text", ""), str), "text is a string")
    for i, kid in enumerate(node.get("kids") or []):
        validate_v0(kid, nid, i, seen)


def walk(node: dict):
    yield node
    for kid in node.get("kids") or []:
        yield from walk(kid)


def main():
    # 1. boot check
    with urllib.request.urlopen(BASE + "/health", timeout=10) as r:
        health = json.loads(r.read().decode())
    must(health.get("ok") is True, f"boot check /health ok={health.get('ok')}")

    # reset workspace for determinism
    out = eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    must(out.get("ok"), f"workspace reset ok: {out}")

    # 2. empty-state scene
    s = scene()
    must(s.get("version") == 0, "scene version 0")
    root = s["root"]
    must(root["kind"] == "column", "root is a column")
    validate_v0(root, "0", None, set())
    must(len(root["kids"]) == 3, "root: cascade + hint + strip")
    must(any(n.get("attrs", {}).get("dim") and "no panes" in n.get("text", "")
             for n in walk(root)), "empty state hint present")

    # 3. mutations reflected
    http("POST", "/filmstrip/open", {"route": "/todos"})
    http("POST", "/filmstrip/open", {"route": "/notes"})
    s = scene()
    validate_v0(s["root"], "0", None, set())
    strip = s["root"]["kids"][2]
    panes = [n for n in strip["kids"] if n["kind"] == "column"]
    must(len(panes) == 2, f"two pane columns: {len(panes)}")
    must(all(p["attrs"].get("size") == "grow:2" for p in panes),
         "third presets -> weight 2")
    titles = {p["kids"][0]["text"]: "sel" in p["kids"][0].get("attrs", {})
              for p in panes}
    must(titles == {"/todos": False, "/notes": True},
         f"focus on /notes only: {titles}")
    must(any("no panes" not in n.get("text", "") or not n.get("attrs", {}).get("dim")
             for n in walk(root)), "empty hint gone")

    # focus left -> /todos takes the ring
    http("POST", "/filmstrip/focus", {"dir": "left"})
    s = scene()
    strip = s["root"]["kids"][2]
    titles = {p["kids"][0]["text"]: "sel" in p["kids"][0].get("attrs", {})
              for p in strip["kids"] if p["kind"] == "column"}
    must(titles == {"/todos": True, "/notes": False},
         f"focus left -> /todos ringed: {titles}")

    # width cycle on the focused pane (/todos, id needed)
    row = eval_lisp("(filmstrip/ws-fields)")
    panes_row = row["value"] if isinstance(row["value"], list) else json.loads(
        row["value"]) if isinstance(row["value"], str) else row["value"]
    todo_id = panes_row["content"][0]["id"]
    http("POST", "/filmstrip/width", {"id": todo_id})
    s = scene()
    strip = s["root"]["kids"][2]
    sizes = {p["kids"][0]["text"]: p["attrs"].get("size")
             for p in strip["kids"] if p["kind"] == "column"}
    must(sizes == {"/todos": "grow:3", "/notes": "grow:2"},
         f"width cycle third->half = weight 3: {sizes}")

    # cascade crumb -> sel on the app column + lists column appears
    http("POST", "/filmstrip/cascade", {"d": 0, "t": "todo"})
    s = scene()
    cascade = s["root"]["kids"][0]
    must(len(cascade["kids"]) == 2, f"cascade walked to lists column: {len(cascade['kids'])}")
    sel_entries = [n["text"] for n in walk(cascade)
                   if n["kind"] == "text" and "sel" in n.get("attrs", {})]
    must(sel_entries == ["todos"], f"cascade sel on the todo app entry: {sel_entries}")
    todo_col = cascade["kids"][1]
    must(any("Inbox" in k.get("text", "") for k in todo_col["kids"]),
         "todo lists column lists Inbox")

    # todo summary body carries item counts
    todos_pane = next(p for p in strip_kids(s) if p["kind"] == "column"
                      and p["kids"][0]["text"] == "/todos")
    must(any(re.match(r"^Inbox \(\d+\)$", k.get("text", "")) for k in todos_pane["kids"]),
         "todo pane body shows list titles with counts")

    # 4. the shim renders it (only on a Wayland desktop)
    if not os.environ.get("WAYLAND_DISPLAY"):
        print("SKIP: WAYLAND_DISPLAY not set — shim leg needs a desktop")
    else:
        env = dict(os.environ)
        env.update({"PP_URL": BASE, "PP_USER": USER, "PP_PASS": PASS,
                    "SOIL_PP_EVAL": os.path.join(ROOT, "scripts", "pp-eval.sh")})
        exe = os.path.join(ROOT, "_build", "default", "native", "soil_hello.exe")
        must(os.path.exists(exe), f"shim built: {exe}")
        proc = subprocess.run([exe, "--frames", "12", "--eval", "(filmstrip/scene)"],
                              cwd=ROOT, env=env, capture_output=True, text=True,
                              timeout=120)
        must(proc.returncode == 0,
             f"soil_hello --eval (filmstrip/scene) exit 0: {proc.stderr[-400:]}")
        print("OK: shim rendered the kiosk scene (12 frames)")

    # reset workspace so the smoke leaves a clean desktop
    eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    print("\nsoil M-A smoke: all checks green.")


def strip_kids(s):
    return s["root"]["kids"][2]["kids"]


if __name__ == "__main__":
    import urllib.parse  # noqa: F401  (used via http())
    main()
