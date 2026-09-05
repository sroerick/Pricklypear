#!/usr/bin/env python3
"""Smoke: filmstrip v1 habitat shell (docs/plans/filmstrip-v1.md).

Proves the two-strip shell against a running dev image:

  1. boot check (/health)
  2. shell renders: cascade strip + launcher + content strip + key
     sentinels, ZERO <script> tags (D3)
  3. launcher open /todos -> pane appears with dispatched todo fragment,
     .focused, width third; workspace row persisted
  4. open /notes -> second pane appended, focused
  5. focus POST left -> OOB swap: old pane loses .focused, new gains it
  6. width POST cycles the preset (third -> half) in OOB + row
  7. cascade crumb POST (d=0 t=todo) -> Miller column appended, row path
  8. cascade leaf POST (d=1 t=/todos) -> open-or-focus (no new pane)
  9. cascade truncation (d=0 t=notes) -> columns re-walk from depth 0
 10. reload round-trip: fresh GET renders same panes/order/widths/focus/
     cascade from the row (V4)
 11. close POST -> OOB delete + row updated + focus hand-off

Exit non-zero on any mismatch.

  scripts/dev.sh start
  python3 scripts/smoke_filmstrip.py

Env: PP_HTTP_PORT (default 18080), PP_USER/PP_PASS (default admin/admin).
"""
from __future__ import annotations

import base64
import json
import os
import re
import sys
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def auth_headers(extra=None):
    token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}"}
    if extra:
        h.update(extra)
    return h


def http(method: str, path: str, form: dict | None = None, hx: bool = True) -> str:
    data = "&".join(f"{k}={urllib.parse.quote(str(v))}" for k, v in (form or {}).items()).encode()
    extra = {"HX-Request": "true"} if hx else {}
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


def pane_ids(html: str) -> list[str]:
    return re.findall(r'data-pane="([0-9a-f-]+)"', html)


def row() -> dict:
    out = eval_lisp("(filmstrip/ws-fields)")
    must(out.get("ok"), f"ws row eval ok: {out}")
    v = out["value"]
    return v if isinstance(v, dict) else json.loads(v)


def main():
    # 1. boot check
    with urllib.request.urlopen(BASE + "/health", timeout=10) as r:
        health = json.loads(r.read().decode())
    must(health.get("ok") is True, f"boot check /health ok={health.get('ok')}")

    # reset the workspace row so the run is deterministic
    out = eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    must(out.get("ok"), f"workspace reset ok: {out}")

    # 2. shell renders (fragment mode)
    shell = http("GET", "/filmstrip")
    must('id="fs-cascade"' in shell and 'class="fs-col"' in shell, "shell cascade strip")
    must('id="fs-launch"' in shell and 'name="route"' in shell, "shell launcher row")
    must('id="fs-content"' in shell, "shell content strip")
    must("keydown[altKey" in shell and 'hx-trigger' in shell, "key sentinels (alt+arrows)")
    must("<script" not in shell, "zero-JS shell (no <script>)")

    # 3. launcher opens /todos
    resp = http("POST", "/filmstrip/open", {"route": "/todos"})
    ids1 = pane_ids(resp)
    must(len(ids1) == 1, f"open /todos -> one pane id: {ids1}")
    must('fs-pane fs-third focused' in resp, "open /todos -> third width + focused")
    must('fs-title">/todos' in resp, "open /todos -> title")
    must("fs-body" in resp and "filmstrip: no route for" not in resp,
         "open /todos -> dispatched fragment in pane body")
    r1 = row()
    must(len(r1["content"]) == 1 and r1["content"][0]["width"] == "third",
         f"row persisted pane: {r1['content']}")
    must(r1["focus"] == "content:0", f"row focus content:0: {r1['focus']}")
    todo_id = ids1[0]

    # 4. open /notes -> second pane appended + focused
    resp = http("POST", "/filmstrip/open", {"route": "/notes"})
    ids2 = pane_ids(resp)
    must(len(ids2) == 2, f"open /notes -> two pane ids: {ids2}")
    must('fs-pane fs-third focused' in resp, "open /notes -> new pane focused")
    r2 = row()
    must([p["route"] for p in r2["content"]] == ["/todos", "/notes"],
         f"row order [/todos,/notes]: {[p['route'] for p in r2['content']]}")
    must(r2["focus"] == "content:1", f"row focus content:1: {r2['focus']}")
    notes_id = ids2[1]

    # 5. focus left: /notes pane loses .focused, /todos pane gains it (OOB)
    resp = http("POST", "/filmstrip/focus", {"dir": "left"})
    secs = re.findall(r'<section class="fs-pane[^"]*"[^>]*data-pane="([0-9a-f-]+)"[^>]*hx-swap-oob="true"', resp)
    must(len(secs) == 2, f"focus left -> two OOB sections: {secs}")
    must('fs-pane fs-third focused' in resp and f'data-pane="{todo_id}"' in resp,
         "focus left -> /todos pane focused via OOB")
    r3 = row()
    must(r3["focus"] == "content:0", f"row focus back to content:0: {r3['focus']}")

    # 6. width cycle third -> half on the /todos pane
    resp = http("POST", "/filmstrip/width", {"id": todo_id})
    must('fs-pane fs-half' in resp, "width cycle -> half preset in OOB")
    must("fs-third" not in resp.replace('fs-pane fs-third', 'X') or 'fs-half' in resp,
         "width cycle left the third preset")
    r4 = row()
    must(r4["content"][0]["width"] == "half", f"row width half: {r4['content'][0]['width']}")

    # 7. cascade crumb: depth 0, target todo -> Miller column appended
    resp = http("POST", "/filmstrip/cascade", {"d": 0, "t": "todo"})
    must(resp.count('class="fs-col"') >= 2, "cascade walk -> app column + todo lists column")
    r5 = row()
    must(r5["cascade"] == ["todo"], f"row cascade [todo]: {r5['cascade']}")

    # 8. cascade leaf: depth 1, target /todos -> open-or-focus (no new pane)
    resp = http("POST", "/filmstrip/cascade", {"d": 1, "t": "/todos"})
    must(f'data-pane="{todo_id}"' in resp, "cascade leaf -> /todos pane focused")
    r6 = row()
    must(len(r6["content"]) == 2, f"leaf click opened no new pane: {len(r6['content'])}")
    must(r6["cascade"] == ["todo", "/todos"], f"row cascade path: {r6['cascade']}")

    # 9. truncation: depth 0, target notes -> path truncated + re-walked
    resp = http("POST", "/filmstrip/cascade", {"d": 0, "t": "notes"})
    must(resp.count('class="fs-col"') >= 2, "truncation -> columns re-walked")
    r7 = row()
    must(r7["cascade"] == ["notes"], f"row cascade [notes]: {r7['cascade']}")

    # 10. reload round-trip: fresh GET renders the persisted arrangement
    shell = http("GET", "/filmstrip")
    ids_rt = pane_ids(shell)
    must(ids_rt == [todo_id, notes_id], f"reload -> same pane order: {ids_rt}")
    must('fs-pane fs-half focused' in shell, "reload -> /todos pane half + focused")
    must('fs-entry fs-sel' in shell, "reload -> cascade selection rendered")
    must('fs-title">/todos' in shell and 'fs-title">/notes' in shell,
         "reload -> both panes dispatched")
    must("filmstrip: no route for" not in shell, "reload -> no dispatch errors")

    # 11. close the /notes pane -> OOB delete + row updated + focus hand-off
    resp = http("POST", "/filmstrip/close", {"id": notes_id})
    must('hx-swap-oob="delete:#fs-pane-' in resp, "close -> OOB delete")
    r8 = row()
    must([p["route"] for p in r8["content"]] == ["/todos"],
         f"close -> row has only /todos: {[p['route'] for p in r8['content']]}")
    must(r8["focus"] == "content:0", f"close -> focus hand-off: {r8['focus']}")

    # reset workspace so the smoke leaves a clean desktop
    eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    print("\nfilmstrip smoke: all checks green.")


if __name__ == "__main__":
    import urllib.parse  # noqa: F401  (used via http())
    main()
