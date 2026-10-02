#!/usr/bin/env python3
"""Smoke: operator console strip (product-experience.borg §activity-ledger
READ; security.borg §the-watched-machine).

Proves the console face against a running dev image:

  1. boot check (/health)
  2. /ops renders the strip for an admin: live-session card, spend
     rollups, knock summary, incidents, ledger tail, and at least one
     HOLD form
  3. the filmstrip app chooser carries the "operator console" leaf and
     POST /filmstrip/cascade d=0 t=/ops opens the console pane
  4. a hold POST (action=hold, kind=revoke-sessions, a bogus target)
     re-renders the strip inline and lands an admin.hold ledger row
     (polled; the flush fiber is ~30s)
  5. a NON-ADMIN is denied at the kernel: (ops/presence) says
     "denied: admin only" and (ops/hold …) refuses

Exit non-zero on any mismatch.

  scripts/dev.sh start
  python3 scripts/smoke_ops_console.py

Env: PP_HTTP_PORT (default 18080), PP_USER/PP_PASS (default admin/admin),
     PP_HTTP (the base URL, default http://127.0.0.1:PORT).
"""
from __future__ import annotations

import base64
import json
import os
import sys
import time
import urllib.parse
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_HTTP", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def auth_headers(user=None, pw=None, extra=None):
    token = base64.b64encode(f"{user or USER}:{pw or PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}"}
    if extra:
        h.update(extra)
    return h


def http(method, path, form=None, user=None, pw=None, hx=True):
    data = "&".join(
        f"{k}={urllib.parse.quote(str(v))}" for k, v in (form or {}).items()
    ).encode()
    extra = {"HX-Request": "true"} if hx else {}
    if data:
        extra["Content-Type"] = "application/x-www-form-urlencoded"
    req = urllib.request.Request(
        BASE + path, data=data or None, method=method,
        headers=auth_headers(user, pw, extra),
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.read().decode("utf-8", "replace")


def eval_lisp(expr, user=None, pw=None):
    body = json.dumps({"expr": expr}).encode()
    req = urllib.request.Request(
        BASE + "/api/eval", data=body, method="POST",
        headers=auth_headers(
            user, pw,
            {"Content-Type": "application/json", "Accept": "application/json"},
        ),
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.loads(r.read().decode())


def must(cond, msg):
    if not cond:
        print("FAIL:", msg)
        sys.exit(1)
    print("OK:", msg)


def main():
    with urllib.request.urlopen(BASE + "/health", timeout=10) as r:
        health = json.loads(r.read().decode())
    must(health.get("ok") is True, f"boot check /health ok={health.get('ok')}")

    # 2. the console renders for an admin
    page = http("GET", "/ops", hx=False)
    must("Operator console" in page, "console heading present")
    must('id="ops-console"' in page, "console root #ops-console present")
    must("ops-session card" in page, "live-session card present")
    must("ops-rollups" in page, "spend rollup table present")
    must("ops-knocks" in page, "knock summary present")
    must("ops-ledger" in page, "ledger tail present")
    must('name="kind"' in page and 'name="target"' in page,
         "at least one HOLD form (kind/target hidden inputs)")

    # 3. the filmstrip chooser carries the console leaf; openings work.
    #    Reset the workspace first so the run is deterministic (a previously
    #    open /ops pane would win open-or-focus and carry an hx-preserve
    #    empty body in the OOB response).
    reset = eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    must(reset.get("ok"), f"workspace reset ok: {reset}")
    shell = http("GET", "/filmstrip")
    must("operator console" in shell, "filmstrip app chooser has the console")
    openresp = http("POST", "/filmstrip/cascade", {"d": "0", "t": "/ops"})
    must("Operator console" in openresp,
         "cascade leaf /ops opens the console pane")
    must("filmstrip: no route for /ops" not in openresp,
         "the pane dispatched (no missing-route placeholder)")

    # 4. a hold POST re-renders the strip and rows admin.hold
    bogus = "00000000-0000-0000-0000-000000000000"
    hold = http("POST", "/ops",
                {"action": "hold", "kind": "revoke-sessions", "target": bogus})
    must("Operator console" in hold, "hold POST re-renders the strip")
    # the ledger flush fiber is ~30s; poll up to ~40s for the row
    found = None
    for _ in range(40):
        out = eval_lisp(
            '(ops/activity "" "" "admin.hold" 200)'
        )
        rows = out.get("value") or []
        for s in rows:
            j = json.loads(s) if isinstance(s, str) else s
            if j.get("class") == "admin.hold" and bogus in (j.get("target") or ""):
                found = j
                break
        if found:
            break
        time.sleep(1)
    must(found is not None, "admin.hold landed for the bogus target")

    # 5. a non-admin is denied at the kernel
    name = f"opssmoke{os.getpid()}"
    created = eval_lisp(f'(create-user "{name}" "pw-{os.getpid()}" #f)')
    must(created.get("ok"), f"create non-admin user: {created}")
    denied = eval_lisp("(ops/presence)", user=name, pw=f"pw-{os.getpid()}")
    must(isinstance(denied.get("value"), str)
         and "denied" in denied["value"],
         "non-admin (ops/presence) denied")
    held = eval_lisp('(ops/hold "revoke-sessions" "x")', user=name,
                     pw=f"pw-{os.getpid()}")
    must(isinstance(held.get("value"), str) and "denied" in held["value"],
         "non-admin (ops/hold) denied")

    print("ALL OK: operator console strip")


if __name__ == "__main__":
    main()
