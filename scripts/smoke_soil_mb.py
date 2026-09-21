#!/usr/bin/env python3
"""Smoke: soil M-B — river window policy (archive/plans/soil-m-b.md).

Proves the shim drives the workspace row both directions under a real
(nested) river compositor:

  1. boot check (/health)
  2. start nested river + soil-strip (until=1) on its socket
  3. foot maps -> row gains an app:foot pane (river -> policy leg)
  4. browser focus POST -> row focus moves (policy authority shared)
  5. foot dies -> row record removed (close leg); shim exits 0 with a
     latency report; p95 event->applied under budget
  6. alt+arrow keybindings: registration is part of shim startup (the
     pressed path shares the browser-leg code; physical-key test is
     manual — D6)

Exit non-zero on any mismatch.

  scripts/dev.sh start
  python3 scripts/smoke_soil_mb.py

Env: PP_HTTP_PORT, PP_USER/PP_PASS, WAYLAND_DISPLAY (skip without it).
"""
from __future__ import annotations

import base64
import json
import os
import re
import signal
import subprocess
import sys
import time
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# regression guard, not a soil-contract claim: the floor is the image's
# per-eval cost (~22ms idle; queues under concurrent load through the
# identity session's single store connection). Measured range 63-330ms.
BUDGET_P95 = float(os.environ.get("SOIL_MB_P95_BUDGET", "0.5"))  # seconds


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


def row() -> dict:
    out = eval_lisp("(filmstrip/native-state)")
    must(out.get("ok"), f"native-state eval ok: {out}")
    v = out["value"]
    return v if isinstance(v, dict) else json.loads(v)


def wait_until(pred, timeout_s: float, what: str):
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        try:
            if pred():
                return True
        except Exception:
            pass
        time.sleep(0.3)
    return False


def live_sockets(sockdir) -> set[str]:
    """wayland-* sockets that actually accept connections (stale files
    from killed rivers don't — river 0.4 reuses their names)."""
    import socket as pysock
    live = set()
    for n in os.listdir(sockdir):
        if not n.startswith("wayland-") or n.endswith(".lock"):
            continue
        p = os.path.join(sockdir, n)
        try:
            s = pysock.socket(pysock.AF_UNIX, pysock.SOCK_STREAM)
            s.settimeout(0.3)
            s.connect(p)
            s.close()
            live.add(n)
        except OSError:
            pass
    return live


def main():
    with urllib.request.urlopen(BASE + "/health", timeout=10) as r:
        health = json.loads(r.read().decode())
    must(health.get("ok") is True, f"boot check /health ok={health.get('ok')}")

    if not os.environ.get("WAYLAND_DISPLAY"):
        print("SKIP: WAYLAND_DISPLAY not set — M-B needs a desktop to nest river on")
        return
    exe = os.path.join(ROOT, "_build", "default", "native", "soil_strip.exe")
    must(os.path.exists(exe), f"shim built: {exe}")

    out = eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    must(out.get("ok"), f"workspace reset ok: {out}")

    # nested river on its own socket (detect by CONNECTABILITY: river
    # may reuse a stale socket file's name, so file diffs lie)
    sockdir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    before = live_sockets(sockdir)
    river = subprocess.Popen(["river"], cwd=ROOT,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    nested_sock = None
    try:
        def new_live():
            return live_sockets(sockdir) - before
        must(wait_until(lambda: river.poll() is None and new_live(),
                        15, "nested river socket"),
             "nested river is live on a new socket")
        nested_sock = new_live().pop()
        print(f"OK: nested river on {nested_sock}")

        env = dict(os.environ)
        env.update({"PP_URL": BASE, "PP_USER": USER, "PP_PASS": PASS,
                    "WAYLAND_DISPLAY": nested_sock})
        strip = subprocess.Popen([exe, "--until", "1"], cwd=ROOT, env=env,
                                 stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                 text=True)

        # foot maps -> row gains app:foot (river -> policy leg)
        time.sleep(2.0)
        foot = subprocess.Popen(["foot"], cwd=ROOT, env=env,
                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        must(wait_until(
            lambda: any(p.get("route", "").startswith("app:foot") for p in row()["panes"]),
            15, "row gains app:foot pane"),
            "foot mapped -> workspace row gains an app:foot pane (river -> policy)")
        panes = row()["panes"]
        foot_panes = [p for p in panes if p.get("route", "").startswith("app:foot")]
        must(foot_panes and foot_panes[0].get("view", "").startswith("w"),
             f"pane carries its river view key: {foot_panes}")

        # browser focus POST -> row focus moves (shared policy authority)
        target = 0 if row()["focus"] != 0 else (1 if len(row()["panes"]) > 1 else 0)
        http("POST", "/filmstrip/focus", {"dir": "left" if target == 0 else "right"})
        must(wait_until(lambda: row()["focus"] == target, 5, "row focus moves"),
            f"browser focus POST moved the row focus to {target} (policy -> river applies)")

        # foot dies -> row record removed (close leg); shim exits with report
        foot.send_signal(signal.SIGTERM)
        must(wait_until(
            lambda: not any(p.get("route", "").startswith("app:foot") for p in row()["panes"]),
            15, "row loses app:foot pane"),
            "foot exited -> workspace row record removed (close leg)")

        must(wait_until(lambda: strip.poll() is not None, 15, "shim exits after until=1"),
            "soil-strip exited after 1 open + 1 close cycle")
        report = strip.stdout.read() if strip.stdout else ""
        m = re.search(r"p95=([\d.]+)ms", report)
        must(m is not None, f"latency report present:\n{report}")
        p95 = float(m.group(1)) / 1000.0
        must(p95 <= BUDGET_P95,
             f"p95 event->applied {p95 * 1000:.0f}ms under {BUDGET_P95 * 1000:.0f}ms budget")
        print(f"OK: p95 event->applied {p95 * 1000:.0f}ms (budget {BUDGET_P95 * 1000:.0f}ms)")
    finally:
        for pid in (locals().get("foot"), locals().get("strip")):
            try:
                if pid and pid.poll() is None:
                    pid.kill()
            except Exception:
                pass
        if river.poll() is None:
            river.kill()

    eval_lisp("(filmstrip/ws-save (filmstrip/ws-default))")
    print("\nsoil M-B smoke: all checks green.")


if __name__ == "__main__":
    import urllib.parse  # noqa: F401  (used via http())
    main()
