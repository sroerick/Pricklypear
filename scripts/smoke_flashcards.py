#!/usr/bin/env python3
"""Smoke: flashcards package (decks, study, SM-2-ish review).

  scripts/dev.sh start
  python3 scripts/smoke_flashcards.py
"""
from __future__ import annotations

import base64
import json
import os
import re
import sys
import urllib.parse
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_SMOKE_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")


def auth_h(extra=None):
    token = base64.b64encode(f"{USER}:{PASS}".encode()).decode()
    h = {"Authorization": f"Basic {token}"}
    if extra:
        h.update(extra)
    return h


def ev(expr: str) -> dict:
    body = json.dumps({"expr": expr}).encode()
    req = urllib.request.Request(
        BASE + "/api/eval",
        data=body,
        method="POST",
        headers=auth_h(
            {"Content-Type": "application/json", "Accept": "application/json"}
        ),
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.loads(r.read().decode())


def get(path: str):
    req = urllib.request.Request(BASE + path, headers=auth_h())
    with urllib.request.urlopen(req, timeout=15) as r:
        return r.status, r.read().decode("utf-8", "replace")


def post(path: str, data: dict):
    body = urllib.parse.urlencode(data).encode()
    req = urllib.request.Request(
        BASE + path,
        data=body,
        method="POST",
        headers=auth_h({"Content-Type": "application/x-www-form-urlencoded"}),
    )
    with urllib.request.urlopen(req, timeout=15) as r:
        return r.status, r.geturl(), r.read().decode("utf-8", "replace")


def must(cond, msg):
    if not cond:
        print("FAIL:", msg)
        sys.exit(1)
    print("OK:", msg)


def main():
    r = ev('(load-library "flashcards")')
    must("loaded" in str(r.get("value")), f"load {r}")
    r = ev('(flash/deck-create "SmokeDeck" "test")')
    deck = re.search(r"[0-9a-f-]{36}", r["value"]).group(0)
    r = ev(f'(flash/add "{deck}" "front-q" "back-a" "")')
    cid = re.search(r"[0-9a-f-]{36}", r["value"]).group(0)
    must(
        "1" in str(ev(f'(count (flash/due "{deck}" 10))').get("value")),
        "one card due",
    )
    code, html = get("/flashcards")
    must(code == 200 and "SmokeDeck" in html, "GET /flashcards")
    code, html = get(f"/flashcards/deck?id={deck}")
    must("front-q" in html, "deck page")
    code, html = get(f"/flashcards/study?deck={deck}")
    must("Reveal" in html, "study front")
    code, html = get(f"/flashcards/study?deck={deck}&id={cid}&reveal=true")
    must("Good" in html and "back-a" in html, "study back")
    st, _, html = post(
        "/flashcards/study",
        {"action": "review", "id": cid, "rating": "good", "deck": deck},
    )
    must(st == 200 and "No cards due" in html, "after good review")
    must(
        "1" in str(ev(f'(dict-get (flash/card "{cid}") "interval")').get("value")),
        "interval=1",
    )
    must(
        "0" in str(ev(f'(count (flash/due "{deck}" 10))').get("value")),
        "none due",
    )
    must(
        "2026-07-13" in str(ev('(date-add-days "2026-07-12" 1)').get("value"))
        or "ok" in str(ev('(date-add-days 1)').get("ok")),
        "date-add-days prim",
    )
    print("ALL PASS")


if __name__ == "__main__":
    main()
