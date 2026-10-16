#!/usr/bin/env python3
"""Poll the Pricklypear chat lobby and print only NEW messages.

Cursor is the last-seen chat-message id, persisted to a state file so
repeated runs don't reprint. Post-production data order is insertion
order, so we track the set of ids seen at first start and print anything
new. Usage:
    scripts/pp-watch-lobby.py --once
    scripts/pp-watch-lobby.py --interval 15 --duration 300
"""
import argparse
import base64
import json
import os
import sys
import time
import urllib.request

STATE = os.path.expanduser("~/.config/pricklypear/lobby-watch.json")


def client():
    url = os.environ["PP_URL"].rstrip("/") + "/api/eval"
    auth = base64.b64encode(
        f'{os.environ["PP_USER"]}:{os.environ["PP_PASS"]}'.encode()
    ).decode()

    def ev(expr):
        req = urllib.request.Request(
            url,
            data=json.dumps({"expr": expr}).encode(),
            headers={
                "Content-Type": "application/json",
                "Authorization": "Basic " + auth,
            },
        )
        return json.load(urllib.request.urlopen(req, timeout=60))["value"]

    return ev


def lobby(ev):
    v = ev("(chat/lobby)")
    if isinstance(v, str):
        v = json.loads(v)
    rows = list(v.values()) if isinstance(v, dict) else v
    return rows


def rowkey(r):
    return (str(r.get("from")), str(r.get("body")), str(r.get("created_at")))


def load_seen():
    try:
        with open(STATE) as f:
            return set(json.load(f))
    except Exception:
        return None


def save_seen(seen):
    os.makedirs(os.path.dirname(STATE), exist_ok=True)
    with open(STATE, "w") as f:
        json.dump(sorted(seen), f)


def fmt(r):
    from_, body, ts = rowkey(r)
    body = body.replace("\n", " ")[:400]
    return f"[{ts or '?'}] {from_}: {body}"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--once", action="store_true")
    ap.add_argument("--interval", type=float, default=15)
    ap.add_argument("--duration", type=float, default=300)
    args = ap.parse_args()

    ev = client()
    seen = load_seen()
    rows = lobby(ev)
    if seen is None:
        # first run: seed the cursor with everything currently present
        seen = {json.dumps(rowkey(r)) for r in rows}
        save_seen(seen)
        print(f"seeded cursor: {len(seen)} existing rows")
    else:
        for r in rows:
            k = json.dumps(rowkey(r))
            if k not in seen:
                print("NEW", fmt(r), flush=True)
                seen.add(k)
        save_seen(seen)
        print(f"(checked at {time.strftime('%H:%M:%S')}, {len(rows)} total)")

    if args.once:
        return

    deadline = time.time() + args.duration
    last_beat = time.time()
    while time.time() < deadline:
        time.sleep(args.interval)
        try:
            rows = lobby(ev)
        except Exception as e:
            print(f"(poll error: {e})", flush=True)
            continue
        for r in rows:
            k = json.dumps(rowkey(r))
            if k not in seen:
                print("NEW", fmt(r), flush=True)
                seen.add(k)
        save_seen(seen)
        if time.time() - last_beat >= 300:
            last_beat = time.time()
            print(
                f"(alive {time.strftime('%H:%M:%S')}, {len(rows)} total, no news)",
                flush=True,
            )


if __name__ == "__main__":
    main()
