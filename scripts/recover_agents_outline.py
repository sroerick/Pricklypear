#!/usr/bin/env python3
"""Rebuild the prod 'agents' note outline from tree_versions (soil-adjacent
incident recovery, 2026-09-04).

Reads /tmp/agents_recover.json (latest non-tombstone tree_versions row per
path under the note), rebuilds the outline tree from path nesting, verifies
structure, appends the filmstrip-live promote node, and writes the note back
via POST /api/eval with a session cookie (the same transport the shim uses).
"""
import base64
import json
import os
import sys
import time
import urllib.parse
import urllib.request

BASE = os.environ.get("PP_URL", "http://127.0.0.1:18080").rstrip("/")
USER = os.environ.get("PP_USER", "admin")
PASS = os.environ.get("PP_PASS", "admin")
DUMP = sys.argv[1] if len(sys.argv) > 1 else "/tmp/agents_recover.json"
NOTE = "data.note.u.admin.wiki.agents"
ROOT_ID = "root"
ROOT_TITLE = "Agents (living AGENTS.md)"


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *a, **k):
        return None


_opener = urllib.request.build_opener(_NoRedirect)


def http_post(path, form):
    data = urllib.parse.urlencode(form).encode()
    req = urllib.request.Request(BASE + path, data=data, method="POST")
    try:
        r = _opener.open(req, timeout=60)
        cookies = r.headers.get_all("Set-Cookie") or []
        body = r.read().decode()
    except urllib.error.HTTPError as e:
        cookies = e.headers.get_all("Set-Cookie") or []
        body = e.read().decode()
    token = None
    for c in cookies:
        if c.startswith("pp_session="):
            token = c.split(";")[0]
    return token, body


def eval_lisp(cookie, expr):
    body = json.dumps({"expr": expr}).encode()
    req = urllib.request.Request(
        BASE + "/api/eval", data=body, method="POST",
        headers={"Cookie": cookie, "Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=120) as r:
        out = json.loads(r.read().decode())
    if not out.get("ok"):
        raise RuntimeError(f"eval error: {out}")
    v = out["value"]
    return v if not isinstance(v, str) else _maybe_json(v)


def _maybe_json(s):
    try:
        return json.loads(s)
    except Exception:
        return s


def main():
    rows = json.load(open(DUMP))
    assert isinstance(rows, list) and rows, "empty recovery dump"

    root_env = None
    heads = {}  # id-sequence tuple -> node envelope
    for r in rows:
        p = r["path"]
        env = r["body"].get("body")
        if p == NOTE:
            root_env = env
            continue
        segs = p.split(".")
        # after 'agents': pairs of (h, id)
        tail = segs[segs.index("agents") + 1:]
        assert tail[0] == "h", f"unexpected path {p}"
        ids = tuple(tail[1::2])
        heads[ids] = env

    assert root_env, "no root version found"
    root = root_env  # {uuid, slug, title, tags, kind, day, body}
    print(f"root: slug={root.get('slug')} title={root.get('title')}")
    print(f"heading nodes recovered: {len(heads)}")

    # build nested tree from id sequences
    top = []
    index = {}
    for ids in sorted(heads, key=lambda t: (len(t), t)):
        env = heads[ids]
        node = [env.get("id"), env.get("title"), env.get("body"),
                env.get("todo") or "", []]
        index[ids] = node
        if len(ids) == 1:
            top.append(node)
        else:
            parent = index.get(ids[:-1])
            if parent is None:
                print(f"WARN: orphan {ids} — attaching at top")
                top.append(node)
            else:
                parent[4].append(node)

    # the clobbered write already created the filmstrip-live heading;
    # separate it so the original 25 sit in their (alphabetical) order
    # and filmstrip-live goes last.
    promote = next((n for n in top if n[0] == "filmstrip-live"), None)
    top = [n for n in top if n[0] != "filmstrip-live"]

    print(f"top-level kids: {len(top)}")
    for k in top:
        print(f"  {k[0]:24s} {str(k[1])[:58]}  (subkids {len(k[4])})")

    # verification against the pre-incident structure
    ids = [k[0] for k in top]
    expected_first = ["music-pkg", "n-api", "n-boot-epoch",
                      "n-bowling-bumpers", "n-core-2026-07-27", "n-crud"]
    expected_last = ["n-tree-traversal", "n-wiki", "n-workout",
                     "perf-lore-2026-09-02", "pin-hygiene"]
    assert ids[:6] == expected_first, f"head ids drifted: {ids[:6]}"
    assert ids[-5:] == expected_last, f"tail ids drifted: {ids[-5:]}"
    assert len(top) == 25, f"expected 25 top-level kids, got {len(top)}"
    n_crud = next(k for k in top if k[0] == "n-crud")
    assert len(n_crud[4]) == 12, f"n-crud subkids {len(n_crud[4])}"
    tools = next(k for k in top if k[0] == "n-tools")
    assert len(tools[4]) == 11, f"n-tools groups {len(tools[4])}"
    sessions = next(k for k in top if k[0] == "n-sessions-lazy")
    assert "pp_session cookie" in sessions[2], "n-sessions-lazy body drift"
    perf = next(k for k in top if k[0] == "perf-lore-2026-09-02")
    assert "TCP_NODELAY" in perf[2], "perf-lore body drift"
    print("verification: 25 kids, ids + bodies spot-checked OK")

    # append the promote node last
    if promote is None:
        promote = ["filmstrip-live", "Filmstrip shell live (2026-09-04)",
                   "filmstrip + pads promoted to system/main (dev stack "
                   "and prod, 2026-09-04): /filmstrip is the habitat "
                   "shell (private) -- a niri content strip of panes "
                   "over a Miller-column cascade (todo/notes/calendar), "
                   "inline launcher, alt+arrows focus, width presets "
                   "third/half/full. The workspace is a per-user row "
                   "(tree datum filmstrip-ws, id = username); reload "
                   "round-trips it.", "", []]
    top.append(promote)

    outline = [ROOT_ID, root.get("title") or ROOT_TITLE,
               root.get("body") or "", "", top]
    outline_json = json.dumps(outline, ensure_ascii=False)

    # log in once, eval the upsert with the session cookie
    token, body = http_post("/login", {"username": USER, "password": PASS})
    assert token, "login did not mint a cookie"
    cookie = token
    print("logged in (session cookie)")

    # write back: the browser save contract (decoded tree as data)
    esc = outline_json.replace("\\", "\\\\").replace('"', '\\"')
    expr = (f'(notes/doc/upsert "agents" "{ROOT_TITLE}" '
            f'(json-parse "{esc}") "wiki" "schema" "")')
    branch = eval_lisp(cookie, "(current-branch)")
    print(f"branch: {branch}")
    if branch != "admin/personal":
        eval_lisp(cookie, '(checkout "admin/personal")')
    out = eval_lisp(cookie, expr)
    print("upsert:", str(out)[:120])

    # verify
    got = eval_lisp(cookie, '(notes/doc/get "agents")')
    o = got[3] if isinstance(got, list) else json.loads(got[3])
    kids = o[4]
    assert len(kids) == 26, f"post-write kid count {len(kids)}"
    got_ids = {k[0] for k in kids}
    assert got_ids == {k[0] for k in top}, "kid id set drifted"
    # tree sibling order != outline array order; content is the contract
    print(f"verified: 26 kids, id set complete ({sorted(got_ids)[0]} ...)")
    print("\nagents note recovery: complete.")


if __name__ == "__main__":
    main()
