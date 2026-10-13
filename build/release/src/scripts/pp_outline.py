#!/usr/bin/env python3
"""pp_outline — tier-0/1 debug loop for the notes/outline node editor.

Structural (non-visual) verification, no browser. The editor is
server-rendered HTML + htmx, so almost everything is checkable as:
  state in Postgres  ->  POST /partials/notes-node-op  ->  new HTML fragment

Tiers covered here:
  tier 0  model:  run outline/notes ops via /api/eval, read the tree
  tier 1  fragment: POST the real partial, assert on returned HTML + model

Visual (contenteditable blur-save, toolbar layout) stays tier 2 CDP:
  scripts/smoke_notes_cdp.mjs / scripts/smoke_notes_selection_cdp.mjs

Usage (against a running image; local dev.sh or prod):
  set -a; . ~/.config/pricklypear/env; set +a
  export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin

  python3 scripts/pp_outline.py fixture            # seed --slug fixture (root + kidA/kidB)
  python3 scripts/pp_outline.py tree               # dump model tree as indented text
  python3 scripts/pp_outline.py op add-child kidA  # POST op, print resulting tree
  python3 scripts/pp_outline.py check              # tier-1 assertions incl. script-parse
  python3 scripts/pp_outline.py scripts-check      # node --check every emitted <script>

Exit 0 on success, 1 on first failure.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.request

PP_URL = os.environ.get("PP_URL", "http://127.0.0.1:18080").rstrip("/")
PP_USER = os.environ.get("PP_USER", "admin")
PP_PASS = os.environ.get("PP_PASS", "admin")
PP_EVAL = os.environ.get(
    "PP_EVAL", "python3 scripts/pp_eval.py"
)  # or built cli/bin/pp_eval.exe

FIXTURE_TREE_SEXPR = (
    '(outline/node* "root" "Root" "" "" '
    '(list (outline/node* "kidA" "Kid A" "" "" (list))'
    '      (outline/node* "kidB" "Kid B" "" "" (list))))'
)

_cookie_jar: dict[str, str] = {}


# ---------- transport: /api/eval ----------

def ev(expr: str) -> str:
    cmd = PP_EVAL.split() + ["-j", expr]
    env = os.environ.copy()
    p = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if p.returncode != 0 and not p.stdout.strip():
        die(f"eval transport: {p.stderr or p.stdout}")
    try:
        data = json.loads(p.stdout)
    except json.JSONDecodeError:
        die(f"bad json: {p.stdout!r}")
    if not data.get("ok"):
        die(f"eval: {expr}\n  -> {data.get('error')}")
    v = data.get("value", "")
    if isinstance(v, str) and len(v) >= 2 and v.startswith('"') and v.endswith('"'):
        v = json.loads(v)
    elif not isinstance(v, str):
        v = str(v)
    return v


def ev_quiet(expr: str) -> str | tuple[str, str]:
    """Like ev() but never die: return value or ('', error)."""
    cmd = PP_EVAL.split() + ["-j", expr]
    p = subprocess.run(cmd, capture_output=True, text=True, env=os.environ.copy())
    try:
        d = json.loads(p.stdout)
    except json.JSONDecodeError:
        return "", f"bad json {p.stdout!r}"
    if not d.get("ok"):
        return "", str(d.get("error"))
    v = d.get("value", "")
    if isinstance(v, str) and v.startswith('"') and v.endswith('"'):
        v = json.loads(v)
    return v


# ---------- transport: HTTP partials (cookie session) ----------

def login() -> list[str]:
    body = f"username={urllib.request.quote(PP_USER)}&password={urllib.request.quote(PP_PASS)}"
    req = urllib.request.Request(
        f"{PP_URL}/login",
        data=body.encode(),
        headers={"Content-Type": "application/x-www-form-urlencoded"},
        method="POST",
    )
    # don't follow redirects; read Set-Cookie
    class NoRedirect(urllib.request.HTTPRedirectHandler):
        def redirect_request(self, req, fp, code, msg, headers, newurl):
            return None

    opener = urllib.request.build_opener(NoRedirect)
    try:
        resp = opener.open(req, timeout=15)
    except urllib.error.HTTPError as e:
        resp = e  # 303 lands here
    sc = resp.headers.get_all("Set-Cookie") or []
    for c in sc:
        nv = c.split(";", 1)[0]
        if "=" in nv:
            k, v = nv.split("=", 1)
            _cookie_jar[k.strip()] = v.strip()
    if not _cookie_jar:
        die(f"login produced no session cookie (status {resp.status})")
    return [f"{k}={v}" for k, v in _cookie_jar.items()]


def http_get(path: str) -> tuple[int, str]:
    req = urllib.request.Request(
        f"{PP_URL}{path}",
        headers={"Cookie": "; ".join(f"{k}={v}" for k, v in _cookie_jar.items())},
    )
    try:
        with urllib.request.urlopen(req, timeout=15) as resp:
            return resp.status, resp.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")


def http_post_op(slug: str, op: str, nid: str, extra: str = "") -> tuple[int, str]:
    q = f"slug={urllib.request.quote(slug)}&op={urllib.request.quote(op)}&node-id={urllib.request.quote(nid)}{extra}"
    req = urllib.request.Request(
        f"{PP_URL}/partials/notes-node-op?{q}",
        data=b"",
        method="POST",
        headers={"Cookie": "; ".join(f"{k}={v}" for k, v in _cookie_jar.items())},
    )
    try:
        with urllib.request.urlopen(req, timeout=15) as resp:
            return resp.status, resp.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")


# ---------- helpers ----------

def die(msg: str):
    print(f"FAIL {msg}", file=sys.stderr)
    sys.exit(1)


def ok(msg: str):
    print(f"OK   {msg}")


def doc_outline(sexpr_slug: str) -> str:
    return f'(notes/doc/outline (notes/doc/get "{sexpr_slug}"))'


def tree_rows(slug: str) -> list[tuple[int, str, str, str]]:
    """Return (depth, id, title, todo) rows via eval of a printer form."""
    expr = (
        "(begin "
        "(define __walk (lambda (n d) "
        "  (cons (list d (outline/id n) (outline/title n) (outline/todo n)) "
        "        (list/foldl (lambda (a k) (append a (__walk k (+ d 1)))) (list) (outline/kids n))))) "
        f"(__walk {doc_outline(slug)} 0))"
    )
    raw = ev(expr)
    rows = []
    for m in re.finditer(r'\((-?\d+)\s+"([^"]*)"\s+"([^"]*)"\s+"?([^")]*)"?\)', raw):
        rows.append((int(m.group(1)), m.group(2), m.group(3), m.group(4).strip('"')))
    if not rows and raw:
        die(f"tree parse of eval failed: {raw!r}")
    return rows


def print_tree(slug: str):
    rows = tree_rows(slug)
    if not rows:
        die("empty tree")
    for depth, nid, title, todo in rows:
        tag = f"[{todo}]" if todo else ""
        print("  " * depth + f"{nid:12} {title} {tag}".rstrip())


# ---------- fixture / op / tree commands ----------

FIXTURE_HINT = (
    "(outline/node* \"root\" \"Root\" \"\" \"\" "
    "(list (outline/node* \"kidA\" \"Kid A\" \"\" \"\" (list)) "
    "(outline/node* \"kidB\" \"Kid B\" \"\" \"\" (list))))"
)


def cmd_fixture(slug: str):
    ev('(load-library "outline")')
    ev('(load-library "notes")')
    ev(
        f'(notes/doc/upsert "{slug}" "Sel Fixture" {FIXTURE_TREE_SEXPR} "" "note" "")'
    )
    ok(f"seeded fixture slug={slug} (root + kidA + kidB)")
    print_tree(slug)


def cmd_tree(slug: str):
    print_tree(slug)


def cmd_op(slug: str, op: str, nid: str):
    if not _cookie_jar:
        login()
    code, frag = http_post_op(slug, op, nid)
    if code != 200:
        die(f"{op} {nid}: HTTP {code}: {frag[:200]}")
    if 'id="outline"' not in frag:
        die(f"{op} {nid}: response missing #outline: {frag[:200]}")
    ss = re.search(r'id="ol-selected-id"[^>]*value="([^"]+)"', frag)
    sel = ss.group(1) if ss else "(no oob selected-id)"
    ok(f"{op} {nid} -> HTTP 200, oob selected-id={sel}")
    print_tree(slug)


# ---------- check (tier-1 assertions) ----------

def cmd_check(slug: str):
    login()
    code, html = http_get(f"/notes?slug={slug}")
    if code != 200:
        die(f"GET /notes?slug={slug} -> {code} (run `fixture` first?)")
    for want in ['id="outline"', 'data-id="root"', "ol-toolbar", "ol-selected-id"]:
        if want not in html:
            die(f"show page missing {want}")
    ok("show page has outline chrome")

    # server-driven op on the FRAGMENT level: add-sibling under kidB
    code, frag = http_post_op(slug, "add-sibling", "kidB")
    if code != 200 or 'id="outline"' not in frag:
        die(f"add-sibling kidB fragment broken (http {code})")
    ok("add-sibling kidB fragment ok")

    kids_expr = f"(count (outline/kids {doc_outline(slug)}))"
    kids = ev(f"(str {kids_expr})")
    if kids != "3":
        die(f"model kids of root after add-sibling = {kids}, want 3")
    ok("model consistent (root has 3 kids after add-sibling)")

    # every emitted <script> must at least parse under node --check.
    script_parse_check_html(html, "show")

    print(f"CHECK OK for slug={slug}")


def script_parse_check_html(html: str, label: str):
    scripts = re.findall(r"<script[^>]*>(.*?)</script>", html, re.S)
    bad = []
    for i, s in enumerate(scripts):
        if not s.strip():
            continue
        p = f"/tmp/pp_outline_script_{label}_{i}.js"
        open(p, "w").write(s)
        r = subprocess.run(["node", "--check", p], capture_output=True, text=True)
        if r.returncode != 0:
            err = (r.stderr.strip().splitlines() or ["?"])[-1]
            bad.append(f"script[{i}]: {err}")
    if bad:
        for b in bad:
            print(f"FAIL javascript parse: {b}", file=sys.stderr)
        sys.exit(1)
    ok(f"all {len(scripts)} scripts parse (node --check)")


# ---------- scripts-check (all libs, static) ----------

_PP_ESC = {"n": "\n", "t": "\t", "r": "\r", "\\": "\\", '"': '"', "'": "'"}


def _read_str(at_j: str, body: str) -> tuple[str, int]:
    """Read a Nopales string literal starting at body[j]=='"'; return (value, new_j after closing quote)."""
    j = at_j + 1
    buf = []
    while j < len(body) and body[j] != '"':
        if body[j] == "\\" and j + 1 < len(body):
            j += 1
            buf.append(_PP_ESC.get(body[j], body[j]))
            j += 1
        else:
            buf.append(body[j])
            j += 1
    return "".join(buf), j + 1


def pp_js_from_block(body: str) -> str:
    """Reconstruct the JavaScript emitted by a `(html/tag "script" ... ...)` block.

    Preserves literal string text exactly (so escape-eaten regexes and missing
    braces surface) and substitutes any runtime-interpolated form with `0`, a
    valid JS expression. So `{lat:,lon:}` (gap dropped) -> `{lat:0,lon:0}`
    (parses), while `m[1].replace(/\\+/g,' ')` stays literal and still trips
    node --check once the Nopales reader ate the backslash.
    """
    # Prefer the (string-append ...) arg list; fallback to a single string-literal body.
    sa = body.find("(string-append")
    if sa == -1:
        # body like (html/script "script" "" "<js>")
        q = body.find('"')
        if q == -1:
            return ""
        v, _ = _read_str(q, body)
        return v
    after = body[sa + len("(string-append"):]
    out = []
    j = 0
    while j < len(after):
        c = after[j]
        if c == ')':
            break  # end of string-append args
        if c.isspace():
            j += 1
            continue
        if c == '"':
            v, j = _read_str(j, after)
            out.append(v)
            continue
        # Non-string interpolated form/atom: consume one balanced form, emit `0`.
        depth = 0
        while j < len(after):
            ch = after[j]
            if ch in '([{':
                depth += 1
            elif ch in ')]}':
                if depth == 0:
                    break  # end of string-append
                depth -= 1
            elif ch.isspace() and depth == 0:
                break
            j += 1
        out.append('0')
    return "".join(out)


def cmd_scripts_check(repo: str):
    import glob

    files = sorted(glob.glob(os.path.join(repo, "libs/**/lib.pp"), recursive=True))
    n_bad = n_ok = 0
    for f in files:
        try:
            src = open(f).read()
        except OSError:
            continue
        for m in re.finditer(
            r"\(define\s+(\S*script\S*)(.*?)(?=\n\(define |\Z)", src, re.S
        ):
            name, body = m.group(1), m.group(2)
            if "html/tag" not in body:
                continue
            js = pp_js_from_block(body)
            if not js.strip():
                continue
            p = f"/tmp/pp_outline_lib_{re.sub(r'[^A-Za-z0-9]+', '_', f + '/' + name)}.js"
            open(p, "w").write(js)
            r = subprocess.run(["node", "--check", p], capture_output=True, text=True)
            if r.returncode == 0:
                n_ok += 1
                print(f"ok   {f}:{name}")
            else:
                n_bad += 1
                last = (r.stderr.strip().splitlines() or ["?"])[-1]
                print(f"BAD  {f}:{name}  -> {last}", file=sys.stderr)
    print(f"scripts-check: {n_ok} ok, {n_bad} bad")
    if n_bad:
        sys.exit(1)


# ---------- main ----------

def main():
    ap = argparse.ArgumentParser(prog="pp_outline")
    ap.add_argument("--slug", default="ui-fixture", help="note slug (default ui-fixture)")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("fixture")
    sub.add_parser("tree")
    o = sub.add_parser("op"); o.add_argument("op_name"); o.add_argument("nid")
    sub.add_parser("check")
    sub.add_parser("scripts-check")
    args = ap.parse_args()

    if args.cmd == "fixture":
        cmd_fixture(args.slug)
    elif args.cmd == "tree":
        cmd_tree(args.slug)
    elif args.cmd == "op":
        cmd_op(args.slug, args.op_name, args.nid)
    elif args.cmd == "check":
        cmd_check(args.slug)
    elif args.cmd == "scripts-check":
        cmd_scripts_check(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


if __name__ == "__main__":
    main()
