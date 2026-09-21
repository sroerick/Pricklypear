#!/usr/bin/env python3
"""Inventory notes UI controls: HTTP op matrix + DOM presence.

Writes docs/notes-ui-inventory.md (and /tmp/pp-notes-ui/inventory.json).

  export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
  python3 scripts/inventory_notes_ui.py
"""
from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import urllib.parse
import urllib.request
import http.cookiejar

SLUG = "ui-fixture"
OUT_DIR = os.environ.get("NOTES_UI_OUT", "/tmp/pp-notes-ui")
INV_MD = "docs/notes-ui-inventory.md"


def ev(expr: str) -> dict:
    cmd = os.environ.get("PP_EVAL", "python3 scripts/pp_eval.py").split() + ["-j", expr]
    p = subprocess.run(cmd, capture_output=True, text=True)
    data = json.loads(p.stdout or "{}")
    return data


def main() -> None:
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    os.chdir(root)
    base = os.environ.get("PP_URL", "http://127.0.0.1:18080").rstrip("/")
    user = os.environ.get("PP_USER", "admin")
    password = os.environ.get("PP_PASS", "admin")
    os.makedirs(OUT_DIR, exist_ok=True)

    # prep
    for e in [
        '(checkout "system/main")',
        '(load-library "outline")',
        '(load-library "notes")',
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" (outline/node "root" "UI Fixture") "" "note" "")',
    ]:
        r = ev(e)
        if not r.get("ok") and "checkout" not in e:
            print("prep fail", e, r, file=sys.stderr)
            sys.exit(1)

    cj = http.cookiejar.CookieJar()
    opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(cj))
    login_data = urllib.parse.urlencode(
        {"username": user, "password": password}
    ).encode()
    req = urllib.request.Request(base + "/login", data=login_data, method="POST")
    opener.open(req, timeout=30)

    def get(path: str) -> tuple[int, str]:
        r = opener.open(base + path, timeout=30)
        return r.status, r.read().decode("utf-8", "replace")

    def post_op(op: str, nid: str) -> tuple[int, str]:
        q = urllib.parse.urlencode(
            {"slug": SLUG, "op": op, "node-id": nid}
        )
        r = opener.open(
            urllib.request.Request(
                base + "/partials/notes-node-op?" + q,
                data=b"",
                method="POST",
            ),
            timeout=30,
        )
        return r.status, r.read().decode("utf-8", "replace")

    rows = []

    # DOM presence on show page
    status, html = get(f"/notes?slug={SLUG}")
    checks = {
        "page 200": status == 200,
        "#outline": 'id="outline"' in html or "id='outline'" in html,
        "root data-id": 'data-id="root"' in html,
        ".ol-toolbar": "ol-toolbar" in html,
        ".ol-actions": "ol-actions" in html,
        "contenteditable title": "contenteditable" in html,
        "htmx script": "htmx" in html,
        "selection script ol-selected": "ol-selected-id" in html,
    }
    for name, ok in checks.items():
        rows.append(
            {
                "control": name,
                "expected": "present",
                "actual": "ok" if ok else "MISSING",
                "layer": "DOM",
                "ok": ok,
            }
        )

    # Reset fixture
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(outline/node "root" "UI Fixture") "" "note" "")'
    )

    ops = [
        ("add-child", "root"),
        ("add-sibling", None),  # filled after kid
        ("cycle-todo", None),
        ("move-down", None),
        ("move-up", None),
        ("archive", None),
        ("unarchive", None),
        ("delete", None),
    ]

    kid = None
    for op, nid in ops:
        if nid is None:
            if not kid:
                rows.append(
                    {
                        "control": f"op:{op}",
                        "expected": "200 + outline",
                        "actual": "SKIP no kid",
                        "layer": "HTTP partial",
                        "ok": False,
                    }
                )
                continue
            nid = kid
        st, body = post_op(op, nid)
        has_outline = 'id="outline"' in body
        ok = st == 200 and has_outline and "returned no value" not in body
        actual = f"HTTP {st}"
        if not has_outline:
            actual += " no #outline"
        if "returned no value" in body:
            actual += " no-value"
        if "err" in body[:200] and st != 200:
            actual += " err"
        rows.append(
            {
                "control": f"op:{op} node={nid}",
                "expected": "200 + #outline HTML",
                "actual": actual if not ok else "ok",
                "layer": "HTTP partial",
                "ok": ok,
            }
        )
        if op == "add-child" and ok:
            r = ev(
                f'(outline/id (car (outline/kids '
                f'(notes/doc/outline (notes/doc/get "{SLUG}")))))'
            )
            if r.get("ok"):
                kid = r["value"].strip().strip('"')

    # Toolbar buttons in DOM (desktop) — present but JS-wired
    for label in ["+kid", "+sib", "up", "dn", "todo", "arch", "del"]:
        present = label in html or label.replace("+", "") in html
        rows.append(
            {
                "control": f"toolbar label {label}",
                "expected": "in page HTML",
                "actual": "present" if present else "not found in initial show",
                "layer": "DOM/JS",
                "ok": present,
                "note": "Desktop toolbar uses JS selId+htmx.ajax; not fully exercised here",
            }
        )

    inv_path = os.path.join(OUT_DIR, "inventory.json")
    with open(inv_path, "w") as f:
        json.dump(rows, f, indent=2)

    md_path = os.path.join(root, INV_MD)
    lines = [
        "# Notes UI inventory",
        "",
        f"Generated by `scripts/inventory_notes_ui.py` against `{base}`.",
        "",
        "| Control | Layer | Expected | Actual | OK |",
        "|---------|-------|----------|--------|----|",
    ]
    for r in rows:
        lines.append(
            f"| {r['control']} | {r['layer']} | {r['expected']} | {r['actual']} | "
            f"{'yes' if r['ok'] else '**NO**'} |"
        )
    lines.extend(
        [
            "",
            "## Notes for agents",
            "",
            "- **NO** rows are the repair backlog.",
            "- HTTP partial ops must return full `#outline` (outerHTML swap).",
            "- Desktop toolbar is JS-selected; mobile ··· embeds node-id in hx-post.",
            "- Reload outline+notes on system/main before fixing so personal pins do not hide git.",
            "",
            f"JSON: `{inv_path}`",
            "",
        ]
    )
    with open(md_path, "w") as f:
        f.write("\n".join(lines))

    failed = sum(1 for r in rows if not r["ok"])
    print(f"Wrote {md_path} ({len(rows)} rows, {failed} failing)")
    print(f"JSON {inv_path}")
    # non-zero if critical HTTP ops fail
    critical = [r for r in rows if r["layer"] == "HTTP partial" and not r["ok"]]
    if critical:
        print("CRITICAL failures:", file=sys.stderr)
        for r in critical:
            print(f"  {r['control']}: {r['actual']}", file=sys.stderr)
        sys.exit(1)
    print("INVENTORY OK")


if __name__ == "__main__":
    main()
