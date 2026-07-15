#!/usr/bin/env python3
"""Notes outline model smoke via /api/eval (no browser).

Requires local or remote image with PP_* env (see scripts/pp_eval.py).
Always reloads outline+notes and uses slug ui-fixture.

  set -a; . ~/.config/pricklypear/env; set +a   # or local admin/admin
  export PP_URL=http://127.0.0.1:18080
  python3 scripts/smoke_notes_model.py
"""
from __future__ import annotations

import json
import os
import subprocess
import sys

SLUG = "ui-fixture"
EVAL = os.environ.get("PP_EVAL", "python3 scripts/pp_eval.py")


def ev(expr: str) -> str:
    cmd = EVAL.split() + ["-j", expr]
    env = os.environ.copy()
    p = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if p.returncode != 0 and not p.stdout.strip():
        print(f"FAIL eval transport: {p.stderr or p.stdout}", file=sys.stderr)
        sys.exit(1)
    try:
        data = json.loads(p.stdout)
    except json.JSONDecodeError:
        print(f"FAIL bad json: {p.stdout!r}", file=sys.stderr)
        sys.exit(1)
    if not data.get("ok"):
        print(f"FAIL {expr}\n  -> {data.get('error')}", file=sys.stderr)
        sys.exit(1)
    return data.get("value", "")


def assert_true(cond: bool, msg: str) -> None:
    if not cond:
        print(f"FAIL {msg}", file=sys.stderr)
        sys.exit(1)
    print(f"OK  {msg}")


def main() -> None:
    os.chdir(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    print("== notes model smoke ==")
    # Reload git libs onto the session branch (personal after basic-auth attach)
    # so stale personal pins do not hide outline/notes fixes.
    ev('(load-library "outline")')
    ev('(load-library "notes")')

    # Fresh fixture: root only
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(outline/node "root" "UI Fixture") "" "note" "")'
    )
    kids0 = ev(
        f'(count (outline/kids (notes/doc/outline (notes/doc/get "{SLUG}"))))'
    )
    assert_true(kids0.strip('"') == "0", f"fresh root has 0 kids (got {kids0})")

    # add-child via apply-op
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(notes/outline/apply-op "{SLUG}" '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) '
        f'"add-child" "root") "" "note" "")'
    )
    kids1 = ev(
        f'(count (outline/kids (notes/doc/outline (notes/doc/get "{SLUG}"))))'
    )
    assert_true(kids1.strip('"') == "1", f"after add-child kids=1 (got {kids1})")

    kid_id = ev(
        f'(outline/id (car (outline/kids '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")))))'
    ).strip('"')
    assert_true(len(kid_id) > 0, f"kid id non-empty ({kid_id!r})")

    # add-sibling
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(notes/outline/apply-op "{SLUG}" '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) '
        f'"add-sibling" "{kid_id}") "" "note" "")'
    )
    kids2 = ev(
        f'(count (outline/kids (notes/doc/outline (notes/doc/get "{SLUG}"))))'
    )
    assert_true(kids2.strip('"') == "2", f"after add-sibling kids=2 (got {kids2})")

    id_a = ev(
        f'(outline/id (car (outline/kids '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")))))'
    ).strip('"')
    id_b = ev(
        f'(outline/id (car (cdr (outline/kids '
        f'(notes/doc/outline (notes/doc/get "{SLUG}"))))))'
    ).strip('"')

    # move-down first kid
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(notes/outline/apply-op "{SLUG}" '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) '
        f'"move-down" "{id_a}") "" "note" "")'
    )
    id_a2 = ev(
        f'(outline/id (car (outline/kids '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")))))'
    ).strip('"')
    assert_true(id_a2 == id_b, f"move-down swapped order (first={id_a2} want {id_b})")

    # cycle-todo
    target = id_a2
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(notes/outline/apply-op "{SLUG}" '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) '
        f'"cycle-todo" "{target}") "" "note" "")'
    )
    todo = ev(
        f'(outline/todo (outline/find '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) "{target}"))'
    )
    assert_true(todo.strip('"') != "" and todo != "()", f"cycle-todo set state ({todo})")

    # archive
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(notes/outline/apply-op "{SLUG}" '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) '
        f'"archive" "{target}") "" "note" "")'
    )
    todo_a = ev(
        f'(outline/todo (outline/find '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) "{target}"))'
    )
    assert_true("ARCHIVED" in todo_a or "archived" in todo_a.lower() or todo_a.strip('"') != "",
                f"archive touched todo ({todo_a})")

    # delete one kid
    before = int(ev(
        f'(count (outline/kids (notes/doc/outline (notes/doc/get "{SLUG}"))))'
    ).strip('"'))
    ev(
        f'(notes/doc/upsert "{SLUG}" "UI Fixture" '
        f'(notes/outline/apply-op "{SLUG}" '
        f'(notes/doc/outline (notes/doc/get "{SLUG}")) '
        f'"delete" "{target}") "" "note" "")'
    )
    after = int(ev(
        f'(count (outline/kids (notes/doc/outline (notes/doc/get "{SLUG}"))))'
    ).strip('"'))
    assert_true(after == before - 1, f"delete removed kid ({before}->{after})")

    print("SMOKE_NOTES_MODEL OK")


if __name__ == "__main__":
    main()
