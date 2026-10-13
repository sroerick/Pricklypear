#!/usr/bin/env python3
"""load-book: seed a `borge book export` JSON artifact into the one tree.

    scripts/load-book.sh <export.book.json> [--url URL] [--dry-run]
                         [--out seed.pp] [--prune] [--canary]

Implements borge/habitat-book.borg `habitat-loader` (P2.1):

  - chapters as TYPED tree rows (type_name `book-chapter`) under the
    shared book root `data.book` — hier kinds (folder|note|heading)
    with body jsonb; the tree's rank ordering carries the export's
    inline chapter order (one-tree sibling order).
  - shape: one row per chapter (kind=note), one per section/subsection
    (kind=heading, nested via the literal `h` label like notes), one
    per example (kind=note, x<N> label), one per code chapter
    (kind=note under data.book.code, ordered by export order).
  - ADDITIVE SEED DISCIPLINE (branching.seed-policy): the loader only
    ever writes paths under `data.book` (plus the optional canary it
    created itself), and only rows typed book/book-chapter. It never
    touches any other path — hand edits elsewhere cannot be clobbered.
    Book rows are seed, not live authority; git stays authoritative
    for .borg (habitat-book decision 2). In v1 nothing can edit book
    rows (chapter editing is out-of-scope-v1), so a re-load refreshes
    them in place; when v2 editing lands the loader must skip rows
    carrying a manual-edit marker.

  - transport: POST /api/eval (the image is the API; same env contract
    as pp-eval: PP_URL, PP_USER, PP_PASS / PP_PASS_FILE). Writes go
    through the kernel prims (tree/upsert, tree/reorder, deftype) —
    never raw SQL — so validation + mirrors run exactly as product
    code. deftype lands on system/main (checkout + restore); tree rows
    are branch-free (data.* namespace, branching.data).

Tree shape written:

    data.book                    folder type=book      book root (rev = export digest)
    data.book.ch                 folder type=book      chapter root
    data.book.ch.<slug>          note   type=book-chapter   {node:chapter, chapter,title,status,prose,verify?,notes?,untracked?,raw?,other?}
    data.book.ch.<slug>.h.<sec>  heading ...              {node:section, level,title,prose,...}  (subsections nest h.*.h.*)
    data.book.ch.<slug>.x<N>     note   type=book-chapter   {node:example, lang,src,expected,order}
    data.book.code               folder type=book      code appendix root
    data.book.code.<path>        note   type=book-chapter   {node:code, path,lang,source}

Exit 0 = loaded AND verified (every planned row read back byte-equal,
counts match); exit 1 = mismatch (orphans reported; --prune deletes
them first). --dry-run prints the plan + generated seed without POSTing.
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import sys
import urllib.error
import urllib.request

BOOK_ROOT = "data.book"
CHAPTERS_ROOT = BOOK_ROOT + ".ch"
CODE_ROOT = BOOK_ROOT + ".code"


# ---------------------------------------------------------------- text utils


def nop_quote(s: str) -> str:
    """Escape a Python str as a Nopales string literal.

    Port of lisp/lib/reader.ml escape_string: quote/backslash/named
    controls + \\ddd for other control bytes; UTF-8 rides raw.
    """
    out = []
    for ch in s:
        o = ord(ch)
        if ch == '"':
            out.append('\\"')
        elif ch == "\\":
            out.append("\\\\")
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\t":
            out.append("\\t")
        elif ch == "\r":
            out.append("\\r")
        elif o < 0x20:
            out.append("\\%03d" % o)
        else:
            out.append(ch)
    return '"' + "".join(out) + '"'


def slug_label(s: str, fallback: str = "x") -> str:
    """Slugify into an ltree-safe label ([a-z0-9_], no leading digit).

    Originals always ride in the row body (chapter/path/title); labels
    only need to be stable + valid.
    """
    s = s.strip().lower()
    buf = []
    for ch in s:
        buf.append(ch if ("a" <= ch <= "z" or "0" <= ch <= "9") else "_")
    out = "".join(buf)
    while "__" in out:
        out = out.replace("__", "_")
    out = out.strip("_")
    if not out:
        out = fallback
    if out[0].isdigit():
        out = "n" + out
    return out[:64]


_B36 = "0123456789abcdefghijklmnopqrstuvwxyz"


def b36(n: int) -> str:
    if n == 0:
        return "0"
    s = ""
    while n:
        s = _B36[n % 36] + s
        n //= 36
    return s


def ranks_for(n: int) -> list[str]:
    """Deterministic sibling ranks, same scheme as Tree_store.rank_rebalance:
    base-36 (i+1) zero-filled to a common width (byte order == numeric)."""
    w = max(1, len(b36(max(n, 1))))
    return [b36(i + 1).rjust(w, "0") for i in range(n)]


def jdump(obj) -> str:
    """Compact, deterministic body JSON (insertion order)."""
    return json.dumps(obj, ensure_ascii=False, separators=(",", ":"))


# ---------------------------------------------------------------- plan build


class Op:
    __slots__ = ("path", "kind", "type_name", "body", "rank")

    def __init__(self, path, kind, type_name, body, rank=None):
        self.path = path
        self.kind = kind
        self.type_name = type_name
        self.body = body
        self.rank = rank

    def form(self) -> str:
        up = '(tree/upsert %s %s %s %s)' % (
            nop_quote(self.path),
            nop_quote(self.kind),
            nop_quote(self.type_name),
            nop_quote(jdump(self.body)),
        )
        # tree/upsert reports failures as STRINGS (it never raises), so a
        # failed upsert inside a load-string batch would count as a loaded
        # form and only surface later as an opaque verify mismatch. Wrap
        # every upsert: success returns the path, anything else raises
        # (error ...) — the load-string prim turns that into
        # "load-string: form N failed: book-load: …" and the client
        # fails loudly with the real reason.
        chk = (
            '((lambda (r) (if (string-eq r %s) r '
            '(error (string-append "book-load: upsert failed: " r)))) %s)'
            % (nop_quote(self.path), up)
        )
        if self.rank is None:
            return chk
        return "(seq %s (tree/reorder %s %s))" % (
            chk,
            nop_quote(self.path),
            nop_quote(self.rank),
        )


def collect_scaffolding(children) -> tuple[list, list, list, str | None, list, str | None]:
    """Fold a node list's non-heading children into row fields.

    Returns (prose_chunks, verify_list, notes_list, status, untracked, raw).
    Unknown kinds land in `other` at the caller level (returned via prose
    marker never — they are surfaced by the caller separately).
    """
    prose: list[str] = []
    verify: list[str] = []
    notes: list[dict] = []
    status: str | None = None
    untracked: list[dict] = []
    raw: str | None = None
    for n in children:
        k = n.get("kind", "")
        if k == "doc":
            prose.append(n.get("body", ""))
        elif k == "status":
            if status is None:
                status = n.get("body", "")
        elif k == "verify":
            verify.append(n.get("body", ""))
        elif k == "agent-note":
            notes.append({"by": n.get("title", ""), "body": n.get("body", "")})
        elif k == "untracked":
            untracked.append(
                {"path": n.get("title", ""), "reason": n.get("body", "")}
            )
        elif k == "raw":
            raw = n.get("body", "")
        # inline: print-only navigation — skipped (habitat TOC is the tree)
    return prose, verify, notes, status, untracked, raw


def walk_sections(children, parent_path, depth, plan, used_labels):
    """Map section/subsection nodes to nested heading rows (h.<label>)."""
    heads = [n for n in children if n.get("kind") in ("section", "subsection")]
    ranks = ranks_for(len(heads))
    for i, n in enumerate(heads):
        base = slug_label(n.get("title", ""), "untitled")
        label = base
        j = 2
        while label in used_labels:
            label = "%s-%d" % (base, j)
            j += 1
        used_labels.add(label)
        path = parent_path + ".h." + label
        prose, verify, notes, status, untracked, raw = collect_scaffolding(
            n.get("children", [])
        )
        body = {
            "node": "section",
            "level": 2 + depth,
            "title": n.get("title", ""),
            "prose": "\n\n".join(prose),
        }
        if status is not None:
            body["status"] = status
        if verify:
            body["verify"] = verify
        if notes:
            body["notes"] = notes
        if untracked:
            body["untracked"] = untracked
        if raw is not None:
            body["raw"] = raw
        other = [
            {"kind": c.get("kind", ""), "title": c.get("title", ""),
             "body": c.get("body", "")}
            for c in n.get("children", [])
            if c.get("kind") not in
            ("doc", "status", "verify", "agent-note", "untracked",
             "raw", "section", "subsection", "inline")
        ]
        if other:
            body["other"] = other
        plan.append(Op(path, "heading", "book-chapter", body, ranks[i]))
        walk_sections(
            n.get("children", []), path, depth + 1, plan, used_labels
        )


def build_plan(export: dict, rev: str) -> list[Op]:
    plan: list[Op] = []
    plan.append(
        Op(
            BOOK_ROOT,
            "folder",
            "book",
            {
                "node": "book-root",
                "title": "The Pricklypear Book",
                "root": export.get("root", ""),
                "mode": export.get("mode", ""),
                "rev": rev,
            },
        )
    )
    plan.append(Op(CHAPTERS_ROOT, "folder", "book", {"node": "book-chapters"}))
    plan.append(Op(CODE_ROOT, "folder", "book", {"node": "book-code"}))

    # chapters, in export (inline) order; rank carries the order
    chapters = export.get("chapters", [])
    crank = ranks_for(len(chapters))
    used_ch = set()
    for i, ch in enumerate(chapters):
        slug = ch.get("slug", "")
        base = slug_label(slug, "chapter")
        label = base
        j = 2
        while label in used_ch:
            label = "%s-%d" % (base, j)
            j += 1
        used_ch.add(label)
        cpath = CHAPTERS_ROOT + "." + label
        nodes = ch.get("nodes", [])
        project = nodes[0] if nodes and nodes[0].get("kind") == "project" else None
        # Raw-fallback chapters (unparseable source) carry ONE raw node and
        # no project wrapper — collect over the whole node list so the true
        # source still lands in the row body.
        kids = project.get("children", []) if project else nodes
        prose, verify, notes, status, untracked, raw = collect_scaffolding(kids)
        body = {
            "node": "chapter",
            "chapter": slug,
            "title": ch.get("title", "") or slug,
            "status": status if status is not None else ch.get("status", ""),
            "prose": "\n\n".join(prose),
        }
        if verify:
            body["verify"] = verify
        if notes:
            body["notes"] = notes
        if untracked:
            body["untracked"] = untracked
        if raw is not None:
            body["raw"] = raw
        plan.append(Op(cpath, "note", "book-chapter", body, crank[i]))

        # sections + examples share one sibling-rank sequence under the chapter
        sec_nodes = [n for n in kids if n.get("kind") in ("section", "subsection")]
        examples = ch.get("examples", [])
        erank = ranks_for(len(sec_nodes) + len(examples))
        used_sec = set()
        for si, n in enumerate(sec_nodes):
            sbase = slug_label(n.get("title", ""), "untitled")
            slabel = sbase
            jj = 2
            while slabel in used_sec:
                slabel = "%s-%d" % (sbase, jj)
                jj += 1
            used_sec.add(slabel)
            spath = cpath + ".h." + slabel
            sprose, sverify, snotes, sstatus, suntracked, sraw = collect_scaffolding(
                n.get("children", [])
            )
            sbody = {
                "node": "section",
                "level": 2,
                "title": n.get("title", ""),
                "prose": "\n\n".join(sprose),
            }
            if sstatus is not None:
                sbody["status"] = sstatus
            if sverify:
                sbody["verify"] = sverify
            if snotes:
                sbody["notes"] = snotes
            if suntracked:
                sbody["untracked"] = suntracked
            if sraw is not None:
                sbody["raw"] = sraw
            plan.append(Op(spath, "heading", "book-chapter", sbody, erank[si]))
            # subsections nest h.*.h.* with their own rank sequence
            sub_plan: list[Op] = []
            walk_sections(
                n.get("children", []), spath, 1, sub_plan, set()
            )
            plan.extend(sub_plan)
        for ei, ex in enumerate(examples):
            xpath = "%s.x%03d" % (cpath, ei)
            ebody = {
                "node": "example",
                "lang": ex.get("lang", ""),
                "src": ex.get("src", ""),
                "expected": ex.get("expected"),
                "order": ei,
            }
            plan.append(
                Op(xpath, "note", "book-chapter", ebody, erank[len(sec_nodes) + ei])
            )

    # code chapters, export order (lexicographic appendix)
    code = export.get("code-chapters", [])
    krank = ranks_for(len(code))
    used_code = set()
    for k, c in enumerate(code):
        p = c.get("path", "")
        base = slug_label(p, "file")
        label = base
        j = 2
        while label in used_code:
            label = "%s-%d" % (base, j)
            j += 1
        used_code.add(label)
        plan.append(
            Op(
                CODE_ROOT + "." + label,
                "note",
                "book-chapter",
                {"node": "code", "path": p,
                 "lang": c.get("lang", ""), "source": c.get("source", "")},
                krank[k],
            )
        )
    return plan


# ---------------------------------------------------------------- nopal host


class EvalError(Exception):
    pass


class Habitat:
    def __init__(self, url, user, password, timeout):
        self.url = url.rstrip("/")
        self.user = user
        self.password = password
        self.timeout = timeout
        self.calls = 0

    def eval(self, expr: str) -> object:
        body = json.dumps({"expr": expr}).encode("utf-8")
        req = urllib.request.Request(
            self.url + "/api/eval",
            data=body,
            method="POST",
            headers={"Content-Type": "application/json"},
        )
        tok = base64.b64encode(
            ("%s:%s" % (self.user, self.password)).encode("utf-8")
        ).decode("ascii")
        req.add_header("Authorization", "Basic " + tok)
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as r:
                payload = json.loads(r.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            detail = e.read().decode("utf-8", "replace")[:300]
            raise EvalError("HTTP %s from %s: %s" % (e.code, self.url, detail))
        except urllib.error.URLError as e:
            raise EvalError("cannot reach %s: %s" % (self.url, e.reason))
        self.calls += 1
        if not payload.get("ok"):
            raise EvalError("eval error: %s" % payload.get("error"))
        return payload.get("value")

    def load_string(self, forms: list[str]) -> str:
        """Run a batch of forms via load-string (fail-fast per form)."""
        src = "\n".join(forms)
        v = self.eval("(load-string %s)" % nop_quote(src))
        s = v if isinstance(v, str) else str(v)
        if s.startswith("load-string:"):
            raise EvalError(s)
        return s


def batch_forms(forms: list[str], max_bytes=300_000, max_forms=80):
    """Group forms into load-string batches (request-size bounded).

    max_bytes bounds the forms' PLAIN size; the wire cost is larger
    (nop_quote escaping + the /api/eval JSON envelope roughly double
    escape-dense bodies) and the image caps request bodies at
    PP_MAX_BODY_BYTES (default 1 MiB) — 300K keeps ~3x headroom."""
    batches, cur, size = [], [], 0
    for f in forms:
        n = len(f.encode("utf-8"))
        if cur and (size + n > max_bytes or len(cur) >= max_forms):
            batches.append(cur)
            cur, size = [], 0
        cur.append(f)
        size += n
    if cur:
        batches.append(cur)
    return batches


# ---------------------------------------------------------------- verify


def verify(hab: Habitat, plan: list[Op]) -> tuple[list[str], list[str]]:
    """Read the whole book subtree back and compare to the plan."""
    value = hab.eval('(tree/subtree "%s" 200000)' % BOOK_ROOT)
    rows = value if isinstance(value, list) else []
    actual = {}
    for r in rows:
        if isinstance(r, dict) and "path" in r:
            actual[r["path"]] = r
    problems: list[str] = []
    planned = {op.path: op for op in plan}
    for op in plan:
        a = actual.get(op.path)
        if a is None:
            problems.append("missing row: %s" % op.path)
            continue
        if a.get("kind") != op.kind:
            problems.append(
                "kind mismatch %s: %s != %s" % (op.path, a.get("kind"), op.kind)
            )
        if a.get("type") != op.type_name:
            problems.append(
                "type mismatch %s: %s != %s"
                % (op.path, a.get("type"), op.type_name)
            )
        ar = a.get("rank")
        if (op.rank or None) != (ar or None):
            problems.append(
                "rank mismatch %s: %r != %r" % (op.path, ar, op.rank)
            )
        if a.get("body") != op.body:
            problems.append("body mismatch %s" % op.path)
    # orphans: rows under data.book that THIS export does not own. The
    # plan is parent-complete and v1 has no book editing, so the plan's
    # path set is exactly what should exist — orphan = exact difference
    # (a descendant-of-planned-folder exception would be vacuous: every
    # chapter lives under the planned data.book.ch folder).
    orphans = sorted(p for p in actual if p not in planned)
    return problems, orphans


# ---------------------------------------------------------------- main


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description="Seed a borge book export into the one tree (habitat-loader)."
    )
    ap.add_argument("export", help="path to <stem>.book.json (borge book export)")
    ap.add_argument("--url", default=os.environ.get("PP_URL", "http://127.0.0.1:18080"))
    ap.add_argument("--user", default=os.environ.get("PP_USER", "admin"))
    ap.add_argument(
        "--password", default=os.environ.get("PP_PASS", "")
    )
    ap.add_argument("--password-file", default=os.environ.get("PP_PASS_FILE", ""))
    ap.add_argument("--timeout", type=float, default=float(os.environ.get("PP_EVAL_TIMEOUT", "180")))
    ap.add_argument("--dry-run", action="store_true", help="plan + seed text only, no POSTs")
    ap.add_argument("--out", default="", help="write the generated Nopales seed to PATH")
    ap.add_argument("--prune", action="store_true", help="delete orphan rows under data.book")
    ap.add_argument("--canary", action="store_true",
                    help="prove non-clobbering: write a marker row OUTSIDE data.book "
                         "before the load and require it byte-identical after")
    args = ap.parse_args(argv)

    raw = open(args.export, "rb").read()
    export = json.loads(raw.decode("utf-8"))
    for key in ("chapters", "code-chapters"):
        if key not in export:
            print("load-book: not a book export (missing %s)" % key, file=sys.stderr)
            return 2
    rev = hashlib.sha256(raw).hexdigest()[:16]

    plan = build_plan(export, rev)
    nch = len(export.get("chapters", []))
    ncode = len(export.get("code-chapters", []))
    nsec = sum(1 for o in plan if o.body.get("node") == "section")
    nex = sum(1 for o in plan if o.body.get("node") == "example")
    print(
        "load-book: plan %d rows (root 3 + %d chapters + %d sections + %d examples + %d code) rev=%s"
        % (len(plan), nch, nsec, nex, ncode, rev)
    )

    if args.out:
        seed = "\n".join(op.form() for op in plan)
        with open(args.out, "w", encoding="utf-8") as f:
            f.write(seed)
        print("load-book: seed text -> %s" % args.out)
    if args.dry_run:
        for op in plan[:12]:
            print("  %-52s %s" % (op.path, op.kind))
        if len(plan) > 12:
            print("  … (%d more)" % (len(plan) - 12))
        print("load-book: dry-run, no POSTs")
        return 0

    password = args.password
    if not password and args.password_file:
        password = open(args.password_file).read().strip()
    if not password:
        print(
            "load-book: set PP_PASS / PP_PASS_FILE (or --password)",
            file=sys.stderr,
        )
        return 2

    hab = Habitat(args.url, args.user, password, args.timeout)
    canary_path = "%s.u.%s.%s" % (
        "data.note",
        slug_label(args.user, "user"),
        slug_label("book-loader-canary", "canary"),
    )
    canary_body = {
        "node": "book-loader-canary",
        "title": "book-loader canary (safe to delete)",
        "body": "written by scripts/load-book.py --canary to prove the "
        "loader never touches paths outside %s" % BOOK_ROOT,
    }

    try:
        # 0. branch bookkeeping: deftype must land on system/main (seed
        # target); tree rows are branch-free. Restore the original branch
        # no matter what — an admin left on main is a footgun.
        orig_branch = hab.eval("(current-branch)")
        if not isinstance(orig_branch, str):
            orig_branch = "system/main"

        if args.canary:
            hab.eval(
                "(tree/upsert %s %s %s %s)"
                % (
                    nop_quote(canary_path),
                    nop_quote("note"),
                    nop_quote("book-loader-canary"),
                    nop_quote(jdump(canary_body)),
                )
            )

        # 1. prep: checkout main + register the types (additive seed)
        types = [
            "(deftype 'book-chapter 'node 'chapter 'title 'status 'prose "
            "'level 'lang 'src 'expected 'order 'path 'source 'verify "
            "'notes 'untracked 'raw 'other)",
            "(deftype 'book 'node 'title 'root 'mode 'rev)",
        ]
        hab.eval(
            '(seq (checkout "system/main") (load-string %s))'
            % nop_quote("\n".join(types))
        )

        try:
            # 2. upsert batches
            batches = batch_forms([op.form() for op in plan])
            for bi, b in enumerate(batches):
                msg = hab.load_string(b)
                print("load-book: batch %d/%d — %s" % (bi + 1, len(batches), msg))

            # 3. verify
            problems, orphans = verify(hab, plan)
            if orphans:
                print(
                    "load-book: %d orphan row(s) under %s (not in this export)"
                    % (len(orphans), BOOK_ROOT)
                )
                for p in orphans[:20]:
                    print("  orphan: %s" % p)
                if len(orphans) > 20:
                    print("  … (%d more)" % (len(orphans) - 20))
                if args.prune:
                    tops = []
                    for p in orphans:
                        if not any(t + "." in p + "." and p.startswith(t + ".") for t in tops):
                            tops = [t for t in tops if not t.startswith(p + ".")]
                            tops.append(p)
                    for p in tops:
                        hab.eval("(tree/delete %s #t)" % nop_quote(p))
                        print("load-book: pruned %s" % p)
                    problems2, orphans2 = verify(hab, plan)
                    problems.extend(problems2)
                    orphans = orphans2
            if args.canary:
                c = hab.eval("(tree/get %s)" % nop_quote(canary_path))
                if not (isinstance(c, dict) and c.get("body") == canary_body):
                    problems.append("CANARY CLOBBERED at %s" % canary_path)
                else:
                    print("load-book: canary intact (non-clobber proven)")
                # Remove the canary on EVERY path (even with verify
                # problems) — a failed load must not leave litter behind.
                hab.eval("(tree/delete %s #t)" % nop_quote(canary_path))
        finally:
            try:
                hab.eval('(checkout %s)' % nop_quote(orig_branch))
            except EvalError:
                pass  # never mask the real failure with a restore error

        if problems:
            print(
                "load-book: VERIFY FAILED (%d problem(s)) — branch restored to %s"
                % (len(problems), orig_branch)
            )
            for p in problems[:20]:
                print("  ! %s" % p)
            if len(problems) > 20:
                print("  … (%d more)" % (len(problems) - 20))
            return 1
        print(
            "load-book: OK — %d rows verified under %s (%d /api/eval calls)"
            % (len(plan), BOOK_ROOT, hab.calls)
        )
        return 0
    except EvalError as e:
        print("load-book: FAILED — %s" % e, file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
