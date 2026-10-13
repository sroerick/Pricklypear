#!/usr/bin/env python3
"""One-shot Obsidian/markdown vault → Pricklypear garden notes (M1).

Uses POST /api/eval (HTTP Basic). Not continuous sync.

  export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
  python3 scripts/import_obsidian_notes.py --vault ~/obsidian_vault --dry-run
  python3 scripts/import_obsidian_notes.py --vault ~/obsidian_vault --apply
  python3 scripts/import_obsidian_notes.py --vault ~/obsidian_vault --apply --limit 20

Import folder prefix: garden/<vault-relative-dirs>
Skips: .obsidian, EXPORT, templates, non-md binaries/tex/pdf/video.
"""
from __future__ import annotations

import argparse
import base64
import json
import os
import re
import sys
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable

SKIP_DIR_NAMES = {".obsidian", "EXPORT", "templates", ".git", "Rorks"}
SKIP_SUFFIXES = {
    ".tex", ".aux", ".log", ".pdf", ".mp4", ".webm", ".mov", ".pyc",
    ".png", ".base", ".jpg", ".jpeg", ".gif", ".svg", ".sqlite", ".tsv",
    ".yaml", ".yml", ".json", ".html", ".patch", ".py", ".txt",
}

FRONT_RE = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.S)
HEADING_RE = re.compile(r"^(#{1,6})\s+(.*)$", re.M)
WIKI_RE = re.compile(r"\[\[([^\]|#]+)(?:#[^\]|]*)?(?:\|([^\]]+))?\]\]")
BIBLE_WIKI_RE = re.compile(r"\[\[bible:([^\]|]+)(?:\|([^\]]+))?\]\]", re.I)


def path_label(s: str) -> str:
    out: list[str] = []
    last_us = False
    for c in s.lower():
        if ("a" <= c <= "z") or ("0" <= c <= "9"):
            out.append(c)
            last_us = False
        elif not last_us:
            out.append("_")
            last_us = True
    lab = "".join(out).strip("_")
    return lab or "x"


def lisp_str(s: str) -> str:
    return json.dumps(s, ensure_ascii=False)


@dataclass
class MdNote:
    rel: str
    slug: str
    title: str
    folder: str
    tags: str
    kind: str
    day: str
    source_id: str
    outline_kids: list
    wiki_targets: list[str] = field(default_factory=list)


def parse_frontmatter(text: str) -> tuple[dict, str]:
    m = FRONT_RE.match(text)
    if not m:
        return {}, text
    meta: dict = {}
    cur_list_key = None
    for line in m.group(1).splitlines():
        if re.match(r"^\s+-\s+", line) and cur_list_key:
            val = re.sub(r"^\s+-\s+", "", line).strip().strip("\"'")
            meta.setdefault(cur_list_key, [])
            if isinstance(meta[cur_list_key], list):
                meta[cur_list_key].append(val)
            continue
        cur_list_key = None
        km = re.match(r"^([A-Za-z0-9_]+):\s*(.*)$", line)
        if not km:
            continue
        k, v = km.group(1), km.group(2).strip()
        if v == "":
            cur_list_key = k
            meta[k] = []
            continue
        if (v.startswith('"') and v.endswith('"')) or (
            v.startswith("'") and v.endswith("'")
        ):
            v = v[1:-1]
        meta[k] = v
    return meta, text[m.end() :]


def bible_wiki_to_sexp(inner: str, label: str | None) -> str:
    s = inner.strip()
    m = re.match(r"^([1-3]?\s*[A-Za-z]+)\s*(\d+)(?::(\d+))?$", s)
    if not m:
        m = re.match(r"^([1-3]?[A-Za-z]+)(\d+)(?::(\d+))?$", s)
    if not m:
        book, ch, vs = re.sub(r"\s+", "", s) or "Gen", "1", ""
    else:
        book = re.sub(r"\s+", "", m.group(1))
        ch = m.group(2)
        vs = m.group(3) or ""
    if label:
        if vs:
            return f"(bible {book} {ch} {vs} {lisp_str(label)})"
        return f"(bible {book} {ch} {lisp_str(label)})"
    if vs:
        return f"(bible {book} {ch} {vs})"
    return f"(bible {book} {ch})"


def normalize_body(text: str) -> tuple[str, list[str]]:
    targets: list[str] = []

    def bible_sub(m: re.Match) -> str:
        return bible_wiki_to_sexp(m.group(1), m.group(2))

    text = BIBLE_WIKI_RE.sub(bible_sub, text)

    def wiki_sub(m: re.Match) -> str:
        raw = m.group(1).strip()
        lab = m.group(2)
        slug = path_label(raw.split("/")[-1].replace(" ", "-"))
        targets.append(slug)
        if lab:
            return f"[[{slug}|{lab}]]"
        return f"[[{slug}]]"

    text = WIKI_RE.sub(wiki_sub, text)
    return text, targets


def build_outline(md_body: str) -> tuple[list, list[str]]:
    text, targets = normalize_body(md_body)
    matches = list(HEADING_RE.finditer(text))
    if not matches:
        if text.strip():
            return [["c1", "Body", text.strip(), []]], targets
        return [], targets

    preamble = text[: matches[0].start()].strip()
    parts: list[tuple[int, str, str]] = []
    for i, m in enumerate(matches):
        level = len(m.group(1))
        title = m.group(2).strip()
        start = m.end()
        end = matches[i + 1].start() if i + 1 < len(matches) else len(text)
        parts.append((level, title, text[start:end].strip()))

    root_kids: list = []
    stack: list[tuple[int, list]] = [(0, root_kids)]
    for idx, (level, title, chunk) in enumerate(parts):
        node = [f"c{idx + 1}", title, chunk, []]
        while stack and stack[-1][0] >= level:
            stack.pop()
        if not stack:
            stack = [(0, root_kids)]
        stack[-1][1].append(node)
        stack.append((level, node[3]))

    if preamble:
        root_kids.insert(0, ["c0", "Notes", preamble, []])
    return root_kids, targets


def outline_to_lisp(kids: list) -> str:
    def node_lisp(n) -> str:
        nid, title, body, ckids = n
        kids_l = " ".join(node_lisp(k) for k in ckids)
        return (
            f"(outline/node* {lisp_str(nid)} {lisp_str(title)} "
            f"{lisp_str(body)} \"\" (list {kids_l}))"
        )

    return f"(list {' '.join(node_lisp(k) for k in kids)})"


def iter_md_files(vault: Path) -> Iterable[Path]:
    for p in sorted(vault.rglob("*.md")):
        rel_parts = p.relative_to(vault).parts
        if any(part in SKIP_DIR_NAMES for part in rel_parts):
            continue
        if p.suffix.lower() in SKIP_SUFFIXES:
            continue
        yield p


def load_note(vault: Path, path: Path) -> MdNote:
    rel = str(path.relative_to(vault))
    text = path.read_text(encoding="utf-8", errors="replace")
    meta, body = parse_frontmatter(text)
    stem = path.stem
    title = str(meta.get("title") or stem)
    org_id = str(meta.get("org_id") or "")
    tags_v = meta.get("tags") or ""
    tags = ",".join(tags_v) if isinstance(tags_v, list) else str(tags_v)
    folder_parts = list(path.relative_to(vault).parts[:-1])
    folder = "/".join(folder_parts)
    kind, day = "note", ""
    if re.match(r"^\d{4}-\d{2}-\d{2}$", stem) and (
        not folder_parts or folder_parts[0].lower() == "diary"
    ):
        kind, day = "daily", stem
    slug = path_label(stem.replace(" ", "-"))
    kids, targets = build_outline(body)
    return MdNote(
        rel=rel,
        slug=slug,
        title=title,
        folder=folder,
        tags=tags,
        kind=kind,
        day=day,
        source_id=org_id,
        outline_kids=kids,
        wiki_targets=targets,
    )


class EvalClient:
    def __init__(self, url: str, user: str, password: str, timeout: float = 180.0):
        self.url = url.rstrip("/") + "/api/eval"
        token = base64.b64encode(f"{user}:{password}".encode()).decode()
        self.headers = {
            "Authorization": f"Basic {token}",
            "Content-Type": "application/json",
        }
        self.timeout = timeout

    def eval(self, expr: str) -> str:
        data = json.dumps({"expr": expr}).encode()
        req = urllib.request.Request(
            self.url, data=data, headers=self.headers, method="POST"
        )
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as resp:
                body = resp.read().decode()
        except urllib.error.HTTPError as e:
            raise RuntimeError(f"HTTP {e.code}: {e.read().decode()[:500]}") from e
        try:
            j = json.loads(body)
        except json.JSONDecodeError:
            return body
        if isinstance(j, dict) and j.get("ok") is False:
            raise RuntimeError(str(j.get("error") or j))
        if isinstance(j, dict) and "value" in j:
            return str(j["value"])
        return body


def ensure_libs(client: EvalClient) -> None:
    for lib in ("std", "outline", "roam", "notes", "bible"):
        client.eval(f'(load-library "{lib}")')


def upsert_note(client: EvalClient, n: MdNote) -> str:
    folder = "garden"
    if n.folder:
        folder = "garden/" + n.folder
    kids = outline_to_lisp(n.outline_kids)
    outline = f'(outline/node* "root" {lisp_str(n.title)} "" "" {kids})'
    tags = n.tags
    if n.source_id:
        tags = (tags + " " if tags else "") + f"org_id:{n.source_id}"
    tags = (tags + " " if tags else "") + f"src:{n.rel}"
    expr = (
        f"(notes/doc/upsert-in {lisp_str(n.slug)} {lisp_str(n.title)} "
        f"{outline} {lisp_str(tags)} {lisp_str(n.kind)} {lisp_str(n.day)} "
        f"{lisp_str(folder)})"
    )
    return client.eval(expr)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--vault", type=Path, required=True)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--apply", action="store_true")
    ap.add_argument("--limit", type=int, default=0)
    args = ap.parse_args()
    vault = args.vault.expanduser().resolve()
    if not vault.is_dir():
        print(f"vault not found: {vault}", file=sys.stderr)
        return 2

    notes: list[MdNote] = []
    collisions: dict[str, list[str]] = {}
    for p in iter_md_files(vault):
        try:
            n = load_note(vault, p)
        except Exception as e:  # noqa: BLE001
            print(f"parse fail {p}: {e}", file=sys.stderr)
            continue
        collisions.setdefault(n.slug, []).append(n.rel)
        notes.append(n)

    for slug, rels in list(collisions.items()):
        if len(rels) <= 1:
            continue
        for n in notes:
            if n.slug != slug:
                continue
            extra = path_label(Path(n.rel).parent.name or "root")
            n.slug = path_label(f"{extra}_{n.slug}")

    bible_links = 0
    wiki_links = 0

    def walk(kids):
        nonlocal bible_links
        for _id, _t, body, ck in kids:
            bible_links += len(re.findall(r"\(bible\s+", body))
            walk(ck)

    for n in notes:
        wiki_links += len(n.wiki_targets)
        walk(n.outline_kids)

    print(f"vault={vault}")
    print(f"md_files={len(notes)}")
    print(
        "slug_collisions_resolved="
        f"{sum(1 for v in collisions.values() if len(v) > 1)}"
    )
    print(f"wiki_link_mentions≈{wiki_links}")
    print(f"bible_sexps≈{bible_links}")
    print(f"folders={len({n.folder for n in notes})}")
    if args.limit:
        notes = notes[: args.limit]
        print(f"limit={args.limit}")

    if args.dry_run or not args.apply:
        print("dry-run only (pass --apply to write)")
        for n in notes[:20]:
            print(
                f"  {n.folder or '.'}/{n.slug}  "
                f"title={n.title!r} kids={len(n.outline_kids)}"
            )
        if len(notes) > 20:
            print(f"  … +{len(notes) - 20} more")
        return 0

    url = os.environ.get("PP_URL", "http://127.0.0.1:18080")
    user = os.environ.get("PP_USER", "admin")
    password = os.environ.get("PP_PASS", "admin")
    client = EvalClient(url, user, password)
    print(f"apply → {url} as {user}")
    ensure_libs(client)
    ok = err = 0
    for i, n in enumerate(notes):
        try:
            upsert_note(client, n)
            ok += 1
            if (i + 1) % 20 == 0:
                print(f"… {i + 1}/{len(notes)}", flush=True)
        except Exception as e:  # noqa: BLE001
            err += 1
            print(f"FAIL {n.rel}: {e}", file=sys.stderr)
    print(f"applied ok={ok} err={err}")
    return 0 if err == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
