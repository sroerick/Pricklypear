#!/usr/bin/env python3
"""Dropbox-shaped file sync on a live Pricklypear image (storage.files-file-lens).

Two layers, one closure (borge/storage.borg):
  bytes  -> media_blobs       (POST /api/media, dedup via /api/media/by-sha256/<hex>)
  paths  -> datum rows of type `file`
            {path, blob-id, sha256, size, mtime, content-type}
The row IS the path index; author + RLS datum confinement IS the filespace.
Blob retention rules and the v0 non-goals (no chunking, no watch service,
no tombstones) live in the borge chapter — don't reimplement policy here.

Env (same as pp_media.py):
  PP_URL   base URL (default http://127.0.0.1:18080)
  PP_USER  username
  PP_PASS  password  (or PP_PASS_FILE)

Usage:
  python3 scripts/pp_files.py init   ~/ppfiles
  python3 scripts/pp_files.py status ~/ppfiles
  python3 scripts/pp_files.py push   ~/ppfiles [--force]
  python3 scripts/pp_files.py pull   ~/ppfiles [--force]
  python3 scripts/pp_files.py sync   ~/ppfiles [--force]   # pull then push (bidirectional)

Exit codes: 0 ok, 1 sync conflicts reported, 2 usage/config error.
"""
from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

STATE_NAME = ".ppfiles.json"
FILE_TYPE = "file"
TYPE_FIELDS = ["path", "blob-id", "sha256", "size", "mtime", "content-type"]
ROWS_LIMIT = 500  # (rows) read cap — see the borge non-goals

# ------------------------------------------------------------------ plumbing


def env_or(name: str, default: str = "") -> str:
    v = os.environ.get(name, default)
    return v if v is not None else default


def load_password(args: argparse.Namespace) -> str:
    if args.password:
        return args.password
    if args.password_file:
        return Path(args.password_file).read_text(encoding="utf-8").strip()
    pf = env_or("PP_PASS_FILE")
    if pf:
        return Path(pf).read_text(encoding="utf-8").strip()
    return env_or("PP_PASS")


def basic_headers(user: str, password: str, extra: dict | None = None) -> dict:
    token = base64.b64encode(f"{user}:{password}".encode()).decode("ascii")
    h = {"Authorization": f"Basic {token}", "Accept": "application/json"}
    if extra:
        h.update(extra)
    return h


class Ctx:
    def __init__(self, args: argparse.Namespace):
        self.url = (args.url or env_or("PP_URL", "http://127.0.0.1:18080")).rstrip("/")
        self.user = args.user or env_or("PP_USER")
        self.passw = load_password(args)
        if not self.user or not self.passw:
            die("PP_USER / PP_PASS (or PP_PASS_FILE) required", 2)

    def api(self, method: str, path: str, *, headers: dict | None = None,
            data: bytes | None = None, expect_json: bool = True):
        req = urllib.request.Request(
            self.url + path, method=method, data=data,
            headers=basic_headers(self.user, self.passw, headers or {}),
        )
        try:
            with urllib.request.urlopen(req, timeout=120) as r:
                body = r.read()
                if expect_json:
                    return (json.loads(body) if body else None, r.status)
                return (body, r.status)
        except urllib.error.HTTPError as e:
            body = e.read()
            if expect_json:
                try:
                    return json.loads(body), e.code
                except Exception:
                    pass
            raise SystemExit(f"http {e.code} on {method} {path}: {body[:200]!r}")

    def prim(self, name: str, *args):
        """One prim call through /api/prim — identical auth/identity to eval."""
        payload = json.dumps({"name": name, "args": list(args)}).encode()
        res, status = self.api(
            "POST", "/api/prim",
            headers={"Content-Type": "application/json"}, data=payload,
        )
        if not res or not res.get("ok", True):
            raise SystemExit(f"prim {name} failed: {res}")
        return res.get("value")

    def push_blob(self, data: bytes, filename: str) -> tuple[str, str]:
        """Upload bytes (dedup first); return (blob-id, sha256)."""
        sha = hashlib.sha256(data).hexdigest()
        found, status = self.api("GET", f"/api/media/by-sha256/{sha}")
        if status == 200 and found and found.get("ok"):
            return found["id"], sha
        res, status = self.api(
            "POST", "/api/media",
            headers={"Content-Type": "application/octet-stream",
                     "X-Filename": filename},
            data=data,
        )
        if status != 201 or not res or not res.get("ok"):
            raise SystemExit(f"blob push failed ({status}): {res}")
        return res["id"], sha

    def fetch_blob(self, blob_id: str) -> bytes:
        res, status = self.api("GET", f"/media/{blob_id}", expect_json=False)
        if status != 200:
            raise SystemExit(f"blob fetch failed for {blob_id} (http {status})")
        return res


def die(msg: str, code: int = 2):
    print(f"pp_files: {msg}", file=sys.stderr)
    raise SystemExit(code)


def sha256_file(p: Path) -> str:
    h = hashlib.sha256()
    with p.open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def load_state(root: Path) -> dict:
    f = root / STATE_NAME
    if f.exists():
        try:
            return json.loads(f.read_text())
        except Exception:
            print(f"pp_files: warning: unreadable state, starting fresh", file=sys.stderr)
    return {}


def save_state(root: Path, state: dict):
    (root / STATE_NAME).write_text(json.dumps(state, indent=1, sort_keys=True))


def ensure_type(ctx: Ctx):
    declared = ctx.prim("type-fields", FILE_TYPE)
    if not declared or declared in ("", None):
        fields = " ".join(f"'{f}" for f in TYPE_FIELDS)
        ctx.prim("deftype", *TYPE_FIELDS)
        print(f"declared type '{FILE_TYPE}' with fields: {' '.join(TYPE_FIELDS)}")
    else:
        # declared types require ALL fields on write; verify we match
        have = json.loads(declared) if isinstance(declared, str) else declared
        missing = [f for f in TYPE_FIELDS if f not in (have or [])]
        if missing:
            die(f"type '{FILE_TYPE}' is declared but missing fields {missing}; "
                f"the file-lens envelope can't be written", 2)


def server_manifest(ctx: Ctx) -> dict[str, dict]:
    """All file rows visible to this identity: {path: {id, **fields}}."""
    value = ctx.prim("rows", FILE_TYPE, ROWS_LIMIT)
    out: dict[str, dict] = {}
    if isinstance(value, list):
        for pair in value:
            if not (isinstance(pair, list) and len(pair) == 2):
                continue
            rid, fields = pair
            if isinstance(fields, str):
                try:
                    fields = json.loads(fields)
                except Exception:
                    continue
            if isinstance(fields, dict) and fields.get("path"):
                out[fields["path"]] = {"id": rid, **fields}
    return out


def envelope(path: str, blob_id: str, sha: str, size: int, mtime: int) -> str:
    return json.dumps({"path": path, "blob-id": blob_id, "sha256": sha,
                       "size": str(size), "mtime": str(mtime),
                       "content-type": "application/octet-stream"})


def local_files(root: Path) -> dict[str, Path]:
    out = {}
    for p in sorted(root.rglob("*")):
        if not p.is_file() or p.name == STATE_NAME:
            continue
        rel = p.relative_to(root).as_posix()
        if rel.startswith(".pp"):
            continue
        out[rel] = p
    return out


# ------------------------------------------------------------------ actions

def action_push(ctx: Ctx, root: Path, force: bool, dry: bool) -> int:
    ensure_type(ctx)
    state = load_state(root)
    manifest = server_manifest(ctx)
    plan = local_files(root)

    conflicts, pushed, skipped = [], 0, 0
    # local deletions -> tombless row deletes (blob stays; see borge non-goals)
    for path, ent in list(state.items()):
        if path not in plan:
            row = manifest.get(path)
            if row:
                if dry:
                    print(f"delete  {path}")
                else:
                    ctx.prim("delete", FILE_TYPE, row["id"])
                    print(f"deleted {path}")
            state.pop(path)
    for path, p in plan.items():
        st = state.get(path) or {}
        sha = sha256_file(p)
        stat = p.stat()
        if st.get("sha256") == sha and st.get("row-id"):
            if path in manifest and manifest[path].get("sha256") == sha:
                skipped += 1
                continue
        row = manifest.get(path)
        if (row and row.get("sha256") != sha
                and int(row.get("mtime") or 0) > stat.st_mtime and not force):
            conflicts.append(f"{path} (server newer — pull or --force)")
            continue
        blob_id = "dry" if dry else ctx.push_blob(p.read_bytes(), p.name)[0]
        if dry:
            print(f"push    {path}")
            continue
        if row:
            ctx.prim("update", FILE_TYPE, row["id"],
                     envelope(path, blob_id, sha, stat.st_size, int(stat.st_mtime)))
            rid = row["id"]
        else:
            rid = ctx.prim("create", FILE_TYPE,
                           envelope(path, blob_id, sha, stat.st_size, int(stat.st_mtime)))
        if isinstance(rid, str) and len(rid) == 36:
            state[path] = {"sha256": sha, "size": stat.st_size, "mtime": stat.st_mtime,
                           "blob-id": blob_id, "row-id": rid}
            pushed += 1
            print(f"pushed  {path} -> {rid[:8]}")
        else:
            print(f"create rejected for {path}: {rid!r}", file=sys.stderr)
    if not dry:
        save_state(root, state)
    print(f"push: {pushed} pushed, {skipped} clean, {len(conflicts)} conflicts")
    for c in conflicts:
        print(f"  conflict: {c}")
    return 1 if conflicts else 0


def action_pull(ctx: Ctx, root: Path, force: bool, dry: bool) -> int:
    ensure_type(ctx)
    state = load_state(root)
    manifest = server_manifest(ctx)
    pulled, conflicts, deleted = [], [], []

    for path, row in sorted(manifest.items()):
        if path.startswith(".pp"):
            continue
        p = root / path
        segs = path.replace("\\", "/").split("/")
        if path.startswith("/") or any(s in ("", ".", "..") for s in segs):
            print(f"  skip (unsafe path): {path}", file=sys.stderr)
            continue
        if not p.exists():
            if dry:
                print(f"pull    {path}")
                continue
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_bytes(ctx.fetch_blob(row["blob-id"]))
            os.utime(p, (int(row.get("mtime") or time.time()),) * 2)
            state[path] = {"sha256": row["sha256"], "size": int(row["size"]),
                           "mtime": int(row["mtime"]), "blob-id": row["blob-id"],
                           "row-id": row["id"]}
            pulled.append(path)
            print(f"pulled  {path} <- {row['blob-id'][:8]}")
            continue
        local_sha = sha256_file(p)
        if local_sha == row["sha256"]:
            state[path] = {"sha256": row["sha256"], "size": int(row["size"]),
                           "mtime": int(row["mtime"]), "blob-id": row["blob-id"],
                           "row-id": row["id"]}
            continue
        st = state.get(path) or {}
        if st and st.get("sha256") == local_sha:
            # local UNTOUCHED since its last sync — server change is a
            # clean fast-forward, same as a first pull would have been
            if dry:
                print(f"ffwd    {path}")
                continue
            p.write_bytes(ctx.fetch_blob(row["blob-id"]))
            os.utime(p, (int(row.get("mtime") or time.time()),) * 2)
            state[path] = {"sha256": row["sha256"], "size": int(row["size"]),
                           "mtime": int(row["mtime"]), "blob-id": row["blob-id"],
                           "row-id": row["id"]}
            pulled.append(path)
            print(f"ffwd    {path} (fast-forwarded to server)")
            continue
        # both sides genuinely changed
        if (p.stat().st_mtime > int(row.get("mtime") or 0)):
            # local is newer — last-writer-wins on push; pull does not clobber
            conflicts.append(f"{path} (local newer — push it)")
            continue
        if force:
            if dry:
                print(f"pull!   {path}")
                continue
            p.write_bytes(ctx.fetch_blob(row["blob-id"]))
            os.utime(p, (int(row.get("mtime") or time.time()),) * 2)
            state[path] = {"sha256": row["sha256"], "size": int(row["size"]),
                           "mtime": int(row["mtime"]), "blob-id": row["blob-id"],
                           "row-id": row["id"]}
            pulled.append(path)
            print(f"forced  {path} (server overwrote local)")
        else:
            if dry:
                print(f"conflict {path}")
                continue
            stamp = time.strftime("%Y%m%d-%H%M%S")
            cp = p.with_name(p.name + f".ppconflict-{stamp}")
            cp.write_bytes(ctx.fetch_blob(row["blob-id"]))
            print(f"conflict {path}: server copy kept as {cp.name}; local untouched")
            conflicts.append(f"{path} (kept both)")
    # deletions on the server are REPORTED, never auto-applied (v0: boring)
    for path in state:
        if path not in manifest and (root / path).exists():
            deleted.append(path)
    save_state(root, state)
    print(f"pull: {len(pulled)} pulled, {len(conflicts)} conflicts"
          + (f", {len(deleted)} locally-held file(s) no longer on server" if deleted else ""))
    for d in deleted:
        print(f"  server-missing (kept locally): {d} — push to re-upload")
    return 1 if conflicts else 0


def action_status(ctx: Ctx, root: Path, force: bool) -> int:
    state = load_state(root)
    manifest = server_manifest(ctx)
    plan = local_files(root)
    to_push = to_pull = conflicts = clean = 0
    for path, p in plan.items():
        row = manifest.get(path)
        sha = None
        if row and row.get("sha256") == (sha := sha256_file(p)):
            clean += 1
        elif row and int(row.get("mtime") or 0) > p.stat().st_mtime:
            conflicts += 1
            print(f"  server-newer: {path}")
        elif row:
            to_push += 1
            print(f"  push:        {path}")
        else:
            to_push += 1
            print(f"  new (push):  {path}")
    for path in manifest:
        if path not in plan:
            to_pull += 1
            print(f"  pull:        {path}")
    print(f"status: {to_push} push / {to_pull} pull / {conflicts} conflict / {clean} clean")
    return 0


# ------------------------------------------------------------------ cli

def main() -> int:
    ap = argparse.ArgumentParser(description="Pricklypear file-lens sync client")
    ap.add_argument("--url", default=env_or("PP_URL", "http://127.0.0.1:18080"))
    ap.add_argument("--user", default=env_or("PP_USER"))
    ap.add_argument("--password")
    ap.add_argument("--password-file")
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name in ("init", "status", "push", "pull", "sync"):
        s = sub.add_parser(name, help=f"{name} a sync root")
        s.add_argument("root", help="local directory to sync (state: <root>/.ppfiles.json)")
        if name in ("push", "pull", "sync"):
            s.add_argument("--force", action="store_true",
                           help="let the other side win on conflict (push: server-newer; pull: server-wins)")
    s = sub.add_parser("dry", help="dry-run push+pull report")
    s.add_argument("root")
    args = ap.parse_args()

    root = Path(args.root).expanduser().resolve()
    if not root.is_dir():
        die(f"not a directory: {root}", 2)
    ctx = Ctx(args)

    if args.cmd == "init":
        root.mkdir(parents=True, exist_ok=True)
        ensure_type(ctx)
        load_state(root)
        print(f"ready: {root} (identity {ctx.user} @ {ctx.url})")
        return 0
    if args.cmd == "status":
        return action_status(ctx, root, False)
    if args.cmd == "dry":
        rc1 = action_push(ctx, root, False, dry=True)
        rc2 = action_pull(ctx, root, False, dry=True)
        return 0
    if args.cmd == "pull":
        return action_pull(ctx, root, getattr(args, "force", False), dry=False)
    if args.cmd == "push":
        return action_push(ctx, root, getattr(args, "force", False), dry=False)
    if args.cmd == "sync":
        rc = action_pull(ctx, root, getattr(args, "force", False), dry=False)
        if rc == 0:
            rc = action_push(ctx, root, getattr(args, "force", False), dry=False)
        return rc
    die("unreachable", 2)


if __name__ == "__main__":
    sys.exit(main())
