#!/usr/bin/env python3
"""Push/pull binary media on a live Pricklypear image (storage.media).

Agents should use this (or POST /api/media directly) — NOT /api/eval —
for multi-KiB images. Eval stays at a 64 KiB body cap.

Env (same as pp_eval.py):
  PP_URL   base URL (default http://127.0.0.1:18080)
  PP_USER  username
  PP_PASS  password  (or PP_PASS_FILE)

Usage:
  python3 scripts/pp_media.py put ./shot.png
  python3 scripts/pp_media.py put ./doc.pdf --type application/pdf
  python3 scripts/pp_media.py get <uuid> -o out.png
  python3 scripts/pp_media.py list
  python3 scripts/pp_media.py meta <uuid>
  python3 scripts/pp_media.py delete <uuid>

Exit codes: 0 ok, 1 HTTP/API error, 2 usage/config.
"""
from __future__ import annotations

import argparse
import base64
import json
import mimetypes
import os
import sys
import urllib.error
import urllib.request
from pathlib import Path


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
    h = {
        "Authorization": f"Basic {token}",
        "Accept": "application/json",
    }
    if extra:
        h.update(extra)
    return h


def request(
    *,
    base_url: str,
    user: str,
    password: str,
    method: str,
    path: str,
    data: bytes | None = None,
    headers: dict | None = None,
    timeout: float = 120.0,
) -> tuple[int, bytes, str]:
    url = base_url.rstrip("/") + path
    req = urllib.request.Request(
        url,
        data=data,
        method=method,
        headers=basic_headers(user, password, headers),
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            body = resp.read()
            ctype = resp.headers.get("Content-Type", "")
            return resp.status, body, ctype
    except urllib.error.HTTPError as e:
        body = e.read() if e.fp else b""
        ctype = e.headers.get("Content-Type", "") if e.headers else ""
        return e.code, body, ctype


def guess_type(path: Path, override: str | None) -> str:
    if override:
        return override
    guessed, _ = mimetypes.guess_type(str(path))
    if guessed:
        return guessed
    # Sensible default for agent screenshots.
    return "application/octet-stream"


def cmd_put(args: argparse.Namespace) -> int:
    path = Path(args.file)
    if not path.is_file():
        print(f"not a file: {path}", file=sys.stderr)
        return 2
    data = path.read_bytes()
    ctype = guess_type(path, args.type)
    if ctype == "application/octet-stream" and not args.type:
        print(
            "warning: could not guess content-type; pass --type "
            "(allowed: image/png jpeg gif webp svg+xml, application/pdf, "
            "audio/mpeg ogg flac wav mp4)",
            file=sys.stderr,
        )
    status, body, _ = request(
        base_url=args.url,
        user=args.user,
        password=args.password_value,
        method="POST",
        path="/api/media",
        data=data,
        headers={
            "Content-Type": ctype,
            "X-Filename": path.name,
        },
        timeout=args.timeout,
    )
    text = body.decode("utf-8", errors="replace")
    if args.json:
        print(text)
    else:
        try:
            j = json.loads(text)
        except json.JSONDecodeError:
            print(text, file=sys.stderr)
            return 1
        if not j.get("ok"):
            print(j.get("error", text), file=sys.stderr)
            return 1
        print(j.get("id", ""))
        if not args.quiet:
            print(f"url={j.get('url')} type={j.get('content_type')} "
                  f"bytes={j.get('byte_size')} sha256={j.get('sha256')}",
                  file=sys.stderr)
    return 0 if 200 <= status < 300 else 1


def cmd_get(args: argparse.Namespace) -> int:
    status, body, ctype = request(
        base_url=args.url,
        user=args.user,
        password=args.password_value,
        method="GET",
        path=f"/media/{args.id}",
        headers={"Accept": "*/*"},
        timeout=args.timeout,
    )
    if status != 200:
        sys.stderr.write(body.decode("utf-8", errors="replace") + "\n")
        return 1
    if args.output:
        Path(args.output).write_bytes(body)
        if not args.quiet:
            print(f"wrote {len(body)} bytes ({ctype}) -> {args.output}",
                  file=sys.stderr)
    else:
        sys.stdout.buffer.write(body)
    return 0


def cmd_list(args: argparse.Namespace) -> int:
    q = f"?limit={args.limit}" if args.limit else ""
    status, body, _ = request(
        base_url=args.url,
        user=args.user,
        password=args.password_value,
        method="GET",
        path=f"/api/media{q}",
        timeout=args.timeout,
    )
    text = body.decode("utf-8", errors="replace")
    if args.json or True:
        if args.json:
            print(text)
        else:
            try:
                j = json.loads(text)
            except json.JSONDecodeError:
                print(text, file=sys.stderr)
                return 1
            if not j.get("ok"):
                print(j.get("error", text), file=sys.stderr)
                return 1
            for it in j.get("items", []):
                print(
                    f"{it.get('id')}\t{it.get('content_type')}\t"
                    f"{it.get('byte_size')}\t{it.get('filename') or ''}\t"
                    f"{it.get('created_at')}"
                )
    return 0 if status == 200 else 1


def cmd_meta(args: argparse.Namespace) -> int:
    # Reuse list endpoint isn't enough — hit GET /media is bytes.
    # Meta via eval would work but keep HTTP-only: list and filter,
    # or use a tiny eval. Prefer eval for one-id meta when available.
    # Direct: we don't have GET /api/media/<id>; use eval bridge.
    expr = f'(media-meta "{args.id}")'
    payload = json.dumps({"expr": expr}).encode()
    status, body, _ = request(
        base_url=args.url,
        user=args.user,
        password=args.password_value,
        method="POST",
        path="/api/eval",
        data=payload,
        headers={"Content-Type": "application/json"},
        timeout=args.timeout,
    )
    text = body.decode("utf-8", errors="replace")
    try:
        j = json.loads(text)
    except json.JSONDecodeError:
        print(text, file=sys.stderr)
        return 1
    if not j.get("ok"):
        print(j.get("error", text), file=sys.stderr)
        return 1
    val = j.get("value", "()")
    if val in ("()", "nil", ""):
        print("not found", file=sys.stderr)
        return 1
    # value is a pretty-printed string; strip surrounding quotes if present
    if isinstance(val, str) and len(val) >= 2 and val[0] == '"' and val[-1] == '"':
        # pp_value may escape — try json
        try:
            inner = json.loads(val)
            print(inner if isinstance(inner, str) else val)
            return 0
        except json.JSONDecodeError:
            pass
    print(val)
    return 0 if status == 200 else 1


def cmd_delete(args: argparse.Namespace) -> int:
    status, body, _ = request(
        base_url=args.url,
        user=args.user,
        password=args.password_value,
        method="DELETE",
        path=f"/api/media/{args.id}",
        timeout=args.timeout,
    )
    text = body.decode("utf-8", errors="replace")
    if args.json:
        print(text)
    else:
        try:
            j = json.loads(text)
        except json.JSONDecodeError:
            print(text, file=sys.stderr)
            return 1
        if not j.get("ok"):
            print(j.get("error", text), file=sys.stderr)
            return 1
        print(j.get("id", args.id))
    return 0 if status == 200 else 1


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(description="Pricklypear media client")
    p.add_argument("--url", default=env_or("PP_URL", "http://127.0.0.1:18080"))
    p.add_argument("--user", default=env_or("PP_USER", "admin"))
    p.add_argument("--password", default="")
    p.add_argument("--password-file", default="")
    p.add_argument("--timeout", type=float, default=120.0)
    p.add_argument("--json", action="store_true", help="raw JSON on stdout")
    p.add_argument("-q", "--quiet", action="store_true")

    sub = p.add_subparsers(dest="cmd", required=True)

    put_p = sub.add_parser("put", help="upload a file")
    put_p.add_argument("file")
    put_p.add_argument("--type", dest="type", default=None,
                       help="Content-Type override")
    put_p.set_defaults(func=cmd_put)

    get_p = sub.add_parser("get", help="download bytes")
    get_p.add_argument("id")
    get_p.add_argument("-o", "--output", default="")
    get_p.set_defaults(func=cmd_get)

    list_p = sub.add_parser("list", help="list caller's media meta")
    list_p.add_argument("--limit", type=int, default=50)
    list_p.set_defaults(func=cmd_list)

    meta_p = sub.add_parser("meta", help="show one blob's meta via eval")
    meta_p.add_argument("id")
    meta_p.set_defaults(func=cmd_meta)

    del_p = sub.add_parser("delete", help="delete a blob (author or admin)")
    del_p.add_argument("id")
    del_p.set_defaults(func=cmd_delete)

    args = p.parse_args(argv)
    pw = load_password(args)
    if not pw:
        print("PP_PASS / --password required", file=sys.stderr)
        return 2
    args.password_value = pw
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
