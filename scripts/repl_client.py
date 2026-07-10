#!/usr/bin/env python3
"""Interactive Pricklypear REPL client (line-protocol front door).

Used by scripts/repl.sh. No netcat required.
"""
from __future__ import annotations

import argparse
import select
import socket
import sys


def main() -> int:
    p = argparse.ArgumentParser(description="Pricklypear interactive REPL client")
    p.add_argument("--host", default="127.0.0.1")
    p.add_argument("--port", type=int, default=17822)
    p.add_argument("--key", required=True, help="OpenSSH public key line")
    p.add_argument(
        "--http-port",
        type=int,
        default=18080,
        help="hint printed for image UI after connect",
    )
    args = p.parse_args()
    key = args.key.strip()
    if not key:
        print("repl: empty key", file=sys.stderr)
        return 2

    try:
        s = socket.create_connection((args.host, args.port), timeout=5.0)
    except OSError as e:
        print(f"repl: cannot connect to {args.host}:{args.port} ({e})", file=sys.stderr)
        print("      start the stack first: scripts/dev.sh start", file=sys.stderr)
        return 1

    s.setblocking(False)
    print(f"[repl] connected to {args.host}:{args.port}")
    print(f"[repl] after login, UI is usually http://{args.host}:{args.http_port}/")
    print("[repl] type Lisp forms; :quit or Ctrl-D to exit")
    print()

    def send(data: bytes) -> None:
        view = memoryview(data)
        while len(view):
            _, w, _ = select.select([], [s], [], 5.0)
            if not w:
                raise TimeoutError("send timeout")
            n = s.send(view)
            view = view[n:]

    # Drain intro / prompt for a short window
    for _ in range(40):
        r, _, _ = select.select([s], [], [], 0.15)
        if not r:
            break
        try:
            chunk = s.recv(4096)
        except BlockingIOError:
            break
        if not chunk:
            break
        sys.stdout.write(chunk.decode(errors="replace"))
        sys.stdout.flush()

    send((key + "\n").encode())

    # Wait for auth + image boot + REPL banner before interactive mode.
    auth_buf = b""
    for _ in range(200):  # up to ~20s
        r, _, _ = select.select([s], [], [], 0.1)
        if not r:
            if auth_buf and (b"> " in auth_buf or b"DENIED" in auth_buf):
                break
            continue
        try:
            chunk = s.recv(4096)
        except BlockingIOError:
            continue
        if not chunk:
            break
        auth_buf += chunk
        sys.stdout.write(chunk.decode(errors="replace"))
        sys.stdout.flush()
        if b"DENIED" in auth_buf:
            print("[repl] authentication denied", file=sys.stderr)
            s.close()
            return 1
        # Prompt looks like "system/main> " or "...REPL..."
        if b"> " in auth_buf or b"Pricklypear REPL" in auth_buf:
            # drain a little more for the prompt line
            r2, _, _ = select.select([s], [], [], 0.2)
            if r2:
                try:
                    more = s.recv(4096)
                except BlockingIOError:
                    more = b""
                if more:
                    sys.stdout.write(more.decode(errors="replace"))
                    sys.stdout.flush()
            break

    def drain_socket(timeout: float = 0.5) -> bool:
        """Read available socket data. Returns False if peer closed."""
        end = False
        while True:
            r, _, _ = select.select([s], [], [], timeout)
            if not r:
                return not end
            try:
                data = s.recv(4096)
            except BlockingIOError:
                return True
            except (ConnectionResetError, BrokenPipeError, OSError):
                print("\n[repl] connection closed")
                return False
            if not data:
                print("\n[repl] connection closed")
                return False
            sys.stdout.write(data.decode(errors="replace"))
            sys.stdout.flush()
            timeout = 0.05  # keep draining burst

    stdin_fd = sys.stdin.fileno()
    interactive = sys.stdin.isatty()
    try:
        if not interactive:
            # Batch/piped mode: send each line, wait for output between lines.
            for line in sys.stdin:
                if line.strip() in (":quit", ":q"):
                    break
                send(line.encode() if line.endswith("\n") else (line + "\n").encode())
                if not drain_socket(timeout=2.0):
                    break
                # small extra drain for slow prompts
                drain_socket(timeout=0.15)
        else:
            while True:
                r, _, _ = select.select([s, stdin_fd], [], [], 0.5)
                if s in r:
                    if not drain_socket(timeout=0.0):
                        break
                if stdin_fd in r:
                    line = sys.stdin.readline()
                    if line == "":
                        break
                    if line.strip() in (":quit", ":q"):
                        break
                    send(line.encode())
    except KeyboardInterrupt:
        print("\n[repl] interrupted")
    finally:
        try:
            s.close()
        except Exception:
            pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
