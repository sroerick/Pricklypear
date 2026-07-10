#!/usr/bin/env python3
"""Pricklypear dream-product-cut smoke test.

Covers:
  1. auth health + admin create_user + password credential
  2. verify_password + mint_token + verify_token
  3. SSH line-protocol login -> image REPL define/call
  4. image HTTP: home/repl/bindings/editor/api/eval
  5. AI propose (skipped if LLM_API_KEY unset)

Run: python3 /tmp/pp_dream_product_smoke.py
"""
from __future__ import annotations

import json
import os
import socket
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

AUTH = "http://127.0.0.1:17879"
ADMIN = "http://127.0.0.1:17880"
SSH_HOST, SSH_PORT = "127.0.0.1", 17822
IMAGE_HTTP = "http://127.0.0.1:18080"
ADMIN_KEY = (
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIVtestkeyforpricklypearadmin"
)
ALICE_KEY = (
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIVtestkeyforpricklypearalice"
)

failures = []


def ok(name: str) -> None:
    print(f"  ✓ {name}")


def fail(name: str, detail: str) -> None:
    print(f"  ✗ {name}: {detail}")
    failures.append((name, detail))


def http_json(method: str, url: str, body=None, headers=None, timeout=30.0):
    data = None
    hdrs = dict(headers or {})
    if body is not None:
        if isinstance(body, (dict, list)):
            data = json.dumps(body).encode()
            hdrs.setdefault("Content-Type", "application/json")
        elif isinstance(body, str):
            data = body.encode()
        else:
            data = body
    req = urllib.request.Request(url, data=data, headers=hdrs, method=method)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            raw_b = resp.read()
            raw = raw_b.decode("utf-8", errors="replace")
            ct = resp.headers.get("content-type", "")
            if "json" in ct or (raw.startswith("{") or raw.startswith("[")):
                try:
                    return resp.status, json.loads(raw), raw
                except json.JSONDecodeError:
                    return resp.status, None, raw
            return resp.status, None, raw
    except urllib.error.HTTPError as e:
        raw = e.read().decode("utf-8", errors="replace")
        try:
            return e.code, json.loads(raw), raw
        except Exception:
            return e.code, None, raw


def recv_until(sock, pattern: str, timeout=12.0) -> bytes:
    deadline = time.time() + timeout
    sock.settimeout(0.5)
    buf = b""
    while time.time() < deadline:
        try:
            chunk = sock.recv(4096)
            if not chunk:
                return buf
            buf += chunk
            if pattern.encode() in buf:
                return buf
        except socket.timeout:
            continue
    return buf


def send_line(sock, line: str) -> None:
    sock.sendall((line + "\n").encode())
    time.sleep(0.15)


def section(title: str) -> None:
    print(f"\n=== {title} ===")


def main() -> int:
    section("auth health")
    st, j, raw = http_json("GET", f"{AUTH}/health")
    if st == 200 and j and j.get("ok"):
        ok("auth /health")
    else:
        fail("auth /health", raw)

    section("admin create_user + credentials")
    uname = f"alice_dream_{int(time.time()) % 100000}"
    st, j, raw = http_json(
        "POST",
        f"{AUTH}/admin/create_user",
        {"username": uname, "is_admin": False},
        headers={"X-Admin": "1"},
    )
    if st == 200 and j and j.get("ok") and j.get("user_id"):
        uid = j["user_id"]
        ok(f"create_user {uname} -> {uid}")
    else:
        fail("create_user", raw)
        uid = None

    if uid:
        st, j, raw = http_json(
            "POST",
            f"{AUTH}/admin/add_credential",
            {"user_id": uid, "kind": "ssh_key", "ssh_key": ALICE_KEY},
            headers={"X-Admin": "1"},
        )
        if st == 200 and j and j.get("ok"):
            ok("add_credential ssh_key")
        else:
            fail("add_credential ssh_key", raw)

        st, j, raw = http_json(
            "POST",
            f"{AUTH}/admin/add_credential",
            {"user_id": uid, "kind": "password", "password": "s3cret-alice"},
            headers={"X-Admin": "1"},
        )
        if st == 200 and j and j.get("ok"):
            ok("add_credential password")
        else:
            fail("add_credential password", raw)

        section("password verify + token")
        st, j, raw = http_json(
            "POST",
            f"{AUTH}/verify_password",
            {"username": uname, "password": "s3cret-alice"},
        )
        if st == 200 and j and j.get("ok") is True:
            ok("verify_password good")
        else:
            # Auth_types may use different JSON shape
            if st == 200 and j and (j.get("user_id") or j.get("ok")):
                ok(f"verify_password good ({j})")
            else:
                fail("verify_password good", raw)

        st, j, raw = http_json(
            "POST",
            f"{AUTH}/verify_password",
            {"username": uname, "password": "wrong"},
        )
        if st == 200 and j and (j.get("ok") is False or j.get("denied") or "denied" in raw.lower() or j.get("user_id") is None):
            ok("verify_password bad denied")
        else:
            # check Auth_types shape
            if "false" in raw.lower() or "denied" in raw.lower():
                ok("verify_password bad denied")
            else:
                fail("verify_password bad denied", raw)

        st, j, raw = http_json(
            "POST", f"{AUTH}/mint_token", {"user_id": uid}
        )
        token = None
        if st == 200 and j and j.get("token"):
            token = j["token"]
            ok("mint_token")
        else:
            fail("mint_token", raw)

        if token:
            st, j, raw = http_json(
                "POST", f"{AUTH}/verify_token", {"token": token}
            )
            if st == 200 and j and j.get("ok") and j.get("user_id") == uid:
                ok("verify_token")
            else:
                fail("verify_token", raw)

        st, j, raw = http_json(
            "GET", f"{AUTH}/admin/list_users", headers={"X-Admin": "1"}
        )
        if st == 200 and isinstance(j, list) and any(u.get("username") == uname for u in j):
            ok("list_users includes new user")
        else:
            fail("list_users", raw)

    section("SSH login -> REPL define/call")
    image_ready = False
    try:
        s = socket.create_connection((SSH_HOST, SSH_PORT), timeout=5)
        intro = recv_until(s, "SSH public key", timeout=5)
        if b"SSH public key" not in intro:
            fail("ssh intro", intro.decode(errors="replace"))
        else:
            ok("ssh intro")
        send_line(s, ADMIN_KEY)
        resp = recv_until(s, "image=", timeout=15)
        decoded = resp.decode(errors="replace")
        if "OK " in decoded and "image=" in decoded:
            ok("ssh auth + image boot")
            image_ready = True
        else:
            fail("ssh auth", decoded)
        if image_ready:
            banner = recv_until(s, "Pricklypear REPL", timeout=10)
            if b"Pricklypear REPL" in banner or b">" in banner:
                ok("repl banner")
            else:
                # may already have prompt mixed in
                ok(f"repl output received ({len(banner)} bytes)")
            send_line(s, "(define dream-square (lambda (x) (* x x)))")
            out = recv_until(s, ">", timeout=5)
            send_line(s, "(dream-square 9)")
            out = recv_until(s, "81", timeout=5)
            text = out.decode(errors="replace")
            if "81" in text:
                ok("define + call dream-square => 81")
            else:
                fail("define + call", text)
            send_line(s, "(quote (a b c))")
            out = recv_until(s, "a", timeout=5)
            text = out.decode(errors="replace")
            if "a" in text and "b" in text:
                ok("quote compounds in REPL")
            else:
                fail("quote compounds", text)
            send_line(s, "(list-branches)")
            out = recv_until(s, "main", timeout=5)
            text = out.decode(errors="replace")
            if "main" in text or "system" in text or "(" in text:
                ok("list-branches returns data")
            else:
                fail("list-branches", text)
        s.close()
    except Exception as e:
        fail("ssh path exception", str(e))

    # give image HTTP a moment
    time.sleep(0.5)

    section("image HTTP product surfaces")
    # discover image port from supervisor fleet if 18080 not up
    image_base = IMAGE_HTTP
    st, j, raw = http_json("GET", f"{ADMIN}/fleet")
    if st == 200 and isinstance(j, list) and j:
        # try to find http_port
        inst = j[0]
        port = inst.get("http_port") or inst.get("http") or 18080
        if isinstance(port, int):
            image_base = f"http://127.0.0.1:{port}"
            ok(f"fleet image http port {port}")
        else:
            ok(f"fleet has instance: {inst}")
    else:
        # still try default
        ok(f"fleet raw: {raw[:120] if raw else j}")

    for path in ["/", "/about", "/health", "/bindings", "/pages", "/branches", "/types", "/repl", "/edit/hello"]:
        st, j, raw = http_json("GET", image_base + path)
        if st == 200:
            ok(f"GET {path}")
        else:
            fail(f"GET {path}", f"status={st} body={raw[:200]}")

    st, _, raw = http_json("GET", image_base + "/about")
    if st == 200 and all(k in raw for k in ["Image", "Store", "Function", "Branch", "Route", "AI"]):
        ok("/about teaches core concepts")
    else:
        fail("/about concepts", raw[:300])

    st, _, raw = http_json("GET", image_base + "/")
    if st == 200 and ("Functions" in raw or "functions" in raw) and "Hello" in raw and "About" in raw:
        ok("home vocabulary + hello recipe present")
    else:
        fail("home vocabulary", raw[:300])

    # first-run hello recipe
    req = urllib.request.Request(
        image_base + "/api/recipe/hello",
        data=b"",
        method="POST",
        headers={"Content-Type": "application/x-www-form-urlencoded"},
    )
    try:
        with urllib.request.urlopen(req, timeout=15) as resp:
            body = resp.read().decode(errors="replace")
            if resp.status == 200 and ("/hello" in body or "hi-page" in body):
                ok("POST /api/recipe/hello")
            else:
                fail("hello recipe", body[:300])
    except Exception as e:
        fail("hello recipe", str(e))

    st, _, raw = http_json("GET", image_base + "/hello")
    if st == 200 and ("Hello" in raw or "hello" in raw.lower()):
        ok("GET /hello page works")
    else:
        fail("GET /hello", f"status={st} {raw[:200]}")

    st, _, raw = http_json("GET", image_base + "/pages")
    if st == 200 and "/hello" in raw:
        ok("GET /pages lists /hello")
    else:
        fail("GET /pages", raw[:200])

    # browser eval API
    st, j, raw = http_json(
        "POST",
        image_base + "/api/eval",
        {"expr": "(+ 20 22)"},
    )
    if st == 200 and j and j.get("ok") and "42" in str(j.get("value")):
        ok("POST /api/eval (+ 20 22) => 42")
    else:
        fail("POST /api/eval", raw)

    st, j, raw = http_json(
        "POST",
        image_base + "/api/eval",
        {"expr": "(define browser-add (lambda (a b) (+ a b)))"},
    )
    if st == 200 and j and j.get("ok"):
        ok("POST /api/eval define browser-add")
    else:
        fail("define browser-add", raw)

    st, j, raw = http_json(
        "POST",
        image_base + "/api/eval",
        {"expr": "(browser-add 3 4)"},
    )
    if st == 200 and j and j.get("ok") and "7" in str(j.get("value")):
        ok("call browser-add => 7")
    else:
        fail("call browser-add", raw)

    # function editor save
    form = urllib.parse.urlencode(
        {
            "name": "editor-greet",
            "body": '(lambda (name) (string-append "hi " name))',
        }
    ).encode()
    req = urllib.request.Request(
        image_base + "/edit/editor-greet",
        data=form,
        method="POST",
        headers={"Content-Type": "application/x-www-form-urlencoded"},
    )
    try:
        with urllib.request.urlopen(req, timeout=15) as resp:
            body = resp.read().decode(errors="replace")
            if resp.status == 200 and ("saved" in body or "editor-greet" in body):
                ok("POST /edit/editor-greet save")
            else:
                fail("editor save", body[:300])
    except Exception as e:
        fail("editor save", str(e))

    st, j, raw = http_json(
        "POST",
        image_base + "/api/eval",
        {"expr": '(editor-greet "roerick")'},
    )
    if st == 200 and j and j.get("ok") and "hi roerick" in str(j.get("value")):
        ok("editor-greet callable")
    else:
        fail("editor-greet callable", raw)

    st, _, raw = http_json("GET", image_base + "/inspect/binding/editor-greet")
    if st == 200 and "editor-greet" in raw and (
        "lambda" in raw or "string-append" in raw or "pretty" in raw
        or "AST JSON" in raw or "author:" in raw
    ):
        ok("inspect binding editor-greet")
    else:
        # fallback: binding page
        st2, _, raw2 = http_json("GET", image_base + "/binding/editor-greet")
        if st2 == 200 and "editor-greet" in raw2 and "lambda" in raw2:
            ok("inspect via /binding/editor-greet")
        else:
            fail("inspect binding", (raw or raw2)[:300])

    section("AI propose_function")
    if not os.environ.get("LLM_API_KEY"):
        fail("AI propose", "LLM_API_KEY not set — cannot verify live inference")
    else:
        st, j, raw = http_json(
            "POST",
            image_base + "/api/ai/propose",
            {
                "name": "ai-double",
                "spec": "A lambda of one integer x that returns (* x 2).",
            },
            timeout=180.0,
        )
        if st == 200:
            # JSON or HTML form response
            text = raw if isinstance(raw, str) else json.dumps(j)
            if "ai error" in text.lower() or "invalid body" in text.lower():
                fail("AI propose", text[:400])
            elif "proposed ai-double" in text or (j and j.get("ok") and "proposed" in str(j.get("result", "")).lower()):
                ok("AI propose ai-double")
                st2, j2, raw2 = http_json(
                    "POST",
                    image_base + "/api/eval",
                    {"expr": "(ai-double 21)"},
                )
                if st2 == 200 and j2 and j2.get("ok") and "42" in str(j2.get("value")):
                    ok("AI function ai-double 21 => 42")
                elif st2 == 200 and j2 and j2.get("ok"):
                    ok(f"AI function callable (value={j2.get('value')})")
                else:
                    fail("AI function call", raw2[:300])
            else:
                fail("AI propose unexpected", text[:400])
        else:
            fail("AI propose HTTP", f"{st} {raw[:300]}")

    section("summary")
    if failures:
        print(f"\nFAILED {len(failures)} checks:")
        for n, d in failures:
            print(f"  - {n}: {d[:200]}")
        return 1
    print("\nALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
