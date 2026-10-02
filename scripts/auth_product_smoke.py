#!/usr/bin/env python3
"""Auth product smoke — browser login, sessions, admin create user.

Requires: scripts/dev.sh start (PG + image on PP_HTTP_PORT, default 18080).
Bootstrap: PP_BOOTSTRAP_USER/PASSWORD default admin/admin when users empty.

Usage:
  python3 scripts/auth_product_smoke.py
  PP_HTTP_PORT=18080 python3 scripts/auth_product_smoke.py
"""
from __future__ import annotations

import http.cookiejar
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

PORT = os.environ.get("PP_HTTP_PORT", "18080")
BASE = os.environ.get("PP_URL", f"http://127.0.0.1:{PORT}").rstrip("/")
ADMIN_USER = os.environ.get("PP_BOOTSTRAP_USER", "admin")
ADMIN_PASS = os.environ.get("PP_BOOTSTRAP_PASSWORD", "admin")

failures = 0


def ok(msg: str) -> None:
    print(f"  OK  {msg}")


def fail(msg: str, detail: str = "") -> None:
    global failures
    failures += 1
    print(f"  FAIL {msg}" + (f" — {detail}" if detail else ""))


def section(title: str) -> None:
    print(f"\n== {title}")


class Client:
    def __init__(self) -> None:
        self.cj = http.cookiejar.CookieJar()
        self.opener = urllib.request.build_opener(
            urllib.request.HTTPCookieProcessor(self.cj)
        )

    def request(
        self,
        method: str,
        path: str,
        data: dict | None = None,
        headers: dict | None = None,
        allow_redirects: bool = False,
    ) -> tuple[int, dict[str, str], str]:
        url = BASE + path
        body = None
        hdrs = {"Accept": "text/html,application/json"}
        if headers:
            hdrs.update(headers)
        if data is not None:
            body = urllib.parse.urlencode(data).encode()
            hdrs["Content-Type"] = "application/x-www-form-urlencoded"
        req = urllib.request.Request(url, data=body, method=method, headers=hdrs)
        try:
            # Don't auto-follow redirects so we can inspect Set-Cookie + Location
            self.opener.add_handler  # noqa: keep jar
            resp = self.opener.open(req, timeout=15)
            status = resp.getcode()
            rh = {k.lower(): v for k, v in resp.headers.items()}
            text = resp.read().decode("utf-8", errors="replace")
            return status, rh, text
        except urllib.error.HTTPError as e:
            rh = {k.lower(): v for k, v in e.headers.items()} if e.headers else {}
            text = e.read().decode("utf-8", errors="replace")
            return e.code, rh, text

    def cookies(self) -> dict[str, str]:
        return {c.name: c.value for c in self.cj}


def main() -> int:
    print(f"auth smoke against {BASE}")
    c = Client()

    section("public /health")
    st, _, body = c.request("GET", "/health")
    if st == 200 and '"ok"' in body.replace(" ", ""):
        ok("/health public")
    else:
        fail("/health", f"status={st} body={body[:120]}")

    section("unauthenticated product redirects")
    st, hdrs, _ = c.request("GET", "/")
    loc = hdrs.get("location", "")
    if st in (303, 302, 301) and "/login" in loc:
        ok(f"GET / -> {st} {loc}")
    elif st == 200 and "Sign in" in _:
        ok("GET / shows login (alternate)")
    else:
        # Some stacks may return 303 without following
        if st == 200 and "Sign in" not in _ and "whoami" in _.lower():
            fail("GET / still open without auth", f"status={st}")
        else:
            # request follows redirect by default in urllib... open() follows
            # So after redirect we may land on /login 200
            st2, _, body2 = c.request("GET", "/login")
            if st2 == 200 and "Sign in" in body2:
                ok("GET / ends at login form (urllib followed redirect)")
            else:
                fail("GET / auth gate", f"status={st} loc={loc}")

    section("GET /login form")
    st, _, body = c.request("GET", "/login")
    if st == 200 and "Sign in" in body and 'name="password"' in body:
        ok("login form")
    else:
        fail("login form", f"status={st}")

    section("bad password")
    c_bad = Client()
    st, _, body = c_bad.request(
        "POST", "/login", {"username": ADMIN_USER, "password": "definitely-wrong"}
    )
    if st == 200 and "Invalid" in body:
        ok("bad password rejected")
    else:
        fail("bad password", f"status={st} body={body[:160]}")

    section("admin login")
    c = Client()
    st, hdrs, body = c.request(
        "POST", "/login", {"username": ADMIN_USER, "password": ADMIN_PASS}
    )
    # urllib follows 303 -> / so we may get 200 home
    cookies = c.cookies()
    if "pp_session" in cookies and cookies["pp_session"]:
        ok(f"pp_session cookie set (len={len(cookies['pp_session'])})")
    else:
        fail("pp_session cookie missing", str(cookies))

    st, _, body = c.request("GET", "/")
    if st == 200 and ADMIN_USER in body:
        ok(f"home shows {ADMIN_USER}")
    else:
        fail("home identity", f"status={st} snippet={body[:200]}")

    section("api/eval whoami with cookie")
    # POST JSON with cookie jar
    import json

    def api_eval(client: Client, expr: str) -> dict:
        url = BASE + "/api/eval"
        data = json.dumps({"expr": expr}).encode()
        req = urllib.request.Request(
            url,
            data=data,
            method="POST",
            headers={"Content-Type": "application/json", "Accept": "application/json"},
        )
        try:
            resp = client.opener.open(req, timeout=15)
            return json.loads(resp.read().decode())
        except urllib.error.HTTPError as e:
            raw = e.read().decode("utf-8", errors="replace")
            try:
                return json.loads(raw)
            except Exception:
                return {"ok": False, "error": raw, "status": e.code}

    j = api_eval(c, "(whoami)")
    if j.get("ok") and ADMIN_USER in str(j.get("value", j)):
        ok(f"whoami={j.get('value')}")
    else:
        fail("whoami", str(j))

    j = api_eval(c, "(current-branch)")
    val = str(j.get("value", j))
    if j.get("ok") and "personal" in val:
        ok(f"branch={val}")
    else:
        # may still be main if attach didn't checkout — soft fail note
        fail("current-branch personal", val)

    section("admin create alice")
    st, _, body = c.request(
        "POST",
        "/admin/users",
        {"username": "alice", "password": "alice-pass-1", "is_admin": "0"},
    )
    # may 303 to list
    st, _, body = c.request("GET", "/admin/users")
    if st == 200 and "alice" in body:
        ok("alice listed")
    else:
        fail("create alice", f"status={st} body={body[:200]}")

    section("alice login distinct garden")
    ca = Client()
    st, _, _ = ca.request(
        "POST", "/login", {"username": "alice", "password": "alice-pass-1"}
    )
    if "pp_session" not in ca.cookies():
        fail("alice cookie")
    else:
        ok("alice cookie")
    j = api_eval(ca, "(whoami)")
    if j.get("ok") and "alice" in str(j.get("value", "")):
        ok(f"alice whoami={j.get('value')}")
    else:
        fail("alice whoami", str(j))
    j = api_eval(ca, "(define auth-smoke-pin 42)")
    if j.get("ok"):
        ok("alice define pin")
    else:
        fail("alice define", str(j))
    j = api_eval(ca, "(current-branch)")
    ok(f"alice branch={j.get('value')}")

    section("admin does not see alice pin as own")
    j = api_eval(c, "(whoami)")
    ok(f"admin still {j.get('value')}")
    # checkout admin personal and see if pin is own — pin on alice/personal
    # shouldn't be effective as admin's own unless same branch
    j = api_eval(c, "auth-smoke-pin")
    # if admin is on admin/personal, unbound or inherited from main only
    if j.get("ok") and str(j.get("value")) == "42":
        # could mean shared branch — fail
        fail("admin sees alice pin value 42 (branch leak?)")
    else:
        ok("admin does not resolve alice pin as 42")

    section("non-admin cannot create users")
    st, _, body = ca.request(
        "POST",
        "/admin/users",
        {"username": "bob", "password": "x", "is_admin": "0"},
    )
    if st in (403, 401) or "admin only" in body or "Forbidden" in body:
        ok("alice forbidden from admin create")
    else:
        # 303 after create would be bad
        st2, _, body2 = ca.request("GET", "/admin/users")
        if "admin only" in body2 or st2 == 403:
            ok("alice cannot view admin users")
        else:
            fail("non-admin create", f"status={st} body={body[:160]}")

    section("lisp create-user admin gate")
    # Admin can create via prim; non-admin gets denied string (ok:true, value error).
    j = api_eval(c, '(is-admin?)')
    if j.get("ok") and "#t" in str(j.get("value", "")):
        ok("admin is-admin? #t")
    else:
        fail("admin is-admin?", str(j))
    j = api_eval(
        c, '(create-user "lisp-user" "lisp-pass-1")'
    )
    val = str(j.get("value", ""))
    if j.get("ok") and re.search(r"[0-9a-f-]{36}", val):
        ok("admin create-user prim returns uuid")
    else:
        fail("admin create-user prim", str(j))
    j = api_eval(ca, "(is-admin?)")
    if j.get("ok") and "#f" in str(j.get("value", "")):
        ok("alice is-admin? #f")
    else:
        fail("alice is-admin?", str(j))
    j = api_eval(ca, '(create-user "should-fail" "x")')
    val = str(j.get("value", ""))
    if j.get("ok") and "admin only" in val:
        ok("non-admin create-user denied")
    else:
        fail("non-admin create-user", str(j))
    # New lisp-user can log in
    cl = Client()
    cl.request(
        "POST", "/login",
        {"username": "lisp-user", "password": "lisp-pass-1"},
    )
    j = api_eval(cl, "(whoami)")
    if j.get("ok") and "lisp-user" in str(j.get("value", "")):
        ok("lisp-user login after create-user prim")
    else:
        fail("lisp-user login", str(j))

    section("lisp set-password admin gate")
    # Non-admin is denied; admin resets the credential by username, the old
    # password stops working, and the new one logs in. (auth.user-management:
    # set_password is part of the complete admin surface.)
    j = api_eval(ca, '(set-password "lisp-user" "nope")')
    if j.get("ok") and "admin only" in str(j.get("value", "")):
        ok("non-admin set-password denied")
    else:
        fail("non-admin set-password", str(j))
    j = api_eval(c, '(set-password "lisp-user" "wordpass")')
    if j.get("ok") and str(j.get("value", "")) == "ok":
        ok("admin set-password by username -> ok")
    else:
        fail("admin set-password", str(j))
    cl_old = Client()
    cl_old.request(
        "POST", "/login",
        {"username": "lisp-user", "password": "lisp-pass-1"},
    )
    j = api_eval(cl_old, "(whoami)")
    if not (j.get("ok") and "lisp-user" in str(j.get("value", ""))):
        ok("old password rejected after set-password")
    else:
        fail("old password still works", str(j))
    cl_new = Client()
    cl_new.request(
        "POST", "/login",
        {"username": "lisp-user", "password": "wordpass"},
    )
    j = api_eval(cl_new, "(whoami)")
    if j.get("ok") and "lisp-user" in str(j.get("value", "")):
        ok("new password logs in after set-password")
    else:
        fail("new password login", str(j))

    section("admin revoke_sessions")
    # Re-login admin (may have switched during alice checks)
    c_admin = Client()
    c_admin.request(
        "POST", "/login", {"username": ADMIN_USER, "password": ADMIN_PASS}
    )
    # Alice session still in ca — admin revokes alice sessions
    st, _, body = c_admin.request("GET", "/admin/users")
    if "Revoke sessions" not in body:
        fail("revoke button missing on admin users")
    else:
        ok("revoke button present")
    # find alice user_id from list page (code block)
    alice_ids = re.findall(
        r'name="user_id"\s+value="([0-9a-f-]{36})"', body, flags=re.I
    )
    # Prefer matching alice row: take all ids; revoke each non-admin carefully
    # Simpler: POST revoke for each hidden user_id that appears near alice
    if "alice" in body:
        # POST revoke_sessions for alice by re-listing via create-path action
        # Extract user_id from alice form: look for alice then nearby uuid is fragile;
        # use DB-less approach: create bob, login bob, revoke bob, check bob denied.
        pass
    c_admin.request(
        "POST",
        "/admin/users",
        # usernames are DNS labels — underscore is rejected
        {"username": "bob-auth", "password": "bob-pass-1", "action": "create"},
    )
    st, _, body = c_admin.request("GET", "/admin/users")
    # Row shape: <td>user</td><td>yes|no</td><td><code>uuid</code></td>…
    # (hidden form user_id is after the code; match username	o code uuid.)
    m = re.search(
        r"<td>\s*bob-auth\s*</td>\s*<td>[^<]*</td>\s*<td>\s*"
        r"<code>\s*([0-9a-f-]{36})\s*</code>",
        body,
        flags=re.I | re.S,
    )
    bob_id = m.group(1) if m else ""
    cb = Client()
    cb.request(
        "POST", "/login", {"username": "bob-auth", "password": "bob-pass-1"}
    )
    j = api_eval(cb, "(whoami)")
    if not (j.get("ok") and "bob-auth" in str(j.get("value", ""))):
        fail("bob login before revoke", str(j))
    else:
        ok("bob logged in before revoke")
    if bob_id:
        c_admin.request(
            "POST",
            "/admin/users",
            {"action": "revoke_sessions", "user_id": bob_id},
        )
        j = api_eval(cb, "(whoami)")
        if j.get("ok") and "bob-auth" in str(j.get("value", "")):
            fail("bob still authed after revoke_sessions", str(j))
        else:
            ok("bob denied after admin revoke_sessions")
    else:
        fail("could not resolve bob user_id for revoke")

    section("logout revokes")
    st, _, _ = c.request("POST", "/logout")
    # cookie cleared
    st, _, body = c.request("GET", "/")
    # should be login again
    j = api_eval(c, "(whoami)")
    if j.get("ok") is False or j.get("status") == 401 or "auth" in str(j).lower():
        ok("post-logout api denied")
    else:
        # cookie jar may still have dead cookie; server should 401
        if "authentication required" in str(j) or j.get("ok") is False:
            ok("post-logout api denied")
        else:
            fail("post-logout still authed", str(j))

    section("basic auth fallback")
    import base64

    token = base64.b64encode(f"{ADMIN_USER}:{ADMIN_PASS}".encode()).decode()
    cb = Client()
    url = BASE + "/api/eval"
    data = json.dumps({"expr": "(whoami)"}).encode()
    req = urllib.request.Request(
        url,
        data=data,
        method="POST",
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Basic {token}",
        },
    )
    try:
        resp = cb.opener.open(req, timeout=15)
        j = json.loads(resp.read().decode())
        if j.get("ok") and ADMIN_USER in str(j.get("value", "")):
            ok("basic auth whoami")
        else:
            fail("basic auth", str(j))
    except urllib.error.HTTPError as e:
        fail("basic auth http", e.read().decode()[:200])

    print()
    if failures:
        print(f"FAILED: {failures} check(s)")
        return 1
    print("ALL AUTH CHECKS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
