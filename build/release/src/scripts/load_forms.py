#!/usr/bin/env python3
"""Load selected top-level Nopales forms from a .pp file to the habitat
via pp_eval (POST /api/eval), one form per request. Forms selected by
a substring match on the stripped form text."""
import json, os, subprocess, sys, urllib.request, base64

sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
sys.path.insert(0, "/home/roerick/dev/wyo.tech/pricklypear/scripts")
from load_upsets_lib import split_forms, strip_comments

def load(path, needles):
    text = open(path).read()
    forms = split_forms(strip_comments(text))
    picked = [f for f in forms if any(n in f for n in needles)]
    url = os.environ["PP_URL"].rstrip("/") + "/api/eval"
    user, pw = os.environ["PP_USER"], os.environ["PP_PASS"]
    auth = base64.b64encode(f"{user}:{pw}".encode()).decode()
    for f in picked:
        body = json.dumps({"expr": f.replace("\n", " ")}).encode()
        req = urllib.request.Request(url, data=body, headers={
            "Authorization": f"Basic {auth}", "Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                out = json.load(r)
        except Exception as e:
            print("FAIL", f.split("\n")[0][:60], "->", e)
            sys.exit(1)
        ok = out.get("ok")
        print(("ok  " if ok else "ERR "), f.split("\n")[0][:70])
        if not ok:
            print("     ", str(out.get("error"))[:200])
            sys.exit(1)
    print(f"{len(picked)} form(s) loaded to {os.environ.get('PP_URL')}")

if __name__ == "__main__":
    load(sys.argv[1], sys.argv[2:])
