#!/usr/bin/env python3
"""pp-suno.py -- generate songs on a self-hosted ACE-Step pod (RunPod)
from the laptop, then push the MP3 into the Pricklypear media store.

Why a laptop-side script: the prod image's http-post returns an empty
body intermittently/deterministically toward some endpoints, so in-habitat
generation against the pod can't be relied on yet (kernel-level, host
access needed). This script uses the laptop network stack, which works,
and gives the pod's output a permanent home in the habitat.

Env:
  PP_URL / PP_USER / PP_PASS  habitat + creds (same as pp_eval)
  SUNO_ACE_URL                pod API base, e.g. https://<pod>-8000.proxy.runpod.net
  SUNO_HF_TOKEN               optional, only if the pod needs it (not for the template)

Usage:
  python3 scripts/pp_suno.py --caption "synthwave, warm pads, 110bpm" \
      --lyrics "[Verse 1]..." --title "Neon Rain" --duration 120
  python3 scripts/pp_suno.py --caption "dungeon ambient loop" --instrumental
  python3 scripts/pp_suno.py --poll <task_id>          # check a running task

Steps: POST /release_task -> poll /query_result until status 1 -> download
/v1/audio?path=... -> pp_media put --title ... -> music-track row -> print id.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import time
import urllib.error
import urllib.request

from pathlib import Path


def env_or(name: str, default: str = "") -> str:
    v = os.environ.get(name, default)
    return v if v is not None else default


def http_json(url: str, payload: dict | None = None, timeout: int = 120) -> dict:
    data = None if payload is None else json.dumps(payload).encode()
    req = urllib.request.Request(url, data=data, method="POST" if payload is not None else "GET")
    req.add_header("Content-Type", "application/json")
    req.add_header("User-Agent", "pricklypear-pp-suno/1.0 (laptop)")
    last = None
    for attempt in range(4):
        try:
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                return json.loads(resp.read().decode())
        except Exception as e:  # transient retries
            last = e
            time.sleep(2)
    raise SystemExit(f"transport failed after retries: {last}")


def http_get_bytes(url: str, timeout: int = 180) -> bytes:
    last = None
    for attempt in range(4):
        try:
            req = urllib.request.Request(url)
            req.add_header("User-Agent", "pricklypear-pp-suno/1.0 (laptop)")
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                return resp.read()
        except Exception as e:
            last = e
            time.sleep(2)
    raise SystemExit(f"download failed after retries: {last}")


def ace_base() -> str:
    base = env_or("SUNO_ACE_URL").rstrip("/")
    if not base:
        raise SystemExit("SUNO_ACE_URL not set (pod API base)")
    return base


def submit(base: str, caption: str, lyrics: str, duration: int | None, batch: int = 2) -> str:
    payload: dict = {"prompt": caption, "batch_size": batch}
    if lyrics:
        payload["lyrics"] = lyrics
    if duration:
        payload["audio_duration"] = max(10, min(600, int(duration)))
    r = http_json(f"{base}/release_task", payload)
    if r.get("code") != 200:
        raise SystemExit(f"submit failed: {r}")
    task_id = r["data"]["task_id"]
    print(f"queued task {task_id}", file=sys.stderr)
    return task_id


def poll(base: str, task_id: str, max_wait: int = 600) -> dict:
    t0 = time.time()
    while time.time() - t0 < max_wait:
        r = http_json(f"{base}/query_result", {"task_id_list": [task_id]})
        if r.get("code") == 200 and r.get("data"):
            elem = r["data"][0]
            status = elem.get("status")
            if status == 1:
                tracks = json.loads(elem.get("result") or "[]")
                return {"status": "SUCCESS", "tracks": tracks}
            if status == 2:
                raise SystemExit(f"task failed: {elem}")
        print(f"  pending... ({int(time.time()-t0)}s)", file=sys.stderr)
        time.sleep(10)
    raise SystemExit("timed out waiting for the task")


def api_eval(expr: str) -> dict:
    """POST /api/eval with HTTP Basic (same env as pp_eval)."""
    import base64
    url = env_or("PP_URL").rstrip("/")
    user, pw = env_or("PP_USER", "admin"), env_or("PP_PASS")
    req = urllib.request.Request(
        url + "/api/eval",
        data=json.dumps({"expr": expr}).encode(), method="POST")
    req.add_header("Content-Type", "application/json")
    req.add_header("Authorization", "Basic " +
                   base64.b64encode(f"{user}:{pw}".encode()).decode())
    with urllib.request.urlopen(req, timeout=120) as resp:
        return json.loads(resp.read().decode())


def habitat_upload(path: Path, title: str, caption: str) -> dict:
    """PUT bytes into the habitat media store, then register the track.
Returns {media_id, track_id} on success; raises SystemExit otherwise."""
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    import subprocess, shutil
    pm = shutil.which("python3") or sys.executable
    r = subprocess.run(
        [pm, str(Path(__file__).parent / "pp_media.py"), "put", str(path),
         "--type", "audio/mpeg"],
        capture_output=True, text=True, timeout=300,
    )
    out = (r.stdout or "") + (r.stderr or "")
    print(out.strip()[-600:], file=sys.stderr)
    if r.returncode != 0:
        raise SystemExit("media upload failed (see above)")
    media_id = (r.stdout or "").strip().splitlines()[-1].strip()
    fields = json.dumps({"title": title, "artist": "ace-step",
                         "album": "", "media_id": media_id, "notes": caption})
    e = api_eval("(create 'music-track \"%s\")" % fields.replace('"', '\\"'))
    if not e.get("ok"):
        raise SystemExit("music-track row failed: " + json.dumps(e))
    track_id = e.get("value") if isinstance(e.get("value"), str) else json.dumps(e.get("value"))
    return {"media_id": media_id, "track_id": track_id}


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--poll", metavar="TASK_ID")
    ap.add_argument("--caption", default="")
    ap.add_argument("--lyrics", default="")
    ap.add_argument("--title", default="")
    ap.add_argument("--duration", type=int, default=None)
    ap.add_argument("--instrumental", action="store_true")
    ap.add_argument("--batch", type=int, default=2, help="songs per task (ACE max 8)")
    ap.add_argument("--no-upload", action="store_true", help="download only")
    args = ap.parse_args()

    base = ace_base()

    if args.poll:
        result = poll(base, args.poll)
        print(json.dumps(result, indent=1)[:4000])
        return

    caption = args.caption or args.title
    lyrics = "" if args.instrumental else args.lyrics
    if not caption and not lyrics:
        raise SystemExit("give --caption and/or --lyrics")

    task_id = submit(base, caption, lyrics, args.duration, args.batch)
    print(f"submitted task {task_id}", file=sys.stderr)

    result = poll(base, task_id)
    tracks = result.get("tracks") or []
    if not tracks:
        raise SystemExit("no tracks in result")

    os.makedirs("/tmp/pp-suno", exist_ok=True)
    downloaded = []
    for i, t in enumerate(tracks, 1):
        furl = t.get("file") or ""
        url = furl if furl.startswith("http") else f"{base}{furl}"
        out = Path(f"/tmp/pp-suno/{task_id[:8]}-{i}.mp3")
        print(f"downloading {url} -> {out}", file=sys.stderr)
        out.write_bytes(http_get_bytes(url))
        downloaded.append((out, t))

    for i, (out, t) in enumerate(downloaded, 1):
        size = out.stat().st_size
        print(json.dumps({"file": str(out), "bytes": size,
                          "prompt": t.get("prompt", caption),
                          "duration": (t.get("metas") or {}).get("duration")}))

    if args.no_upload:
        for out, _t in downloaded:
            print(f"saved: {out}")
        return

    if not args.title:
        args.title = caption[:60] or "Untitled"
    for idx, (out, _t) in enumerate(downloaded, 1):
        title = f"{args.title}" if len(downloaded) == 1 else f"{args.title} ({idx})"
        info = habitat_upload(out, title, caption)
        print(json.dumps({"uploaded": {"title": title, **info}}))


if __name__ == "__main__":
    main()
