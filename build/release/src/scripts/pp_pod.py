#!/usr/bin/env python3
"""Run commands on the RunPod pod over its PTY-requiring SSH.

Sends lines slowly with pauses, then drains output until a sentinel or
timeout. Pure stdlib (subprocess + threads)."""
import subprocess
import sys
import time
import threading

HOST = "jzvxeopclkrhj5-64411bed@ssh.runpod.io"
KEY = "/home/roerick/.ssh/id_ed25519"
SENTINEL = "PPPODDONE"

def main() -> None:
    cmds = sys.argv[1:] or ["hostname"]
    lines = list(cmds) + [f"echo {SENTINEL}", "exit"]
    p = subprocess.Popen(
        ["ssh", "-tt", "-o", "BatchMode=yes", "-o",
         "StrictHostKeyChecking=accept-new", "-i", KEY, HOST],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, text=True, bufsize=0,
    )

    out: list[str] = []

    def reader() -> None:
        try:
            while True:
                ch = p.stdout.read(1)
                if not ch:
                    break
                out.append(ch)
        except Exception:
            pass

    # Background reader; dies with the process.
    t = threading.Thread(target=reader, daemon=True)
    t.start()

    # wait for the shell prompt to appear
    def so_far() -> str:
        return "".join(out)

    deadline = time.time() + 30
    while time.time() < deadline:
        s = so_far()
        if "/app#" in s or "root@" in s.split("-- RUNPOD.IO --")[-1]:
            break
        time.sleep(0.3)
    time.sleep(1.5)

    for line in lines:
        p.stdin.write(line + "\n")
        p.stdin.flush()
        time.sleep(1.2)

    # drain until sentinel or process exit
    deadline = time.time() + 180
    while time.time() < deadline:
        if SENTINEL in so_far():
            time.sleep(1)
            break
        if p.poll() is not None:
            break
        time.sleep(0.5)
    try:
        p.kill()
    except Exception:
        pass
    text = so_far()
    # strip echo noise: print only from the first command onward
    sys.stdout.write(text)
    sys.stdout.write(f"\n[[total {len(text)} bytes]]\n")

if __name__ == "__main__":
    main()
