#!/bin/ksh
# pricklypear-watchdog — restart the image if rcctl check fails.
#
# OpenBSD has no systemd Restart=always. rc.d starts once at boot; if the
# single-image process fatals (e.g. uncaught httpaf closed-writer), it stays
# dead until something restarts it. Install as:
#   /usr/local/sbin/pricklypear-watchdog
# and run from root crontab every minute:
#   * * * * * /usr/local/sbin/pricklypear-watchdog
#
# Uses the service's own rc_check (pidfile + /health), not ad-hoc ps/grep.

set -eu

PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin"

service="${PP_SERVICE:-pricklypear}"
lockdir="${PP_WATCHDOG_LOCK:-/var/run/pricklypear/watchdog.lock}"
runtime_dir="${PP_RUNTIME_DIR:-/var/run/pricklypear}"

log() {
  logger -t pricklypear-watchdog -- "$*"
  print -ru2 -- "[watchdog] $*"
}

# Serialize overlapping cron runs (slow start / stuck ftp health).
if ! mkdir "${lockdir}" 2>/dev/null; then
  exit 0
fi
trap 'rmdir "${lockdir}" 2>/dev/null || true' EXIT

# Ensure runtime dir exists so a wiped /var/run still works after reboot.
install -d -o _pricklypear -g _pricklypear -m 0750 "${runtime_dir}" 2>/dev/null || true

# Boot grace (2026-08-21 dups fix): seed boot takes 10-30s. If a youngest
# image process is fresh (< 90s), it is still booting — hands off. Without
# this, the minute cron RESTARTS the service mid-seed, and the resulting
# orphan (holding :18080, off-pidfile) + dead EADDRINUSE children turned
# into a per-minute restart loop. Look at the YOUNGEST image: overlapping
# starts only ever make an additional fresh one.
youngest="$(ps -axo pid=,etime=,comm= 2>/dev/null \
  | awk '$3 == "image" {print $2}' \
  | awk -F: 'NF==2 {s=$1*60+$2} NF==3 {s=$1*3600+$2*60+$3} NF==4 {s=$1*86400+$2*3600+$3*60+$4} {print s}' \
  | sort -n | head -1)"
if [ -n "${youngest}" ] && [ "${youngest}" -lt 90 ]; then
  exit 0
fi

if rcctl check "${service}" >/dev/null 2>&1; then
  exit 0
fi

# Live-but-unhealthy is common after Eio mutex poison / lost pidfile:
# process may still hold :18080 while rc_check fails on ok:false or missing
# pid. Always stop (pkill + pidfile clear via rc_stop) before start so we
# never hit EADDRINUSE and leave the habitat dark.
log "${service} down or unhealthy; attempting restart"
rcctl stop "${service}" >/dev/null 2>&1 || true
sleep 1
if rcctl start "${service}" >/dev/null 2>&1; then
  # Give boot a real moment (stdlib seed + PG measured 10-30s on prod)
  # before recheck; <3s misread as failure.
  sleep 20
  if rcctl check "${service}" >/dev/null 2>&1; then
    log "${service} started OK"
    exit 0
  fi
  log "${service} start returned success but check still fails"
  exit 1
fi

log "${service} start failed; see /var/log/pricklypear/image.log"
exit 1
