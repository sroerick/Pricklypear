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

if rcctl check "${service}" >/dev/null 2>&1; then
  exit 0
fi

log "${service} down; attempting start"
if rcctl start "${service}" >/dev/null 2>&1; then
  # Give boot a moment (stdlib seed + PG) before recheck.
  sleep 3
  if rcctl check "${service}" >/dev/null 2>&1; then
    log "${service} started OK"
    exit 0
  fi
  log "${service} start returned success but check still fails"
  exit 1
fi

log "${service} start failed; see /var/log/pricklypear/image.log"
exit 1
