#!/bin/ksh
# pricklypear-watchdog — cull orphans + restart the image when it is down.
#
# OpenBSD has no systemd Restart=always. rc.d starts once at boot; if the
# single-image process fatals (e.g. uncaught httpaf closed-writer), it stays
# dead until something restarts it. Install as:
#   /usr/local/sbin/pricklypear-watchdog
# and run from root crontab every minute:
#   * * * * * /usr/local/sbin/pricklypear-watchdog
#
# Liveness gate: the service's own rc_check (health-only by deliberate
# design, 2026-08-21 lesson — "a healthy listener IS a healthy service").
# Do NOT re-add pidfile checks to rc_check; pidfile awareness lives HERE.
#
# 2026-09-03 orphan cull (promote-window task 5): rc.subr (v1.167) gates its
# stop command on rc_check passing —
#     stop)  _rc_do rc_check || exit 0
# so `rcctl stop` is a NO-OP exactly when this watchdog runs (rc_check just
# failed): it never tears down an unhealthy service and never culls an
# ORPHAN — an image still holding :18080 off-pidfile. That blind spot is the
# 2026-09-03 prod incident: after the promote restart died EADDRINUSE, the
# watchdog "restarted" without ever stopping anything, and its "started OK"
# was measured against the ORPHAN still answering /health. So on a failed
# rc_check (past boot grace) the watchdog culls by itself — belt TERM +
# bounded port-free poll + straggler KILL, mirroring rc_stop's teardown tail
# in /etc/rc.d/pricklypear — then rcctl start. The post-start recheck is
# pidfile-aware: ok:true alone no longer yields "started OK" (a surviving
# orphan answers /health too while the pidfile stays dead).

set -eu

PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin"

service="${PP_SERVICE:-pricklypear}"
lockdir="${PP_WATCHDOG_LOCK:-/var/run/pricklypear/watchdog.lock}"
runtime_dir="${PP_RUNTIME_DIR:-/var/run/pricklypear}"
service_user="_pricklypear"

# Same resolution order as run-image.ksh: source the env file, THEN resolve
# knobs, so the watchdog inspects the same pidfile and ports the image uses.
env_file="${PP_ENV_FILE:-/etc/pricklypear/pricklypear.env}"
if [ -f "${env_file}" ]; then
	# shellcheck disable=SC1090
	. "${env_file}"
fi
pidfile="${PP_IMAGE_PIDFILE:-${runtime_dir}/image.pid}"
http_port="${PP_HTTP_PORT:-18080}"
repl_port="${PP_REPL_PORT:-17878}"
# Bounded recheck after start (replaces the blind `sleep 20`): poll every
# STEP up to MAX seconds. Prod seed boot measured 10-33s (2026-09-03), so
# the fixed 20s declared failure on slow-but-successful boots; 90s leaves
# margin. Env-tunable for rehearsals.
recheck_max="${PP_WATCHDOG_RECHECK_MAX:-90}"
recheck_step="${PP_WATCHDOG_RECHECK_STEP:-2}"

log() {
	logger -t pricklypear-watchdog -- "$*"
	print -ru2 -- "[watchdog] $*"
}

# Serialize overlapping cron runs (slow start / stuck health polls).
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
# starts only ever make an additional fresh one. (Corollary: past this gate
# every image process is >= 90s old, so any image found holding the port by
# the orphan check below is automatically older than the boot grace.)
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

# --- teardown helpers (mirror /etc/rc.d/pricklypear; keep in sync) --------

# True (0) if some tcp socket is LISTENing on $1. Same probe as rc_stop's.
port_listens() {
	netstat -anf inet 2>/dev/null | grep LISTEN \
		| grep -Eq "[.]${1}([[:space:]]|$)"
}

# Poll until port $1 is free. $2 = max seconds (default 10), 0.25s steps.
# Returns 1 if still held at expiry.
wait_port_free() {
	port="${1:?wait_port_free: port required}"
	max=$(( (${2:-10}) * 4 ))
	i=0
	while [ "${i}" -lt "${max}" ]; do
		port_listens "${port}" || return 0
		sleep 0.25
		i=$((i + 1))
	done
	port_listens "${port}" || return 0
	return 1
}

# Pids LISTENing on tcp/$1: fstat listener lines carry the local address and
# NO `<--`/`-->` peer arrow; pid is field 3. Same awk as rc.d's
# rc_kill_port_holders (formats captured from the OpenBSD 7.9 prod host).
port_holder_pids() {
	fstat 2>/dev/null | awk -v port="${1}" '
		$5 == "internet" && $7 == "tcp" &&
		index($0, "<--") == 0 && index($0, "-->") == 0 &&
		$9 ~ "[.:]" port "$" { print $3 }' | sort -u
}

# Print the pidfile pid only if it names a live process; else fail.
live_pidfile_pid() {
	[ -f "${pidfile}" ] || return 1
	wppid="$(cat "${pidfile}" 2>/dev/null)" || return 1
	case "${wppid}" in ''|*[!0-9]*) return 1 ;; esac
	kill -0 "${wppid}" 2>/dev/null || return 1
	print -r -- "${wppid}"
}

# Print a description of the task-5 ORPHAN if one exists: an image process
# LISTENing on the HTTP port while the pidfile names no live pid. Age beyond
# the boot grace is implied — the gate above exits while ANY image is < 90s.
orphan_desc() {
	for pid in $(port_holder_pids "${http_port}"); do
		comm="$(ps -p "${pid}" -o comm= 2>/dev/null)" || continue
		[ "${comm}" = "image" ] || continue
		if live_pidfile_pid >/dev/null 2>&1; then
			continue	# pidfile names a live pid — not an orphan
		fi
		if [ -f "${pidfile}" ]; then
			stale="dead"
		else
			stale="missing"
		fi
		age="$(ps -p "${pid}" -o etime= 2>/dev/null || true)"
		print -r -- "image pid ${pid} (up ${age:-?}) holds tcp/${http_port} while pidfile pid is ${stale}"
		return 0
	done
	return 1
}

# Teardown that actually runs when rc_check has failed (rcctl stop cannot:
# rc.subr's stop exits early on a failed rc_check — see header). Belt TERM,
# bounded port-free poll on BOTH listen ports, straggler KILL restricted to
# image processes (this runs autonomously from cron: never kill arbitrary
# port holders — log them for a human instead), pidfile rm. Keep in sync
# with rc_stop in /etc/rc.d/pricklypear.
cull() {
	pkill -U "${service_user}" -f '/bin/image' 2>/dev/null || true
	for p in "${http_port}" "${repl_port}"; do
		if wait_port_free "${p}" 10; then
			continue
		fi
		for pid in $(port_holder_pids "${p}"); do
			comm="$(ps -p "${pid}" -o comm= 2>/dev/null)" || comm=""
			if [ "${comm}" != "image" ]; then
				log "tcp/${p} held by non-image pid ${pid} (${comm:-?}); not killing autonomously"
				continue
			fi
			log "kill -KILL straggler image pid ${pid} still holding tcp/${p}"
			kill -KILL "${pid}" 2>/dev/null || true
		done
		if ! wait_port_free "${p}" 2; then
			log "tcp/${p} STILL HELD after cull; start may EADDRINUSE"
		fi
	done
	rm -f "${pidfile}" 2>/dev/null || true
}

# --- restart path (rc_check failed, past boot grace) ----------------------

# Name the orphan case explicitly: this log line is the post-mortem
# breadcrumb for "health was measured against a ghost" (2026-09-03).
if orphan="$(orphan_desc)"; then
	log "orphan cull: ${orphan} — treating as down"
fi
log "${service} down or unhealthy; culling and restarting"
cull

if rcctl start "${service}" >/dev/null 2>&1; then
	# Bounded, pidfile-aware recheck (2026-09-03): ok:true alone is NOT
	# success — a surviving orphan answers /health too. Require rc_check
	# ok AND a live pidfile pid before declaring "started OK"; warn once
	# (not per poll) when health is ok but the pidfile is stale.
	waited=0
	orphan_warn=0
	while [ "${waited}" -lt "${recheck_max}" ]; do
		sleep "${recheck_step}"
		waited=$((waited + recheck_step))
		if ! rcctl check "${service}" >/dev/null 2>&1; then
			continue
		fi
		if npid="$(live_pidfile_pid)"; then
			log "${service} started OK (pid ${npid}, boot took ~${waited}s)"
			exit 0
		fi
		if [ "${orphan_warn}" -eq 0 ]; then
			orphan_warn=1
			log "health ok but pidfile names no live pid — a surviving orphan may be answering; not declaring success"
		fi
	done
	log "${service} start returned success but recheck failed after ${waited}s (see /var/log/pricklypear/image.log)"
	exit 1
fi

log "${service} start failed; see /var/log/pricklypear/image.log"
exit 1
