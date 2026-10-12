# town-metrics.sh - alert checks for the pricklypear.club town box.
# Spec: docs/monitoring-spec.md (cactus monitoring). Zero product code:
# local liveness probes + filesystem reads; writes one state file.
#
# 2026-10-06 revision (roerick: endpoint reports OFF ntfy, dead-box pages
# wanted): digest mode kept for manual runs but pushes nothing; alerts =
# tuna local liveness (127.0.0.1:18090, page after 2 consecutive hourly
# misses, one clear on recovery) + disk pressure. The old PP-db checks
# (logins48h dead-box, media pressure, new-signup) are gone - PP has been
# dark on town since 09-15 and they structurally misfired. Outside-in
# dead-box watch lives on vultr (public-audit/box-watch.sh, 10-min).
#
# Install (root, town):
#   /usr/local/sbin/town-metrics.sh          this file, 755 root:wheel
#   /etc/pricklypear-metrics.conf            600 root:wheel, NTFY_URL set
#   root crontab:
#     7  *  * * *  /usr/local/sbin/town-metrics.sh alerts
#     (15:00Z digest line retired 10-06)
#
# Config /etc/pricklypear-metrics.conf (sourced ksh):
#   NTFY_URL=https://ntfy.sh/<topic>   ALERTS push here (alerts only)

set -u
PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin	# cron PATH lacks /usr/local/bin (curl)

CONF=/etc/pricklypear-metrics.conf
FAILS=/var/db/tuna-watch.state
NTFY_URL=

[ -r "$CONF" ] && . "$CONF"

log() { logger -t town-metrics "$*"; }

notify() {  # $1 = title, $2 = body
	if [ -n "$NTFY_URL" ]; then
		curl -sS -m 10 -H "Title: $1" -d "$2" "$NTFY_URL" >/dev/null 2>&1 \
			|| log "ntfy push failed: $1"
	fi
}

cmd_digest() {  # mail/logger only - pushes nothing (roerick 10-06)
	disk=$(df -P /var | awk 'NR==2 {print $5}')
	line="town $(date -u +'%FT%TZ') digest retired 10-06; alerts=dead-box(tuna)+disk; disk_used=$disk"
	print "$line"
	log "$line"
}

cmd_alerts() {
	alive=$(curl -s -m 5 -o /dev/null -w '%{http_code}' http://127.0.0.1:18090/ 2>/dev/null)
	prev=0; [ -r "$FAILS" ] && prev=$(cat "$FAILS")
	if [ -z "$alive" ] || [ "$alive" = 000 ]; then
		n=$((prev + 1)); print "$n" > "$FAILS"
		if [ "$n" -eq 2 ]; then
			log "ALERT dead-box: tuna :18090 unreachable x$n"
			notify "town ALERT dead-box" "tuna 127.0.0.1:18090 no answer x$n (hourly local check)"
		else
			log "dead-box fail#$n: tuna :18090 no answer"
		fi
	else
		if [ "$prev" -ge 2 ]; then
			log "clear dead-box: tuna :18090 answering ($alive)"
			notify "town clear dead-box" "tuna :18090 answering again ($alive)"
		fi
		print 0 > "$FAILS"
	fi

	disk=$(df -P /var | awk 'NR==2 {sub(/%/,""); print $5}')
	if [ "$disk" -ge 90 ]; then
		log "ALERT disk: /var at ${disk}%"
		notify "town ALERT disk" "/var used ${disk}% (town)"
	fi
}

case "${1:-alerts}" in
digest)	cmd_digest ;;
alerts)	cmd_alerts ;;
*)	print "usage: $0 [digest|alerts]" >&2; exit 2 ;;
esac
