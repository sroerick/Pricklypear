#!/bin/ksh
# town-metrics.sh - P0 digest + alert checks for the pricklypear.club town box.
# Spec: docs/monitoring-spec.md (cactus monitoring). Zero product code:
# read-only Postgres queries plus filesystem reads; writes one state file.
#
# Install (root, town):
#   /usr/local/sbin/town-metrics.sh          this file, 755 root:wheel
#   /etc/pricklypear-metrics.conf            optional, 600 root:wheel
#   root crontab:
#     0  15 * * *  /usr/local/sbin/town-metrics.sh digest
#     7  *  * * *  /usr/local/sbin/town-metrics.sh alerts
#
# Config /etc/pricklypear-metrics.conf (sourced ksh):
#   NTFY_URL=https://ntfy.sh/<topic>   when set, digest/alerts push here
#   MEDIA_MB_PER_TENANT=64             must match profiles/town @limit
#
# Modes:
#   digest  one line of counts: stdout (cron mail) + ntfy when configured.
#   alerts  threshold checks; fires via ntfy and logger(1). Unimplemented
#           here by design: 429-storm needs usage_events (P1 denial
#           receipts), 5xx-spike needs a request log with statuses (P2).

set -u
PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin	# cron PATH lacks /usr/local/bin (curl)

CONF=/etc/pricklypear-metrics.conf
STATE=/var/db/town-metrics.state
DB=pricklypear
MEDIA_MB_PER_TENANT=64
NTFY_URL=

[ -r "$CONF" ] && . "$CONF"

log() { logger -t town-metrics "$*"; }

psql_one() {  # $1 = SQL returning one scalar; empty output on failure
	su - _postgresql -c "psql -d $DB -At -v ON_ERROR_STOP=1 -c \"$1\"" 2>/dev/null
}

notify() {  # $1 = title, $2 = body
	if [ -n "$NTFY_URL" ]; then
		curl -sS -m 10 -H "Title: $1" -d "$2" "$NTFY_URL" >/dev/null 2>&1 \
			|| log "ntfy push failed: $1"
	fi
}

db_ready() {  # $1 = first query result; die loudly if the DB is unreachable
	if [ -z "$1" ]; then
		print "town-metrics: psql query failed (db $DB unreachable?)" >&2
		exit 2
	fi
}

cmd_digest() {
	users=$(psql_one "select count(*) from users")
	db_ready "$users"
	logins24h=$(psql_one "select count(*) from auth_log where success and happened_at > now() - interval '24 hours'")
	writes24h=$(psql_one "select count(*) from data_rows where created_at > now() - interval '24 hours'")
	media_n=$(psql_one "select count(*) from media_blobs")
	media_bytes=$(psql_one "select coalesce(sum(byte_size),0) from media_blobs")
	schedules=$(psql_one "select count(*) from schedules")
	db_mb=$(( $(psql_one "select pg_database_size('$DB')") / 1024 / 1024 ))
	disk=$(df -P /var | awk 'NR==2 {print $5}')
	line="town $(date -u +'%FT%TZ') users=$users logins24h=$logins24h writes24h=$writes24h 429s=na(p1) media=$media_n/$(( media_bytes / 1024 / 1024 ))MB db=${db_mb}MB disk_used=$disk schedules=$schedules"
	print "$line"
	notify "town digest" "$line"
}

cmd_alerts() {
	cap=$(( MEDIA_MB_PER_TENANT * 1024 * 1024 ))

	hot=$(su - _postgresql -c "psql -d $DB -At -v ON_ERROR_STOP=1 -c \"select author||':'||sum(byte_size)||'/'||$cap from media_blobs group by author having sum(byte_size) > $cap * 8 / 10\"" 2>/dev/null)
	if [ -n "$hot" ]; then
		log "ALERT media pressure: $hot"
		notify "town ALERT media pressure" "$hot"
	fi

	logins48h=$(psql_one "select count(*) from auth_log where success and happened_at > now() - interval '48 hours'")
	db_ready "$logins48h"
	if [ "$logins48h" -eq 0 ]; then
		log "ALERT dead-box: 0 successful logins in 48h"
		notify "town ALERT dead-box" "0 successful logins in 48h on $DB"
	fi

	users=$(psql_one "select count(*) from users")
	db_ready "$users"
	prev=0
	[ -r "$STATE" ] && prev=$(cat "$STATE")
	if [ "$users" -gt "$prev" ]; then
		log "ALERT new signup: $prev -> $users"
		notify "town ALERT new signup" "users $prev -> $users on $DB"
	fi
	print "$users" > "$STATE"
}

case "${1:-digest}" in
digest)	cmd_digest ;;
alerts)	cmd_alerts ;;
*)	print "usage: $0 [digest|alerts]" >&2; exit 2 ;;
esac
