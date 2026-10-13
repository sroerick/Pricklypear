#!/bin/ksh
# doasq web-surface install — the SECOND root command (after the queue
# bootstrap). Review, then:  doas ksh /home/hollis/staging/doasq/web-install.ksh
#
# What it does (idempotent):
#   1. RELOCATES the queue /var/doasq -> /var/www/doasq (the httpd
#      chroot — the CGI can only see inside /var/www). The existing
#      requests/approvals/runs/audit migrate intact; the perm model
#      carries over (requests: hollis; approvals: roerick + www (the
#      web surface); runs/audit: root).
#   2. Installs the CGI to /var/www/cgi-bin/doasq-queue.
#   3. Appends the httpd.conf fragment (the /cgi-bin/doasq-queue*
#      location with htpasswd authentication) — idempotent.
#   4. Creates the htpasswd file PLACEHOLDER — roerick sets the
#      credential himself:  htpasswd /var/www/htpasswds/doasq roerick
#   5. Enables + starts slowcgi, restarts httpd.
#   6. Updates the CLI default (DOASQ_DIR) and re-installs doasq.

set -u

STAGE=/home/hollis/staging/doasq
QOLD=/var/doasq
Q=/var/www/doasq

die() { print -u2 -- "web-install: $*"; exit 1; }
[ "$(id -un)" = "root" ] || die "run as root (doas ksh $0)"
[ -f "$STAGE/cgi/doasq-queue" ] || die "staged CGI missing at $STAGE"

print -- "== 1. relocate the queue into the httpd chroot"
if [ -d "$QOLD" ] && [ ! -d "$Q/requests" ]; then
  mkdir -p "$(dirname "$Q")"
  mv "$QOLD" "$Q" || die "relocation failed"
  print -- "   moved $QOLD -> $Q"
fi
mkdir -p "$Q/requests" "$Q/approvals" "$Q/runs"
chown hollis:hollis "$Q/requests";   chmod 755 "$Q/requests"
chown roerick:www "$Q/approvals";    chmod 770 "$Q/approvals"
chown root:wheel "$Q" "$Q/runs";     chmod 755 "$Q" "$Q/runs"
[ -f "$Q/audit.log" ] || { touch "$Q/audit.log"; chown root:wheel "$Q/audit.log"; chmod 644 "$Q/audit.log"; }
print -- "   perms: requests(hollis) approvals(roerick+www) runs/audit(root)"

print -- "== 2. install the CGI"
install -m 755 -o root -g wheel "$STAGE/cgi/doasq-queue" /var/www/cgi-bin/doasq-queue
print -- "   /var/www/cgi-bin/doasq-queue"

print -- "== 3. re-install the CLI with the new default path"
sed 's|DOASQ_DIR="${DOASQ_DIR:-/var/doasq}"|DOASQ_DIR="${DOASQ_DIR:-/var/www/doasq}"|' \
  "$STAGE/bin/doasq" > /usr/local/bin/doasq.new || die "cli rewrite failed"
mv /usr/local/bin/doasq.new /usr/local/bin/doasq
chmod 755 /usr/local/bin/doasq
print -- "   doasq default -> /var/www/doasq"

print -- "== 4. htpasswd placeholder"
mkdir -p /var/www/htpasswds
touch /var/www/htpasswds/doasq
chown root:www /var/www/htpasswds/doasq; chmod 640 /var/www/htpasswds/doasq
print -- "   (roerick sets the credential:) htpasswd /var/www/htpasswds/doasq roerick"

print -- "== 5. httpd.conf fragment"
frag='location "/cgi-bin/doasq-queue*" {
    authenticate "doasq" "/htpasswds/doasq"
}'
if grep -qs "doasq-queue" /var/www/conf/httpd.conf 2>/dev/null || \
   grep -qs "doasq-queue" /etc/httpd.conf 2>/dev/null; then
  print -- "   already present"
else
  httpd_conf="$(grep -ls "server \"" /var/www/conf/httpd.conf /etc/httpd.conf 2>/dev/null | head -1)"
  [ -n "$httpd_conf" ] || die "httpd.conf not found"
  cp "$httpd_conf" "${httpd_conf}.pre-doasq"
  {
    print ""
    print -- "$frag"
  } >> "$httpd_conf"
  print -- "   appended to $httpd_conf (review it)"
fi

print -- "== 6. slowcgi + httpd"
rcctl enable slowcgi 2>/dev/null || true
rcctl get slowcgi 2>/dev/null | grep -q state || rcctl start slowcgi 2>/dev/null || true
rcctl restart httpd 2>/dev/null || true

print -- "== done. roerick: set the htpasswd, then https://wyo.town/cgi-bin/doasq-queue"
