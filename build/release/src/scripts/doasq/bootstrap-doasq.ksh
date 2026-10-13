#!/bin/ksh
# doasq bootstrap — the ONE root command that stands the queue up.
# Review this file, then run:  doas ksh /home/hollis/staging/doasq/bootstrap-doasq.ksh
#
# What it does (idempotent):
#   1. /var/doasq tree with the exact perm model
#   2. installs doasq + doasq-exec to /usr/local/bin
#   3. ONE doas.conf rule: permit nopass hollis cmd /usr/local/bin/doasq-exec
#   4. migrates any staged requests from /home/hollis/doasq-queue
# Root stays with roerick: this standing grant is a single cmd, and every
# use of it is tied to an approved request + full audit.

set -u

STAGE=/home/hollis/staging/doasq
Q=/var/doasq

die() { print -u2 -- "bootstrap: $*"; exit 1; }
[ "$(id -un)" = "root" ] || die "run as root (doas ksh $0)"
[ -f "$STAGE/bin/doasq" ] || die "staged files missing at $STAGE"

print -- "== 1. queue tree"
mkdir -p "$Q/requests" "$Q/approvals" "$Q/runs" || die "mkdir failed"
chown hollis:hollis "$Q/requests";  chmod 755 "$Q/requests"
chown roerick:roerick "$Q/approvals"; chmod 750 "$Q/approvals"
chown root:wheel "$Q" "$Q/runs";    chmod 755 "$Q" "$Q/runs"
touch "$Q/audit.log"; chown root:wheel "$Q/audit.log"; chmod 644 "$Q/audit.log"

print -- "== 2. install CLI + runner"
install -m 755 -o root -g wheel "$STAGE/bin/doasq" /usr/local/bin/doasq
install -m 755 -o root -g wheel "$STAGE/bin/doasq-exec" /usr/local/bin/doasq-exec

print -- "== 3. doas rule"
rule='permit nopass hollis cmd /usr/local/bin/doasq-exec'
if grep -qsF "$rule" /etc/doas.conf; then
  print -- "   already present"
else
  cp /etc/doas.conf /etc/doas.conf.pre-doasq 2>/dev/null || true
  print -r -- "$rule" >> /etc/doas.conf || die "cannot append to /etc/doas.conf"
  print -- "   added: $rule"
fi

print -- "== 4. migrate staged requests"
if [ -d /home/hollis/doasq-queue/requests ]; then
  mv /home/hollis/doasq-queue/requests/*.req "$Q/requests/" 2>/dev/null &&
    print -- "   moved" || print -- "   none staged"
fi

print -- "== done. hollis: doasq list / doasq submit; roerick: doasq approve ID"
print -- "doasq.conf rule stands alone: root is untouched; every exec ties to an approved, hashed, audited request."
