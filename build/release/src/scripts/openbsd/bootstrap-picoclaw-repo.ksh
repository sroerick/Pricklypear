#!/bin/ksh
# bootstrap-picoclaw-repo.ksh — provision picoclaw's git workspace on wyo.town
#
# Usage (from roerick, interactive TTY so doas can prompt):
#   ksh ~/bootstrap-picoclaw-repo.ksh          # repo part succeeds; key part via doas
#   doas ksh ~/bootstrap-picoclaw-repo.ksh     # everything, incl. authorized_keys
#
# Non-interactive (e.g. over ssh without a TTY): repo + cgit metadata still
# complete; the authorized_keys step prints instructions instead.
#
# Idempotent: safe to re-run.

set -eu

REPO=/var/www/pub/git/picoclaw-workspace.git
KEY='ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEb0s2H9z0lRnfNkO/ps+YRa6RoeY7iH8VDQCoQccYMl _picoclaw@wyo.town'

# ---------------------------------------------------------------- 1) trust key
# /home/git/.ssh is 0700 git:git — needs root or the git user. doas -u git
# works when roerick has a TTY for the password prompt.
KEY_OK=0
if [ "$(id -u)" = "0" ]; then
    mkdir -p /home/git/.ssh
    chmod 700 /home/git/.ssh
    grep -qF "$KEY" /home/git/.ssh/authorized_keys 2>/dev/null \
        || printf '%s\n' "$KEY" >> /home/git/.ssh/authorized_keys
    chown git:git /home/git/.ssh/authorized_keys
    chmod 600 /home/git/.ssh/authorized_keys
    KEY_OK=1
elif doas -u git ksh -c '
        set -eu
        mkdir -p /home/git/.ssh
        chmod 700 /home/git/.ssh
        grep -qF "$1" /home/git/.ssh/authorized_keys 2>/dev/null \
            || printf "%s\n" "$1" >> /home/git/.ssh/authorized_keys
        chown git:git /home/git/.ssh/authorized_keys
        chmod 600 /home/git/.ssh/authorized_keys
    ' ksh "$KEY"; then
    KEY_OK=1
fi

if [ "$KEY_OK" = "1" ]; then
    echo "[ok] picoclaw key trusted in /home/git/.ssh/authorized_keys"
else
    echo "[!!] NOT DONE: could not update /home/git/.ssh/authorized_keys"
    echo "     (needs root or the git user; doas wanted a password / no TTY)."
    echo "     Fix: re-run this script elevated —  doas ksh ~/bootstrap-picoclaw-repo.ksh"
    echo "     (appends the picoclaw key idempotently, never duplicates)"
fi

# ---------------------------------------------------------------- 2) bare repo
# /var/www/pub/git is git:wheel, g+w — roerick (wheel) can create the repo
# directly; no elevation needed. Then hand group ownership to git so pushes
# arriving as the git user can write.
if [ ! -d "$REPO" ]; then
    git init --bare --shared=group "$REPO" >/dev/null
    echo "[ok] created $REPO"
fi

chgrp -R git "$REPO"
chmod -R g+w "$REPO"
find "$REPO" -type d -exec chmod g+s {} +
# NOTE: don't run `git config`/`git symbolic-ref` here — as root, git's
# safe.directory ownership check rejects a repo owned by another user
# ("fatal: not in a git directory"). --shared=group set the config at init;
# write HEAD directly instead (same bytes symbolic-ref would produce).
printf 'ref: refs/heads/main\n' > "$REPO/HEAD"

echo "[ok] repo ready: group git, g+w, sharedRepository=group, HEAD -> main"

# ------------------------------------------------------- 3) cgit metadata
# Fixes cgit "Bad file descriptor" / blank description for new repos.
printf '%s\n' "picoclaw workspace: docs, memory, scripts" > "$REPO/description"
printf '%s\n' "picoclaw" > "$REPO/owner"
chmod 644 "$REPO/description" "$REPO/owner"
chgrp git "$REPO/description" "$REPO/owner"
chmod g+w "$REPO/description" "$REPO/owner"
echo "[ok] description + owner set for cgit"

# ---------------------------------------------------------------- summary
# (file-based reads — safe under doas/root where git would hit the
# safe.directory ownership check)
echo
echo "repo:     $REPO"
echo "bare:     $([ -f "$REPO/HEAD" ] && [ -d "$REPO/objects" ] && echo true || echo false)"
echo "HEAD:     $(cat "$REPO/HEAD")"
echo "shared:   $(grep -i sharedRepository "$REPO/config" | tr -d '\t' || echo '(not set)')"
echo "push URL: git@wyo.town:/var/www/pub/git/picoclaw-workspace.git"
echo "web:      https://git.wyo.town/picoclaw-workspace"
[ "$KEY_OK" = "1" ] && echo "key:      trusted — picoclaw can push now" \
                    || echo "key:      NOT trusted yet — see [!!] above"
