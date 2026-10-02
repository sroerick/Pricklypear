#!/bin/ksh
# post-receive: build on the OpenBSD build host (poohstack) and ship to runtime.
#
# Installed as the bare-repo hook (or called from a thin wrapper).
# Habitat-specific values MUST be set in the hook wrapper / environment
# (no production host defaults in-tree):
#   TARGET_HOST   (required) runtime host
#   BRANCH WORKTREE TARGET_USER TARGET_STAGE REMOTE_PROMOTE OPAM_SWITCH

set -eu

PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:${HOME}/.opam/poohstack/bin"

BRANCH="${BRANCH:-master}"
REF="refs/heads/${BRANCH}"
WORKTREE="${WORKTREE:-${HOME}/apps/pricklypear}"
OPAM_SWITCH="${OPAM_SWITCH:-poohstack}"
TARGET_HOST="${TARGET_HOST:-}"
TARGET_USER="${TARGET_USER:-deploy}"
TARGET_STAGE="${TARGET_STAGE:-/home/deploy/incoming}"
REMOTE_PROMOTE="${REMOTE_PROMOTE:-/usr/local/sbin/pricklypear-promote}"
zero_rev="0000000000000000000000000000000000000000"
lockdir="${WORKTREE}.deploy.lock"
target_rev=""
oldrev=""

log() { print -ru2 -- "[post-receive] $*"; }
die() { log "FAILED: $*"; exit 1; }

while read -r _oldrev newrev refname; do
  [ "${refname}" = "${REF}" ] || continue
  [ "${newrev}" = "${zero_rev}" ] && exit 0
  oldrev="${_oldrev}"
  target_rev="${newrev}"
done

[ -n "${target_rev}" ] || exit 0
[ -n "${TARGET_HOST}" ] || die "set TARGET_HOST in the hook environment (no default)"

BARE_REPO="${GIT_DIR:-}"
if [ -z "${BARE_REPO}" ]; then
  BARE_REPO="$(git rev-parse --absolute-git-dir 2>/dev/null || true)"
fi
[ -n "${BARE_REPO}" ] || die "not running inside a git hook context"
# Absolute-ize and clear hook env so worktree git commands don't use bare GIT_DIR
case "${BARE_REPO}" in
  /*) ;;
  *) BARE_REPO="$(cd "${BARE_REPO}" && pwd)" ;;
esac
unset GIT_DIR GIT_WORK_TREE GIT_QUARANTINE_PATH

mkdir -p "$(dirname "${WORKTREE}")"
mkdir "${lockdir}" 2>/dev/null || die "another deploy is already running"
trap 'rmdir "${lockdir}" 2>/dev/null || true' EXIT HUP INT TERM

if [ ! -d "${WORKTREE}/.git" ]; then
  log "clone worktree ${WORKTREE}"
  git clone "${BARE_REPO}" "${WORKTREE}"
fi

log "sync worktree to ${target_rev}"
git -C "${WORKTREE}" fetch "${BARE_REPO}" "${REF}"
git -C "${WORKTREE}" checkout -B "${BRANCH}" "${target_rev}"
git -C "${WORKTREE}" reset --hard "${target_rev}"

log "build release (opam switch ${OPAM_SWITCH})"
# shellcheck disable=SC1090
eval "$($(command -v opam) env --switch="${OPAM_SWITCH}" --set-switch)"
cd "${WORKTREE}"
/bin/ksh scripts/build-release-openbsd.ksh "${WORKTREE}/build"

archive="${WORKTREE}/build/pricklypear-openbsd.tar.gz"
[ -f "${archive}" ] || die "missing ${archive}"

release_id="$(print -r -- "${target_rev}" | cut -c1-12)"
remote_path="${TARGET_STAGE}/${release_id}.tar.gz"

log "scp -> ${TARGET_USER}@${TARGET_HOST}:${remote_path}"
ssh -o BatchMode=yes "${TARGET_USER}@${TARGET_HOST}" "mkdir -p ${TARGET_STAGE}"
scp -o BatchMode=yes "${archive}" "${TARGET_USER}@${TARGET_HOST}:${remote_path}"

log "promote on ${TARGET_HOST}"
ssh -o BatchMode=yes "root@${TARGET_HOST}" \
  "${REMOTE_PROMOTE} ${remote_path} ${release_id}"

# ----------------------------------------------------------------
# VAULT-DRIFT REMINDER (borge/branching.borg vault-authority, card
# 8dff7c9e). Promote deploys the KERNEL. The additive boot seed only
# ADDS names absent on system/main, so a libs/*.pp edit to an EXISTING
# define is committed but NOT deployed — it stays in the vault while
# prod runs the old body. If this hook has habitat credentials, run the
# body-level drift view for the changed packages so the operator sees
# exactly which .pp edits will not go live. Soft check: credentials
# absent (or pp-sync not built) only prints the reminder.
# ----------------------------------------------------------------
pp_sync_bin="${WORKTREE}/_build/default/cli/bin/pp_sync.exe"
if [ -n "${PP_USER:-}" ] && [ -n "${PP_PASS:-}" ]; then
  if [ ! -x "${pp_sync_bin}" ]; then
    ( cd "${WORKTREE}" && dune build cli/bin/pp_sync.exe ) >/dev/null 2>&1 \
      || true
  fi
fi
if [ -n "${PP_USER:-}" ] && [ -n "${PP_PASS:-}" ] && [ -x "${pp_sync_bin}" ]; then
  changed_pkgs="$(git -C "${WORKTREE}" diff --name-only \
      "${oldrev:-HEAD~1}" "${target_rev}" -- libs/*/lib.pp 2>/dev/null \
    | sed -n 's|^libs/\([^/]*\)/lib\.pp$|\1|p' | sort -u)"
  if [ -n "${changed_pkgs}" ]; then
    log "Nopales .pp changed in this release — checking vault drift:"
    for p in ${changed_pkgs}; do
      "${pp_sync_bin}" status --url "${PP_URL:-https://pricklypear.rocks}" \
        --user "${PP_USER}" --password "${PP_PASS}" "${p}" 2>&1 \
        | sed 's/^/[drift] /' || true
    done
    log "body-stale names above are in git but NOT live — promote them from the"
    log "habitat (personal/dev -> system/main), then pp-sync snapshot to refresh"
    log "the vault. See borge/branching.borg vault-authority."
  fi
else
  log "NOTE: kernel deployed; libs/*.pp edits to existing names are NOT live"
  log "      (additive seed). Check with: pp-sync status <pkg> — see"
  log "      borge/branching.borg vault-authority (card 8dff7c9e)."
fi

# Self-update AFTER success — never overwrite the running script mid-flight
if [ -f "${WORKTREE}/scripts/post-receive-prod-hook.ksh" ]; then
  cp "${WORKTREE}/scripts/post-receive-prod-hook.ksh" \
    "${HOME}/bin/pricklypear-post-receive"
  chmod 755 "${HOME}/bin/pricklypear-post-receive"
fi

log "deploy OK ${release_id}"
