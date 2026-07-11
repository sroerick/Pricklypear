#!/bin/ksh
# post-receive: build on wyo.town (poohstack) and ship to Vultr runtime.
#
# Installed as the bare-repo hook (or called from a thin wrapper).
# Env overrides:
#   BRANCH WORKTREE TARGET_HOST TARGET_USER TARGET_STAGE REMOTE_PROMOTE
#   OPAM_SWITCH

set -eu

PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:${HOME}/.opam/poohstack/bin"

BRANCH="${BRANCH:-master}"
REF="refs/heads/${BRANCH}"
WORKTREE="${WORKTREE:-${HOME}/apps/pricklypear}"
OPAM_SWITCH="${OPAM_SWITCH:-poohstack}"
TARGET_HOST="${TARGET_HOST:-155.138.208.45}"
TARGET_USER="${TARGET_USER:-deploy}"
TARGET_STAGE="${TARGET_STAGE:-/home/deploy/incoming}"
REMOTE_PROMOTE="${REMOTE_PROMOTE:-/usr/local/sbin/pricklypear-promote}"
zero_rev="0000000000000000000000000000000000000000"
lockdir="${WORKTREE}.deploy.lock"
target_rev=""

log() { print -ru2 -- "[post-receive] $*"; }
die() { log "FAILED: $*"; exit 1; }

while read -r _oldrev newrev refname; do
  [ "${refname}" = "${REF}" ] || continue
  [ "${newrev}" = "${zero_rev}" ] && exit 0
  target_rev="${newrev}"
done

[ -n "${target_rev}" ] || exit 0

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
  "${REMOTE_PROMOTE} '${remote_path}' '${release_id}'"

log "deploy OK ${release_id}"
