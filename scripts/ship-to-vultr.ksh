#!/bin/ksh
# Ship Pricklypear to the Vultr runtime host.
#
# Modes:
#   1) On wyo.town (build host), from a checkout:
#        /bin/ksh scripts/ship-to-vultr.ksh
#   2) From laptop, rsync sources to wyo.town then remote-build+ship:
#        /bin/ksh scripts/ship-to-vultr.ksh --from-laptop
#
# Env knobs:
#   TARGET_HOST   default 155.138.208.45
#   TARGET_USER   default deploy
#   TARGET_ROOT   default root@TARGET_HOST for promote
#   BUILD_HOST    default wyo.town (laptop mode)
#   BUILD_DIR     default ~/apps/pricklypear on build host

set -eu

PP_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TARGET_HOST="${TARGET_HOST:-155.138.208.45}"
TARGET_USER="${TARGET_USER:-deploy}"
TARGET_STAGE="/home/deploy/incoming"
BUILD_HOST="${BUILD_HOST:-wyo.town}"
BUILD_DIR="${BUILD_DIR:-apps/pricklypear}"
MODE="${1:-}"

log() { print -ru2 -- "[ship] $*"; }
die() { log "$*"; exit 1; }

build_and_pack() {
  cd "$PP_ROOT"
  /bin/ksh scripts/build-release-openbsd.ksh "$PP_ROOT/build"
}

ship_archive() {
  archive="$1"
  [ -f "${archive}" ] || die "missing archive ${archive}"

  if git rev-parse HEAD >/dev/null 2>&1; then
    release_id="$(git rev-parse --short=12 HEAD)"
  else
    release_id="$(date -u +%Y%m%d%H%M%S)"
  fi

  remote_path="${TARGET_STAGE}/${release_id}.tar.gz"
  log "scp ${archive} -> ${TARGET_USER}@${TARGET_HOST}:${remote_path}"
  ssh -o BatchMode=yes "${TARGET_USER}@${TARGET_HOST}" "mkdir -p ${TARGET_STAGE}"
  scp -o BatchMode=yes "${archive}" "${TARGET_USER}@${TARGET_HOST}:${remote_path}"

  log "promote as root on ${TARGET_HOST}"
  ssh -o BatchMode=yes "root@${TARGET_HOST}" \
    "/bin/ksh -c 'if [ -x /usr/local/sbin/pricklypear-promote ]; then
        exec /usr/local/sbin/pricklypear-promote ${remote_path} ${release_id}
      else
        # first promote: extract promote script from archive
        tmp=\$(mktemp -d /tmp/pp-first.XXXXXX)
        tar -C \"\$tmp\" -xzf ${remote_path}
        /bin/ksh \"\$tmp/pricklypear-promote.ksh\" ${remote_path} ${release_id}
        rm -rf \"\$tmp\"
      fi'"
  log "ship complete release_id=${release_id}"
}

case "${MODE}" in
  --from-laptop)
    log "rsync sources to ${BUILD_HOST}:${BUILD_DIR}"
    ssh -o BatchMode=yes "${BUILD_HOST}" "mkdir -p ${BUILD_DIR}"
    rsync -az --delete \
      --exclude _build --exclude .git --exclude build \
      "${PP_ROOT}/" "${BUILD_HOST}:${BUILD_DIR}/"
    # ensure .git metadata for MANIFEST if present
    if [ -d "${PP_ROOT}/.git" ]; then
      sha="$(git -C "${PP_ROOT}" rev-parse HEAD)"
      ssh -o BatchMode=yes "${BUILD_HOST}" \
        "cd ${BUILD_DIR} && mkdir -p .git && print -r -- ${sha} > .git/HEAD.sha"
    fi
    log "remote build + ship on ${BUILD_HOST}"
    ssh -o BatchMode=yes "${BUILD_HOST}" \
      "cd ${BUILD_DIR} && /bin/ksh scripts/ship-to-vultr.ksh"
    ;;
  "")
    # Assume we are already on OpenBSD build host
    uname -s | grep -q OpenBSD || die "build must run on OpenBSD (use --from-laptop)"
    archive="$(build_and_pack)"
    ship_archive "${archive}"
    ;;
  *)
    die "usage: $0 [--from-laptop]"
    ;;
esac
