#!/bin/ksh
# Wrapper: source env, exec the single image binary, write pidfile.
set -eu

ENV_FILE="${PP_ENV_FILE:-/etc/pricklypear/pricklypear.env}"
RELEASE_ROOT="${PP_RELEASE_ROOT:-/usr/local/libexec/pricklypear/current}"
LOG_DIR="${PP_LOG_DIR:-/var/log/pricklypear}"
PIDFILE="${PP_IMAGE_PIDFILE:-/var/run/pricklypear/image.pid}"
BIN="${RELEASE_ROOT}/bin/image"

[ -f "${ENV_FILE}" ] || { print -ru2 -- "missing ${ENV_FILE}"; exit 1; }
[ -x "${BIN}" ] || { print -ru2 -- "missing ${BIN}"; exit 1; }

set -a
# shellcheck disable=SC1090
. "${ENV_FILE}"
set +a

export PP_DB_HOST="${PP_DB_HOST:-127.0.0.1}"
export PP_DB_PORT="${PP_DB_PORT:-5432}"
export PP_DB_NAME="${PP_DB_NAME:-pricklypear}"
export PP_DB_USER="${PP_DB_USER:-pricklypear}"
export PP_DB_PASS="${PP_DB_PASS:-}"
export PP_USERNAME="${PP_USERNAME:-admin}"
export PP_BRANCH_NAME="${PP_BRANCH_NAME:-main}"
export PP_BRANCH_ID="${PP_BRANCH_ID:-00000000-0000-0000-0000-000000000001}"
export PP_HTTP_PORT="${PP_HTTP_PORT:-18080}"
export PP_REPL_PORT="${PP_REPL_PORT:-17878}"
export PP_LIBS_DIR="${PP_LIBS_DIR:-${RELEASE_ROOT}/libs}"
# Public release face: code browser + static CM + book/tgz
export PP_SOURCE_ROOT="${PP_SOURCE_ROOT:-${RELEASE_ROOT}/src}"
export PP_STATIC_DIR="${PP_STATIC_DIR:-${RELEASE_ROOT}/static}"
export PP_PUBLIC_DIR="${PP_PUBLIC_DIR:-${RELEASE_ROOT}/public}"

mkdir -p "$(dirname "${PIDFILE}")" "${LOG_DIR}"
print -r -- "$$" > "${PIDFILE}"
trap 'rm -f "${PIDFILE}"' EXIT HUP INT TERM

cd "${RELEASE_ROOT}"
exec "${BIN}" >>"${LOG_DIR}/image.log" 2>&1
