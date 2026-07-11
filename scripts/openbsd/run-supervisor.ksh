#!/bin/ksh
# Wrapper: source env, point at image binary, exec supervisor, write pidfile.
set -eu

ENV_FILE="${PP_ENV_FILE:-/etc/pricklypear/pricklypear.env}"
RELEASE_ROOT="${PP_RELEASE_ROOT:-/usr/local/libexec/pricklypear/current}"
LOG_DIR="${PP_LOG_DIR:-/var/log/pricklypear}"
PIDFILE="${PP_SUPERVISOR_PIDFILE:-/var/run/pricklypear/supervisor.pid}"
BIN="${RELEASE_ROOT}/bin/supervisor"

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
export PP_ADMIN_PORT="${PP_ADMIN_PORT:-17880}"
export PP_SSH_PORT="${PP_SSH_PORT:-17822}"
export PP_AUTH_HOST="${PP_AUTH_HOST:-127.0.0.1}"
export PP_AUTH_PORT="${PP_AUTH_PORT:-17879}"
export PP_IMAGE_BIN="${PP_IMAGE_BIN:-${RELEASE_ROOT}/bin/image}"
export PP_LIBS_DIR="${PP_LIBS_DIR:-${RELEASE_ROOT}/libs}"
# Ensure child images inherit DB settings via Unix.environment ()
export PP_IMAGE_BIN
export PP_LIBS_DIR

mkdir -p "$(dirname "${PIDFILE}")" "${LOG_DIR}"
print -r -- "$$" > "${PIDFILE}"
trap 'rm -f "${PIDFILE}"' EXIT HUP INT TERM

# Supervisor resolves migrations/ relative to CWD — must be release root.
cd "${RELEASE_ROOT}"
exec "${BIN}" >>"${LOG_DIR}/supervisor.log" 2>&1
