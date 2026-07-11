#!/bin/ksh
# Wrapper: source env, force auth DB role, exec authd, write pidfile.
set -eu

ENV_FILE="${PP_ENV_FILE:-/etc/pricklypear/pricklypear.env}"
RELEASE_ROOT="${PP_RELEASE_ROOT:-/usr/local/libexec/pricklypear/current}"
LOG_DIR="${PP_LOG_DIR:-/var/log/pricklypear}"
PIDFILE="${PP_AUTHD_PIDFILE:-/var/run/pricklypear/authd.pid}"
BIN="${RELEASE_ROOT}/bin/authd"

[ -f "${ENV_FILE}" ] || { print -ru2 -- "missing ${ENV_FILE}"; exit 1; }
[ -x "${BIN}" ] || { print -ru2 -- "missing ${BIN}"; exit 1; }

set -a
# shellcheck disable=SC1090
. "${ENV_FILE}"
set +a

# Auth unikernel must connect as the restricted role.
export PP_DB_USER="${PP_AUTH_DB_USER:-pricklypear_auth}"
export PP_DB_PASS="${PP_AUTH_DB_PASS:-${PP_DB_PASS:-}}"
export PP_DB_HOST="${PP_DB_HOST:-127.0.0.1}"
export PP_DB_PORT="${PP_DB_PORT:-5432}"
export PP_DB_NAME="${PP_DB_NAME:-pricklypear}"
export PP_AUTH_PORT="${PP_AUTH_PORT:-17879}"

mkdir -p "$(dirname "${PIDFILE}")" "${LOG_DIR}"
print -r -- "$$" > "${PIDFILE}"
trap 'rm -f "${PIDFILE}"' EXIT HUP INT TERM

# Relative migration paths are not used by authd, but keep CWD stable.
cd "${RELEASE_ROOT}"
exec "${BIN}" >>"${LOG_DIR}/authd.log" 2>&1
