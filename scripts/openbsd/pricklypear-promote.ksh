#!/bin/ksh
# Promote a Pricklypear release archive on the runtime host (OpenBSD).
# Run as root.
#
# Single-image model: one service (pricklypear), one process.
#
# Usage:
#   /usr/local/sbin/pricklypear-promote <archive.tar.gz> <release-id>

set -eu

PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin"

if [ $# -ne 2 ]; then
  print -ru2 -- "usage: $0 <release-archive> <release-id>"
  exit 1
fi

archive="$1"
release_id="$2"

release_root="${RELEASE_ROOT:-/usr/local/libexec/pricklypear}"
releases_dir="${release_root}/releases"
current_link="${release_root}/current"
previous_link="${release_root}/previous"
config_dir="${CONFIG_DIR:-/etc/pricklypear}"
config_file="${config_dir}/pricklypear.env"
config_group="${CONFIG_GROUP:-_pricklypear}"
runtime_user="${RUNTIME_USER:-_pricklypear}"
service="pricklypear"
health_url="${HEALTH_URL:-http://127.0.0.1:18080/health}"
target_dir="${releases_dir}/${release_id}"
tmpdir="$(mktemp -d /tmp/pp-promote.XXXXXXXXXX)"
stage_dir="${tmpdir}/release"
previous_target=""

log() { print -ru2 -- "[promote] $*"; }
die() { log "failed: $*"; exit 1; }

cleanup() { rm -rf "${tmpdir}"; }
trap cleanup EXIT HUP INT TERM

[ -f "${archive}" ] || die "archive not found: ${archive}"
[ ! -e "${target_dir}" ] || die "release already exists: ${target_dir}"

install -d -o root -g wheel -m 0755 "${release_root}" "${releases_dir}"
install -d -o root -g "${config_group}" -m 0750 "${config_dir}"
install -d -o "${runtime_user}" -g "${runtime_user}" -m 0750 \
  /var/db/pricklypear /var/log/pricklypear /var/run/pricklypear

mkdir -p "${stage_dir}"
tar -C "${stage_dir}" -xzf "${archive}"

for required in bin/image bin/run-image.ksh \
  rc.d/pricklypear \
  pricklypear.env.example MANIFEST; do
  [ -e "${stage_dir}/${required}" ] || die "archive missing ${required}"
done

chmod 755 "${stage_dir}/bin/image" "${stage_dir}/bin/run-image.ksh"
chmod 555 "${stage_dir}/rc.d/pricklypear"

mv "${stage_dir}" "${target_dir}"

# Install rc.d service
install -o root -g wheel -m 0555 \
  "${target_dir}/rc.d/pricklypear" "/etc/rc.d/${service}"

# Install promote helper system-wide (from archive if present)
if [ -f "${target_dir}/pricklypear-promote.ksh" ]; then
  install -o root -g wheel -m 0555 \
    "${target_dir}/pricklypear-promote.ksh" /usr/local/sbin/pricklypear-promote
fi

# Env example always refreshed; live env only created if missing
install -o root -g "${config_group}" -m 0640 \
  "${target_dir}/pricklypear.env.example" "${config_dir}/pricklypear.env.example"
if [ ! -f "${config_file}" ]; then
  install -o root -g "${config_group}" -m 0640 \
    "${target_dir}/pricklypear.env.example" "${config_file}"
  log "created ${config_file} from example — edit secrets before relying on boot"
fi

# Point release paths at the current symlink (operators + run-image defaults).
ensure_env_key() {
  key="$1"
  val="$2"
  if grep -q "^${key}=" "${config_file}" 2>/dev/null; then
    tmpf="${config_file}.new"
    # OpenBSD sed: rewrite the key line
    sed "s|^${key}=.*|${key}=${val}|" "${config_file}" > "${tmpf}"
    mv "${tmpf}" "${config_file}"
  else
    print -- "${key}=${val}" >> "${config_file}"
  fi
}
ensure_env_key PP_IMAGE_BIN "${current_link}/bin/image"
ensure_env_key PP_LIBS_DIR "${current_link}/libs"
ensure_env_key PP_SOURCE_ROOT "${current_link}/src"
ensure_env_key PP_STATIC_DIR "${current_link}/static"
ensure_env_key PP_PUBLIC_DIR "${current_link}/public"
chown root:"${config_group}" "${config_file}"
chmod 640 "${config_file}"

if [ -L "${current_link}" ]; then
  previous_target="$(readlink "${current_link}")"
  [ -n "${previous_target}" ] && ln -sfn "${previous_target}" "${previous_link}"
fi

ln -sfn "${target_dir}" "${current_link}"

# Enable the service
rcctl enable "${service}" 2>/dev/null || true

rollback() {
  [ -n "${previous_target}" ] || return 1
  log "rolling back to ${previous_target}"
  ln -sfn "${previous_target}" "${current_link}"
  rcctl restart "${service}" >/dev/null 2>&1 || true
}

# Drop the stale image listener (one process, one port, clean shutdown).
pkill -U "${runtime_user}" 2>/dev/null || true
sleep 1
rm -f /var/run/pricklypear/*.pid 2>/dev/null || true

log "restart ${service}"
if ! rcctl restart "${service}"; then
  rcctl start "${service}" || {
    rollback || true
    die "image failed to start; see /var/log/pricklypear/image.log"
  }
fi

# Health check (best-effort)
ok=0
attempts=0
while [ "${attempts}" -lt 20 ]; do
  body="$(ftp -o - "${health_url}" 2>/dev/null || true)"
  if print -r -- "${body}" | grep -qi '"ok"'; then
    ok=1
    break
  fi
  attempts=$((attempts + 1))
  sleep 1
done

if [ "${ok}" -ne 1 ]; then
  log "WARN: health endpoint not ready after promote"
  log "  image log: /var/log/pricklypear/image.log"
fi

log "promoted ${release_id}"
log "current -> ${current_link} -> ${target_dir}"
cat "${target_dir}/MANIFEST" 2>/dev/null || true
