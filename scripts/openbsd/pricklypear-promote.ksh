#!/bin/ksh
# Promote a Pricklypear release archive on the runtime host (OpenBSD).
# Run as root.
#
# Usage:
#   /usr/local/sbin/pricklypear-promote <archive.tar.gz> <release-id>
#
# Or from a staged incoming path after scp:
#   /usr/local/sbin/pricklypear-promote /home/deploy/incoming/abc123.tar.gz abc123

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
auth_service="pricklypear_authd"
sup_service="pricklypear_supervisor"
health_auth="${HEALTH_AUTH_URL:-http://127.0.0.1:17879/health}"
health_sup="${HEALTH_SUP_URL:-http://127.0.0.1:17880/health}"
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

for required in bin/authd bin/supervisor bin/image \
  bin/run-authd.ksh bin/run-supervisor.ksh \
  rc.d/pricklypear_authd rc.d/pricklypear_supervisor \
  pricklypear.env.example MANIFEST; do
  [ -e "${stage_dir}/${required}" ] || die "archive missing ${required}"
done

# Normalize perms
chmod 755 "${stage_dir}/bin/authd" "${stage_dir}/bin/supervisor" \
  "${stage_dir}/bin/image" \
  "${stage_dir}/bin/run-authd.ksh" "${stage_dir}/bin/run-supervisor.ksh"
chmod 555 "${stage_dir}/rc.d/pricklypear_authd" \
  "${stage_dir}/rc.d/pricklypear_supervisor"

mv "${stage_dir}" "${target_dir}"

# Install rc.d scripts
install -o root -g wheel -m 0555 \
  "${target_dir}/rc.d/pricklypear_authd" "/etc/rc.d/${auth_service}"
install -o root -g wheel -m 0555 \
  "${target_dir}/rc.d/pricklypear_supervisor" "/etc/rc.d/${sup_service}"

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

# Force PP_IMAGE_BIN in live env to current release image path
if grep -q '^PP_IMAGE_BIN=' "${config_file}" 2>/dev/null; then
  tmpf="${config_file}.new"
  sed "s|^PP_IMAGE_BIN=.*|PP_IMAGE_BIN=${current_link}/bin/image|" \
    "${config_file}" > "${tmpf}"
  mv "${tmpf}" "${config_file}"
  chown root:"${config_group}" "${config_file}"
  chmod 640 "${config_file}"
else
  print -- "PP_IMAGE_BIN=${current_link}/bin/image" >> "${config_file}"
fi

if [ -L "${current_link}" ]; then
  previous_target="$(readlink "${current_link}")"
  [ -n "${previous_target}" ] && ln -sfn "${previous_target}" "${previous_link}"
fi

ln -sfn "${target_dir}" "${current_link}"

# Enable services
rcctl enable "${auth_service}" 2>/dev/null || true
rcctl enable "${sup_service}" 2>/dev/null || true

rollback() {
  [ -n "${previous_target}" ] || return 1
  log "rolling back to ${previous_target}"
  ln -sfn "${previous_target}" "${current_link}"
  rcctl restart "${auth_service}" >/dev/null 2>&1 || true
  rcctl restart "${sup_service}" >/dev/null 2>&1 || true
}

# Drop stale listeners before restart (avoids EADDRINUSE on small boxes)
pkill -U "${runtime_user}" 2>/dev/null || true
sleep 1
rm -f /var/run/pricklypear/*.pid 2>/dev/null || true

log "restart ${auth_service}"
if ! rcctl restart "${auth_service}"; then
  # first boot may need start
  rcctl start "${auth_service}" || {
    rollback || true
    die "authd failed to start; see /var/log/pricklypear/authd.log"
  }
fi

sleep 1
log "restart ${sup_service}"
if ! rcctl restart "${sup_service}"; then
  rcctl start "${sup_service}" || {
    rollback || true
    die "supervisor failed to start; see /var/log/pricklypear/supervisor.log"
  }
fi

# Health checks (best-effort; ftp -o - works on OpenBSD base)
ok=0
attempts=0
while [ "${attempts}" -lt 20 ]; do
  auth_body="$(ftp -o - "${health_auth}" 2>/dev/null || true)"
  sup_body="$(ftp -o - "${health_sup}" 2>/dev/null || true)"
  if print -r -- "${auth_body}" | grep -qi . && \
     print -r -- "${sup_body}" | grep -qi .; then
    ok=1
    break
  fi
  attempts=$((attempts + 1))
  sleep 1
done

if [ "${ok}" -ne 1 ]; then
  log "WARN: health endpoints not ready after promote (auth/sup may still be migrating)"
  log "  auth log: /var/log/pricklypear/authd.log"
  log "  sup log:  /var/log/pricklypear/supervisor.log"
  # Do not auto-rollback on health — migrations may need a moment; operator checks.
fi

log "promoted ${release_id}"
log "current -> ${current_link} -> ${target_dir}"
cat "${target_dir}/MANIFEST" 2>/dev/null || true
