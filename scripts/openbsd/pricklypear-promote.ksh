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

# Install crash watchdog (cron should call this every minute).
if [ -f "${target_dir}/pricklypear-watchdog.ksh" ]; then
  install -o root -g wheel -m 0555 \
    "${target_dir}/pricklypear-watchdog.ksh" /usr/local/sbin/pricklypear-watchdog
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

# ---------------------------------------------------------------------------
# Apply pending DB migrations BEFORE swapping the release / restarting.
# Migrations ship in the archive (migrations/*.sql) and are idempotent
# (IF NOT EXISTS + CREATE OR REPLACE + a schema_migrations guard row). We
# skip any version already recorded in schema_migrations so expensive
# backfill DO blocks (e.g. 013's 92k-row datum-path rewrite) do not re-run
# on every promote. Runs while the OLD image is still live: additive
# migrations are safe under concurrent trigger fire. A non-additive
# migration (ALTER on a trigger-bearing table after bulk writes) must
# drop/recreate the trigger within the file itself.
# On failure we die before touching the symlink or the running image, so
# the old release keeps serving.
# ---------------------------------------------------------------------------
apply_migrations() {
  mig_dir="${target_dir}/migrations"
  [ -d "${mig_dir}" ] || { log "no migrations/ in release; skipping"; return 0; }
  [ -f "${config_file}" ] || die "missing ${config_file} for DB credentials"
  set -a; . "${config_file}"; set +a
  : "${PP_DB_HOST:?PP_DB_HOST unset in ${config_file}}"
  : "${PP_DB_PORT:?PP_DB_PORT unset in ${config_file}}"
  : "${PP_DB_NAME:?PP_DB_NAME unset in ${config_file}}"
  : "${PP_DB_USER:?PP_DB_USER unset in ${config_file}}"
  : "${PP_DB_PASS:?PP_DB_PASS unset in ${config_file}}"
  ppsql() { PGPASSWORD="${PP_DB_PASS}" psql -h "${PP_DB_HOST}" -p "${PP_DB_PORT}" \
              -U "${PP_DB_USER}" -d "${PP_DB_NAME}" "$@"; }
  # schema_migrations may not exist yet on a fresh DB -> empty means "run all".
  applied="$(ppsql -At -c \
    "SELECT coalesce(string_agg(version::text,','),'') FROM schema_migrations" \
    2>/dev/null)" || applied=""
  n=0
  for f in $(ls "${mig_dir}"/*.sql 2>/dev/null | sort); do
    base="$(basename "$f" .sql)"
    ver=$(( 10#${base%%_*} ))   # 013 -> 13 (strip leading zeros, no octal)
    case ",${applied}," in *",${ver},"*) continue ;; esac
    log "applying migration ${ver} (${base})"
    if ! ppsql -v ON_ERROR_STOP=1 -f "$f"; then
      log "migration ${ver} FAILED; old release still live (current -> $(readlink "${current_link}" 2>/dev/null || echo '?'))"
      return 1
    fi
    n=$((n + 1))
  done
  log "migrations: ${n} applied, rest already-applied"
  return 0
}

apply_migrations || die "database migrations failed; old release still serving"

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

# No pre-restart teardown here (2026-09-03): rc_stop owns teardown — TERM →
# pid-gone poll → pkill belt → bounded port-free poll on BOTH listen ports →
# straggler KILL → pidfile removed. The old script-local `pkill`+`sleep 1`
# raced rcctl restart (blind 1s can be shorter than the image's SIGTERM
# handling → EADDRINUSE boot death) and waited a fixed second for nothing on
# the common path.

# promote_start MUST be captured BEFORE the restart: rcctl restart BLOCKS
# through the image's whole boot (rc_stop teardown + rc_start + pwait), so
# a promote_start taken after it returns POSTdates the new image's
# boot_epoch by the entire boot time. That is exactly the
# 2026-09-05 incident: an 11.7s boot (cold seed) made the healthy NEW image
# answer with boot_epoch < promote_start, the gate read it as the old
# orphan, waited out 30s and rolled back a good release. Captured before
# the restart, boot_epoch >= promote_start still means "started by THIS
# restart" (an orphan old image was already serving, its boot_epoch is
# far older), and a boot finishing after the restart returns passes too.
promote_start=$(date +%s)

log "restart ${service}"
if ! rcctl restart "${service}"; then
  rcctl start "${service}" || {
    rollback || true
    die "image failed to start; see /var/log/pricklypear/image.log"
  }
fi

# Health gate: require ok:true (not mere "ok"), 0.25s x 120 = 30s budget
# (was 20 x 1s — a 1s poll floor on every promote). On timeout do NOT
# declare success: roll back to the previous release and die, so the deploy
# hook sees the failure and prod keeps serving the old release.
# (2026-09-03, promote task 4) ok:true alone is NOT proof: a pre-restart
# ORPHAN still holding :18080 answers ok:true too — that is exactly how the
# 2026-09-03 prod incident served the old release under a green gate. So the
# gate also requires boot_epoch (unix seconds at image boot, emitted by
# /health) >= promote_start: only an image started by THIS restart passes.
# -ge (not >) absorbs `date +%s` integer truncation (a new image booting
# inside promote_start's own second must not fail the gate); an orphan
# false-accept would need the old image to have booted during this promote's
# same second — impossible on the common path since it was already serving
# when promote began. promote_start is captured pre-restart (see above).
ok=0
attempts=0
orphan_seen=0
while [ "${attempts}" -lt 120 ]; do
  body="$(ftp -o - "${health_url}" 2>/dev/null || true)"
  if print -r -- "${body}" | grep -q '"ok"[[:space:]]*:[[:space:]]*true'; then
    boot_epoch="$(print -r -- "${body}" | sed -n 's/.*"boot_epoch"[[:space:]]*:[[:space:]]*\([0-9][0-9]*\).*/\1/p')"
    if [ -n "${boot_epoch}" ] && [ "${boot_epoch}" -ge "${promote_start}" ]; then
      ok=1
      break
    fi
    if [ "${orphan_seen}" -eq 0 ]; then
      orphan_seen=1
      log "ok:true but boot_epoch ${boot_epoch:-missing} < promote_start ${promote_start} — old image/orphan still answering; waiting for the NEW release"
    fi
  fi
  attempts=$((attempts + 1))
  sleep 0.25
done

if [ "${ok}" -ne 1 ]; then
  log "health endpoint not ok:true within 30s of restart"
  log "  image log: /var/log/pricklypear/image.log"
  if rollback; then
    die "health gate failed (30s); rolled back to ${previous_target}"
  else
    die "health gate failed (30s); no previous release to roll back to"
  fi
fi

log "promoted ${release_id}"
log "current -> ${current_link} -> ${target_dir}"
cat "${target_dir}/MANIFEST" 2>/dev/null || true
