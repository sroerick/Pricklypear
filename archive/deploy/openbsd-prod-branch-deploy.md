# OpenBSD Prod Branch Deploy (Pricklypear)

Dogfood habitat: **build on wyo.town, run on a dedicated OpenBSD VPS**.

## Hosts

| Role | Host | OS | Notes |
|------|------|----|-------|
| Build | `wyo.town` | OpenBSD 7.9 amd64 | `poohstack` opam switch (mirage-crypto 1.2 + awa 0.6 + dream) |
| Runtime | `155.138.208.45` (`pricklypear`) | OpenBSD 7.9 amd64 | Vultr dogfood, ~1 GB / 1 CPU |
| Git forge | `roerick@wyo.town:/home/git/srv/git/pricklypear.git` | | bare repo + post-receive (push as roerick so hook has opam) |

## Public access (live)

**Auth is the image**, not nginx. Session cookie `pp_session` (browser) or
HTTP basic against the image credentials table (agents/`pp_eval`). Hard-public
paths: `/login`, `/logout`, `/health`, plus the public release face
`/welcome`, `/book.pdf`, `/src.tgz`, `/code`, `/code/*`, `/assets/*`.
(`/book` HTML explainer is private.)
Anon `/` redirects to `/welcome`. Everything else requires auth.
Backends stay loopback-only via pf.

| URL | Backend | Edge auth |
|-----|---------|-----------|
| **http://155.138.208.45/** | nginx → image `:18080` | none (app gate) |
| **http://155.138.208.45:8080/** | nginx → supervisor `:17880` | nginx basic (if anything listens) |

```sh
# public health
curl -sS http://155.138.208.45/health
# unauth / -> 303 /login
curl -sS -o /dev/null -D- http://155.138.208.45/ | head
# app basic auth (pp_eval)
curl -sS -u admin:"$APP_PASS" -H 'Content-Type: application/json' \
  -d '{"expr":"(whoami)"}' http://155.138.208.45/api/eval
```

Change **app** passwords via `/admin/users` (or bootstrap env). The old nginx
htpasswd path is only for :8080 admin edge if used:
`/etc/pricklypear/htpasswd` (and optional `/etc/pricklypear/HTTP_BASIC_AUTH.txt`).

Line-protocol REPL front door is still loopback-only (`127.0.0.1:17822`).
SSH tunnel if needed:

```sh
ssh -L 17822:127.0.0.1:17822 root@155.138.208.45
print -r -- "$(cat ~/.ssh/id_ed25519.pub)" | nc 127.0.0.1 17822
```

## Push-to-deploy

```sh
# laptop
cd /home/roerick/dev/wyo.tech/pricklypear
git remote add wyo roerick@wyo.town:/home/git/srv/git/pricklypear.git   # once
git push wyo master
```

**Why `roerick@` not `git@`?** The post-receive hook needs the `poohstack`
opam switch under `/home/roerick/.opam`. Pushing as `git@` runs the hook as
the `git` user (no opam). Later we can switch to `doas -u roerick` like todo/quack.

Flow:

1. bare repo receives `master`
2. hook `/home/git/srv/git/pricklypear.git/hooks/post-receive`
3. runs `/home/roerick/bin/pricklypear-post-receive` (roerick, poohstack)
4. updates `~/apps/pricklypear`, `dune build`, packs release
5. `scp` → `deploy@155.138.208.45:incoming/<sha12>.tar.gz`
6. `root@vultr` runs `/usr/local/sbin/pricklypear-promote`

Direct ship without git (from wyo.town checkout):

```sh
/bin/ksh scripts/ship-to-vultr.ksh
```

## Runtime layout (Vultr)

| Path | Purpose |
|------|---------|
| `/usr/local/libexec/pricklypear/releases/<id>` | Versioned release trees |
| `/usr/local/libexec/pricklypear/current` | Live release symlink |
| `/etc/pricklypear/pricklypear.env` | Runtime env
| `PP_IO` in env | `lwt` (default) or `eio` (Httpun_eio + Io_eio PG) | (640 `root:_pricklypear`) — **quote values with spaces** |
| `/var/log/pricklypear/{authd,supervisor}.log` | Service logs |
| `/home/deploy/incoming` | Staged release archives |
| `/etc/nginx/nginx.conf` | reverse proxy :80 (app auth) / :8080 (optional basic) |
| `/etc/pricklypear/htpasswd` | nginx basic users for :8080 only |
| `/etc/pf.conf` | allow 22, 80, 8080 only (backends blocked) |

### Users

| User | Role |
|------|------|
| `deploy` | SSH for release stage (laptop + wyo.town keys) |
| `_pricklypear` | rc.d runtime |
| `_postgresql` | Postgres |

### Packages (runtime)

`postgresql-server`, `postgresql-contrib`, `curl`, `git`, `rsync`, `libev`, `gmp`

### Env vars (must match OCaml)

```
PP_DB_HOST PP_DB_PORT PP_DB_NAME PP_DB_USER PP_DB_PASS
PP_AUTH_DB_USER PP_AUTH_DB_PASS
PP_AUTH_PORT PP_ADMIN_PORT PP_SSH_PORT
PP_IMAGE_BIN PP_BOOTSTRAP_ADMIN_KEY   # quote the pubkey
```

## Scripts (in-repo)

| Script | Role |
|--------|------|
| `scripts/build-release-openbsd.ksh` | `dune build` + tar on OpenBSD |
| `scripts/ship-to-vultr.ksh` | pack + scp + promote |
| `scripts/post-receive-prod-hook.ksh` | git hook body (installed as `~/bin/pricklypear-post-receive` on wyo.town) |
| `scripts/openbsd/pricklypear-promote.ksh` | extract release, rc.d, restart |
| `scripts/openbsd/pricklypear-watchdog.ksh` | cron helper: `rcctl check` → `rcctl start` if down |
| `scripts/rc.d/pricklypear` | single-image OpenBSD service |
| `scripts/openbsd/run-image.ksh` | env wrapper (CWD = release root) |

## Services

Single-image model: one process (`pricklypear`), no supervisor/authd.

```sh
rcctl check pricklypear nginx postgresql
rcctl restart pricklypear
rcctl reload nginx
```

OpenBSD does not auto-respawn crashed daemons. A root cron watchdog polls
`rcctl check` every minute and restarts if needed:

```cron
* * * * * /usr/local/sbin/pricklypear-watchdog
```

Promote installs the watchdog binary; install the crontab line once on the
runtime host. Logs go to syslog tag `pricklypear-watchdog`.

| Port | Service |
|------|---------|
| 80 / 443 | nginx → image UI (app session/basic auth) |
| 8080 | optional nginx basic → legacy admin (if present) |
| 18080 | image HTTP (loopback via pf) |
| 17878 | image REPL (loopback) |

## Build-host notes

- Checkout / worktree: `/home/roerick/apps/pricklypear`
- Switch: `poohstack` with **mirage-crypto 1.2 + awa 0.6 + dream** (do not pin crypto back to 0.11)
- wyo.town SSH key is authorized on Vultr as `root` and `deploy` for direct ship

## Still open / later

- [ ] TLS via acme-client + relayd (needs hostname)
- [ ] Expose real awa-ssh or line-protocol publicly (careful)
- [ ] Refresh poohstack switch tarball cache
- [ ] Image port stability if multi-image (relayd currently assumes `:18080`)


## PP_IO (runtime scheduler)

- Default / rollback: omit or `PP_IO=lwt` (httpun-lwt-unix + pgx_lwt).
- Eio: `PP_IO=eio` in `/etc/pricklypear/pricklypear.env`, then `rcctl restart pricklypear`.
- Confirm: `grep connected /var/log/pricklypear/image.log | tail` shows `connected (eio)` and `httpun-eio`.
