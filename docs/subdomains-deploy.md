# Username subdomains on prod (*.pricklypear.rocks) — deploy notes

Card 57a8d68c (2026-10-17). Kernel half was already in (auth.user-subdomains):
`http_core.ml` `classify_host` parses `<user>.<base>` into `Host_garden`,
`username_dns_ok` validates names, `post_login` redirects to the garden URL,
and the session cookie gains `Domain=.<base>` + `Secure` when `PP_BASE_DOMAIN`
is set. What this deployment adds is the env + proxy plumbing.

## What was deployed

1. `scripts/openbsd/pricklypear.env.example` now carries
   `PP_BASE_DOMAIN=pricklypear.rocks` (documented).
2. `scripts/openbsd/pricklypear-promote.ksh` carries that key into the live
   `/etc/pricklypear/pricklypear.env` ensure-if-missing (never overwrites an
   operator value; unset requires removing it from the example too).
3. `scripts/openbsd/nginx-subdomains.conf.example` is the wildcard server
   block (see below).

## Deploy path

The env activation rides the normal release pipeline: the release tarball
ships the new promote script; the promote that installs it self-installs the
script from the archive, so the env key is applied by the NEXT promote
(two-release dance — one to install the script, one to run it). Git pushes to
the `wyo` remote are auto-promoted by the post-receive hook.

## Verification (after the second promote)

```sh
curl -sS https://pricklypear.rocks/health            # apex shared faces, ok:true
curl -sk https://roerick.pricklypear.rocks/health    # garden host (cert warn until wildcard cert)
curl -sk -u "$PP_USER:$PP_TOKEN" -o /dev/null -w '%{redirect_url}\n' \
  https://roerick.pricklypear.rocks/notes            # authed non-owner -> own garden URL
```

## Remaining (root on the runtime host — roerick)

- TLS: a wildcard cert for `*.pricklypear.rocks` via LE DNS-01 (zone is at
  ns3/ns4.epik.com; per-host HTTP-01 cannot scale to usernames). Install to
  `/etc/ssl/pricklypear.rocks.wild.fullchain.pem` +
  `/etc/ssl/private/pricklypear.rocks.wild.key`.
- nginx: install `scripts/openbsd/nginx-subdomains.conf.example` as
  `/etc/nginx/conf.d/pricklypear-subdomains.conf`, `nginx -t`,
  `rcctl reload nginx`.

## Risk note

With `PP_BASE_DOMAIN` live, `post_login` redirects existing users to their
`https://<user>.pricklypear.rocks/` garden, and new session cookies carry
`Domain=.pricklypear.rocks; Secure`. Old host-only apex cookies stay valid;
bookmarks still work (apex serves the shared faces). Accepted on the card.
