# profiles/

Instance deploy profiles (spec_town_profile, 2026-09-13). One repo,
several shapes: instances differ by profile file, never by a fork.

- No `PP_PROFILE` env: full discovery — every `libs/<pkg>/lib.pp`
  seeds (mainframe behavior, byte-identical).
- `PP_PROFILE=<name>`: `profiles/<name>` is the boot seed set AND the
  tenant-tier load-library allowlist. Admin sessions keep full prims.
- `PP_PROFILE_DIR` overrides the profile directory (default: beside
  `libs/`).

File format: one package name per line; `#` comments; blank lines
ignored. Preferred-order packages (see `Boot_seed.seed_package_priority`)
keep their canonical seed order; other listed packages follow in file
order.

Fail-closed: `PP_PROFILE` set but the file is missing, unreadable, or
empty means NOTHING is seeded and tenants may load nothing. Admin
can still `load-library` to recover. A public box never falls back
to the full organ set.

Tenant tier in a profiled instance: every non-admin session
(`Session_types.restricted`) gets the delegation-loader semantics —
seeded-only `load-library`, no code/route/package persist, no
reseed/deftype. `profiles/town` is the first consumer.
