# Cactus monitoring spec (town instance)

Goal: one glance answers "is anything alive on the cactus"; alerts land in
minutes (G4); counts and usernames only, never content.

Baseline 2026-09-13: 2 users (admin, smoketenant), 11 logins/24h, 0 data
rows, 919 seeded defines, 2 media blobs, 0 schedules. Machinery green.

## What exists today

Postgres (127.0.0.1, db `pricklypear`): `users`, `credentials`, `auth_log`
(kind/success/source_ip/happened_at), `data_rows` (created_at/author),
`media_blobs`, `function_defs`, `schedules`, `sessions`.

Gaps: rate-limit denials are in-memory only (no trace), there is no admin
metrics view, and nobody gets woken up when something breaks.

## Design

### 1. usage_events (new migration, app change)

Single append-only table: `id, kind, username, at, meta jsonb`.

Kinds: `signup`, `login_ok`, `login_fail`, `eval`, `write`, `media_put`,
`media_denied`, `rate_denied`, `schedule_create`.

One INSERT per event, written inline by the app. Retention 90 days via a
cleanup pass in the same migration runner. This table is the substrate for
everything below.

### 2. Denial receipts (tier_limits.ml, app change)

On 429 write-denial: INSERT `rate_denied` with the window snapshot in meta.
On media-quota denial: `media_denied`. Cost is one INSERT on the failure
path only. After this, "how many agents bumped the limit" is a query, and
abuse is visible instead of silent.

### 3. Admin metrics endpoint (GET /admin/metrics, admin-only)

JSON + simple HTML, reading mostly from existing tables plus usage_events:

- totals: users, claimed, logins 24h/7d, DAU/WAU, writes 24h/7d, media
  count/bytes, schedules, rate_denied 24h
- per-tenant table: last login, writes 7d, media bytes, 429 count

Auth: same admin gate as /admin/users. Tier limits apply as everywhere.

### 4. Daily digest (cron, no product code - ship first)

Root cron on town, 15:00Z: run the counts query (see
`scripts/town-metrics.sh`, to be added) and push one ntfy line: users,
logins 24h, writes 24h, 429s 24h, media bytes, disk free. Ntfy topic = the
operator's existing one; configured in the cron env, not in the repo.

### 5. Alerts (G4, read the digest query on a loop)

| Alert | Condition | Why |
|-------|-----------|-----|
| dead-box | 0 successful logins in 48h | image down or auth broken |
| 429 storm | >50 rate_denied/hour | abuse or a broken client |
| media pressure | any tenant >80% quota | capacity before complaints |
| new signup | instant | nice-to-know ping |
| 5xx spike | >1% of requests/hour | phase 2, needs request log |

### 6. Privacy stance

Counts, usernames, timestamps. No content, no payloads, no IP retention
beyond what auth_log already keeps. Matches the fail-closed profile.

## Phases

- **P0 (no app change):** digest cron + counts query. Covers the daily
  question immediately.
- **P1 (next app touch, rides with tier_limits work):** usage_events
  migration, denial receipts, /admin/metrics.
- **P2:** alert loop reading metrics, ntfy wiring, 5xx tracking.
