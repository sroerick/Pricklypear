-- 037_db_safety_net.sql — server-side timeout safety net (BORGE-1790987430)
--
-- Incident 2026-10-02 (pricklypear.rocks): recurring /api/eval stalls, twice
-- that day. A backend (pid 572) ran BEGIN + one SELECT on tree_nodes and then
-- went `idle in transaction` for 30+ minutes holding transactionid 3255420.
-- With all four server-side guards at their defaults (0 = disabled),
-- PostgreSQL never reclaims it. It blocked a chain of UPDATE tree_nodes
-- writers (22-26 min each, Lock/transactionid then Lock/tuple, confirmed via
-- pg_blocking_pids). The image serializes DB work through one pgx connection
-- per identity, so every subsequent eval and page route queued behind the
-- stuck writers -> 499s/504s, while /health stayed green (it only does
-- SELECT 1 — the lock-blind leg, BORGE-1790987437). Recovery was
-- pg_terminate_backend(572) / an image restart.
--
-- FIX: role-level session defaults on the IMAGE role — the role that applies
-- this migration, expressed as CURRENT_USER. In dev that is postgres
-- (scripts/dev.sh runs image as PP_DB_USER); on prod it is pricklypear (the
-- promote hook connects as PP_DB_USER = the image role). `ALTER ROLE ... SET`
-- is a login-time default: it takes effect on NEW sessions only, so the
-- migration session itself is unaffected, and it applies only to connections
-- made as this role (admin sessions as other roles keep their own settings).
-- Per PG docs, "ordinary roles can only set defaults for themselves" — so a
-- non-superuser image role may ALTER ROLE CURRENT_USER without CREATEROLE.
--
-- Values (justification in the plan + commit body; measured against the
-- known legitimate single-op floor of ~80s for notes/doc/get on big docs,
-- logged in /issues):
--
--   idle_in_transaction_session_timeout = 180000  (3 min)
--     Primary fix. A leaked reader/writer that goes idle INSIDE an open
--     transaction is reaped by Postgres after 3 min, releasing its locks.
--     3 min is 10x shorter than the observed 30-min leak and 2.25x above
--     the 80s legit floor. The image's connection-per-identity pool idles
--     BETWEEN transactions, so this never fires on a healthy idle session.
--   statement_timeout = 300000  (5 min)
--     Bounds a genuinely runaway statement (infinite SQL loop, catastrophic
--     scan). statement_timeout counts lock-wait time, so it is a second
--     backstop for the blocked-writer class. 5 min = 3.75x headroom over
--     the 80s notes/doc/get floor, and it would have killed the 22-26 min
--     runaway writers in the incident. (The plan's suggested 120s gives only
--     40s headroom over the known floor — too tight for a "generous but
--     finite" net.)
--   lock_timeout = 30000  (30 s)
--     A blocked writer errors instead of queueing behind a long-lived lock
--     holder. Normal traffic's worst lock holds are single-row upserts
--     (sub-second); boot/backfill lock holds precede serving (the image is
--     not accepting requests while it seeds), so no legitimate 30s+ lock
--     wait exists on the interactive path. Surfaces a wait as an error the
--     image's with_transaction can roll back and re-raise, instead of an
--     indefinite hang.
--
-- Deliberately NOT set:
--   idle_session_timeout: the image's pool idles a connection per identity
--   BETWEEN transactions; reaping idle-but-not-in-transaction sessions would
--   break the pool. Only the in-transaction variant is safe here.
--
-- Deployment sessions are explicitly opted OUT of the net so a future
-- backfill migration is never constrained by the very net it is installing:
-- scripts/dev.sh apply_migrations and the promote hook's apply_migrations
-- both set PGOPTIONS="-c statement_timeout=0 -c lock_timeout=0
-- -c idle_in_transaction_session_timeout=0" for their own psql connections.
--
-- Idempotent + additive: ALTER ROLE ... SET re-issued with the same value is
-- a no-op, and the schema_migrations version row makes the whole file run
-- once anyway. No schema/data rewrite, no trigger changes, no backfill.

ALTER ROLE CURRENT_USER SET idle_in_transaction_session_timeout = 180000;
ALTER ROLE CURRENT_USER SET statement_timeout            = 300000;
ALTER ROLE CURRENT_USER SET lock_timeout                 = 30000;

INSERT INTO schema_migrations (version, description)
SELECT 37, 'db safety net: idle-in-tx 3min / statement 5min / lock 30s role defaults (BORGE-1790987430)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 37);
