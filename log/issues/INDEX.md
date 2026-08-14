# log/issues (pulled from habitat notes doc `log/issues`)

**Fix loop:** [`FIX-LOOP.md`](FIX-LOOP.md) — handoff instructions to fix all 7.

- [i-fields-as-string-2026-08-12](log/issues/i-fields-as-string-2026-08-12.md) — DESIGN: CRUD returns fields as VStr of JSON text (triple-escaping in api/eval)
- [i-findrows-orderby-2026-08-12](log/issues/i-findrows-orderby-2026-08-12.md) — SMELL: bible-specific ORDER BY baked into generic find-rows
- [i-rls-nontx-2026-08-12](log/issues/i-rls-nontx-2026-08-12.md) — BUG: RLS floor inactive on non-transactional CRUD reads
- [i-rootbody-1](log/issues/i-rootbody-1.md) — BUG: notes/tree/save-outline drops root body
- [i-seed-list-3copies-2026-08-12](log/issues/i-seed-list-3copies-2026-08-12.md) — SMELL: seed type list maintained in 3 places, no single source of truth
- [i-stale-comments-2026-08-12](log/issues/i-stale-comments-2026-08-12.md) — CLEANUP: stale comments in crud.ml data_rel and module header
- [i-updatefield-race-2026-08-12](log/issues/i-updatefield-race-2026-08-12.md) — BUG: update-field has lost-update race (non-atomic read-modify-write)
