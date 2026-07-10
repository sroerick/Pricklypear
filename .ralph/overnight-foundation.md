# Overnight Foundation Loop

## Goals
1. Refactor `.borg` into a literate multi-file book; align spec/code
2. Implement capability effects
3. Implement LLM coding-tool stdlib (inner/outer/seed)
4. Bonus polish
5. Test everything

## Priority 1 — Literate borge book modularization
- [x] Inventory sections
- [x] Book chapter map
- [x] Split into chapter `.borg` files + root inlines
- [x] Human comments preserved (monolith.bak backup)
- [x] borge balance/parse/check/lint clean (15 files)
- [x] AGENTS.md + README updated for multi-file book
- [x] Backup: `pricklypear.borg.monolith.bak`

## Priority 2 — Effects
- [x] `lisp/lib/effect.ml` capability lattice + inference
- [x] Prim effect table
- [x] 12 unit tests (`effect_test.ml`)
- [x] Badges on `/bindings` (effects column)
- [x] AI propose allowlist (Pure|ReadDb|Html; `PP_AI_ALLOW_WRITE=1` elevate)
- [x] Spec status effects → implemented

## Priority 3 — LLM coding-tool stdlib
- [x] Surgical rewrite prompts in `llm_client`
- [x] `llm-inner` freeze rewrite
- [x] `llm-outer` wrap freeze
- [x] `llm-rewrite` general
- [x] `llm-seed` recursive growth (seed + judge + outer)
- [x] Effect gate on freezes
- [x] Registered as session prims
- [x] `ensure_callable_body` wraps bare expr as lambda
- [x] Live smoke: llm-inner, llm-seed, propose

## Priority 4 — Bonus
- [x] implements path / multi-file check green
- [x] dream smoke + overnight verify script
- [x] AGENTS/README book notes
- [x] Cheap robustness: AI body lambda wrap

## Optimization notes (review)
- Image HTTP is a large module (~800+ lines) — candidate future split (pages/api/chrome) not blocking
- `load_defs_async` fire-and-forget is fine for v1; optional barrier later
- LLM via curl subprocess is OK; pool/reuse not needed yet
- Effect inference is O(AST); cache on persist later if hot

## Final Verification
- Exact command: `scripts/verify_overnight.sh`
- Working directory: `/home/roerick/dev/wyo.tech/pricklypear`
- Env: `LLM_API_*` from `../TREE/.env` (for AI steps)
- Result: **`[verify] ALL GREEN`** (re-confirmed 2026-07-09 iteration-1 gate)
  - borge multi-file check 15/15
  - dune build + 106 alcotest cases (lisp 32+12 effects, common 10, auth 4, image 48)
  - product smoke ALL CHECKS PASSED
  - effects badges present
  - llm-inner freeze + call
  - llm-seed growth + call

## Notes
Chapter files: philosophy, architecture, object-model, branching, lisp-interpreter,
storage, multi-user, auth, supervisor, image, product-experience, migrations,
workflow, ai-authoring — inlined from root `pricklypear.borg` in book order.
