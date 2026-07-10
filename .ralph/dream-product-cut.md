# Pricklypear Dream Product Cut

Close the product gaps identified in the it31 plan pass so the dream
workflow works end-to-end:

> log in → use base functions → create/edit functions → AI can propose
> functions on a branch → human inspects/accepts

## Goals
- Human can authenticate and land in a live image without smoke-test ceremony
- Base functions work (eval, CRUD, branches, routes)
- Create/edit functions from browser + REPL
- OpenAI-compatible inference client + AI propose_function path
- All functionality verified with build/tests/smokes that a monitor can rerun

## Checklist
- [x] Fix quote-of-compounds (eval) + list-branches stub
- [x] auth.user-management: create_user + add_credential (ssh_key + password hash)
- [x] auth password verify (interim salted SHA256 via mirage-crypto until argon2)
- [x] auth RPC: /admin/create_user, /admin/add_credential, /verify_password, /mint_token, /verify_token
- [x] image system-browser shell (nav chrome with whoami/branch)
- [x] image browser-repl (POST /api/eval)
- [x] image function-editor (GET/POST /edit/:name)
- [x] image inspector (GET /inspect/binding/:name, /inspect/row/:type/:id)
- [x] OpenAI-compat llm_client (env LLM_API_BASE/KEY/MODEL; curl TLS)
- [x] AI authoring: propose_function prim + POST /api/ai/propose
- [x] AI writes to current branch with agent provenance author
- [x] dune build @all green
- [x] dune runtest green (existing + new tests) — 94 tests (lisp 32 + common 10 + auth 4 + image 48)
- [x] scripts/dev.sh stack smoke: login path, define, edit, AI propose
- [x] Final monitor-rerunnable verification command recorded

## Verification

### Unit / build
```
eval $(opam env --switch=poohstack --set-switch)
dune build @all          # exit 0
dune runtest --force     # 94 tests pass
```

### Product smoke (with stack up)
```
python3 scripts/dream_product_smoke.py
```
All checks passed including:
- admin create_user + ssh/password credentials
- verify_password + mint/verify_token
- SSH login → REPL define/call (dream-square 9 => 81)
- quote compounds + list-branches
- browser GET / /repl /edit /bindings
- POST /api/eval define + call
- POST /edit/editor-greet save + call
- AI propose ai-double + (ai-double 21) => 42 with author agent:<model>

### Key implementation notes
- Password hash interim: `sha256$salt_hex$digest_hex` (NOT argon2)
- LLM client uses curl + OpenAI chat/completions (Synthetic default)
- Images boot on real system/main UUID (fixed broken synthetic branch_id FK)
- AI author recorded as `agent:<LLM_MODEL>`

## Final Verification
- Exact command: `scripts/verify_dream_product.sh`
- Working directory: `/home/roerick/dev/wyo.tech/pricklypear`
- Required env (optional but needed for AI check): `LLM_API_BASE`, `LLM_API_KEY`, `LLM_MODEL`
  (auto-sourced from `../TREE/.env` if present)
- Required preserved artifacts: opam switch `poohstack`, dune build tree, PG cluster at `/tmp/pp-pgsup`, scripts/{verify_dream_product.sh,dream_product_smoke.py}
- Result (re-run 2026-07-08 iteration-1 gate): `[verify] ALL GREEN`
  - dune build @all exit 0
  - dune runtest 94 tests pass (lisp 32 + common 10 + auth 4 + image 48)
  - product smoke ALL CHECKS PASSED including AI ai-double 21 => 42

## Notes
- Spec-first product sections were planned in it31; this loop implemented them.
- Remaining deferred (not blocking dream cut): linenoise, argon2, Solo5, rich merge UI, setuid permissions.
- Monitor can re-run `scripts/verify_dream_product.sh` from repo root.
