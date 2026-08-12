# i-fields-as-string-2026-08-12

## DESIGN: CRUD returns fields as VStr of JSON text (triple-escaping in api/eval)

All CRUD read prims do SELECT fields::text and wrap in VStr. The JSON string gets Lisp-printed with backslash escaping (pp_value), then JSON-encoded again by api_eval. Result: triple-escaped output for every quote in field data. Also: create docstring says Lisp layer does not yet have a dict/assoc value type but dict-set/dict-get/json-parse/json-emit all exist now. Two-part fix: (1) api_eval should return value as native JSON not pp_value string; (2) CRUD prims should json-parse fields into structured Lisp values at the DB boundary. Found 2026-08-12.

## DECISIONS (obituary rule — record what/why before replacing shape)

**Decision: implement Part 1 (api_eval native JSON). Defer Part 2 (structured CRUD
return values) as a deliberate non-goal of this loop.**

- The actual defect is triple-escaping in the `POST /api/eval` JSON path
  (`image/lib/http_pages.ml api_eval`, ~line 1576). That path used
  `Reader.pp_value v` (Lisp-printer: backslash-escapes quotes) then wrapped the
  result in a JSON string via `json_response`, so a CRUD `VStr fields-json`
  (already containing `{...}` JSON text) got escaped again -> `"value":
  "{\"name\":\"...\"}"` with every inner quote doubled. **Part 1 fixes this** by
  serializing the eval result with `Prims_pure.value_to_json`, which re-parses a
  JSON-lookalike `VStr` into a real nested JSON object, so field data round-trips
  as clean valid JSON. See issue acceptance #1 and #3.
- **Part 2 (CRUD read prims json-parse `fields` into structured Lisp values at the
  DB boundary) is deferred.** The v1 dict working set — `dict-get`, `dict-set`,
  `dict-keys` — all take a `VStr` of JSON as their first argument (pure/prims_pure.ml
  `prim_dict_get`/`prim_dict_set`). Changing the CRUD return shape from `VStr
  fields-json` to a structured value would break every existing
  `(dict-get row "field")` call site (notes, bible, todo, update-field, agent
  tools) unless those dict prims and the whole value system are reworked in the
  same change — a much larger, coordinated design change than this loop's scope.
  Issue acceptance #2 explicitly permits keeping existing `(dict-get row "field")`
  callers working; Part 1 does that unchanged, so all acceptance criteria are met
  without Part 2.
- If/when a structured dict/assoc value type is adopted (coordinated with
  `dict-*` + CRUD callers), revisit Part 2. File a borge issue then; do not do it
  piecemeal here.
- `agent_tools.ml ~690` (`value_smoke_kind`) and the browser REPL render paths in
  `http_core.ml` (231/1444/1460) are **intentionally left on `pp_value`** — they
  are human/text display paths, not the JSON api. Only the JSON `POST /api/eval`
  path needed native JSON (per FIX-LOOP note: "may intentionally stay pp_value").

## Resolution (2026-08-12)

**Part 1 implemented — api_eval now emits native JSON.**

- `image/lib/http_pages.ml` `api_eval` (~line 1576): replace
  `("value", \`String (Reader.pp_value v))` with
  `("value", Prims_pure.value_to_json v)`. `Prims_pure.value_to_json` already
  exists (used by `json-emit`) and re-parses a VStr that looks like a JSON
  object/array into a real nested JSON value, so CRUD `fields` VStrs round-trip
  as native JSON objects with quotes **not** triple-escaped. Plain strings stay
  strings; lists become JSON arrays; numbers/bools become JSON primitives. The
  browser REPL + agent-tool text render paths are unchanged (human display).
- `image/test/primitives_test.ml`: added regression test
  `api_eval native-JSON: CRUD VStr stays clean (no triple-escape)` verifying
  `Prims_pure.value_to_json (VStr "{\"name\":\"alice\",\"age\":30}")` yields a
  native `\`Assoc` with `name="alice"` and `age=30` (i.e. NOT an escaped string).

**Part 2 deferred** — see DECISIONS above. CRUD return shape stays `VStr
fields-json`; `dict-get`/`dict-set` keep working unchanged (acceptance #2).

**Verification**
- `dune build` green (after `eval $(opam env --switch=poohstack --set-switch)`).
- `dune runtest` green — full suite, 164 primitives tests incl. new regression.
- `borge drift`: structural drift none; total 19 = pre-existing baseline (18) +
  accepted 016_migration item — **no new drift from this change**.
- Wire-contract check: published agent docs (`agent_discovery.ml`) do not pin the
  `value` field to always-be-a-string; native JSON is a compatible improvement and
  is the issue's stated intent.
