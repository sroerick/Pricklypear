# Handoff: Global tree traversal (one-tree)

**Scope.** Implement the four `planned` subsections added to
`borge/one-tree.borg` (after `tree-read-prims`): **tree-path-prims**,
**sibling-order**, **tree-edges**, **tree-browser**. The spec text is
the authority — read those four subsections first, plus
`typed-nodes`, `tree-datum-prims`, `tree-read-prims`, and
`branching-interaction` for context. This file only adds code-map
facts, sequencing, and decision points discovered during review.

**Repo:** /home/roerick/dev/wyo.tech/pricklypear · kernel work is
git-authoritative (OCaml + migrations); build on the `poohstack`
opam switch (`eval $(opam env --switch=poohstack --set-switch)`),
`dune build @all`, `dune runtest`. Dev stack: `scripts/dev.sh start`
(PG 5433, HTTP 18080), smoke `ksh scripts/verify_one_tree.sh`.

## Ground rules

- **Make mode: the spec is locked.** If you find the four
  subsections wrong/underspecified, `borge issue new "..."` — do NOT
  edit `borge/one-tree.borg`. Status flips (planned → implemented)
  happen in the completion commit for each task; run `borge drift`
  after every task (expect no *structural* drift; ~31 pre-existing
  `unspecified-script` warnings are baseline).
- **Migrations are additive-only.** Next number: **025**.
- **Branch hygiene:** work off `master` on a fresh feature branch.
  Do NOT build on `sync/notes-vault` (it carries an unpushed vault
  sync + unrelated uncommitted surface work; the spec edit for these
  subsections is being committed separately from it).
- Out-of-scope habitat findings → `log/issues` via
  `(notes/issues/add ...)`; end with a ~3-line `log/sessions` entry.

## Verified code facts (2026-08, re-verify before relying)

- Tree read SQL lives in `image/lib/tree_store.ml`
  (`list_children` level-predicate at ~1407, `list_subtree` ~620),
  prim glue in `image/lib/crud.ml`, prim registration in
  `image/lib/prims_db.ml` (`register_crud "tree/..."`). Existing
  prims: `tree/get` (opt `:gen`), `list`, `children-of`, `subtree`,
  `lens-get/children/subtree`, `find-by`, `list-kind`,
  `search-text`, `get-uuid`, `upsert`, `move`, `delete`, `backlinks`,
  `node-path`, `gen-read/gen-diff/undo`.
- Children today order by `path::text ASC` (or `created_at`) — an
  accident, not a model. `node_to_json` already emits
  `created_at`/`updated_at`; `rank` should ride the same envelope
  path.
- `tree_nodes` payload is a jsonb envelope `{v,kind,path,type,body,
  author,hash}`; `kind` column currently `fn|type|route|datum` plus
  hierarchical kinds. Mirrors to scaffolding tables live in
  `migrations/009_tree.sql` triggers — additive columns that are NOT
  part of the mirrored envelope need no mirror change (verify).
- **`type_defs.fields` is a jsonb array of bare field NAMES**
  (migration 001; `crud.ml` deftype comment: "Type declarations may
  eventually drive field-level validation; for now they're
  documentation"). `check_type_fields` is presence-only via
  `Tree_store.validate_fields`. **There is no `:ref` field type
  today** — see Task 3 decision point.
- **`tree_versions` (migration 018/019) snapshots
  kind/type_name/path/gen/payload — no rank.** The sibling-order
  spec smoke "gen-read at old gen shows old order" therefore
  requires versioning rank, see Task 2.
- `backlinks` today = seq scan of every datum body for exact string
  equality (`tree_store.ml` ~484, kind='datum' only).
- Alcotest suites: `image/test/tree_store_test.ml` and friends.
  Auth/deny-anon pattern: mutating tree prims call `deny_anon`.

## Task 1 — tree-path-prims (pure, do first, ~small)

Implement `(tree/parent path)`, `(tree/leaf path)`,
`(tree/segments path)`, `(tree/child path label)`,
`(tree/ancestors path)` as **pure** prims (no DB, no auth), label
semantics identical to `tree/path-label` (find its encoder and share
it — do not fork the encoding). Invalid paths raise `Prim_error`.
Register next to the other `tree/*` prims.

Acceptance: the 3 spec smokes + alcotest cases for label edge cases
(uuid labels, dashes, dots-in-labels, root path → nil parent).

Notes-package alias cleanup (notes/tree/parent-of etc. → thin
aliases) is habitat Nopales work on `admin/personal` → promote;
optional, do last, don't block on it.

## Task 2 — sibling-order (migration 025)

1. `ALTER TABLE tree_nodes ADD COLUMN rank text`. Partial btree on
   `rank WHERE rank IS NOT NULL` if the planner needs it — measure
   children-of before/after; the CHILDREN-BY-LEVEL invariant (level
   predicate, never anti-join) must hold.
2. `ORDER BY rank NULLS LAST, path::text ASC` in `list_children` /
   `list_subtree` (check `tree/list` depth forms too). No signature
   changes; `node_to_json` gains `rank`.
3. **Versioning:** `ALTER TABLE tree_versions ADD COLUMN rank text`
   + echo the column in the `tree_version_append` trigger shape, so
   `gen-read`/`gen-diff`/`undo` carry order. Verify the
   partitioned-shadow machinery (019) picks up the column.
4. Prims (mutations `deny_anon`): `tree/reorder`, `tree/move-before`,
   `tree/move-after`, `tree/siblings`, `tree/next`, `tree/prev`,
   `tree/insert-child ... [:after anchor]`. Mid-point rank
   allocation, rebalance on exhaustion (write-path detail; pick a
   documented base-62-ish scheme, e.g.: insert at end = last rank +
   one "digit"; between a/b = midpoint; rebalance a sibling set when
   no midpoint remains).
5. Acceptance: the 4 spec smokes, incl. versioned reorder
   (`gen-read` at old gen shows old order; `undo` restores) and
   lens-ownership of rank (child-lens override owns its rank).
   Perf smoke: children-of on `data.note.u.admin` stays single-digit
   ms (the anti-join regression was real once — i.e. keep it an
   index scan).

Follow-up (NOT this task): notes per-node move-up/move-down is now
ungated; hand to the notes track (the capture-migration handoff in
flight explicitly excluded reorder — note the ungate in
`log/sessions`).

## Task 3 — tree-edges (depends on nothing in 1–2, can parallel)

1. Edge = node of **kind `link`** at `<source-path>.lnk.<uuid>`,
   body `{to, kind}` (+ optional `label`). Open the kind set in the
   write path for `link` (see how `tree-datum-prims` opened `datum`);
   validate body shape in OCaml: `to` non-empty path-text, `kind`
   non-empty label. "from" is positional — never in the body.
2. Prims: `(link from to [kind [label]])` — from must resolve in
   the session lens, to stored verbatim (may dangle);
   `(unlink edge-path)` — tombstone (goes through the same
   delete/versioning path as nodes); `(out-links path)` — edge JSON
   + resolution status (resolved against reader lens → target
   summary; else dangling marker); `(backlinks needle [limit])` —
   edges (`to = needle`, index) UNION legacy body-string matches,
   legacy half logged DEPRECATED. Index: partial on
   `(payload->'body'->>'to') WHERE kind = 'link'`.
3. `tree/delete` / `tree/move` report incoming-edge count (return
   value or structured warning — pick one, keep it machine-readable);
   no forbid. UI warning surfaces are Task 4, not here.
4. **Decision point — `:ref` materialization.** Spec says declared
   `:ref` fields materialize as edge nodes at typed-boundary
   validation, but `deftype`/`type_defs` have no field types (see
   facts above). Choose ONE:
   a. Extend `fields` jsonb to allow `{name, type, target}` objects
      alongside bare strings (presence validation accepts both);
      extend `(deftype)` surface syntax accordingly; hook
      materialization into `check_type_fields`. This is the spec-
      faithful route.
   b. If (a) balloons: implement edges + prims WITHOUT `:ref`
      materialization, flip the subsection to `partial`, and
      `borge issue new` the `:ref` gap with what you learned.
   Do NOT silently ship links-as-strings-only — that was the
   rejected design.
5. Acceptance: the 5 spec smokes (link/out-links/backlinks; dangle
   after target `tree/move` with textual legacy backlink still
   findable; delete-with-inbound reports count but succeeds;
   edges in `gen-diff` + `undo` of unlink; :ref materialization per
   chosen route).

## Task 4 — tree-browser (depends on 1–3)

Route `/tree?path=P` (private by default; anonymous confined to
public prefixes per `tree-read-prims`), root defaulting to
`(tree/garden-root)`. Stack: `pure-html` + `HTML.Hx.*`, lazy level
expansion via htmx partials (pattern: `docs/notes-ui-contract.md`
lazy-load; one children query per expand, CHILDREN-BY-LEVEL only —
never a subtree walk for a page render). Breadcrumb from
`(tree/ancestors P)`; kind/type filter; ranked order from Task 2;
`?gen=` as-of view with `gen-diff` highlight vs head; detail pane =
body JSON, created/updated, author, rank, out-links/backlinks from
Task 3, edit affordance honoring `tree/upsert` auth. Do not touch
the existing `tree/*` (scripture docs) package, `/data`, `/bindings`,
`/sys` in v1.

Acceptance: the 4 spec smokes.

## Suggested sequencing

T1 (small warm-up) → T2 (migration, the structural one) → T3 (can
overlap T2, disjoint files mostly) → T4 last. Commit per task with
the subsection status flip in the same commit; `borge drift` clean
(no new structural drift) before each flip; final `borge report`
delta goes in the last commit message. Update the live `agents` note
only if you learn something durable about the tree prims.
