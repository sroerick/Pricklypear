# Borge status reconcile checklist

## Before/After
- **Baseline:** 93 implemented | 5 verified | 30 planned | 3 deprecated | 74.8%
- **Final:**
```
habitat-workflow.borg (pricklypear-habitat-workflow)              8          0          0         0         0
------------------------------------------------------------------------------------------------------------
total                                                           149          5          0         4         3

149 implemented | 5 verified | 0 in-progress | 4 planned | 3 deprecated | 161 total tracked
Completion: 95.7%
```
- **Final verify cmd (cwd repo root):** `borge report` and `for f in pricklypear.borg borge/*.borg; do borge balance "$f" || exit 1; done`
- Artifact: `/tmp/pp-borge-status-reconcile-final.txt`
Generated for full pass. 95 planned/partial/in-progress sections.
Report baseline: 93 implemented | 5 verified | 30 planned | 3 deprecated.
Note: `partial` is not counted in `borge report` totals.
Goal: for each item, verify against code/habitat and flip status if built; leave planned if truly not built; mark verified only with evidence.

## Progress log
- **2026-08-02 formal-verification.borg**: confirmed still planned (design only).
- **2026-08-02 pricklypear.borg**: root stays partial; agent note lists open chapter work.
- **BEFORE/AFTER**: baseline 93 implemented / 30 planned / 74.8% → final see report below.
- **2026-08-02 storage.borg**: project partial→implemented (all children done).
- **2026-08-02 migrations.borg**: applied-by-image→implemented; branching-interaction confirmed partial.
- **2026-08-02 spot-check single-image.borg + philosophy.borg**: all already implemented.
- **2026-08-02 one-tree.borg**: confirmed partial (tree core impl; branching-interaction + migration-plan open). No flips.
- **2026-08-02 object-model.borg**: confirmed partial (CRUD live; type-system :ref lattice open). No status flips.
- **2026-08-02 lisp-interpreter.borg**: eval+macros+parents → implemented; dune runtest lisp 123 OK.
- **2026-08-02 multi-user.borg**: namespaces/budgets/chat/user-plan → implemented; permissions stays partial.
- **2026-08-02 branching.borg**: agent-promote, package-updates, branch-browser-ui → implemented; schema-authority+ast-nodes confirmed open.
- **2026-08-02 auth.borg**: multi-user-product, user-subdomains, route-visibility → implemented; verification+user-ssh-repl confirmed partial.
- **2026-08-02 image.borg**: htmx, http-robustness, system-browser, tree-browser, function-editor, inspector, dnu-stubs → implemented; repl-experience + per-request-context confirmed partial; parents stay partial.
- **2026-08-02 structural-editing.borg**: keymap+non-goals → implemented; ast-structure-view confirmed planned; parents stay partial.
- **2026-08-02 ai-authoring.borg**: all → implemented (review-merge, provenance, parents).
- **2026-08-02 product-experience.borg**: all remaining → implemented (analytics, reliability-work-queue, horizon, parents). Live /analytics + ops/summary.
- **2026-08-02 calendar.borg**: project+section partial→implemented (model already impl). Live /calendar 200, 27 events.
- **2026-08-02 spot-check flashcards.borg + multipart.borg**: all statuses already implemented; code present (flash routes/SRS, multipart prims + media upload). /flashcards currently 500 runtime — not a status issue.
- **2026-08-02 habitat-workflow.borg**: all planned/partial → implemented (project+section+spec-docs+spec-checks+non-goals). Soft promote lint live; /book/todo 200; spec/todo+logs exist.
- **2026-08-02 dabar.borg**: project planned→partial; note-bible-links/work-packages/non-goals → implemented; search-fts confirmed partial. Live /dabar+/strongs 200; bible/search love=40; link-html Gen 1:1.
- **2026-08-02 notes.borg**: project planned→partial; storage/links-and-roam/wiki-llm-loop/personal-garden/search-modal → implemented; garden-import planned→partial; outline-actions+tree-cutover confirmed partial. Live: notes/doc/get agents, /notes 200, /partials/notes-search?q=agents 200, wiki/schema.
- **2026-08-02 workout.borg**: all 7 planned → **implemented**. Evidence: `libs/workout/lib.pp` (~2612 lines), seeded in session.ml, live `(workout/programs)` → free/ksk1/ksk2/ksk3, `(length (workout/session-list 3))` → 3, GET `/workout/log` 200. Note: GET `/workout` currently 500 (runtime bug, not missing package).

## borge/ai-authoring.borg
- [x] `pricklypear-ai-authoring` (partial) — borge/ai-authoring.borg:1
- [x] `pricklypear-ai-authoring.ai-authoring` (partial) — borge/ai-authoring.borg:5
- [x] `pricklypear-ai-authoring.ai-authoring.review-merge` (planned) — borge/ai-authoring.borg:496
- [x] `pricklypear-ai-authoring.ai-authoring.provenance` (partial) — borge/ai-authoring.borg:517

## borge/auth.borg
- [x] `pricklypear-auth` (partial) — borge/auth.borg:1
- [x] `pricklypear-auth.auth` (partial) — borge/auth.borg:5
- [x] `pricklypear-auth.auth.verification` (partial) — borge/auth.borg:65
- [x] `pricklypear-auth.auth.multi-user-product` (partial) — borge/auth.borg:201
- [x] `pricklypear-auth.auth.user-subdomains` (partial) — borge/auth.borg:467
- [x] `pricklypear-auth.auth.user-ssh-repl` (partial) — borge/auth.borg:613
- [x] `pricklypear-auth.auth.route-visibility` (planned) — borge/auth.borg:852

## borge/branching.borg
- [x] `pricklypear-branching` (partial) — borge/branching.borg:1
- [x] `pricklypear-branching.branching` (partial) — borge/branching.borg:5
- [x] `pricklypear-branching.branching.agent-promote` (planned) — borge/branching.borg:151
- [x] `pricklypear-branching.branching.schema-authority` (partial) — borge/branching.borg:175
- [x] `pricklypear-branching.branching.package-updates` (partial) — borge/branching.borg:242
- [x] `pricklypear-branching.branching.branch-browser-ui` (partial) — borge/branching.borg:656
- [x] `pricklypear-branching.branching.ast-nodes` (planned) — borge/branching.borg:850

## borge/calendar.borg
- [x] `pricklypear-calendar` (partial) — borge/calendar.borg:1
- [x] `pricklypear-calendar.calendar` (partial) — borge/calendar.borg:5

## borge/dabar.borg
- [x] `pricklypear-dabar` (planned) — borge/dabar.borg:1
- [x] `pricklypear-dabar.dabar` (partial) — borge/dabar.borg:5
- [x] `pricklypear-dabar.dabar.note-bible-links` (planned) — borge/dabar.borg:195
- [x] `pricklypear-dabar.dabar.search-fts` (partial) — borge/dabar.borg:236
- [x] `pricklypear-dabar.dabar.work-packages` (planned) — borge/dabar.borg:251
- [x] `pricklypear-dabar.dabar.non-goals` (planned) — borge/dabar.borg:273

## borge/formal-verification.borg
- [x] `pricklypear-formal-verification` (planned) — borge/formal-verification.borg:1
- [x] `pricklypear-formal-verification.formal-verification` (planned) — borge/formal-verification.borg:5

## borge/habitat-workflow.borg
- [x] `pricklypear-habitat-workflow` (planned) — borge/habitat-workflow.borg:1
- [x] `pricklypear-habitat-workflow.habitat-workflow` (planned) — borge/habitat-workflow.borg:5
- [x] `pricklypear-habitat-workflow.habitat-workflow.spec-docs` (partial) — borge/habitat-workflow.borg:90
- [x] `pricklypear-habitat-workflow.habitat-workflow.spec-checks` (partial) — borge/habitat-workflow.borg:178
- [x] `pricklypear-habitat-workflow.habitat-workflow.non-goals` (planned) — borge/habitat-workflow.borg:282

## borge/image.borg
- [x] `pricklypear-image` (partial) — borge/image.borg:1
- [x] `pricklypear-image.image` (partial) — borge/image.borg:5
- [x] `pricklypear-image.image.repl-experience` (partial) — borge/image.borg:87
- [x] `pricklypear-image.image.htmx-interface` (partial) — borge/image.borg:159
- [x] `pricklypear-image.image.http-robustness` (partial) — borge/image.borg:228
- [x] `pricklypear-image.image.per-request-context` (partial) — borge/image.borg:371
- [x] `pricklypear-image.image.system-browser` (partial) — borge/image.borg:579
- [x] `pricklypear-image.image.tree-browser` (partial) — borge/image.borg:766
- [x] `pricklypear-image.image.function-editor` (partial) — borge/image.borg:1592
- [x] `pricklypear-image.image.inspector` (partial) — borge/image.borg:1653
- [x] `pricklypear-image.image.dnu-stubs` (planned) — borge/image.borg:1859

## borge/lisp-interpreter.borg
- [x] `pricklypear-lisp-interpreter` (partial) — borge/lisp-interpreter.borg:1
- [x] `pricklypear-lisp-interpreter.lisp-interpreter` (partial) — borge/lisp-interpreter.borg:5
- [x] `pricklypear-lisp-interpreter.lisp-interpreter.eval` (partial) — borge/lisp-interpreter.borg:431
- [x] `pricklypear-lisp-interpreter.lisp-interpreter.macros` (partial) — borge/lisp-interpreter.borg:531

## borge/migrations.borg
- [x] `pricklypear-migrations` (partial) — borge/migrations.borg:1
- [x] `pricklypear-migrations.migrations` (partial) — borge/migrations.borg:5
- [x] `pricklypear-migrations.migrations.applied-by-image` (partial) — borge/migrations.borg:12
- [x] `pricklypear-migrations.migrations.branching-interaction` (partial) — borge/migrations.borg:113

## borge/multi-user.borg
- [x] `pricklypear-multi-user` (partial) — borge/multi-user.borg:1
- [x] `pricklypear-multi-user.multi-user` (partial) — borge/multi-user.borg:14
- [x] `pricklypear-multi-user.multi-user.namespaces` (partial) — borge/multi-user.borg:81
- [x] `pricklypear-multi-user.multi-user.budgets` (partial) — borge/multi-user.borg:120
- [x] `pricklypear-multi-user.multi-user.permissions` (partial) — borge/multi-user.borg:154
- [x] `pricklypear-multi-user.multi-user.chat` (planned) — borge/multi-user.borg:199
- [x] `pricklypear-multi-user.multi-user.user-plan` (partial) — borge/multi-user.borg:341

## borge/notes.borg
- [x] `pricklypear-notes` (planned) — borge/notes.borg:1
- [x] `pricklypear-notes.notes` (partial) — borge/notes.borg:5
- [x] `pricklypear-notes.notes.storage` (partial) — borge/notes.borg:139
- [x] `pricklypear-notes.notes.links-and-roam` (partial) — borge/notes.borg:231
- [x] `pricklypear-notes.notes.outline-actions` (partial) — borge/notes.borg:446
- [x] `pricklypear-notes.notes.wiki-llm-loop` (planned) — borge/notes.borg:533
- [x] `pricklypear-notes.notes.personal-garden` (planned) — borge/notes.borg:872
- [x] `pricklypear-notes.notes.garden-import` (planned) — borge/notes.borg:912
- [x] `pricklypear-notes.notes.tree-cutover` (partial) — borge/notes.borg:977
- [x] `pricklypear-notes.notes.search-modal` (planned) — borge/notes.borg:1080

## borge/object-model.borg
- [x] `pricklypear-object-model` (partial) — borge/object-model.borg:1
- [x] `pricklypear-object-model.object-model` (partial) — borge/object-model.borg:5
- [x] `pricklypear-object-model.object-model.type-system` (partial) — borge/object-model.borg:68

## borge/one-tree.borg
- [x] `pricklypear-one-tree` (partial) — borge/one-tree.borg:1
- [x] `pricklypear-one-tree.one-tree` (partial) — borge/one-tree.borg:5
- [x] `pricklypear-one-tree.one-tree.branching-interaction` (partial) — borge/one-tree.borg:349
- [x] `pricklypear-one-tree.one-tree.migration-plan` (partial) — borge/one-tree.borg:402

## borge/product-experience.borg
- [x] `pricklypear-product-experience` (partial) — borge/product-experience.borg:1
- [x] `pricklypear-product-experience.product-experience` (partial) — borge/product-experience.borg:5
- [x] `pricklypear-product-experience.product-experience.analytics` (partial) — borge/product-experience.borg:377
- [x] `pricklypear-product-experience.product-experience.reliability-work-queue` (partial) — borge/product-experience.borg:445
- [x] `pricklypear-product-experience.product-experience.horizon` (planned) — borge/product-experience.borg:536

## borge/storage.borg
- [x] `pricklypear-storage` (partial) — borge/storage.borg:1

## borge/structural-editing.borg
- [x] `pricklypear-structural-editing` (partial) — borge/structural-editing.borg:1
- [x] `pricklypear-structural-editing.structural-editing` (partial) — borge/structural-editing.borg:5
- [x] `pricklypear-structural-editing.structural-editing.keymap` (partial) — borge/structural-editing.borg:175
- [x] `pricklypear-structural-editing.structural-editing.ast-structure-view` (planned) — borge/structural-editing.borg:226
- [x] `pricklypear-structural-editing.structural-editing.non-goals` (planned) — borge/structural-editing.borg:321

## borge/workout.borg
- [x] `pricklypear-workout` (planned) — borge/workout.borg:1
- [x] `pricklypear-workout.workout` (planned) — borge/workout.borg:5
- [x] `pricklypear-workout.workout.model` (planned) — borge/workout.borg:380
- [x] `pricklypear-workout.workout.document-and-capture` (planned) — borge/workout.borg:390
- [x] `pricklypear-workout.workout.programs` (planned) — borge/workout.borg:399
- [x] `pricklypear-workout.workout.ui` (planned) — borge/workout.borg:408
- [x] `pricklypear-workout.workout.agent-access` (planned) — borge/workout.borg:417

## pricklypear.borg
- [x] `pricklypear` (partial) — pricklypear.borg:1

## Files with zero planned/partial (still spot-check)
- [x] borge/flashcards.borg — confirm statuses still accurate
- [x] borge/multipart.borg — confirm statuses still accurate
- [x] borge/philosophy.borg — confirm statuses still accurate
- [x] borge/single-image.borg — confirm statuses still accurate
