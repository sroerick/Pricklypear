# A Computer Science curriculum on Pricklypear

**Thesis:** a multi-user Lisp OS is a near-complete CS curriculum wearing work
clothes. Every layer a CS degree teaches — programming languages, data
structures, databases, operating systems, networking, security, software
engineering, AI — exists *for real* in this system, specified in a literate
book, running in production, and reachable from a REPL. So the course doesn't
use Pricklypear as a case study; it uses Pricklypear as the **textbook, the
lab bench, and the gradebook**.

Three loops, repeated in every unit:

1. **Read** — the borge chapter (the spec, in literate order).
2. **Dissect** — the real module(s) that implement it (`lisp/lib`, `image/lib`).
3. **Build** — a lab on the live habitat (repl, package, route, notes doc).

Habitat-native courseware (the course hosts itself):

- Readings = notes docs (`(notes/doc/get "course/<unit>")`).
- Lab bench = `/repl` + `POST /api/eval` + `pp-eval`.
- Student workspaces = `<student>/personal` branches (survive deploys).
- Grading = **promote**: review the diff, then merge to `system/main`.
- Bug reports = `log/issues` (students learn issue hygiene as coursework).
- Review = `/branches`, `/bindings`, `/types`, `/timing`, `/code`.
- Spaced review = `libs/flash` (flashcards); schedule = `libs/calendar`.
- Capstone = ship + promote a real package, with its own borge chapter.

---

## Coverage map: CS area → Pricklypear artifact

| CS area | Borge chapter | Code / surface |
|---|---|---|
| Programming fundamentals | `lisp-interpreter.borg` (user view) | Nopales, `/repl` |
| PL & interpreters | `lisp-interpreter.borg` | `lisp/lib/{reader,ast,eval,env,special_forms}.ml` |
| Compilers / code-as-data | `branching.borg`, `structural-editing.borg` | ASTs as JSONB in `function_defs`, `ast_diff.ml`, `/binding/<name>` |
| Data structures & algorithms | `one-tree.borg` | `tree_nodes`/`tree_branches`, CTE inheritance, `prims_graph.ml`, `call_graph.ml` |
| Databases | `storage.borg`, `one-tree.borg`, `migrations.borg` | Postgres, `store.ml`, `migrations/`, SERIALIZABLE txns, JSONB, mirrors |
| Operating systems | `single-image.borg`, `image.borg` | one unix image, `session_registry.ml`, users, lazy env (thunks ≈ demand paging) |
| Concurrency | `single-image.borg` §runtime | OCaml 5 effects, Eio vs Lwt rims, `io.mli`, reentrant tx mutex |
| Background & scheduled work | `jobs.borg` | `jobs.ml` runner fiber, step checkpoints, cron, `SKIP LOCKED` |
| Imperative programming | `lisp-interpreter.borg` (mutation as design question) | kernel OCaml (refs, ring buffers, runners); `set!` dialect lab |
| Computer architecture | (chapter candidate: layout & cost) | OxCaml layouts / `stack_` / SIMD; `spikes/mirage_solo5/` |
| Machine learning | (companion text: *The Little Learner*) | `libs/learn` port, tensor prims, jobs-based training runs |
| Networking | `image.borg`, `prim-seam.borg` | `http.ml`, Httpun_eio, Eio.Net, routing, nginx edge, multipart |
| Web / HCI | `hypermedia-harness.borg`, `product-experience.borg` | htmx, `pure-html`, `HTML.Hx.*`, a11y harness, `/assets` |
| Security | `auth.borg`, `multi-user.borg` | `auth_password.ml` (argon2id), `auth_store.ml`, cookies, Basic, public-route ACLs |
| Version control / time | `branching.borg`, `tree-time.borg` | branches in Postgres, generations, as-of reads, snapshots |
| Distributed (edges of) | `prim-seam.borg`, `local-first.borg` | client lenses, `sync.ml`, `libs/replication`, merge semantics |
| Software engineering | `habitat-workflow.borg`, whole book | spec-as-source-of-truth, `borge drift`, issues, promote/seed policy |
| Testing & verification | `formal-verification.borg` | `alcotest`, `lisp/conformance/`, planned proof track |
| Graphics | `product-experience.borg` | SVG/canvas over htmx routes |
| Observability & performance | `image.borg` (page-timing) | `/timing`, `function_stats`, `db_cost.ml`, `ops_*` |
| AI & agents | `agent.borg`, `ai-authoring.borg` | `/agent.txt`, `/api/eval`, `llm_client.ml`, agents as first-class users |
| Theory of computation | `philosophy.borg`, `formal-verification.borg` | lambda calculus, universality of eval, metacircular evaluator |
| Ethics & governance | `philosophy.borg`, `multi-user.borg` | cotenant isolation, moderation, AI authorship |

---

## Four-year program

### Year 1 — Speaking the language

**CS 101 · Nopales as a first language** *(chapters: philosophy, lisp-interpreter)*
S-expressions, lists, recursion, higher-order functions, the REPL as lab
bench. First program is a route on your own branch — you ship something
visible in week 1.
*Lab:* define a function + route on `<you>/personal`; read its own AST at
`/binding/<name>`.

**CS 102 · Data & its shape** *(object-model, one-tree)*
Data structures where the system actually keeps them: lists in Nopales, then
the one big tree — paths, typed nodes, `b.<branch>.fn.<label>`. Tree
traversal as the year's algorithm work (depth, backlinks, CTE walks).
*Lab:* `deftype` a small domain (courses, enrollments), CRUD it at `/data`,
then query it in Nopales.

### Year 2 — How the machine thinks

**CS 201 · Interpreters** *(lisp-interpreter)*
Build the reader, the AST, the evaluator — then *extend* Nopales with a new
special form or prim. Closures, environments, tail calls, `(delay)`/`(force)`
lazy thunks. Read `eval.ml` as the week's primary source.
*Lab:* add a feature to the language; prove it with a conformance test in
`lisp/conformance/`.

**CS 202 · Code as data** *(branching, structural-editing)*
Programs stored as JSONB ASTs: homoiconicity made institutional. Branches as
fork/merge over program text; AST diffing; inheritance of definitions.
*Lab:* fork a branch, patch a function's AST, merge back; diff two
generations of a binding.

**CS 203 · Databases** *(storage, one-tree, migrations)*
Real SQL against real tables: schemas, additive-only migrations, transactions
(SERIALIZABLE — and *why*), triggers keeping mirrors in lockstep, JSONB, the
FTS gap as a design exercise. `split_sql_statements` as a parsing sequel.
*Lab:* design + land a migration; write the query the `/timing` page
punishes, then make it fast.

**CS 204 · The imperative dialect**
Where is the mutation? Read the kernel as an imperative program: refs,
mutable fields, the `function_stat` ring, the jobs runner fiber. Then
extend Nopales with `set!`, `while`, and mutable vectors in
`special_forms.ml` — and hold the seminar the system forces: in a DB-backed
multi-user Lisp, *the database is the mutable store*; local mutation is a
cache. Imperative programming taught as a language-design decision, not a
given.
*Lab:* land `set!` behind a conformance test; write one program twice (pure
and imperative); argue which version belongs on the habitat.

### Year 3 — The connected machine

**CS 301 · Networking & the HTTP floor** *(image, prim-seam)*
The full request lifecycle: TCP (Eio.Net), HTTP parsing (httpun), routing,
middleware, sessions, response rendering. Media and multipart as the hard
case.
*Lab:* implement a route package end-to-end; trace one request from socket to
HTML.

**CS 302 · Hypermedia & human interface** *(hypermedia-harness, product-experience)*
HTML as UI substrate, htmx as the interaction model, `pure-html` as the
type-safe layer. Escaping vs `txt ~raw:true` — where XSS lives. Accessibility
snapshots as acceptance tests; UI contracts as documentation.
*Lab:* build (or rework) a UI face against a written UI contract; run the
a11y harness.

**CS 303 · Security & multi-user** *(auth, multi-user)*
Threat-model the system: passwords (argon2id + legacy migration story),
sessions (cookie flags, registry, staleness), Basic auth, privilege model
(admin, public routes, hard-private kernel paths), isolation of cotenants on
one image. Bootstrap users and secret management as deployment security.
*Lab:* attack a deliberately weakened dev image; then fix it and write the
defense into the chapter.

**CS 304 · OS concepts, single-image edition** *(single-image, image)*
What an OS is when the "OS" is one process serving many users: process model,
users, sessions, environment loading as demand paging (`VThunk` lazy env),
stats rings, resource accounting. Unikernels (Solo5 spike) as the
decommissioned-then-revisited question.
*Lab:* load-test the shared-session model; explain the per-identity-session
fix from the inside.

**CS 306 · Memory, layout & machines (OxCaml)**
Computer architecture without leaving the language. OxCaml makes
representation first-class: layouts (`value` vs `immediate` vs unboxed
`float64#` vs SIMD `vec128#`), `stack_` allocation with `local` regions and
escape checking, allocation control against GC pauses, and a modes system
that statically rules out data races. Measure what boxing costs in the
interpreter's `value` type and the tensor kernel; then make it disappear.
The Solo5 spike remains the "where is the machine boundary" question.
*Lab:* take a hot kernel loop, annotate it unboxed, benchmark before/after,
and explain the win in cache lines and GC pressure.

### Year 4 — Method, frontier, capstone

**CS 401 · Engineering as a practice** *(habitat-workflow + the whole book)*
Spec-driven development: the book as source of truth, drift as a checked
invariant, issues as the escape hatch, obituaries before reversals. Testing
pyramid, observability (`/timing`, function stats), release engineering
(promote gates, seed policy, snapshots).
*Lab:* take an assigned `log/issues` item from report → spec → code → test →
promote.

**CS 402 · Time, versions, and the edges of distribution** *(tree-time,
branching, local-first)*
Generations, as-of reads, undo as storage semantics; snapshots and backups;
then the honest boundary of the single image: client lenses, sync, replication
— merge semantics and conflict resolution as the distributed-systems lesson.
*Lab:* build an offline-capable lens on a client; graft it back.

**CS 403 · AI & agents** *(agent, ai-authoring)*
Agents as first-class users: `/.well-known/agent.json`, `/agent.txt`,
`/api/eval`, the agents note as living documentation written *by and for*
machines. LLM integration (`llm_client.ml`), authoring loops, evals — and the
governance question of who may eval what.
*Lab:* make a package fully agent-operable and have another student's agent
use it cold.

**CS 405 · Learning: *The Little Learner* in Nopales** *(companion toolkit: Malt)*
Port Friedman & Mendhekar chapter by chapter: tensors, extended operators,
gradient descent, neurons, dense and convolutional networks, automatic
differentiation — ending at the book's Morse-code recognizer. The port
inherits Malt's representation ladder (`learner` → `nested-tensors` →
`flat-tensors`) and extends it: **pure Nopales lists → optimized Nopales →
flat OCaml float-array prims → OxCaml unboxed + SIMD**. Each rung is a
benchmark; the performance cliff between rungs is the architecture lesson
(CS 306) arriving on schedule. Training runs are durable background jobs
(`submit-job`, step checkpoints); loss curves render as SVG routes.
*Lab:* chapters 0–5 in pure Nopales on your branch; autodiff via the
`extend`-operator machinery; capstone track: the Morse recognizer, trained
by a job, plotted at `/learn/morse`.

**CS 404 · Capstone: ship a chapter**
Design → spec a real package as a borge chapter → implement → test → promote
→ seed. Deliverables: working package on `system/main`, its book chapter, its
spec notes docs, its issue trail. Theory thread throughout: lambda calculus,
the metacircular evaluator (write Nopales eval in Nopales), universality, and
the formal-verification chapter as the open frontier.

---

## What this approach covers — and what it doesn't

**Deep coverage (~90% of a core CS degree):** programming, PL/interpreters,
data structures, databases, networking, web/HCI, security, OS concepts,
imperative programming (kernel reading + `set!` dialect), computer
architecture (OxCaml), machine learning (*The Little Learner* port),
software engineering, versioning, AI/agents, testing, observability, ethics.

**Remaining gaps** (shrunk, and honest about them):

- *Parallel at scale* — the habitat has concurrency (Eio effects, fibers)
  and real background/scheduled work (`jobs.borg`), but no cluster story.
  The single image is deliberately anti-scale; teach MPI/HPC as seminar
  contrast, not as failure.
- *Graphics* — easy, as claimed: SVG/canvas over htmx routes cover the
  teaching core; rasterization pipelines stay a side quest.
- *Compiler back-ends* — we build front-ends and interpreters; register
  allocation and instruction selection remain OCaml's business (peeked at
  through OxCaml's lowering).
- *Embedded / RTOS* — absent by design; the unikernel spike is the nearest
  neighbor.
- *Formal methods* — chapter is planned, not landed; the curriculum grows
  with it.

## Publishing: how the book is built → per-course textbooks (survey, 2026-08)

Two parallel book systems exist today, explicitly un-merged:

1. **The codebook (PDF)** — `borge book print . -o stem` (via
   `scripts/public_artifacts.sh`, pdflatex "latex-listings" backend). Walks
   the source tree in filesystem order; **one file = one chapter**; line
   numbers + wide annotation margin. `.book.manifest` sidecar = output
   record. Only selection knob: DIR. Current vendored `/book.pdf` predates
   the multi-file pivot (manifest 2026-07-12; contains retired chapters
   architecture/supervisor/workflow; missing one-tree/jobs/prim-seam/…).
   Regenerating from the current inline-root tree is untested ground.
2. **The live /book (HTML, private)** — kernel routes `/book/<pkg>` →
   Nopales `spec/book-page` (`libs/spec/lib.pp`). Chapter = notes doc
   `spec/<pkg>`; outline owns narrative order; binding references inline
   doc+source; unresolved refs render as visible holes ("drift as
   rendering" — `habitat-workflow.book-view`). Covers the Nopales package
   layer, not the .borg kernel chapters; merging the two volumes is an
   explicitly deferred question.

Per-course textbook = front matter + selected chapters *in course order* +
labs/exercises. The per-file chapter granularity needed is already in
place; chapter→course mapping is many-to-many (one-tree serves CS
102/201/202). Three paths, increasing investment:

- **A. Staged dir, no new tooling** — per course: synthetic root .borg +
  selected chapter files + selected code trees; `book print <dir> -o
  course-cs201`. Works today *if* book print follows `(inline)` from the
  root; filesystem-order fragility; statuses/agent-notes render verbatim
  (feature in CS 401, noise elsewhere).
- **B. Small borge feature** — `book print --root <file>.borg` rendering
  exactly an inline tree (root = chapter selection + order + course front
  matter from project doc). borge is external to this repo; this is the
  clean paper path.
- **C. Habitat-first** — import .borg chapters into the one tree (borge has
  `nodes`/`inline` AST views; sexp-shaped files are Nopales-readable) so
  courses compose live like /book/<pkg>; PDF via print CSS later. This is
  the deferred "two books merge" question in course dress.

Missing for any path: course front matter + labs as structured data (today
living only in this file's prose); a decision on paper-first (A/B) vs
habitat-first (C).

**Probe (2026-08-29, `borge book print .` on current tree):** pipeline
works mechanically (pdflatex OK, ~110s, 188 files, ~1170 pp) but:
- `(inline)` is NOT honored — root prints its inline stubs; the 32 chapters
  wash up on **p788+** of a 1170-page book, behind all OCaml, in
  borge/ readdir order. The literate book is currently a 400-page appendix.
- File order is **readdir order** — non-deterministic across checkouts
  (TOC showed io.mli, pp_client.ml, common/lib… before the walk hits
  borge/). Clone history changes your book's order.
- `libs/**/*.pp` (the entire Nopales product layer) and `migrations/*.sql`
  are **absent** — codebook today = OCaml + spec only; courses whose
  capstones are Nopales packages print nothing of the student's layer.
- Chapter statuses/agent notes/verify blocks render verbatim (workshop
  mode). Register problem → solved by render mode, not diverged documents.

**Spec drafted 2026-08-29:** `borge/habitat-book.borg` (planned; inlined as
chapter 33). It carries the five locked decisions (spec IS the book; git
authoritative; one parser two projections; habitat-first eval; courses
compose never copy) and the per-subsection verify smokes.

**Direction (adopted):** the spec IS the book — chapters are written to be
readable, and stay the source of truth; no forked textbook. Render pipeline
= projection (inline-aware order, curated selection, reader vs workshop
mode), habitat = the readable/publishable home, and chapter code examples
are evaluable: Nopales natively (read-time or render-time eval — the same
Prim_call seam; "the book that tests itself"), OCaml via render-time
build-and-capture (no live OCaml evaluator needed initially).

## Design principles for the course itself

1. **Nothing simulated.** Every lab runs against the real habitat; production
   incidents become course material.
2. **The book is the syllabus.** Chapter order = course order; course feedback
   files as issues, which is also the workflow being taught.
3. **Promotion is assessment.** The same gate that guards production guards
   grades — review, diff, merge.
4. **Agents are classmates.** Every student pairs with an agent from day one,
   on a system that documents itself to agents; the ethics of that is
   examined, not assumed.
5. **The course can be taken by machines.** If an agent can complete the
   readings and labs via `/api/eval`, the courseware is well-specified — a
   nice invariant to maintain.
6. **Cost is the architecture lesson.** Every abstraction in the stack has a
   measurable price — boxed values, untyped SQL strings, one big image. The
   benchmark ladder (Nopales list → optimized Nopales → OCaml prim → OxCaml
   unboxed/SIMD) teaches the machine without ever leaving the program.
