# Agent UI harnesses (research notes)

**Status:** living wiki (2026-07-19). Research synthesis for how coding
agents should build/verify front end on Pricklypear’s no-JS stack
(server-rendered HTML + htmx + pure-html). Not a product spec — if this
disagrees with a `.borg` chapter or `AGENTS.md`, those win.

**Why this page exists:** agents are strong at editing templates and weak
at knowing whether UI *looks* or *behaves* correctly. Backend-leaning /
htmx stacks need an explicit observe → judge → fix loop, not SPA tooling
cargo-culted from React.

**Related local rules:**

- Global tiered UI testing: `~/.pi/agent/AGENTS.md` (wget / workflow /
  screenshot+vision)
- Pi skill: `browser-cdp` (`auto-test.sh`, CDP screenshots)
- Example contract: [`notes-ui-contract.md`](notes-ui-contract.md)
- HTMX/markup skill: `dream-html` (pure-html `HTML.Hx.*`)

---

## The problem in one line

> Build → observe **real browser (or HTML) state** → fix → repeat

Agents that stop at “handler returned 200” or “I edited the CSS” will
lie. The industry converged on closing that loop; tools disagree only on
*what* to observe.

---

## Three observation strategies

### 1. Accessibility tree / DOM snapshots (default for htmx)

Mainstream agent pattern in 2026:

- Playwright MCP / Playwright CLI / Canary / Assrt / Stagehand (DOM mode)
- Agent sees a compact YAML-ish tree, e.g.
  `button "Save" [ref=e12]`, `textbox "Title" [ref=e5]`
- Acts by **ref** (snapshot-local id), not CSS selectors
- Cheap (text tokens), works without vision models, survives class renames

**Why this fits Pricklypear:** server-rendered HTML *is* the UI contract.
Good labels, buttons, headings, and stable control ids become the agent
API. No virtual DOM, no hashed CSS modules.

Playwright guidance (adopt this):

| Layer | Handle |
|-------|--------|
| **Runtime** (agent about to click) | fresh a11y snapshot → act by `ref` |
| **Disk** (committed regression) | `getByRole` / `getByLabel` / occasional `getByTestId` |
| **Never default** | CSS chains, XPath, `nth-child` paths |

Refs are **not** DOM attributes. Re-snapshot after every navigation or
major swap; old refs go stale.

### 2. Screenshots + vision (layout only)

Use when the question is visual, not behavioral:

- Section/full-page PNG → vision-capable subagent describes / diffs
- Local pattern already mandated: parent must **not** “see” images
  unreliably; delegate to a vision model (`hf:moonshotai/Kimi-K2.6` etc.)
- Tools in the wild: AgentScreenshots, PageBolt, Shiplight visual path,
  Midscene (screenshot-first)

**Rule:** do **not** drive clicks from pixels. Vision **judges**; a11y
tree **acts**.

Reserve screenshots for:

- spacing / overflow / stacking
- mobile vs desktop layout
- “does this match the intended shell?”

Not for “is the todo row present?” (use HTML or a11y text).

### 3. Hybrid (where the market is going)

Serious harnesses combine signals:

| Need | Signal |
|------|--------|
| Click / fill / assert text | a11y snapshot |
| “Does it look right?” | screenshot + vision |
| Multi-step user journey | browser automation + artifacts (trace / HAR / video) |
| Cheap regression after green | freeze into Playwright `getByRole` or HTML fixture |

Stagehand’s agent default moved toward **hybrid** (DOM tools + coordinate
tools). Midscene is visual-first (canvas/WebGL strength). For Pricklypear
product UI (forms, lists, htmx swaps), **DOM/a11y-first** is the right
default; hybrid only if something is truly pixel-only.

---

## How people structure the harness

Recurring layers (Canary, Sharingan, Shiplight, agent-qa, harness-fe,
Claude-in-Chrome, Playwright MCP):

```
1. Intent        "verify login rejects bad password; error in #form-errors"
2. Browser rt    Playwright / CDP / headed Chrome / storage-state session
3. Observation   a11y snapshot (+ optional scoped screenshot)
4. Judgment      cheap model for DOM/text; vision only for layout
5. Artifact      report.html / trace / PNG / reusable script
6. Graduation    once agent discovers a flow, save a deterministic check
```

**Graduation** is the cost-control move: agent QA once → CI/replay forever
without another LLM pass (Canary’s pitch; same idea as freezing a smoke).

**Auth wall:** almost every tool hits OAuth/MFA. Best pattern is
human-in-the-loop once (headed login) → capture cookies / storage state →
reuse. Do not make the agent solve login every run. For Pricklypear
agents, prefer HTTP Basic / session via `pp_eval` and known test users
over scraping `/login` unless the login UI itself is under test.

---

## Why no-JS / htmx is an advantage

1. **HTML is the contract.** `wget -qO- url | grep pattern` is a real
   first-tier check. SPA teams often cannot do this.
2. **Fragments are units.** Assert the partial from an `hx-get` /
   `hx-post`, not a client state machine.
3. **Semantic markup = agent API.** Roles and accessible names double as
   a11y *and* automation hooks. Prefer that over a forest of
   `data-testid` (testid is an escape hatch).
4. **Skills beat generic “frontend” prompts.** Dedicated HTMX skills
   forbid JSON-for-UI and SPA rebuilds — same niche as our `dream-html`
   skill and pure-html `Hx.*` rules.
5. **Optional agent tags.** PhantomUI-style `data-ai-id` / `data-ai-role`
   help when a11y is weak. On a clean SSR tree, good labels usually
   suffice.

### HTMX-specific gotchas (from real agent/QA ports)

Faultsense’s todolist-htmx port and HTMX agent skills surface patterns
we should respect when writing smokes or agent instructions:

1. Prefer **`HX-Location`** (client nav preserving session) over hard
   redirect when testing in-page agent sessions.
2. Error responses often **don’t swap by default** — return a fragment
   with retarget/reswap (or 200 + error partial) so the UI (and agent)
   can observe failure.
3. **`hx-swap-oob`** vs assertion OOB: different concepts; don’t confuse
   them. Prefer `innerHTML` OOB when you must preserve attributes on
   long-lived nodes.
4. Long-lived controls with dynamic assertion values go **stale** after
   mutations — re-render them via OOB on each mutation response if a
   smoke depends on those attrs.
5. Server returns **HTML**, not JSON, for UI routes. Agent prompts that
   invent REST for todo/notes/etc. are wrong; use `/api/eval` / Nopales.

---

## Tool landscape (snapshot, 2026)

Not endorsements — orientation only. Prefer local/simple over platforms
unless a clear gap appears.

| Kind | Examples | Notes |
|------|----------|--------|
| a11y snapshot runtime | Playwright MCP, Playwright CLI | Default interaction substrate |
| Agent QA + script capture | Canary | Discover once → reusable Playwright + report.html |
| Verify-while-building | Shiplight | MCP browser + YAML intent tests + locator cache/heal |
| NL tests + memory | agent-qa (Vostride) | Self-heal + memory; heavier product |
| Semantic HTML tags + MCP | PhantomUI | `data-ai-*` + snapshot tools |
| Autonomous suite gen | Sharingan | Claude skill: discover routes, human auth, Playwright suite |
| Screenshot CLI | AgentScreenshots, headless chromium | Fast section PNGs for vision |
| DOM-first AI SDK | Stagehand | `act` / `extract` / `observe`; hybrid default trend |
| Vision-first AI SDK | Midscene | Better when UI is canvas/pixels, not our case |
| React runtime bond | harness-fe | Source maps + overlay; SPA-oriented |
| Vendor browser in IDE | Claude Code + Chrome ext | Build-and-verify in headed browser; plan-gated |

Pricklypear already covers a thin local slice: **browser-cdp**, headless
chromium screenshots, vision **delegate**, `wget` content checks,
package smokes (`smoke_notes_ui.sh`, etc.).

---

## Failure modes (and fixes)

| Failure | Fix |
|---------|-----|
| Agent claims success from HTTP 200 | Require DOM text, a11y assert, or screenshot proof |
| Brittle CSS selectors | Snapshot refs at runtime; `getByRole` on disk |
| Vision in parent context flaky | Always vision **subagent**; scoped PNG |
| Context drowned in full-page shots | Section/selector crops; a11y first |
| Agent can’t log in | Capture storage state / use Basic + test user |
| No definition of done | Write observable acceptance criteria first |
| Agent disables lint/types to “fix” | Gates the agent cannot turn off (CI, borge, smokes) |
| One-off chat QA never becomes CI | Graduate green flows into smokes/fixtures |

Agents clear errors; they do not preserve taste unless the harness
forces observable checks (same lesson as strict typing experiments with
coding agents).

---

## Pricklypear tiered verification (canonical)

Match method to question — lightest tool that gives confidence:

| Goal | Method | Cost |
|------|--------|------|
| Text/element present | `wget -qO- URL \| grep` or `pp_eval` model assert | lowest |
| Form / htmx workflow | Scripted POST + redirect/fragment check; CDP if needed | low |
| Control behavior | Smoke scripts (`scripts/smoke_*.sh/py`) against live image | low–med |
| Layout / CSS / responsive | Screenshot → vision subagent (`browser-cdp` / chromium) | higher |
| Full journey evidence | Trace / multi-step CDP + optional video | highest |

**Agent rules of thumb:**

1. Fix **behavior** (model + DOM) before CSS polish.
2. One control or scenario per tight iteration when repairing UI.
3. Prefer git `libs/*` on `system/main`; personal pins can hide fixes.
4. Screenshots only for layout; clicks/ops/HTML for correctness.
5. After a flow is green twice, **graduate** it into a smoke or contract
   row (see [`notes-ui-contract.md`](notes-ui-contract.md)).

### Suggested loop for product UI work

```text
change template / Nopales route / pure-html
    ↓
dune build (and restart image if needed)
    ↓
content:   wget -qO- url | grep 'expected'
    ↓
interact:  a11y snapshot → fill/click by role (or existing smoke)
    ↓
layout?:   screenshot section → vision subagent describe / pass-fail
    ↓
stable?:   add/adjust scripts/smoke_* or docs/*-ui-contract.md row
```

### What we should usually skip

- Full MCP browser for every copy tweak
- Vision on every iteration
- React-centric runtime instrumentation platforms
- Natural-language-only E2E in CI with no graduation path
- Inventing REST for capabilities already on `/api/eval`

## htmx version & feature budget (2026-08-06)

Upgraded from htmx 1.9.12 (unpkg CDN) to **htmx 4.0.0-beta6** (vendored at
`image/static/htmx.min.js`, no CDN dependency — matches `borge/multipart.borg`
stance). The htmx-2-compat extension (`htmx-2-compat.min.js`) restores
implicit attribute inheritance, 1.9/2.x error no-swap defaults, and old
camelCase event names so existing markup keeps working during incremental
migration. The custom `hx-multipart` extension was ported to the htmx 4
event-based API (`registerExtension` + `htmx_after_request`/`htmx_before_swap`
hooks; no longer touches `evt.detail.xhr`).

Feature inventory (271 `hx-*` attributes across `libs/` + `image/`):

| Category | Share | Attrs |
|----------|-------|-------|
| fixi-five core (get/post/target/swap/trigger) | 76% | 205 |
| `hx-on:` event scripting | 15 / 9 files | maps to moxi.js (fixi sister) |
| `hx-confirm` | 10 | fixi `cfg.confirm` (built in) |
| `hx-multipart` / `hx-part-id` | 8 | custom ext (ported to v4 API) |
| `hx-swap-oob` | 4 | recoverable as `fx:after` pattern |
| `hx-push-url` | 4 | history (no fixi equivalent — gap) |
| `hx-ext`, `hx-indicator`, `hx-select`, `hx-include` | ≤2 each | thin tail |

**Not used:** `hx-sync`, `hx-boost`, `hx-ws`, `hx-sse`, `hx-validate` —
the htmx-only divergence zones that would have forced keeping the
monolithic client. Their absence is why a fixi-shaped simulator is faithful.

**Simulator invariant (for the planned headless-hypermedia client):** write
the simulator against the fixi-five intersection — the semantic core shared
by both htmx and fixi (same fetch+swap loop, same five attributes).
`simulator-spec ⊆ production-client behavior`, always. The simulator
implements the five swap attrs + ancestor lookup for inherited
`hx-target`/`hx-swap`/`hx-confirm` (needed under compat). It returns
`unsupported-feature` for htmx-only features so those flows graduate to
the CDP tier instead of being silently under-modeled. This lets the
production client shrink toward the fixi family later without touching
the simulator — the testable contract doesn't move.

**Incremental migration cleanup (tracked, not blocking):**
- `hx-on::after-request` → should become `hx-on::after:request` (compat
  fires old names; rename when done with compat)
- `hx-ext="hx-multipart"` → inert in htmx 4 (extensions activate by
  inclusion); harmless, remove when convenient
- `htmx.config.prefix` defaults to `data-hx-` — both `hx-*` and
  `data-hx-*` work; no action needed

---

## Intent / acceptance criteria template

Before asking an agent to “fix the UI,” write checks it can falsify:

```text
Page: /notes?slug=ui-fixture
Setup: system/main; outline+notes loaded; fixture seeded

Pass:
  - GET 200; #outline present; root data-id=root
  - POST add-child on root → new kid in DOM and (notes model)
  - Error path: empty title → message in #form-errors (or agreed node)
  - Desktop: .ol-toolbar visible (layout optional screenshot)

Fail if:
  - Only HTTP 200 with empty body
  - Personal branch pin serves stale libs
  - Agent “fixed” by removing the assertion
```

Contracts beat vibes. Notes already model this; new packages should get
the same treatment when UI matters.

---

## Optional future harness shape (not implemented)

If we grow past ad-hoc scripts, a thin local CLI is enough — no need to
adopt a hosted QA platform:

```text
pp-ui-check content <url> <grep...>
pp-ui-check flow   <script-or-steps>
pp-ui-check visual <url> [--selector ...] [--viewport ...]
```

Implementation sketch:

- `content` → wget/curl + grep (exit codes)
- `flow` → existing smokes / CDP recipe / Playwright headed with
  storage state
- `visual` → chromium screenshot + mandatory vision subagent prompt
  returning PASS/FAIL + bullets

Artifacts under `/tmp/pp-ui/` or `docs/` inventories, same as notes UI.

Only build this when repeated agent UI work hurts more than one more
script. Until then, enforce the tier table in prompts and contracts.

---

## External references (research pass 2026-07-19)

- Playwright aria snapshots: https://playwright.dev/docs/aria-snapshots
- Playwright MCP / a11y-snapshot interaction model (refs, not CSS)
- Canary (agent QA → script + report): https://github.com/wizenheimer/canary
- Shiplight (verify while building): https://www.shiplight.ai/
- Stagehand (Browserbase) agent modes: DOM / hybrid / CUA
- Midscene vs Stagehand (visual vs DOM grounding)
- PhantomUI (`data-ai-*` + MCP)
- Sharingan (Claude Code autonomous Playwright suite skill)
- AgentScreenshots (fast scoped PNGs for agents)
- Claude Code + Chrome (build-and-verify in headed browser)
- HTMX agent skills (HTML-over-the-wire discipline)
- Faultsense todolist-htmx (SSR swap assertion patterns)
- PageBolt / vision verification loops (screenshot-as-eyes)

Links rot; the **patterns** on this page are the durable part.

---

## Changelog

| Date | Note |
|------|------|
| 2026-07-19 | Initial research wiki from external survey + local AGENTS/smoke practice |
| 2026-08-06 | htmx 1.9.12 → 4.0.0-beta6 (vendored, no CDN); compat ext + hx-multipart ported to v4 API; feature inventory table + fixi-five budget policy added |
