# Security architecture — keys, secrets, and the trust graph

**Status:** DRAFT v0.1 (hollis, 2026-10-19) — roerick's ask: a security
architecture we can run AND sell to organizations.
**Model locked by roerick:** the org root key sits with the principal
(roerick). All capability flows downstream of it through documented,
observable grants. Agents never hold root; agents hold capabilities.

---

## 1. The three rules

1. **Nothing secret is unlisted.** Every key/token/secret in the org has a
   ledger row: what it opens, who holds it, how it rotates, how it dies.
   The ledger stores DESCRIPTORS (fingerprints, prefixes, expiry) — never
   secret material. The map is not the territory.
2. **Nothing privileged is unapproved.** Root capability arrives as one of
   exactly two shapes: (a) a narrow reviewed helper (the
   `wyo-site-promote` pattern — one command, one dir, auto-rollback), or
   (b) a doasq card (agent proposes bytes, human approves, runner executes,
   card carries the audit). No free-form root for agents. Ever.
3. **Nothing important is unaudited.** Every privileged write lands in an
   audit surface: the habitat activity ledger (acted-by attribution), the
   doasq card trail, or a system log shipped as summary rows. Monitoring
   reads these; humans read the monitoring.

## 2. The trust graph today (honest inventory, wyo.town)

### Identities (holders, not secrets)
| identity | box access | habitat access | notes |
|---|---|---|---|
| roerick | root (wheel + doas) | admin seat, user=admin | org root key holder; also epik DNS, GitHub, rsync.net account owner |
| hollis (agent) | shell uid, NO root; 2 narrow doas caps | delegated data+eval token | capabilities listed below |
| gregor | root on gregor-box; wyo shell uid (git group, no doas) | seat (admin/agent branch) | factory supervisor operator |
| factory workers | ephemeral sidecars via pp-flow | run rows + card scope | one run = one gate |

### Key inventory (seeds the ledger; fingerprints only here)
| key/token | fingerprint / id | custodian | opens | rotation |
|---|---|---|---|---|
| hollis id_ed25519 | SHA256:TA3f5w8E... | hollis | wyo shell (agent lane) | regen 2026-10-08; yearly |
| deploy_ship ed25519 | SHA256:XJq/3x6O... | hollis (+gregor per hook) | deploy@155.138.208.45 ship lane | shared pipeline key — SPLIT ME (see §5) |
| rsync_net ed25519 | SHA256:Xh6fg9xM... | hollis | rsync.net de3289 (backups only) | backup-only scope; yearly |
| ppdesignate token | (prefix-only in ledger) | hollis | habitat data+eval, delegated | rotate on schedule TBC |
| tuna token | (prefix-only) | hollis | tuna claims | ditto |
| kohelet / shofet tokens | (prefix-only) | hollis | kohelet+shofet identities | ditto |
| hook-pp-creds (admin PP login) | (file, 640 hollis:git) | hollis-held for hook | guarded seed reseed | rotate quarterly |
| wyo-site-promote | root helper, doas rule | hollis-executable | site docroot swap ONLY | n/a (narrow by shape) |
| doasq-exec | root helper, doas rule | hollis-executable | approved-card execution | n/a |
| gregor box root key | (gregor holds) | gregor | gregor-box everything | gregor's ledger row |
| LLM provider keys | scattered envs today | various | providers | MIGRATE to gateway (card 66b62597) |
| vaultwarden ADMIN_TOKEN | (planned, argon2) | roerick | vault admin | set-before-exposure |
| postgres roles | (planned) | per-db | per-db | per-role |

### Known gaps this inventory exposes (fix list)
- `deploy_ship` is a SHARED pipeline key (hollis+gregor) — shared keys
  break attribution. Split per-uid.
- LLM provider keys live in scattered env files — the gateway
  consolidation (GoModel card 66b62597) is the structural fix.
- Token rotation cadences are "TBC" — the ledger makes them schedulable.
- `~/secrets` files are 600/640 correctly, but the DIRECTORY has no
  inventory linkage; the ledger is that linkage.

## 3. The build: Key Ledger inside Pricklypear (phase 1)

Inside PP, not a separate monitor, because the hard parts already exist
here: the audit grammar (activity ledger + acted-by), the seat model
(delegation tiers), claims, and cards. A separate monitor becomes correct
only if PP's trust model ever constrains it — extraction, not genesis.
(The autolith analytics surface is prior art for the extraction path.)

- **Ledger:** shared-space rows (`data.s.key.*`) — id, kind
  (ssh-key/token/api/password-derived/helper), custodian identity,
  fingerprint/prefix, grants (what it opens), location pattern, rotation
  cadence, last-rotated, revocation procedure pointer, backup status.
- **Visualization:** a `/security` route — per-identity capability view
  ("what can hollis touch today"), per-key grant view, overdue-rotation
  list, recent privileged actions (from the activity ledger).
- **Writers:** the ledger is written by the SAME discipline as the site
  helper — narrow verbs, acted-by stamped, changes produce card comments.
  A key without a ledger row is an incident (the claim-kinds card
  f526bace pattern applied to secrets: enforced at the boundary).

## 4. Monitoring (phase 2)

- State-change monitors per security surface, the vw-monitor pattern:
  authorized_keys files, doas.conf, new listening ports, cert expiry,
  token age, ledger-vs-reality diff (a key on the box with no ledger row
  alerts; a ledger row with no key on the box alerts).
- Aggregation: ship SUMMARY rows to the habitat ledger (the
  boundary-origin pattern — system logs stay on-box; the ledger gets the
  fact, not the firehose).

## 5. Rotation & revocation (phase 3)

- Per-key revocation runbook in the ledger row (what to kill, what to
  re-encrypt, who to notify).
- The deploy_ship split as the first rotation exercise: two keys,
  attribution restored, ledger updated, old key revoked — proving the
  whole loop on a real key.

## 6. For organizations (the product shape)

The same three rules + the same ledger/dashboard, branded: the customer's
principal holds the root key, agents hold capabilities, every grant is a
ledger row, every privileged action is approved bytes, every secret has a
rotation story. What we ran for ourselves is the demo.
