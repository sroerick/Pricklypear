# Security audit — doas (wyo.town privileged-escalation surface)

**Date:** 2026-10-27
**Auditor:** hollis (agent), card `6940aa9b-5b43-418f-85b4-ac8ad1fb8486`
**doas version audited:** doas as shipped in **OpenBSD 7.9** (base-system
`/usr/bin/doas`; doas(1) has no `-V`, so the version is pinned by the OS
release the prod host runs). Source of that pin — command run in the
worktree, exit 0:

```
$ /usr/bin/grep -rn "OpenBSD 7.9" scripts
scripts/build-release-openbsd.ksh:3:# Must run on OpenBSD 7.9 amd64 with the poohstack opam switch.
```

On-host confirmation is one command (appendix, `audit-doas.ksh`):
`uname -rms` → expected `OpenBSD 7.9 amd64`.

**doas.conf path audited:** **`/etc/doas.conf`** (the default; the only
override in the tree is `DOAS_CONF=${DOAS_CONF:-/etc/doas.conf}`, hit at
`scripts/openbsd/git-wyo-stagit.ksh:34`).

**Scope.** Every doas grant this repository installs or relies on, plus the
installer code that writes them. Point-in-time review of the rule set the
repo declares; the live file on wyo.town must be re-read with the appendix
script (the worktree is a Linux checkout and has no doas(1)).

**Method.** `grep` over the tree for every writer and reader of doas.conf
(commands and outputs are quoted verbatim under each finding), then a
line-by-line read of the two writers: `scripts/doasq/bootstrap-doasq.ksh`
(the doasq queue's one standing grant) and
`scripts/openbsd/git-wyo-stagit.ksh` (`ensure_doas_permit`). Threat model
cross-checked against `borge/doasq.borg` (section `threat-model`) and
`docs/security-architecture.md` §1–2.

---

## 1. The complete doas.conf rule set the repo declares

Two rules, both `permit`, both `nopass`. Everything else in the tree only
*reads* doas.conf or *calls* doas.

| # | rule (verbatim) | who may run | what | as whom | verdict |
|---|---|---|---|---|---|
| R1 | `permit nopass hollis cmd /usr/local/bin/doasq-exec` | user `hollis` (agent) only | `/usr/local/bin/doasq-exec` with any argv | `root` (implicit — no `as`) | **PASS, with finding F1** |
| R2 | `permit nopass :wheel as git cmd /home/git/srv/git/stagit-regen.ksh` | members of `$PUSHER_GROUP` (default `:wheel`) | `stagit-regen.ksh` with any argv | `git` | **PASS, with findings F2/F3** |

Evidence for R1 — command run, exit 0:

```
$ /usr/bin/grep -rn "permit nopass" scripts borge
scripts/doasq/bootstrap-doasq.ksh:8:#   3. ONE doas.conf rule: permit nopass hollis cmd /usr/local/bin/doasq-exec
scripts/doasq/bootstrap-doasq.ksh:34:rule='permit nopass hollis cmd /usr/local/bin/doasq-exec'
scripts/doasq/doasq-exec:4:# backed by the single doas rule:  permit nopass hollis cmd /usr/local/bin/doasq-exec
scripts/doasq/doasq-exec-v2:4:Invoked via the single doas rule (permit nopass hollis cmd doasq-exec).
scripts/openbsd/git-wyo-stagit.ksh:266:        print -r -- "permit nopass :$PUSHER_GROUP as $GIT_USER cmd $REGEN"
```

Evidence that nothing else writes the file — command run, exit 0:

```
$ /usr/bin/grep -rn "doas.conf" scripts docs borge
scripts/doasq/bootstrap-doasq.ksh:35:if grep -qsF "$rule" /etc/doas.conf; then
scripts/doasq/bootstrap-doasq.ksh:38:  cp /etc/doas.conf /etc/doas.conf.pre-doasq 2>/dev/null || true
scripts/doasq/bootstrap-doasq.ksh:39:  print -r -- "$rule" >> /etc/doas.conf || die "cannot append to /etc/doas.conf"
scripts/openbsd/git-wyo-stagit.ksh:34:DOAS_CONF=${DOAS_CONF:-/etc/doas.conf}
scripts/openbsd/git-wyo-stagit.ksh:268:    priv doas -C "$DOAS_CONF" >/dev/null 2>&1 \
```

### R1 — `permit nopass hollis cmd /usr/local/bin/doasq-exec`

- **Who:** exactly `hollis`; no group, no wildcard, no `:wheel`. Agents
  other than hollis cannot use it. ✔
- **What:** one path, `cmd` scoped, absolute, root-owned
  (`install -m 755 -o root -g wheel …` at `bootstrap-doasq.ksh:31`). The
  binary is not writable by hollis, so the grant cannot be widened by
  editing the helper. ✔
- **As whom:** root (no `as` clause). That is the intent: the helper *is*
  the root step. ✔
- **Why `nopass` is acceptable here:** password is not the control — the
  approval gate is. `borge/doasq.borg` (`runner`, `threat-model`) states
  it: hollis cannot self-approve (`/var/doasq/approvals` is 750
  roerick-only), the approval is sha-bound to the exact bytes and
  one-shot. `nopass` is what makes phone/ssh approval viable. ✔
- **Verdict: PASS.** The *rule* is as narrow as doas can express. See F1
  for what the rule's narrowness does **not** buy.

### R2 — `permit nopass :wheel as git cmd /home/git/srv/git/stagit-regen.ksh`

- **Who:** `$PUSHER_GROUP`, default `:wheel`
  (`git-wyo-stagit.ksh:36 PUSHER_GROUP=${PUSHER_GROUP:-wheel}`).
- **What:** one script, absolute path, created by root
  (`install -o git -g git -m 755 "$hk" "$REPO/hooks/post-receive"` /
  regen installed into `/home/git/srv/git`).
- **As whom:** `git` — **not root**. This is the important one: the
  privilege ceiling is the `git` service uid, which owns the stagit
  htdocs tree and nothing else. No root path from this rule. ✔
- **Verdict: PASS.** Findings F2/F3 are scoping nits, not escalations.

---

## 2. Findings

Severity: P0 = root/tenant break, P1 = real risk needing a fix, P2 =
hardening.

### F1 (P1) — R1's `cmd` narrowness does not constrain *what runs*; the
approval gate does, and it is a human control with no timeout

R1 grants root for a helper that executes arbitrary text
(`scripts/doasq/doasq-exec:65`):

```
$ /usr/bin/grep -rn 'ksh -c' scripts/doasq
scripts/doasq/doasq-exec:65:/bin/ksh -c "$cmd_text" >"$out" 2>&1
scripts/doasq/doasq-exec-v2:12:  run via ksh -c; post the output as a card comment; move the card to
```

(exit 0). So the effective capability of R1 is *caller-chosen root shell*,
narrowed only by: (a) `/var/doasq/approvals` unreadable/unwritable by
hollis, (b) sha256 of the exact bytes in the approval, (c) one-shot atomic
claim, (d) full-text audit. All four are present in the code (read of
`doasq-exec:41–73`). This matches the accepted residual risk in
`borge/doasq.borg` (`threat-model`: "a command can still DO anything root
can — the queue gates WHO, not WHAT"). Recorded, not a defect.
**Residual to keep watching:** no exec timeout (same spec paragraph), so a
queued long-running/blocking command holds root indefinitely and the
approval cannot be revoked mid-run.

### F2 (P1) — `PUSHER_GROUP` is environment-overridable at install time

`git-wyo-stagit.ksh:36`:

```
$ /usr/bin/grep -rn 'PUSHER_GROUP=' scripts
scripts/openbsd/git-wyo-stagit.ksh:36:PUSHER_GROUP=${PUSHER_GROUP:-wheel}
```

(exit 0). The script runs as root (its own header says so) and writes
`permit nopass :$PUSHER_GROUP as git cmd $REGEN`. An operator running
`PUSHER_GROUP=guests ksh git-wyo-stagit.ksh` — or a root-level caller
that inherits the variable from an agent environment — installs a doas
grant for whatever group is named, with no validation and no echo-back of
the resolved group. Today's default (`:wheel`) is *weaker* than it looks
(wheel members are already root-capable) but harmless; a typo'd or
injected value is a new grant. **Fix:** validate `$PUSHER_GROUP` against
existing groups (`getent group` / `/etc/group`) and refuse anything not
explicitly allow-listed; print the resolved rule before writing (it does
print after — keep that).

### F3 (P2) — R2 has no `args` clause, and regen takes a caller-supplied path

`permit … cmd …/stagit-regen.ksh` permits *any* argv. The post-receive
script passes `"$dir"` (`git-wyo-stagit.ksh:208`, `:210`, `:221`, `:223`).
An attacker in the pusher group can hand `stagit-regen.ksh` a foreign
directory path. Ceiling is the `git` uid, so this writes as `git`, not
root — but a path that resolves under
`/var/www/htdocs/git.wyo.town` is world-readable content. **Fix:**
`args`-pin the shape (`args *` is not tighter, so instead validate in the
wrapper that `$1` is one of the repo roots, refusing anything outside
`$GITROOTS`), which also makes the grant self-documenting.

### F4 (P1) — the bootstrap appends the rule with no newline guard and no
`doas -C` verification

`scripts/doasq/bootstrap-doasq.ksh:33–40` (read in full):

```ksh
rule='permit nopass hollis cmd /usr/local/bin/doasq-exec'
if grep -qsF "$rule" /etc/doas.conf; then
  print -- "   already present"
else
  cp /etc/doas.conf /etc/doas.conf.pre-doasq 2>/dev/null || true
  print -r -- "$rule" >> /etc/doas.conf || die "cannot append to /etc/doas.conf"
  print -- "   added: $rule"
fi
```

Two problems, both demonstrated by the code above (exit 0 on the grep that
shows it):

1. **No trailing-newline guard.** `>>` after a file whose last line has no
   newline concatenates the grant onto that line. If the last line is a
   comment, the grant silently becomes a comment (a *fail-closed* outage);
   if it is another `permit`, the resulting line is a parse error and
   doas's parser rejects the file. Cheap fix: ensure a newline
   (`tail -c1 /etc/doas.conf | read -r _ || print >> /etc/doas.conf`)
   before appending.
2. **No `doas -C` after the write.** The sibling installer does verify
   (`scripts/openbsd/git-wyo-stagit.ksh:268–269`,
   `priv doas -C "$DOAS_CONF" >/dev/null 2>&1 || say "warning…"`), the
   doasq bootstrap does not — and it is the one that *creates* the queue's
   only root path, at the very moment an error is likely. Fix: run
   `doas -C /etc/doas.conf` after appending and, on failure, restore
   `/etc/doas.conf.pre-doasq` and die.

### F5 (P2) — neither writer normalizes doas.conf ownership/mode

No `chmod`/`chown` on `/etc/doas.conf` anywhere in the tree; command run,
exit 0 (`/usr/bin/grep -rn 'chmod .*doas' scripts` returns nothing). The
audit scripts check perms for `/var/doasq/{requests,approvals,runs,audit.log}`
(`bootstrap-doasq.ksh:24–27`) but not for the file that authorizes root.
doas refuses a config writable by non-root, so this fails closed, but the
operator gets an opaque denial later instead of an error at install time.
**Fix:** in the bootstrap, assert `stat -f '%Su:%Sg %Lp' /etc/doas.conf`
is `root:wheel 644` (or stricter) and die otherwise.

### F6 (P2) — `doas -C` failure in the stagit installer only warns

`git-wyo-stagit.ksh:268–269` downgrades a syntax check to a `say`
warning. Low impact (fail-closed parser, and the fragment is a single
literal line), but the queue bootstrap should not copy that softening —
see F4.2.

---

## 3. What the audit did *not* find

- No `permit` without `cmd` in either writer (no `permit persist :wheel`
  style blanket grant installed by this repo).
- No `permit` with `as root` written explicitly, no wildcard user, no
  `setenv` clause, no `keepenv`.
- hollis holds no interactive root: `docs/security-architecture.md:33`
  ("shell uid, NO root; 2 narrow doas caps") is what the code shows — R1
  and R2 are those two caps, and R2 is `as git`, not root.
- `doasq-exec-v2` is invoked through the *same* R1 rule and reads only
  `/var/lib/doasq/{list-id,approver}` + the habitat API; it adds no rule.

## 4. Overall verdict

**doas: CLEAN — two passes, six findings, none of them an escalations
defect.** R1 and R2 are each a single absolute `cmd` for a named principal
or named group, root-owned, and R2's ceiling is the `git` service uid. The
security of the R1 grant rests on the doasq approval gate, which is
present in code and matches the spec's accepted residual risks. The open
items (F2 group validation, F3 argument scoping, F4 newline +
`doas -C` in the bootstrap, F5 conf mode assertion) are installer
hardening: each can turn a deliberate, reviewable grant into an
unreviewed or opaque one, and each is a five-line fix in the two scripts
named above. Re-audit after F2, F3 and F4 land, and re-read the live
`/etc/doas.conf` with the appendix script on the host.

---

## Appendix — re-running this audit on wyo.town

`scripts/doasq/audit-doas.ksh` reads `$DOAS_CONF` (default
`/etc/doas.conf`), prints the host/version, the file's ownership and mode,
and gives every non-comment line a PASS / REVIEW / FAIL verdict, then a
summary. Run it as roerick:

```sh
uname -rms                      # expected: OpenBSD 7.9 amd64
ls -l /etc/doas.conf            # expected: -rw-r--r-- root wheel
doas -C /etc/doas.conf; echo rc=$?   # 0 = parses
ksh scripts/doasq/audit-doas.ksh
```

Its rule verdicts: a `permit` with no `cmd` (or no path) is FAIL (any
command as the target); `nopass` and a missing `as` are REVIEW
(deliberate here for R1, flagged so the reader re-confirms it);
`args`-pinned rules are the strongest and report PASS-args. `deny` lines
always PASS.
