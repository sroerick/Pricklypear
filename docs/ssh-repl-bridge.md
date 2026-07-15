# SSH → REPL bridge (auth.user-ssh-repl)

Phase 1 (shipped): credentials kind `ssh_key`, `/settings/ssh`, prims
`add-ssh-key` / `list-ssh-keys` / `revoke-ssh-key` / `verify-ssh-key`.

Phase 2 door options:

## Option A — OpenBSD sshd ForceCommand (ops escape)

1. Users register OpenSSH pubkeys in the image (`/settings/ssh`).
2. `AuthorizedKeysCommand` dumps keys from Postgres or a sync script:
   ```
   SELECT public_data->>'openssh' FROM credentials WHERE kind='ssh_key';
   ```
3. `ForceCommand` runs a small client that:
   - maps the authenticated Unix/SSH user name to Pricklypear username
   - opens the image line-protocol REPL (`PP_REPL_PORT`, default 17878)
   - or `POST /api/eval` with Basic auth for the mapped user

Example sketch (`/usr/local/bin/pp-ssh-repl`):

```sh
#!/bin/ksh
# $USER is the SSH username = pricklypear username
export PP_URL=http://127.0.0.1:18080
export PP_USER="$USER"
# password from agent or key-derived session — prefer loopback eval with
# a short-lived token minted by a root helper that called verify_ssh_key.
exec nc 127.0.0.1 "${PP_REPL_PORT:-17878}"
```

Identity must match: never open a shared admin REPL for all SSH users.

## Option B — awa-ssh in-image (product primary, later)

Listen on `PP_SSH_PORT` when set; hostkey seed in `system_keys`;
pubkey auth via `Auth_store.verify_ssh_key`; attach `Session.t` with
`whoami` + personal branch; bridge channel to REPL.

Until B ships, use Option A or browser `/repl` + HTTP Basic.
