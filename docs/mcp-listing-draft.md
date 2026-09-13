# MCP listing draft: Pricklypear as an MCP server

Draft listing content for the MCP registries (PulseMCP, Smithery, Glama,
mcp.so, official registry). The goal: agents doing capability discovery
find Pricklypear, hand their human a one-line config, and get a persistent
habitat without anyone writing integration code.

## Server identity

- **Name:** `pricklypear`
- **Display:** Pricklypear Habitat
- **Transport:** Streamable HTTP at `https://pricklypear.club/mcp` (to be
  built; see checklist)
- **Auth:** per-tenant bearer token; the human creates the account, the
  token goes in the client config. Same contract as `/api/eval` today.
- **Profile docs:** `https://pricklypear.club/agent.txt` and
  `/.well-known/agent.json` already exist; keep them in sync with this
  surface.

## One-liner (directory description)

> Give your agent a persistent Lisp habitat: memory, notes, data rows,
> schedules, and live functions that survive restarts, on a multi-user
> Nopales image. Write code once, call it forever.

## Draft tool surface

| Tool | Maps to | Notes |
|------|---------|-------|
| `pp_eval` | `/api/eval` | Run Nopales in the tenant garden; defines persist |
| `pp_notes_upsert` | notes prim | Create/update notes in the tenant garden |
| `pp_notes_search` | notes prim | Full-text search across own notes |
| `pp_data_query` | data prims | Query tenant data rows |
| `pp_data_insert` | data prims | Insert tenant data rows |
| `pp_schedule_set` | schedules prim | Cron-style schedules, utc or local tz |
| `pp_schedule_list` | schedules prim | List own schedules |
| `pp_media_put` | media prim | Store media, respects quota |

All tools execute against the caller's own garden; nothing crosses tenants
(the author prim and RLS are the existing guarantees).

## Client config snippet

```json
{
  "mcpServers": {
    "pricklypear": {
      "url": "https://pricklypear.club/mcp",
      "headers": { "Authorization": "Bearer <tenant-token>" }
    }
  }
}
```

## Checklist to make it real

1. Build the MCP transport endpoint on the image (streamable HTTP; a thin
   wrapper that maps tools to existing prims; no new storage).
2. Enforce the per-tenant token + tier limits on `/mcp` the same as
   `/api/eval` (writes/min, media quota).
3. Version the tool list in the listing; registries re-index on their own
   cadence.
4. Submit: PulseMCP, Smithery, Glama, mcp.so, official registry. Each takes
   a name, description, transport URL, and auth note - the copy above is
   written to paste in.
5. After listing, add an `/agents` page section: "found us via an MCP
   registry? start here" pointing at the invite flow.

## Listing copy (longer form)

Pricklypear is a multi-user Lisp OS image where an agent's work compounds:
defines stay defined, memory stays remembered, schedules keep firing after
the session ends. Humans get an account; the agent gets a garden with
notes, data, and cron. The MCP surface exposes the core prims as tools so
any MCP-capable client can give its agent a home directory in one config
line. Free tier available; see the site for limits.
