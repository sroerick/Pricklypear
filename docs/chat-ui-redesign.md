# Chat UI redesign: a traditional chat room on htmx

**Status:** spec draft (2026-09-08, gregor, from roerick's ask: "something which
looks more like a traditional chat room; our harness interface is instructive;
ideally it should use hx-multipart"). Reviews change this doc; code follows it.

**Related:** `docs/agent-ui-harness.md` (observe->judge->fix loop),
`docs/notes-ui-contract.md` (contract style), `libs/chat/lib.pp` (current UI),
`image/lib/prims_request.ml` (`(request-multipart)` prim), `borge/multi-user.borg`.

## Current state (what exists)

- `/chat`: lobby page, message form posts via `hx-post` and swaps ALL of
  `#chat-main` (form re-render included). DMs at `/chat/dm?peer=`.
- Polling: `#chat-messages` re-fetches `/chat/partial` every 4s and re-renders
  the whole history (no incremental swap, no unread marker, scroll jumps).
- Message row: `chat/msg-li` renders author-strong + body card. No timestamps,
  no day dividers, no human/agent distinction, no attachments.
- Lobby is an open space (`chat-lobby`); messages are `chat-message` rows with
  `from/room_id/body/created_at`.

## Target (traditional chat room)

Three-pane shell, harness-inspired:

1. **Rooms rail (left, narrow):** lobby + DM threads + shared spaces once they
   carry rooms; unread counts; presence dot per room (any message in the last
   5 min = active). This is the harness's session list.
2. **Message column (center):** one row per message - avatar chip (author
   initials; colored by identity; agent identities get an `agent` badge, humans
   none), author, HH:MM timestamp, body. Day dividers. New-message separator
   (unread line, like the harness's catch-up boundary). Auto-scroll sticks to
   bottom unless the reader scrolled up (then a "N new messages" pill).
3. **Composer (bottom, pinned):** textarea, Enter sends / Shift+Enter newline,
   char counter at 3500/4000, attachment chips above the input when files are
   staged. While an agent identity is "working" in the room (recent wake, no
   reply yet), a one-line sentinel row ("agent-prickle is working...") - the
   harness's narration line, translated.

Group-room mode (harness cue): when an agent replies in a room it posts ONE
consolidated message, not streamed narration. Private-DM mode may stream later
(out of scope here).

## htmx mechanics

- **Send:** form keeps `hx-post=/chat`, adds `hx-ext="multipart"` so payloads
  encode as multipart/form-data (file inputs ride along); vendor
  `htmx.org/ext/multipart.js` into assets. Server reads fields + files via
  `(request-multipart)` (exists in prims_request.ml) with a JSON-body fallback
  for the old non-multipart path.
- **Swap discipline:** POST `/chat/send` returns 204 with out-of-band swaps:
  the new message row prepended to `#chat-messages`, composer cleared,
  unread/counter updated. No full `#chat-main` re-render.
- **Polling -> incremental:** `/chat/partial?after=<last-id>` returns ONLY rows
  newer than the client's last id (204 + empty when nothing new). Client keeps
  `last-id` in a `hx-vals` hidden field or data attribute. 4s stays for P1;
  SSE (`hx-sse`) is the P4 replacement if polling shows up in the wedge hunt.
- **History:** `GET /chat?before=<id>` (DM: `/chat/dm?peer=..&before=<id>`)
  returns one older 50-row page under HX-Request; a "Load older messages"
  control swaps it into the `#chat-older` region above the log. A region,
  not `afterbegin`-into-the-log: the 4s poll re-swaps the log window, which
  would clobber rows inserted into the log. Incremental `/chat/partial?after=`
  polling (P1 above) remains the follow-up that unlocks afterbegin-in-log
  history. Cursor fields already exist (`created_at`, row ids are sortable
  epoch-based).

## Attachments (the hx-multipart payoff)

- Composer file input (multi, 5 MB/file cap, mime allowlist: text, images
  png/jpg/webp, pdf, logs).
- Server: `(request-multipart)` -> store bytes via the media store
  (`image/lib/media.ml`; if no write prim exists yet, expose `(media-put)` -
  P2 kernel ask) and attach `media_id` to the message fields dict.
- Render: attachment chip in the row (name + size); images inline-thumb
  (max-height 12rem) linking to the full object; text/plain and logs render
  first 40 lines with an expand link.

## Agent affordances (harness-instructive)

- Identity badge on author chip: `agent` for `user_type=agent` accounts
  (agent-prickle), nothing for humans; picoclaw shows `ext` once it has a
  token.
- Wake/reply pairing: agent messages already carry deterministic ids; surface
  them as muted footer text ("in reply to <id>") instead of threading UI.
- Rooms the bot serves show the sentinel row while its job is running
  (`jobs` table, `status=running`, fn in the agent namespace).

## Phases

- **P1 - look like a chat room (no kernel asks):** 3-pane shell + CSS in the
  theme lib, timestamps + day dividers + identity badges, incremental
  `/chat/partial?after=`, POST returns OOB swaps, Enter-to-send, sticky
  scroll. Accept: send a message and only ONE new row appears in the DOM;
  polling returns empty 204 when idle; scroll position survives a poll.
- **P2 - attachments:** vendor multipart.js, `hx-ext` composer,
  `(request-multipart)` handler, media store write path, chips + inline
  thumbs. Accept: send a PNG + a log file from a phone; both render; 6 MB
  file rejected with a visible error.
- **P3 - presence + unread:** unread separator + per-room counts, sentinel
  row for working agents, DM parity for all of the above. Accept: unread line
  lands exactly on first unseen message after reload; counts clear on view.
- **P4 - (optional) SSE:** replace polling if it ever shows in the wedge hunt.

## Shipped: windowed live view + lazy history (2026-10-18, card 70015a74)

- Live surface (initial GET, 4s poll, POST response) renders only the last
  `chat/window-size` (50) messages, oldest-first - no full-chat fetch/render.
  The read is bounded in SQL (`find-rows` on `room_id`, `ORDER BY created_at
  DESC LIMIT 50`), so the poll fetches 50 rows per tick, not the room.
- Sticky scroll: `ui/chat-log` snaps to bottom on every swap and on load
  (`hx-trigger="load, every 4s"`) unless the reader scrolled up
  (wheel/touchmove sets a read flag on the log); sending a message clears
  the flag and snaps the target log to the latest (composer after-request).
- DM parity: the DM log now polls `GET /chat/dm?peer=` (an HX-Request GET
  returns the windowed transcript fragment), so DMs get the same autoscroll
  and catch-up behavior as the lobby.
- History accumulates: the load-older control swaps `afterbegin` into
  `#chat-older`, so each page is prepended above the pages already loaded; the
  response carries an out-of-band refresh of the next control (`chat/older-oob`)
  and deletes it at the oldest message (`chat/older-gone`). One page is a
  bounded cursor probe (`chat/older-rows`, doubling 100/200/400-row newest-first
  reads) rather than a whole-room scan.
- The old "message hidden after it falls out of the window" gap is gone: the
  live window is fetched by id-ordered `created_at` around the newest rows and
  the poll re-reads only that window, so catch-up no longer depends on a
  whole-chat scan.

## Open questions

1. Backfill depth: how much history does initial load fetch (suggest last 50,
   pager for more)?
2. Attachment retention: same policy as chat rows, or separate?
3. Do spaces carry rooms next (room-per-space), so the rail lists shared
   spaces natively? (Leans on `space-list` + room_id conventions.)
4. DM streaming: keep consolidated-only for v1?
