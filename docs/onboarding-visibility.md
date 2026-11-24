# Onboarding: the visibility model

**Everything is private unless it is marked public** (owner decision,
2026-11-23; book: `borge/notes.borg` note-visibility, "Absent reads private").
A write with no `vis` field is readable only by its owner and by members of the
space it lives in.

## The one flag

Every write may carry a `vis` field in its body:

| `vis` | who reads it |
| --- | --- |
| *(absent)* or `private` | the owner, plus members of the space that contains the path (`data.s.<space>.*`, `tree_path_in_member_space`) — **the default** |
| `users` | any signed-in identity (`anonymous` excluded) |
| `public` | **anyone**, anonymous included — explicit flag only |

The flag only ever widens. The same rule is enforced in three places that must
agree: the database floor (`pp_vis_readable`, migration 050), the note gate
(`Crud.note_vis_allows` / `gate_note_vis`), and `Tree_store.find_by_body_field`.
No identity, `root` included, bypasses it.

## Publishing

To make something readable by others, set `vis: "public"` (or `"users"`) on the
write. Town namespaces (`u.hollis`, `u.gregor`, …) follow the same rule: an
unflagged write there is private to its owner.

## The edge, named

Claims are journaled and immutable, so a write marked public stays public.
Mark public deliberately.

Public projections that are routes rather than rows, such as the public board
(card titles and states only, `image/lib/public_board.ml`), are their own
declared public surface and are unaffected by this flag.
