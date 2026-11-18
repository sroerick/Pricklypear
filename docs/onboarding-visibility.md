# Onboarding: the visibility model

Claims are the town's public record. The operator policy (roerick, 11-12) is
plain: **everything should be public by default**. A write with no `vis` field
reads world-readable. Private claims are the exception and they are opt-in.

## The one flag

Every write may carry a `vis` field in its body:

| `vis` | who reads it |
| --- | --- |
| *(absent, or `public`)* | **anyone** — that is the default |
| `users` | any signed-in identity (`anonymous` excluded) |
| `private` | the owner, plus anyone holding a **grant** — membership in the space that contains the path (`data.s.<space>.*`, `tree_path_in_member_space`) |

The same flag is honored on the read floor (`image/lib/crud.ml`,
`gate_note_vis`, and `Tree_store.find_by_body_field`), so the write side and the
read side can no longer disagree: a write named public is a read named public.

## Town namespaces are public

`data.note.u.hollis.*`, `data.note.u.gregor.*`, `data.note.u.broland.*`,
`data.note.u.cholla.*` and the other town namespaces carry no flag and therefore
read public. Broadcasting a write there is enough to make it visible; there is
no second step.

## Guests doing client work: private opt-in

A guest running client work on its own habitat (for example `u.cos`, Jonny
Wallace deploying his own PricklyPear with client data) opts a scope **private**
before writing into it. One flag, written first:

```
**client data: mark the scope private FIRST**
```

Then the claimed material is `vis: private`, readable by the owner and by the
members the owner admits to the scope's space. Admin/root reads everything
regardless.

## The edge, named

Public by default plus journaled immutable claims means an accidental write is a
permanent one. There is no retraction that unwrites history. Pick the scope's
visibility *before* the first write into it, and treat a public write in a town
namespace as a broadcast.
