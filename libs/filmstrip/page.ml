(* The filmstrip page: PP's public surface keeps filmstrip/page, but the
   strip rendering delegates to the shared package (a bare number never
   appears on a tile -- the shared renderer stamps every one). *)

let strip () = Filmstrip_shared.render_strip (Filmstrip.tiles ())

let commits () =
  Filmstrip_shared.render_commits (Filmstrip.recent_commits ())

(* The page: the launcher strip, then the read-only commit filmstrip. *)
let page () = strip () ^ commits ()
