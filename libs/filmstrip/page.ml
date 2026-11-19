(* The filmstrip page: PP's public surface keeps filmstrip/page, but the
   strip rendering delegates to the shared package (a bare number never
   appears on a tile -- the shared renderer stamps every one). *)

let strip () = Filmstrip_shared.render_strip (Filmstrip.tiles ())

let commits () =
  Filmstrip_shared.render_commits (Filmstrip.recent_commits ())

(* The banana button: one declared button on the page; pressing it raises
   the (in)famous alert and nothing else -- no state, no navigation, so
   dismissing the alert leaves the page exactly as it was. *)
let banana () =
  "<p class=\"fs-banana-row\"><button type=\"button\" class=\"fs-btn\" \
   onclick=\"alert('Yes! we have no bananas')\">banana</button></p>"

(* The page: the banana button, the launcher strip, then the read-only
   commit filmstrip. *)
let page () = banana () ^ strip () ^ commits ()
