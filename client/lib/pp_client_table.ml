(* The browser evaluator's boot prim table (client/bin/main.ml boot).

   ONE shared catalog — [Prims_pure.install] (pure/prims_pure.ml) —
   instead of the ~110-line hand enumeration this replaces
   (i-1788670799-f: it had drifted 8 prims behind the image — sort,
   string-join, string-split, prose-tokens, render-tokens, err/shape,
   hx-get, hx-post — so any browser eval reaching libs/std failed with
   unbound var on string-split).

   The pure surface IS the browser's capability boundary: no DB / IO /
   net / wall-clock prims here (wall-clock lives in image-only
   prims_time.ml because it reads Unix; sha1 in prims_crypto.ml; the
   image's random-uuid is a Mirage_crypto shadow of the pure one).

   Kept in a library (not client/bin) so the parity test links the
   EXACT table the browser boots with, natively. *)

let build () : Pricklypear_lisp.Eval.prim_table =
  let t = Pricklypear_lisp.Eval.make_prim_table () in
  Prims_pure.install t;
  t
