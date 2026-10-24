(* Debug prims (roerick 10-22): write-churn + L2 cache counters from
   Tree_store; the same summary is served at /debug/stats. Kept out of
   Prims_data on purpose: prims_data references Primitives (its register
   table), so a Primitives -> Prims_data edge closes a module cycle. *)
let prim_debug_stats (_args : Pricklypear_lisp.Ast.value list)
    : Pricklypear_lisp.Ast.value =
  Pricklypear_lisp.Ast.VStr (Tree_store.debug_stats_json ())
