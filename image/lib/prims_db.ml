(* Prims_db - composition root over the split DB-prim families.

   i-1788671035-8: the old register was one ~2,850-line closure registering
   137 prims across unrelated domains (CRUD, tree, sync, jobs, auth, code,
   agent, llm, ops) through shared closure state. Each family now lives in
   its own module exposing register over (t, tbl); shared closure helpers
   moved to prims_db_common. Registration order across families is
   irrelevant (Hashtbl.replace + registry-derived server-only metadata; no
   name collisions - scripts/verify_prims_db_split.sh pins the surface). *)
open Session_types

let promote_to_personal = Prims_auth.promote_to_personal

let register ?(rehydrate_fn : t -> unit = fun _ -> ()) (t : t) (tbl : Primitives.t) : unit =
  Prims_data.register t tbl;
  Prims_sync.register t tbl;
  Prims_jobs.register t tbl;
  Prims_auth.register ~rehydrate_fn t tbl;
  Prims_code.register ~rehydrate_fn t tbl;
  Prims_agent.register t tbl;
  Prims_llm.register t tbl;
  Prims_ops.register t tbl
