(* Ops (log tail, summary, incidents) prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  (* Ops: structured log tail + analytics summary (product-experience). *)
  Primitives.register tbl "log-tail"
    (fun args ->
      let n, level =
        match args with
        | [] -> 50, None
        | [ Pricklypear_lisp.Ast.VInt n ] -> Int64.to_int n, None
        | [ Pricklypear_lisp.Ast.VStr lv ] -> 50, Ops_log.level_of_string lv
        | [ Pricklypear_lisp.Ast.VInt n; Pricklypear_lisp.Ast.VStr lv ] ->
            Int64.to_int n, Ops_log.level_of_string lv
        | [ Pricklypear_lisp.Ast.VStr lv; Pricklypear_lisp.Ast.VInt n ] ->
            Int64.to_int n, Ops_log.level_of_string lv
        | _ -> 50, None
      in
      let evs = Ops_log.tail ~n ?level () in
      (Pricklypear_lisp.Ast.VList
           (List.map Ops_log.event_to_value evs)));

  Primitives.register tbl "ops/summary"
    (fun _args ->
      let ok, fail = Ops_incidents.count_logins t.store in
      let j = Ops_stats.summary_yojson ~logins_ok:ok ~logins_fail:fail in
      (Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j)));

  Primitives.register tbl "ops/incidents"
    (fun _args ->
      let j = Ops_incidents.list_open_incidents t.store in
      (Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j)));
