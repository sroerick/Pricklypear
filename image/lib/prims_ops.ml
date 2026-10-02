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

  (* (ops/activity [actor] [since] [class]) -> VList of JSON row strings
     {at,actor,class,target,outcome,cost}. product-experience
     §activity-ledger READ face.

     ADMIN: unfiltered; may pass [actor] to scope. NON-ADMIN: force-
     filtered to its OWN actor rows at the SQL layer (read_rows ignores a
     supplied actor and ANDs `actor = $caller`). A watched machine shows
     its watch to the watched. Read-only, safe for agent monitors.

     A class argument is validated against the closed class grammar and
     REJECTED when unknown (safe_sort_sql precedent). An empty string
     means "no filter" for any positional slot. A trailing VInt sets the
     row limit (default 100, capped at 1000). *)
  Primitives.register tbl "ops/activity"
    (fun args ->
      let uname = Prims_db_common.caller_username ~t () in
      let is_admin = Prims_db_common.is_verified_admin ~t () in
      let no_filter s = s = "" || s = "*" || s = "all" in
      (* Positional: slot 0 = actor, 1 = since, 2 = class. An empty
         string means "no filter" but STILL consumes its slot, so the
         following argument is not misread as a timestamp (a class in
         the since slot is a 22007). A VInt anywhere sets the limit. *)
      let actor = ref None and since = ref None and cls = ref None in
      let limit = ref None in
      let slot = ref 0 in
      List.iter
        (function
          | Pricklypear_lisp.Ast.VStr s ->
              (match !slot with
               | 0 -> if not (no_filter s) then actor := Some s
               | 1 ->
                   (* Disambiguate the optional since/class pair: a value
                      in the class grammar is the class, anything else
                      is a timestamp. `(ops/activity "alice" "api.eval")`
                      then works without an empty-string placeholder. *)
                   if (not (no_filter s)) && Activity_ledger.valid_class s
                   then cls := Some s
                   else if not (no_filter s) then since := Some (`Iso s)
               | 2 -> if not (no_filter s) then cls := Some s
               | _ -> ());
              incr slot
          | Pricklypear_lisp.Ast.VInt n -> limit := Some (Int64.to_int n)
          | _ -> ())
        args;
      let actor = !actor and since = !since and cls = !cls and limit = !limit in
      match cls with
      | Some c when not (Activity_ledger.valid_class c) ->
          (Pricklypear_lisp.Ast.VStr
             (Printf.sprintf "ops/activity: unknown activity class %S" c))
      | _ ->
          let rows =
            Activity_ledger.read_rows ~store:t.store ~caller:uname ~is_admin
              ?actor ?since ?cls ?limit ()
          in
          (Pricklypear_lisp.Ast.VList
             (List.map (fun s -> Pricklypear_lisp.Ast.VStr s) rows)));

  (* (ops/knock) -> JSON {knocks:[...], trifecta:[...]}. product-experience
     §activity-ledger KNOCK SCHEMA + lethal-trifecta row.

     ADMIN ONLY. Deterministic detection over STRUCTURE-ONLY ledger rows
     in a 24h window, then files at most ONE open ops-incident per
     (kind, actor) through the existing fingerprint queue — a repeat in a
     later scan BUMPS the count, never files a second row. LAW 1: no
     model in the loop; an LLM may later rewrite title/body, never counts.
     The trifecta legs are CLASS COUNTS ONLY (never content). *)
  Primitives.register tbl "ops/knock"
    (fun _args ->
      let is_admin = Prims_db_common.is_verified_admin ~t () in
      if not is_admin then
        (Pricklypear_lisp.Ast.VStr "ops/knock: denied: admin only")
      else begin
        let window = 86_400.0 in
        let since = Unix.gettimeofday () -. window in
        let events =
          Activity_ledger.scan_events ~store:t.store ~since ~limit:5000 ()
        in
        let knocks = Knock_policy.detect events in
        let filed =
          List.map
            (fun (k : Knock_policy.knock) ->
              let id =
                Ops_incidents.file_security_incident ~store:t.store
                  ~fingerprint:k.fingerprint ~kind:k.kind ~actor:k.actor
                  ~title:k.title ~count0:k.count
              in
              `Assoc
                [ ("id", `String id); ("kind", `String k.kind);
                  ("actor", `String k.actor); ("count", `Int k.count);
                  ("title", `String k.title) ])
            knocks
        in
        let tri =
          List.map
            (fun (r : Knock_policy.trifecta_row) ->
              `Assoc
                [ ("actor", `String r.actor);
                  ("private_data", `Bool r.private_data);
                  ("untrusted_ingest", `Bool r.untrusted_ingest);
                  ("effect_channel", `Bool r.effect_channel);
                  ("flag", `Bool r.flag) ])
            (Knock_policy.trifecta events)
        in
        Pricklypear_lisp.Ast.VStr
          (Yojson.Safe.to_string
             (`Assoc
                [ ("window_seconds", `Int (int_of_float window));
                  ("scanned", `Int (List.length events));
                  ("knocks", `List filed); ("trifecta", `List tri) ]))
      end);
