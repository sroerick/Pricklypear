(** branch-ops pick row (branching.op-transplant, spec commit 3f4d6d1,
    2026-10-16): the store oracle for OP-LEVEL TRANSPLANT — named writes move
    from the source branch's gen stream onto a target branch as NEW WRITES,
    cherry-pick over the log.

    The PRIM (session_control_prims "branch/pick") owns identity and
    authorization; this module owns the graph and the landing, mirroring
    branch_push.ml.

    SEMANTICS (spec-locked in make phase):
    - RESTORE-IS-A-WRITE (the checkpoints discipline): picked bodies land
      through the tree write helpers at the TARGET head gen — version trail
      + reverse mirrors + code_history before-images fire for free, the gen
      counter only moves forward, target base_gen untouched. The pick is
      itself a log entry on the target (origin='pick', origin_detail =
      <source>@<coordinate>, migration 042), so a bad pick is undone by
      picking the before-image back — no special revert path.
    - CLOBBER GUARD: a target OWN pin whose hash differs from the picked
      body is DIVERGED — divergence is never silently overwritten
      (branching.merge philosophy). Default: the name is skipped with
      diverged-target + both hashes; ~force lands it (the before-image in
      code_history + the log entry naming the source satisfy the obituary
      rule). A target pin whose hash EQUALS the picked body is skipped as
      unchanged (pin hygiene: never write what would be identical).
    - CLOSURE FIRST (branch/push discipline; the 2026-10-05 pulse-row-from
      class): Call_graph.push_closure runs over the picked fn bodies
      against the target chain; an unbound ref auto-includes from the
      source's resolved pins or REFUSES the pick WHOLE with the complete
      hole report. Never a partial landing.
    - ABSENT IS A VALUE, NOT AN ERROR: a requested name with no
      non-tombstone row at the source coordinate REFUSES the pick — an
      unbind is a separate explicit op, never smuggled through a pick.
    - LANDING TX: all landed writes share ONE transaction stamped
      set_config('pp.origin','pick') (rider-safe: the GUCs reset in a
      finally), so the version trail records the pick's provenance
      atomically (migration 042). *)

type source_at =
  | Pick_head
  | Pick_cp of string
  | Pick_gen of Int64.t

let source_at_s = function
  | Pick_head -> "head"
  | Pick_cp n -> "cp:" ^ n
  | Pick_gen g -> "gen:" ^ Int64.to_string g

type pick_row = {
  pr_kind : string;    (* "fn" | "type" | "route" *)
  pr_name : string;
  pr_hash : string;
  pr_payload : string; (* full node payload jsonb — the landing source *)
}

type skip_row = {
  sk_kind : string;
  sk_name : string;
  sk_reason : string;
}

type pick_result =
  | Pick_ok of {
      landed : pick_row list; (* requested + auto, actually written *)
      auto : string list;     (* closure auto-included names that landed *)
      skipped : skip_row list; (* unchanged / diverged-target, with reason *)
    }
  | Pick_holes of (string * string list) list
      (* refused: name x refs unbound on the target and defined nowhere *)
  | Pick_refused of string
      (* structural: same branch, empty set, no such cp, absent-at-coordinate *)

let holes_msg = Branch_push.holes_msg

  let rec pick (store : Store.t) ~(source_id : string) ~(target_id : string)
    ~(names : string list) ~(at : source_at) ~(force : bool)
    ~(kernel : string list) ~(author : string) ~(source_ref : string) :
    pick_result =
  if source_id = target_id then
    Pick_refused "cannot pick a branch into itself"
  else
    let of_pins (ps : Store.pin_row list) : pick_row list =
      List.map
        (fun p ->
           {
             pr_kind = p.Store.p_kind;
             pr_name = p.Store.p_name;
             pr_hash = p.Store.p_hash;
             pr_payload = p.Store.p_payload;
           })
        ps
    in
    (* 1. resolve the source coordinate (head / checkpoint / gen) *)
    let resolve () : (pick_row list, string) result =
      match at with
      | Pick_head -> Ok (of_pins (Store.pins_at_head store ~branch_id:source_id))
      | Pick_cp name -> (
          match Store.lookup_checkpoint store ~branch_id:source_id ~name with
          | None -> Error ("no such checkpoint on source: " ^ name)
          | Some cp ->
              Ok
                (of_pins
                   (Store.pins_at_gen store ~branch_id:source_id
                      ~cutoff:cp.Store.cp_gen)))
      | Pick_gen g ->
          Ok (of_pins (Store.pins_at_gen store ~branch_id:source_id ~cutoff:g))
    in
    match resolve () with
    | Error why -> Pick_refused why
    | Ok resolved ->
        let requested = Call_graph.uniq_stable names in
        if requested = [] then Pick_refused "no names requested"
        else
          (* 2. ABSENT is a value: a pick of an absent name refuses *)
          let find_pin n =
            List.find_opt (fun (p : pick_row) -> String.equal p.pr_name n) resolved
          in
          let absent =
            List.filter (fun n -> match find_pin n with None -> true | _ -> false) requested
          in
          if absent <> [] then
            Pick_refused
              ("no definition at the source coordinate ("
              ^ source_at_s at
              ^ "): " ^ String.concat ", " absent
              ^ " (unbind is a separate explicit op)")
          else
            (* 3. clobber guard vs the target's OWN pins (tree_nodes rows) *)
            let tgt = Store.pins_at_head store ~branch_id:target_id in
            let tgt_hash n =
              match
                List.find_opt
                  (fun (p : Store.pin_row) -> String.equal p.Store.p_name n)
                  tgt
              with
              | Some p -> Some p.Store.p_hash
              | None -> None
            in
            let classify rows =
              List.fold_left
                  (fun (ok_rows, skipped) (r : pick_row) ->
                   match tgt_hash r.pr_name with
                     | None -> (r :: ok_rows, skipped)
                   | Some h when String.equal h r.pr_hash ->
                         ( ok_rows,
                          {
                            sk_kind = r.pr_kind;
                            sk_name = r.pr_name;
                            sk_reason = "unchanged on target";
                          }
                          :: skipped )
                   | Some h ->
                         if force then (r :: ok_rows, skipped)
                       else
                         ( ok_rows,
                           {
                             sk_kind = r.pr_kind;
                             sk_name = r.pr_name;
                             sk_reason =
                               Printf.sprintf
                                 "diverged-target: target %s vs picked %s" h
                                 r.pr_hash;
                           }
                           :: skipped ))
                ([], []) rows
            in
            let requested_rows = List.map (fun n -> Option.get (find_pin n)) requested in
            let land0, skipped0 = classify requested_rows in
            let land_fns =
              List.filter (fun (r : pick_row) -> String.equal r.pr_kind "fn") land0
            in
            (* 4. closure first (fn bodies only; types/routes have no refs) *)
            let run_closure () =
              if land_fns = [] then Ok (land0, [], skipped0)
              else
                let src_defs =
                  List.filter_map
                    (fun (r : pick_row) ->
                       if String.equal r.pr_kind "fn" then
                         match Store.payload_fields r.pr_payload with
                         | Some body, _ -> (
                             match
                               Pricklypear_lisp.Ast.expr_of_body_json body
                             with
                             | Some e -> Some (r.pr_name, e)
                             | None -> None)
                         | None, _ -> None
                       else None)
                    resolved
                in
                let tgt_visible =
                  List.map (fun (n, _, _, _) -> n)
                    (Store.list_function_names_meta_at store
                       ~branch_id:target_id)
                in
                match
                  Call_graph.push_closure
                    ~requested:(List.map (fun (r : pick_row) -> r.pr_name) land_fns)
                    ~src_defs ~tgt_visible ~kernel
                with
                | Error holes -> Error holes
                | Ok (_pushed, auto) ->
                    (* auto-included deps resolve from the same coordinate
                       and pass the same clobber guard *)
                    let auto_rows =
                      List.filter_map (fun n -> find_pin n) auto
                    in
                    let auto_land, auto_skipped = classify auto_rows in
                    Ok
                      ( land0 @ auto_land,
                        List.map (fun (r : pick_row) -> r.pr_name) auto_land,
                        skipped0 @ auto_skipped )
            in
            match run_closure () with
            | Error holes -> Pick_holes holes
              | Ok (ok_rows, auto, skipped) ->
                (* 5. land in ONE tx stamped origin=pick (migration 042) *)
                let detail = source_ref ^ "@" ^ source_at_s at in
                  let outcomes =
                    land_rows ~store ~detail ~target_id ~author ok_rows in
                let landed, land_skipped =
                  List.fold_left
                    (fun (ok, bad) ((r : pick_row), err) ->
                       match err with
                       | None -> (r :: ok, bad)
                       | Some why ->
                           ( ok,
                             {
                               sk_kind = r.pr_kind;
                               sk_name = r.pr_name;
                               sk_reason = "landing failed: " ^ why;
                             }
                             :: bad ))
                    ([], skipped) outcomes
                in
                Pick_ok { landed = List.rev landed; auto; skipped = land_skipped }

  and land_rows ~(store : Store.t) ~(detail : string) ~(target_id : string)
      ~(author : string) (rows : pick_row list) :
    (pick_row * string option) list =
  if rows = [] then []
  else
    Store.with_transaction store (fun conn ->
        match conn with
        | None -> List.map (fun r -> (r, Some "no database")) rows
        | Some conn ->
            Fun.protect
              (fun () ->
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn ~params:[]
                     "SELECT set_config('pp.origin', 'pick', true)"
                 in
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn
                     ~params:[ Pgx.Value.of_string detail ]
                     "SELECT set_config('pp.origin_detail', $1, true)"
                 in
                 List.map
                   (fun (r : pick_row) ->
                      let res =
                        match r.pr_kind with
                        | "fn" -> (
                            let body, doc =
                              Store.payload_fields r.pr_payload in
                            match body with
                            | None -> Error "picked payload has no body"
                            | Some b ->
                                Tree_store.upsert_fn conn
                                  ~branch_id:target_id ~name:r.pr_name
                                  ~body_json:b ~author ~doc)
                        | "type" -> (
                            let fields, _ =
                              Store.payload_fields r.pr_payload in
                            match fields with
                            | None -> Error "picked payload has no fields"
                            | Some f ->
                                Tree_store.upsert_type conn
                                  ~branch_id:target_id ~name:r.pr_name
                                  ~fields_json:f ~author)
                        | "route" -> (
                            match Store.route_fields r.pr_payload with
                            | None ->
                                Error "picked payload has no handler/visibility"
                            | Some (handler, visibility) ->
                                Tree_store.upsert_route conn
                                  ~branch_id:target_id ~route:r.pr_name
                                  ~handler ~visibility)
                        | other -> Error ("unknown kind: " ^ other)
                      in
                      (r, match res with Ok _ -> None | Error why -> Some why))
                   rows)
              ~finally:(fun () ->
                 (* rider-safe reset: a pick joining an ambient transaction
                    must not stamp the ambient body's later writes *)
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn ~params:[]
                     "SELECT set_config('pp.origin', 'eval', true)"
                 in
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn ~params:[]
                     "SELECT set_config('pp.origin_detail', '', true)"
                 in
                 ()))

(* ---- file-bridge P2 ingest (branching.file-bridge, spec commit 3f4d6d1,
   2026-10-16) ---- One server call lands a projection file's def-op batch.
   The CLIENT parses the file (File_projection, lisp/lib) and emits ops; the
   server validates base-gen freshness, runs the clobber guard + closure
   (op-transplant's rules, same code), and lands through the tree write
   helpers inside ONE transaction stamped origin='file'.

   CONFLICT RULE (the spec's): a name whose target head hash differs from
   BOTH the incoming body and the manifest's base body is DIVERGED — the
   server cannot see the manifest's base bodies, so the client sends each
   op's base_hash (the hash of the base body the file was authored against,
   as read from the target before editing). Target head = base_hash -> clean
   redefine; target head differs from both -> diverged-target skip unless
   ~force; no target pin -> create. JSONB-LEVEL body equality (never text):
   body_hash digests the jsonb-NORMALIZED text, so client formatting is
   irrelevant. *)

type ingest_op = {
  io_op : string;               (* "define" | "unbind" *)
  io_name : string;
  io_body : string;             (* fn body jsonb; define only *)
  io_doc : string option;
  io_base_hash : string option; (* base body hash the file was authored against *)
}

type ingest_result =
  | Ingest_ok of {
      i_landed : string list;  (* defines + unbinds actually written *)
      i_skipped : skip_row list;
    }
  | Ingest_holes of (string * string list) list
  | Ingest_refused of string

(* Land the classified batch inside ONE tx stamped origin='file' (042);
   rider-safe reset in a finally, the same discipline as the pick landing. *)
let land_ingest ~(store : Store.t) ~(detail : string) ~(target_id : string)
    ~(author : string)
    (defines : (string * string * string option) list)
    (unbinds : string list) : (string * string option) list =
  if defines = [] && unbinds = [] then []
  else
    Store.with_transaction store (fun conn ->
        match conn with
        | None ->
            List.map (fun (n, _, _) -> (n, Some "no database")) defines
            @ List.map (fun n -> (n, Some "no database")) unbinds
        | Some conn ->
            Fun.protect
              (fun () ->
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn ~params:[]
                     "SELECT set_config('pp.origin', 'file', true)"
                 in
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn
                     ~params:[ Pgx.Value.of_string detail ]
                     "SELECT set_config('pp.origin_detail', $1, true)"
                 in
                 let d_res =
                   List.map
                     (fun (n, b, d) ->
                        ( n,
                          match
                            Tree_store.upsert_fn conn ~branch_id:target_id
                              ~name:n ~body_json:b ~author ~doc:d
                          with
                          | Ok _ -> None
                          | Error why -> Some why ))
                     defines
                 in
                 let u_res =
                   List.map
                     (fun n ->
                        ( n,
                          match
                            Tree_store.delete_fn conn ~branch_id:target_id
                              ~name:n
                          with
                          | Ok _ -> None
                          | Error why -> Some why ))
                     unbinds
                 in
                 d_res @ u_res)
              ~finally:(fun () ->
                 (* rider-safe reset: an ingest joining an ambient transaction
                    must not stamp the ambient body's later writes *)
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn ~params:[]
                     "SELECT set_config('pp.origin', 'eval', true)"
                 in
                 let (_ : Pgx.Value.t list list) =
                   Store.exec conn ~params:[]
                     "SELECT set_config('pp.origin_detail', '', true)"
                 in
                 ()))

let ingest (store : Store.t) ~(target_id : string) ~(ops : ingest_op list)
    ~(base_gen : Int64.t) ~(force : bool) ~(kernel : string list)
    ~(author : string) ~(file_name : string) : ingest_result =
  if ops = [] then Ingest_refused "no ops in the batch"
  else
    (* freshness sanity: a manifest authored against a future branch *)
    let head_gen = Store.branch_head_gen store ~branch_id:target_id in
    if head_gen < base_gen then
      Ingest_refused
        (Printf.sprintf "manifest base-gen %Ld is ahead of the branch head %Ld"
           base_gen head_gen)
    else
      let tgt = Store.pins_at_head store ~branch_id:target_id in
      let tgt_hash n =
        match
          List.find_opt
            (fun (p : Store.pin_row) -> String.equal p.Store.p_name n)
            tgt
        with
        | Some p -> Some p.Store.p_hash
        | None -> None
      in
      let classify_define (op : ingest_op) : [ `Land | `Skip of string ] =
        match Store.body_hash store ~body_json:op.io_body with
        | Error why -> `Skip ("hash failed: " ^ why)
        | Ok ih -> (
            match tgt_hash op.io_name with
            | None -> `Land
            | Some th when String.equal th ih -> `Skip "unchanged on target"
            | Some th -> (
                match op.io_base_hash with
                | Some bh when String.equal bh th -> `Land
                | _ ->
                    if force then `Land
                    else
                      `Skip
                        (Printf.sprintf "diverged-target: target %s vs file %s"
                           th ih)))
      in
      let classify_unbind (op : ingest_op) : [ `Land | `Skip of string ] =
        match tgt_hash op.io_name with
        | None -> `Skip "already absent"
        | Some th -> (
            match op.io_base_hash with
            | Some bh when String.equal bh th -> `Land
            | _ ->
                if force then `Land
                else `Skip (Printf.sprintf "diverged-target: target %s" th))
      in
      let classify_defs (oplist : ingest_op list) :
          ingest_op list * skip_row list =
        List.fold_left
            (fun (ok_rows, skip) (op : ingest_op) ->
               match classify_define op with
               | `Land -> (op :: ok_rows, skip)
               | `Skip why ->
                   ( ok_rows,
                     { sk_kind = "fn"; sk_name = op.io_name; sk_reason = why }
                     :: skip ))
          ([], []) oplist
      in
      let defines_all =
        List.filter (fun (o : ingest_op) -> String.equal o.io_op "define") ops in
      let unbinds_all =
        List.filter (fun (o : ingest_op) -> String.equal o.io_op "unbind") ops in
      let other_skips =
        List.map
          (fun (o : ingest_op) ->
             {
               sk_kind = "op";
               sk_name = o.io_name;
               sk_reason = "unknown op: " ^ o.io_op;
             })
          (List.filter
             (fun (o : ingest_op) ->
               not (String.equal o.io_op "define")
               && not (String.equal o.io_op "unbind"))
             ops)
      in
      let defs_land0, skips1 = classify_defs defines_all in
      let unbinds_land, skips2 =
        List.fold_left
            (fun (ok_rows, skip) (op : ingest_op) ->
               match classify_unbind op with
               | `Land -> (op.io_name :: ok_rows, skip)
               | `Skip why ->
                   ( ok_rows,
                     { sk_kind = "fn"; sk_name = op.io_name; sk_reason = why }
                     :: skip ))
          ([], []) unbinds_all
      in
      (* closure over the landed defines: refs bound on the target chain are
         free; a dep defined by the incoming batch auto-includes (and passes
         the same conflict guard); a ref bound nowhere holes the ingest WHOLE *)
      let run_closure () =
        let land_names = List.map (fun (o : ingest_op) -> o.io_name) defs_land0 in
        if land_names = [] then Ok (defs_land0, [], skips1 @ skips2 @ other_skips)
        else
          let src_defs =
            List.filter_map
              (fun (o : ingest_op) ->
                 match Pricklypear_lisp.Ast.expr_of_body_json o.io_body with
                 | Some e -> Some (o.io_name, e)
                 | None -> None)
              defines_all
          in
          let tgt_visible =
            List.map (fun (n, _, _, _) -> n)
              (Store.list_function_names_meta_at store ~branch_id:target_id)
          in
          match
            Call_graph.push_closure ~requested:land_names ~src_defs
              ~tgt_visible ~kernel
          with
          | Error holes -> Error holes
          | Ok (_pushed, auto) ->
              let auto_ops =
                List.filter_map
                  (fun n ->
                     List.find_opt
                       (fun (o : ingest_op) -> String.equal o.io_name n)
                       defines_all)
                  auto
              in
              let auto_land, auto_skip = classify_defs auto_ops in
              Ok
                ( defs_land0 @ auto_land,
                  List.map (fun (o : ingest_op) -> o.io_name) auto_land,
                  skips1 @ skips2 @ auto_skip @ other_skips )
      in
      match run_closure () with
      | Error holes -> Ingest_holes holes
      | Ok (defs_land, auto, skipped) ->
          let detail =
            Printf.sprintf "file:%s@gen:%Ld" file_name base_gen in
          (* Doc preservation: the file format carries no docstrings, so a
             redefine with io_doc = None keeps the target pin's existing
             doc instead of nulling it (the upsert would otherwise drop it). *)
          let tgt_docs =
            List.filter_map
              (fun (p : Store.pin_row) ->
                 if String.equal p.Store.p_kind "fn" then
                   match Store.payload_fields p.Store.p_payload with
                   | Some _, doc -> Some (p.Store.p_name, doc)
                   | None, _ -> None
                 else None)
              tgt
          in
          let doc_for (op : ingest_op) =
            match op.io_doc with
            | Some d -> Some d
            | None -> (
                match List.assoc_opt op.io_name tgt_docs with
                | Some d -> d
                | None -> None)
          in
          let outcomes =
            land_ingest ~store ~detail ~target_id ~author
              (List.map
                 (fun (o : ingest_op) -> (o.io_name, o.io_body, doc_for o))
                 defs_land)
              unbinds_land
          in
          let landed, land_skipped =
            List.fold_left
              (fun (ok, bad) (n, err) ->
                 match err with
                 | None -> (n :: ok, bad)
                 | Some why ->
                     ( ok,
                       {
                         sk_kind = "fn";
                         sk_name = n;
                         sk_reason = "landing failed: " ^ why;
                       }
                       :: bad ))
              ([], skipped) outcomes
          in
          Ingest_ok
            {
              i_landed = List.rev landed;
              i_skipped = land_skipped;
            }
