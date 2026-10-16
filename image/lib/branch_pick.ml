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
