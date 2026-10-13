(** branch-ops push row (branching.borg branch-ops table): the branch/push
    store oracle — dependency-closure preflight + the promote copy. The
    PRIM (session_control_prims "branch/push") owns identity and
    authorization; this module owns the graph: which names travel
    together, what refuses, and the atomic write onto <ref-owner>/personal.

    CLOSURE (2026-10-05 incident: pulse-row-from promoted without
    workstreams/stale-node? -> system/main 500): Call_graph.push_closure —
    the holes App-Var analytic over the SOURCE chain's effective defs.
    Names bound on the TARGET chain (or kernel prims / special forms) are
    free; an unbound ref defined by the source is AUTO-INCLUDED
    (recursively); an unbound ref defined NOWHERE holes its carrier and
    the push is refused whole with a complete report.

    OWNERSHIP (pin hygiene: the write copies only names the source branch
    OWNS — promote_functions' per-name own-row lookup). An inherited-only
    name would silently skip, so it is REFUSED with a fork-first message:
    no silent skips, no half-landed targets. *)

type push_result =
  | Push_ok of { pushed : string list; auto : string list }
    (* landed: requested (in order) + auto-included closure deps *)
  | Push_holes of (string * string list) list
    (* refused: name x refs unbound on the target and defined nowhere *)
  | Push_refused of string
(* structural refusal: self-push, inherited-only name, empty default set *)

let holes_msg (holes : (string * string list) list) : string =
  String.concat "; "
    (List.map
       (fun (name, missing) ->
          if missing = [] then
            Printf.sprintf "%s (no definition on the source branch)" name
          else
            Printf.sprintf "%s needs %s" name
              (String.concat ", " missing))
       holes)

let push (store : Store.t) ~(source_id : string) ~(target_id : string)
    ~(names : string list) ~(kernel : string list) : push_result =
  if source_id = target_id then
    Push_refused "cannot push a branch into itself"
  else begin
    let src_meta =
      Store.list_function_names_meta_at store ~branch_id:source_id
    in
    let tgt_visible =
      List.map (fun (n, _, _, _) -> n)
        (Store.list_function_names_meta_at store ~branch_id:target_id)
    in
    (* Opaque-body contract (i-1788671083-c): parse at this boundary; a
       malformed row is skipped, as the env loader does. *)
    let src_defs =
      List.filter_map
        (fun (n, body, _, _) ->
           match Pricklypear_lisp.Ast.expr_of_body_json body with
           | Some e -> Some (n, e)
           | None -> None)
        src_meta
    in
    let requested =
      if names <> [] then Call_graph.uniq_stable names
      else
        (* default: the source's OWN code, promote_to_personal's standing
           (depth-0 rows only — an inherited name is the parent's, and the
           ownership check below refuses it anyway) *)
        List.filter_map
          (fun (n, _, _, d) -> if d = 0 then Some n else None)
          src_meta
    in
    if requested = [] then
      Push_refused "no functions defined on this branch"
    else
      match
        Call_graph.push_closure ~requested ~src_defs ~tgt_visible ~kernel
      with
      | Error holes -> Push_holes holes
      | Ok (pushed, auto) ->
          let full = pushed @ auto in
          let inherited_only =
            List.filter_map
              (fun n ->
                 match
                   Store.lookup_function_on_branch store
                     ~branch_id:source_id ~name:n
                 with
                 | Some _ -> None
                 | None -> Some n)
              full
          in
          (match inherited_only with
           | _ :: _ ->
               Push_refused
                 (String.concat ", " inherited_only
                 ^ ": defined only by an ancestor of the source branch \
                    (fork onto the source first)")
           | [] ->
               let (_ : int) =
                 Store.promote_functions store ~source_id ~target_id
                   ~names:full
               in
               Push_ok { pushed; auto })
  end
