(** Prim registration extracted from session.ml (Eio-floor Phase 4). *)
open Session_types

let split_branch_ref (s : string) : string * string =
  match String.split_on_char '/' s with
  | [ o; n ] -> (o, n)
  | [ n ] -> ("system", n)
  | _ -> ("system", s)

let method_to_string (m : http_method) : string =
  match m with `GET -> "GET" | `POST -> "POST"

let require_request (t : t) : request =
  match t.current_request with
  | Some r -> r
  | None ->
      raise
        (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.User_error ("no active request", "bad-args")))

let str_name = function
  | Pricklypear_lisp.Ast.VStr s | Pricklypear_lisp.Ast.VSymbol s -> Some s
  | _ -> None

let depth_arg = function
  | None -> 1
  | Some (Pricklypear_lisp.Ast.VInt n) -> Int64.to_int n
  | Some (Pricklypear_lisp.Ast.VStr s) ->
      (try int_of_string s with _ -> 1)
  | Some _ -> 1

let vlist_strs xs =
  Pricklypear_lisp.Ast.VList (List.map (fun s -> Pricklypear_lisp.Ast.VStr s) xs)

let impact_to_json (e : impact_event) : string =
  let open Yojson.Safe in
  to_string
    (`Assoc
       [ ("name", `String e.name)
       ; ("at", `Float e.at)
       ; ("branch", `String e.branch)
       ; ("author", `String e.author)
       ; ("dependents", `List (List.map (fun s -> `String s) e.dependents))
       ; ("routes", `List (List.map (fun s -> `String s) e.routes))
       ; ("status", `String e.status)
       ])

let change_to_json (e : change_event) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("op", `String e.op)
       ; ("name", `String e.name)
       ; ("at", `Float e.at)
       ; ("branch", `String e.branch)
       ; ("author", `String e.author)
       ])

let record_define_side_effects = Call_graph.record_define_side_effects
let impact_summary_text = Call_graph.impact_summary_text


let register (t : t) (tbl : Primitives.t) : unit =
  Primitives.register tbl "callees"
    (fun args ->
      match args with
      | [ n ] | [ n; _ ] as a ->
          (match str_name n with
           | None -> (vlist_strs [])
           | Some name ->
               let depth =
                 match a with
                 | [ _; d ] -> depth_arg (Some d)
                 | _ -> 1
               in
               let snap = Call_graph.snapshot_of_session t in
               (vlist_strs (Call_graph.callees snap ~name ~depth)))
      | _ -> (vlist_strs []));
  Primitives.register tbl "callers"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None -> (vlist_strs [])
           | Some name ->
               let snap = Call_graph.snapshot_of_session t in
               (vlist_strs (Call_graph.callers snap name)))
      | _ -> (vlist_strs []));
  Primitives.register tbl "dependents"
    (fun args ->
      match args with
      | [ n ] | [ n; _ ] as a ->
          (match str_name n with
           | None -> (vlist_strs [])
           | Some name ->
               let depth =
                 match a with
                 | [ _; d ] -> depth_arg (Some d)
                 | _ -> 8
               in
               let snap = Call_graph.snapshot_of_session t in
               (vlist_strs (Call_graph.dependents snap ~name ~depth)))
      | _ -> (vlist_strs []));
  Primitives.register tbl "used-by-routes"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None -> (vlist_strs [])
           | Some name ->
               let snap = Call_graph.snapshot_of_session t in
               (vlist_strs (Call_graph.used_by_routes snap name)))
      | _ -> (vlist_strs []));
  Primitives.register tbl "holes"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None -> (vlist_strs [])
           | Some name ->
               let snap = Call_graph.snapshot_of_session t in
               (vlist_strs (Call_graph.holes snap name)))
      | _ -> (vlist_strs []));
  Primitives.register tbl "impact-queue"
    (fun _args ->
      let xs =
        List.map
          (fun e -> Pricklypear_lisp.Ast.VStr (impact_to_json e))
          t.impact_queue
      in
      (Pricklypear_lisp.Ast.VList xs));
  Primitives.register tbl "impact-clear"
    (fun args ->
      (match args with
       | [] | [ Pricklypear_lisp.Ast.VStr "all" ]
       | [ Pricklypear_lisp.Ast.VSymbol "all" ] ->
           t.impact_queue <- []
       | [ n ] ->
           (match str_name n with
            | Some name ->
                t.impact_queue <-
                  List.filter
                    (fun (e : impact_event) -> e.name <> name)
                    t.impact_queue
            | None -> ())
       | _ -> ());
      Pricklypear_lisp.Ast.VNil);
  Primitives.register tbl "impact-mark"
    (fun args ->
      match args with
      | [ n; st ] ->
          (match str_name n, str_name st with
           | Some name, Some status ->
               t.impact_queue <-
                 List.map
                   (fun (e : impact_event) ->
                     if e.name = name then { e with status } else e)
                   t.impact_queue;
               (Pricklypear_lisp.Ast.VStr status)
           | _ -> Pricklypear_lisp.Ast.VNil)
      | _ -> Pricklypear_lisp.Ast.VNil);
  Primitives.register tbl "change-transcript"
    (fun _args ->
      let xs =
        List.map
          (fun e -> Pricklypear_lisp.Ast.VStr (change_to_json e))
          t.change_transcript
      in
      (Pricklypear_lisp.Ast.VList xs));
  Primitives.register tbl "branch-changes"
    (fun args ->
      let branch_id =
        match args with
        | [] -> (Some t.manifest.branch_id)
        | [ n ] ->
            (match str_name n with
             | None -> None
             | Some ref_ ->
                 let owner, name = split_branch_ref ref_ in
                 Store.lookup_branch_id_by_name t.store ~owner ~name)
        | _ -> None
      in
      match branch_id with
      | None -> (Pricklypear_lisp.Ast.VList [])
      | Some bid ->
          let own = Store.list_own_functions t.store ~branch_id:bid in
          let routes = Store.list_own_routes t.store ~branch_id:bid in
          let parent_id = Store.branch_parent_id t.store ~branch_id:bid in
          let ov =
            Store.list_overrides t.store ~branch_id:bid ~parent_id
          in
          let class_of name =
            match List.find_opt (fun (n, _, _) -> n = name) ov with
            | Some (_, _, None) -> "new"
            | Some (_, _, Some _) -> "override"
            | None -> "own"
          in
          let fn_items =
            List.map
              (fun (name, author, updated) ->
                let j =
                  Yojson.Safe.to_string
                    (`Assoc
                       [ ("name", `String name)
                       ; ("kind", `String "fn")
                       ; ("author", `String author)
                       ; ("updated_at", `String updated)
                       ; ("class", `String (class_of name))
                       ])
                in
                Pricklypear_lisp.Ast.VStr j)
              own
          in
          let route_items =
            List.map
              (fun (path, handler) ->
                let j =
                  Yojson.Safe.to_string
                    (`Assoc
                       [ ("name", `String path)
                       ; ("kind", `String "route")
                       ; ("handler", `String handler)
                       ; ("class", `String "route")
                       ])
                in
                Pricklypear_lisp.Ast.VStr j)
              routes
          in
          (Pricklypear_lisp.Ast.VList (fn_items @ route_items)));
  Primitives.register tbl "stub-function"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None ->
               (Pricklypear_lisp.Ast.VStr "stub-function: need name")
           | Some name ->
               let existing = Store.lookup_function t.store name in
               (match existing with
                | Some _ ->
                    (Pricklypear_lisp.Ast.VStr
                         ("stub-function: already bound: " ^ name))
                | None ->
                    let body_src =
                      Printf.sprintf
                        "(lambda ()\n  \"TODO: implement %s\")" name
                    in
                    let r =
                      Agent_tools.define_binding t ~name ~body_src
                        ~author:t.manifest.username
                    in
                    (match r with
                     | Error e ->
                         (Pricklypear_lisp.Ast.VStr ("stub-function: " ^ e))
                     | Ok _ ->
                         let () = record_define_side_effects t ~name in
                         (Pricklypear_lisp.Ast.VStr
                              ("stubbed " ^ name ^ "; "
                               ^ impact_summary_text t name)))))
      | _ ->
          (Pricklypear_lisp.Ast.VStr "stub-function: need 1 name"))

