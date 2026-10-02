(** branching.branch-resolved-reads (T1.4) — DB-backed acceptance.

    A fork is readable WITHOUT checkout: a session whose manifest points at
    one branch calls function-source / function-ast / find-code / code-diff
    with an explicit owner/branch ref, and the answer comes from the FORK's
    chain, never the session's branch. branch-changes on a data-only fork
    reports the datum footprint instead of reading EMPTY. (describe …) carries
    the SCOPE grammar line.

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
module Session = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p =
  Printf.sprintf "brr_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

let contains_sub (s : string) (sub : string) : bool =
  let n = String.length sub in
  if n = 0 then true
  else
    let rec go i =
      if i + n > String.length s then false
      else if String.sub s i n = sub then true else go (i + 1)
    in
    go 0

(* A session backed by [store] whose manifest claims [branch_id]. The
   session's own branch is the PARENT we sit on while reading the fork. *)
let make_session store ~branch_id ~branch_name ~username : Session.t =
  let manifest =
    M.make ~user_id:username ~username ~branch_id ~branch_name
      ~boot_mode:M.Boot_on_connect ~image_version:"test" ()
  in
  Session.make ~manifest ~store ()

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let write_fn store ~branch_id ~name ~body ~author =
  S.with_branch store ~branch_id (fun () ->
      S.persist_function store name body ~doc:None author)

let eval_str (t : Session.t) (src : string) : string =
  match Session.eval_string_rim t src with
  | VStr s -> s
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VStr, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_v (t : Session.t) (src : string) : value =
  Session.eval_string_rim t src

let fork store ~owner ~child_name =
  match S.fork_branch store ~parent_id:main_id ~owner ~child_name with
  | Some id -> id
  | None -> Alcotest.fail "fork_branch failed"

let cleanup_branch store ~branch_id =
  List.iter
    (fun sql ->
      try
        ignore
          (S.exec (Option.get (S.conn_of store))
             ~params:[ Pgx.Value.of_string branch_id ] sql)
      with _ -> ())
    [ "DELETE FROM view_bindings WHERE branch_id = $1";
      "DELETE FROM type_defs WHERE branch_id = $1";
      "DELETE FROM function_defs WHERE branch_id = $1";
      "DELETE FROM branches WHERE id = $1" ]

let body_of_str (s : string) : string =
  Pricklypear_lisp.Ast.expr_to_body_json
    (Lambda ([], Lit (VStr s)))

let test_branch_resolved_reads () =
  with_store (fun store ->
      let owner = uniq "own" in
      let child = uniq "child" in
      let child_id = fork store ~owner ~child_name:child in
      let ref_ = owner ^ "/" ^ child in
      let name = uniq "fn" in
      (* the fork pins a body that DIFFERS from what main would answer *)
      write_fn store ~branch_id:child_id ~name
        ~body:(body_of_str "FORK-BODY") ~author:owner;
      (* a unique needle only present in the fork body *)
      let needle = uniq "nedl" in
      write_fn store ~branch_id:child_id ~name:(name ^ "-needle")
        ~body:(body_of_str needle) ~author:owner;
      (* the session sits on system/main the whole time — never the fork *)
      let t =
        make_session store ~branch_id:main_id ~branch_name:"system/main"
          ~username:owner
      in
      (* 1. function-source with the ref reads the FORK body, no checkout *)
      let src =
        eval_str t (Printf.sprintf "(function-source %S %S)" name ref_)
      in
      Alcotest.(check bool) "function-source via ref returns fork body" true
        (contains_sub src "FORK-BODY");
      Alcotest.(check string) "session stayed on system/main" "system/main"
        (eval_str t "(current-branch)");
      (* 2. function-ast with the ref reifies the fork body *)
      (match eval_v t (Printf.sprintf "(function-ast %S %S)" name ref_) with
       | VNil -> Alcotest.fail "function-ast via ref returned nil"
       | v ->
           let rendered = Pricklypear_lisp.Reader.pp_value v in
           Alcotest.(check bool) "function-ast via ref carries fork body" true
             (contains_sub rendered "FORK-BODY"));
      (* 3. find-code with the ref finds the fork's unique needle *)
      let fc = eval_v t (Printf.sprintf "(find-code %S %S)" needle ref_) in
      (match fc with
       | VList names ->
           let ns =
             List.filter_map (function VStr s -> Some s | _ -> None) names
           in
           Alcotest.(check bool)
             (Printf.sprintf "find-code via ref finds fork-only def (got %s)" (Pricklypear_lisp.Reader.pp_value fc))
             true (List.mem (name ^ "-needle") ns)
       | v ->
           Alcotest.fail
             (Printf.sprintf "find-code via ref: expected list, got %s"
                (Pricklypear_lisp.Reader.pp_value v)));
      (* 4. code-diff accepts the ref (no crash, returns text) *)
      ignore (eval_str t (Printf.sprintf "(code-diff %S 1 %S)" name ref_));
      (* 5. an unknown branch ref under our own tenant is a clean error
         string, not a crash (a foreign owner would be denied first — also
         clean). *)
      let missing =
        eval_str t
          (Printf.sprintf "(function-source %S %S)" name (owner ^ "/ghost"))
      in
      Alcotest.(check bool) "missing ref is a clean error" true
        (contains_sub missing "no such branch");
      cleanup_branch store ~branch_id:child_id)

let test_branch_changes_data_footprint () =
  with_store (fun store ->
      let owner = uniq "own" in
      let child = uniq "child" in
      let child_id = fork store ~owner ~child_name:child in
      let ref_ = owner ^ "/" ^ child in
      let type_name = uniq "tdat" in
      S.persist_type store ~name:type_name ~fields:[ "title" ] ~author:owner;
      (* a datum row written by the branch owner AFTER the fork watermark *)
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           ignore
             (T.upsert_datum conn ~path:"data.u.tester.new" ~type_name
                ~body_json:{|{"title":"fork-datum"}|} ~author:owner
                ~rank:None));
      let t =
        make_session store ~branch_id:main_id ~branch_name:"system/main"
          ~username:owner
      in
      (match eval_v t (Printf.sprintf "(branch-changes %S)" ref_) with
       | VList items ->
           let jsons =
             List.filter_map (function VStr s -> Some s | _ -> None) items
           in
           let data_rows =
             List.filter (fun s -> contains_sub s "\"kind\":\"data\"") jsons
           in
           Alcotest.(check bool) "branch-changes has a data footprint" true
             (data_rows <> []);
           let ours =
             List.find_opt (fun s -> contains_sub s type_name) data_rows
           in
           Alcotest.(check bool) "footprint names the fork's datum type" true
             (ours <> None)
       | v ->
           Alcotest.fail
             (Printf.sprintf "branch-changes: expected list, got %s"
                (Pricklypear_lisp.Reader.pp_value v)));
      (* clean up the datum + type *)
      (match S.conn_of store with
       | Some conn ->
           List.iter
             (fun sql ->
               try
                 ignore
                   (S.exec conn
                      ~params:[ Pgx.Value.of_string type_name ] sql)
               with _ -> ())
             [ "DELETE FROM tree_nodes WHERE type_name = $1 AND NOT (path <@ 'data.l'::ltree)";
               "DELETE FROM type_defs WHERE name = $1";
               "DELETE FROM tree_nodes WHERE type_name = $1" ]
       | None -> ());
      cleanup_branch store ~branch_id:child_id)

let test_scope_line () =
  with_store (fun store ->
      let t =
        make_session store ~branch_id:main_id ~branch_name:"system/main"
          ~username:"admin"
      in
      Alcotest.(check bool) "describe(find-code) declares scope chain" true
        (contains_sub (eval_str t "(describe \"find-code\")") "scope: chain");
      Alcotest.(check bool)
        "describe(find-rows-author) declares scope store" true
        (contains_sub
           (eval_str t "(describe \"find-rows-author\")")
           "scope: store");
      Alcotest.(check bool) "describe(find-text) declares scope garden" true
        (contains_sub
           (eval_str t "(describe \"find-text\")")
           "scope: garden"))

let () =
  Alcotest.run "branch_resolved_reads"
    [
      ( "branch-resolved-reads",
        [
          Alcotest.test_case "fork read without checkout + clean ref errors"
            `Quick test_branch_resolved_reads;
          Alcotest.test_case "branch-changes reports datum footprint" `Quick
            test_branch_changes_data_footprint;
          Alcotest.test_case "describe carries a SCOPE line" `Quick
            test_scope_line;
        ] );
    ]
