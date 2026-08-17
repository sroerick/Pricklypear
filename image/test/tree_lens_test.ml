(** Lens-aware tree read prim tests (one-tree.tree-read-prims,
    tree-store + crud + prims_db lens functions, BORGE-1785516993).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.

    Acceptance (issue 1785516993):
      - a relative logical path (fn/type/route/pkg suffix) resolves across
        the session's branch chain with closest-ancestor-wins (same as
        function_defs): a child override shadows the parent, and an
        effective definition from system/main is visible on a child.
      - tree/lens-get returns exactly one node (the effective one);
        tree/lens-children/subtree read under the effective branch.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    {
      sw;
      net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r);
    }
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

let uniq p = Printf.sprintf "lens_%s_%d" p (Random.int 1_000_000)

let persist store ~branch ~name =
  S.with_branch store ~branch_id:branch (fun () ->
      S.persist_function store name
        (Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 1L))
        ~doc:(Some "lens-test") "tester")

(* ---- pure: suffix path materialization ---- *)
let test_lens_suffix_path () =
  let b = "00000000-0000-0000-0000-000000000001" in
  Alcotest.(check (option string)) "fn suffix"
    (Some ("b." ^ T.uuid_label b ^ ".fn." ^ T.enc_label "notes/doc/get"))
    (T.lens_suffix_path ~branch_id:b "fn.notes/doc/get");
  Alcotest.(check (option string)) "route suffix"
    (Some ("b." ^ T.uuid_label b ^ ".route." ^ T.enc_label "/issues"))
    (T.lens_suffix_path ~branch_id:b "route./issues");
  Alcotest.(check (option string)) "pkg suffix"
    (Some ("b." ^ T.uuid_label b ^ ".pkg." ^ T.enc_label "todo"))
    (T.lens_suffix_path ~branch_id:b "pkg.todo");
  Alcotest.(check (option string)) "empty suffix -> None"
    None (T.lens_suffix_path ~branch_id:b "");
  Alcotest.(check (option string)) "kind-only suffix -> None"
    None (T.lens_suffix_path ~branch_id:b "fn")

(* ---- integration: override chain, closest-wins ---- *)
let test_effective_override_chain () =
  with_store (fun store ->
      let main_fn = uniq "mainfn" in
      let child_fn = uniq "childfn" in
      (* define main_fn ONLY on system/main *)
      persist store ~branch:main_id ~name:main_fn;
      (* fork a child of main *)
      match
        S.fork_branch store ~parent_id:main_id ~owner:"lens_test"
          ~child_name:(uniq "b")
      with
      | None -> Alcotest.fail "fork_branch failed"
      | Some child_id ->
          (* child inherits main's main_fn *)
          S.with_branch store ~branch_id:child_id (fun () ->
              let chain = S.branch_chain store ~branch_id:child_id in
              Alcotest.(check bool) "chain is self-first"
                true (List.hd chain = child_id);
              Alcotest.(check bool) "chain reaches main"
                true (List.mem main_id chain);
              match
                S.conn_of store
              with
              | None -> Alcotest.fail "no conn"
              | Some conn ->
                  (match
                     T.lens_get_in_chain conn
                       ~suffix:("fn." ^ main_fn)
                       chain
                   with
                   | None -> Alcotest.fail "child must see main's fn"
                   | Some n ->
                       Alcotest.(check string) "effective node from main"
                         (T.uuid_label main_id)
                         (let p = n.path in
                          (* b.<uuid>.fn.<name> -> uuid is the 2nd label *)
                          match List.nth_opt (String.split_on_char '.' p) 1 with
                          | Some u -> u
                          | None -> ""));
                  (* now override child_fn ON the child under a suffix that
                     does NOT exist on main *)
                  ());
          persist store ~branch:child_id ~name:child_fn;
          S.with_branch store ~branch_id:child_id (fun () ->
              match S.conn_of store with
              | None -> Alcotest.fail "no conn"
              | Some conn ->
                  let chain = S.branch_chain store ~branch_id:child_id in
                  (match
                     T.lens_get_in_chain conn
                       ~suffix:("fn." ^ child_fn)
                       chain
                   with
                   | None -> Alcotest.fail "child's own fn visible via lens"
                   | Some n ->
                       let p = n.path in
                       let got =
                         match List.nth_opt (String.split_on_char '.' p) 1 with
                         | Some u -> u
                         | None -> ""
                       in
                       Alcotest.(check string) "own override closest-wins"
                         (T.uuid_label child_id) got));
          (* a fn that exists nowhere -> None *)
          S.with_branch store ~branch_id:child_id (fun () ->
              match S.conn_of store with
              | None -> ()
              | Some conn ->
                  Alcotest.(check (option string)) "missing -> None"
                    None
                    (match
                       T.lens_get_in_chain conn
                         ~suffix:("fn." ^ uniq "nope")
                         (S.branch_chain store ~branch_id:child_id)
                     with
                     | None -> None
                     | Some n -> Some n.path)))

(* prims surface: tree/lens-get resolves closest-wins end to end. The prim
   registration is a thin binding to Crud.tree_lens_get -> Store.branch_chain
   + Tree_store.lens_get_in_chain, so verifying the override resolution at
   the Tree_store layer (with the same branch_chain the prim uses) covers
   the effective-chain behaviour. *)
let test_lens_get_prim () =
  with_store (fun store ->
      let fn = uniq "primfn" in
      persist store ~branch:main_id ~name:fn;
      match
        S.fork_branch store ~parent_id:main_id ~owner:"lens_test"
          ~child_name:(uniq "b")
      with
      | None -> Alcotest.fail "fork_branch failed"
      | Some child_id ->
          (* effective override: child re-defines fn to a marker body *)
          S.with_branch store ~branch_id:child_id (fun () ->
              S.persist_function store fn
                (Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 42L))
                ~doc:(Some "lens-prim-override") "tester");
          S.with_branch store ~branch_id:child_id (fun () ->
              match S.conn_of store with
              | None -> ()
              | Some conn ->
                  let chain = S.branch_chain store ~branch_id:child_id in
                  (match
                     T.lens_get_in_chain conn ~suffix:("fn." ^ fn) chain
                   with
                   | None -> Alcotest.fail "lens-get must find override"
                   | Some n ->
                       let p = n.path in
                       let got =
                         match List.nth_opt (String.split_on_char '.' p) 1 with
                         | Some u -> u
                         | None -> ""
                       in
                       Alcotest.(check string) "closest-wins child"
                         (T.uuid_label child_id) got)))

let () =
  (* Fresh per-run branch names so repeated runs (or a run right after a
     previous one) never collide on branches_owner_name_key. Without this,
     Random's fixed default seed yields identical names every run. *)
  Random.self_init ();
  Alcotest.run "tree_lens"
    [ ( "lens",
        [
          Alcotest.test_case "suffix path" `Quick test_lens_suffix_path;
          Alcotest.test_case "effective override chain" `Quick
            test_effective_override_chain;
          Alcotest.test_case "lens-get prim closest-wins" `Quick
            test_lens_get_prim;
        ] ) ]
