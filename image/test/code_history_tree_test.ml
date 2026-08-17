(** tree-time.projections Phase 3: branching.code-history / code-diff now read
    from the versioned tree (tree_versions) instead of the 012 before-image
    table, keeping signatures UNCHANGED.

    SEMANTIC NOTE (tree-time.coexistence-plan Phase 3): 012 stored the OLD
    before-image per UPDATE/DELETE (never on the first INSERT). tree_versions
    stores the post-write head payload at each gen, so a tree-backed history
    read reports ONLY the rows STRICTLY BELOW HEAD — the previous states,
    newest-first "read the version row at gen-1" — and the live head row is
    never history (an insert-only fn has no history until rewritten/deleted).
    The 012 table remains the fallback source only for pre-shadow (pre-019)
    names, so no historical depth is lost during the coexistence window.

    These tests exercise the tree-backed path directly (Store.list_code_history
    routes to tree_versions when the code path has version rows) and confirm
    code-diff still produces a diff against the nth-newest history entry.

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
open Pricklypear_lisp.Ast

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

let uniq p = Printf.sprintf "cht_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

(* A tree-backed code-history entry has no gen field, so newest-first is
   asserted by body content: the newest entry's body carries the last-written
   definition payload (the version row's inner 'body' key). *)
let contains_sub s sub =
  let ls = String.length s and lb = String.length sub in
  let rec go i = if i + lb > ls then false
    else if String.sub s i lb = sub then true
    else go (i + 1)
  in
  lb <= ls && (if lb = 0 then true else go 0)

let body_contains (e : S.code_history_entry) ~(needle : string) : bool =
  match e.body with
  | Some b -> contains_sub b needle
  | None -> false

(* Direct tree writes (T.upsert_fn) are shadowed by migration 019 into
   tree_versions, so Store.list_code_history routes to the tree-backed path
   (rel=function_defs, op=UPDATE, author=tester, newest-first). *)
let test_tree_backed_history () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let name = uniq "fn" in
          (* three writes -> the current head is the 3rd; history is the 2
             STRICTLY-BELOW-HEAD rows (states before the latest change),
             newest-first = [v2, v1]. Match 012's before-image semantic (an
             insert-only fn has NO history until rewritten). *)
          T.upsert_fn conn ~branch_id:main_id ~name
            ~body_json:"{\"params\":[],\"body\":1}" ~author:"tester" ~doc:None;
          T.upsert_fn conn ~branch_id:main_id ~name
            ~body_json:"{\"params\":[],\"body\":2}" ~author:"tester" ~doc:None;
          T.upsert_fn conn ~branch_id:main_id ~name
            ~body_json:"{\"params\":[],\"body\":3}" ~author:"tester" ~doc:None;
          let hist = S.list_code_history store ~branch_id:main_id ~name in
          (match hist with
           | [ n1; n2 ] ->
               List.iter
                 (fun e ->
                   Alcotest.(check string) "rel is function_defs" "function_defs" e.S.rel;
                   Alcotest.(check string) "op is UPDATE" "UPDATE" e.S.op;
                   Alcotest.(check (option string)) "author is tester"
                     (Some "tester") e.S.author)
                 [ n1; n2 ];
               (* newest-first: n1 = state just before the latest change = v2; n2 = v1 *)
               Alcotest.(check bool) "newest body = 2" true
                 (body_contains n1 ~needle:"\"body\":2");
               Alcotest.(check bool) "oldest body = 1" true
                 (body_contains n2 ~needle:"\"body\":1");
               Alcotest.(check bool) "newest not tombstone (body present)" true
                 (n1.body <> None)
           | _ -> Alcotest.fail "expected exactly 2 tree-backed history rows");
          (* code-diff vs nth-newest still produces a diff *)
          (match S.code_diff store ~branch_id:main_id ~name ~n:1 with
           | Ok d ->
               Alcotest.(check bool) "code-diff non-empty" true (d <> "")
           | Error e -> Alcotest.fail ("code-diff failed: " ^ e));
          (* code-diff with an out-of-range n reports an error, not a crash *)
          (match S.code_diff store ~branch_id:main_id ~name ~n:999 with
           | Ok _ -> Alcotest.fail "expected out-of-range error"
           | Error _ -> ());
          (* cleanup: tombstone the fn *)
          ignore (T.delete_fn conn ~branch_id:main_id ~name))

let () =
  Alcotest.run "code_history_tree"
    [
      ( "tree-backed-code-history",
        [
          Alcotest.test_case "code-history/code-diff read from tree_versions" `Quick
            test_tree_backed_history;
        ] );
    ]
