(** borge/workstreams.borg §shared-register (T2.3) — DB-backed acceptance.

    The Board list lives in the OPEN 'board' data space (034 row-level
    routing). These tests pin the register path end to end:

      1. a NON-OPERATOR identity with no Board in its own garden still sees
         the shared Board in (todo/list/all) and notes/issues/board-list-id
         resolves it with its shared space;
      2. notes/issues/add lands exactly ONE card, in data.s.board.* — never a
         garden Inbox fallback (the 2026-10-02 vanish), single object per
         issue;
      3. ids survive: the list id is the Board's own id, the card keeps the
         id todo/item/add-space returned, and re-reading resolves the same row;
      4. anonymous is denied the create primitive (register content is
         box-internal; routes stay private).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
module Session = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest
module Db = Pricklypear_image_lib.Store_db
module Al = Pricklypear_image_lib.Activity_ledger
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p =
  Printf.sprintf "sreg_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

(* point libs at the repo root so load-library finds the working copies *)
let point_pp_libs_dir () : unit =
  let rec up n dir =
    if n > 10 then failwith "shared_register_test: libs/ not found"
    else
      let p = Filename.concat dir "libs/todo/lib.pp" in
      if Sys.file_exists p then Unix.putenv "PP_LIBS_DIR" (Filename.concat dir "libs")
      else up (n + 1) (Filename.dirname dir)
  in
  up 0 (Sys.getcwd ())

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
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let make_session store ~username : Session.t =
  let manifest =
    M.make ~user_id:username ~username ~branch_id:main_id
      ~branch_name:"system/main" ~boot_mode:M.Boot_on_connect
      ~image_version:"test" ()
  in
  Session.make ~manifest ~store ()

(* Loading the product libs (todo/notes) persists them onto the session's
   branch. Run the tests on a throwaway fork so system/main is untouched. *)
let scratch_branch store ~owner : string =
  let name = uniq "scratch" in
  match S.fork_branch store ~parent_id:main_id ~owner ~child_name:name with
  | Some id -> id
  | None -> Alcotest.fail "fork_branch failed"

let drop_branch store ~branch_id =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      List.iter
        (fun sql ->
          try ignore (Db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql)
          with _ -> ())
        [ "DELETE FROM view_bindings WHERE branch_id = $1";
          "DELETE FROM type_defs WHERE branch_id = $1";
          "DELETE FROM function_defs WHERE branch_id = $1";
          "DELETE FROM branches WHERE id = $1" ]

let make_session_on store ~username ~branch_id : Session.t =
  let manifest =
    M.make ~user_id:username ~username ~branch_id
      ~branch_name:"scratch" ~boot_mode:M.Boot_on_connect
      ~image_version:"test" ()
  in
  let t = Session.make ~manifest ~store () in
  (* boot/attach bind the branch's inherited defs into a fresh env; tests
     must do the same or load-library's skip map sees identical inherited
     bodies and binds nothing into the empty env. *)
  Session.rehydrate t;
  t

let load_product t =
  (match Session.load_library t "todo" with
   | Ok msg ->
       Printf.printf "[test] todo load: %s\n%!" msg;
       if String.length msg >= 14 && String.sub msg 0 14 = "load-library: "
       then Alcotest.fail ("todo partial load: " ^ msg)
   | Error e -> Alcotest.fail ("todo load: " ^ e));
  match Session.load_library t "notes" with
  | Ok msg ->
      Printf.printf "[test] notes load: %s\n%!" msg;
      if String.length msg >= 14 && String.sub msg 0 14 = "load-library: "
      then Alcotest.fail ("notes partial load: " ^ msg)
  | Error e -> Alcotest.fail ("notes load: " ^ e)

let eval_v (t : Session.t) (src : string) : value =
  try Session.eval_string_rim t src with
  | Pricklypear_lisp.Ast.Interp_error e ->
      Alcotest.fail
        (Printf.sprintf "%s: %s" src
           (Pricklypear_lisp.Ast.pp_interp_error e))

let eval_str (t : Session.t) (src : string) : string =
  match eval_v t src with
  | VStr s -> s
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VStr, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_pair (t : Session.t) (src : string) : string * string =
  match eval_v t src with
  | VPair (VStr a, VStr b) -> (a, b)
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected pair, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_int (t : Session.t) (src : string) : int =
  match eval_v t src with
  | VInt n -> Int64.to_int n
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VInt, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_bool (t : Session.t) (src : string) : bool =
  match eval_v t src with
  | VBool b -> b
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VBool, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

(* run [expr] under the per-request context so whoami/author are [username]. *)
let as_identity (t : Session.t) ~(username : string) (expr : string) : value =
  let ctx =
    Pricklypear_image_lib.Request_context.make
      ~manifest:t.manifest
      ~env:t.eval_state.env
      ~is_admin:t.is_admin
      ~routes:(Pricklypear_image_lib.Request_context.copy_routes t.routes)
  in
  Pricklypear_image_lib.Request_context.with_current ctx (fun () ->
      (* the manifest itself already carries [username]; the ctx snapshot is
         what prims read. Re-derive the manifest to be sure. *)
      ignore username;
      eval_v t expr)

(* Create the shared Board list owned by the OPERATOR (representing the row
   034 moved), returning its id. Uses the raw (create) prim with the row
   "space" field so it lands in data.s.board.*. *)
let make_board store ~operator : string =
  let t = make_session store ~username:operator in
  let json =
    "{\"title\":\"Board\",\"description\":\"shared register test\",\
     \"is_default\":\"false\",\"space\":\"board\"}"
  in
  match
    (try Ok (eval_v t (Printf.sprintf "(create \"todo-list\" %S)" json))
     with Pricklypear_lisp.Ast.Interp_error e -> Error e)
  with
  | Ok (VStr id) -> id
  | Ok v -> Alcotest.fail ("make_board: " ^ Pricklypear_lisp.Reader.pp_value v)
  | Error e -> Alcotest.fail ("make_board: " ^ Pricklypear_lisp.Ast.pp_interp_error e)

let q_one conn ?(params = []) sql : string =
  match Db.exec conn ~params sql with
  | [ [ c ] ] -> ( match Pgx.Value.to_string c with Some v -> v | None -> "")
  | _ -> Alcotest.fail ("query shape: " ^ sql)

let cleanup store ~ids =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      List.iter
        (fun id ->
          try
            ignore
              (Db.exec conn
                 ~params:[ Pgx.Value.of_string id ]
                 "DELETE FROM tree_nodes WHERE kind='datum' AND tree_datum_id(path)=$1::uuid");
            ignore
              (Db.exec conn ~params:[ Pgx.Value.of_string id ]
                 "DELETE FROM data_rows WHERE id=$1::uuid")
          with _ -> ())
        ids

(* ---- the acceptance ---- *)

let test_non_operator_sees_shared_board () =
  with_store (fun store ->
      point_pp_libs_dir ();
      let operator = uniq "op" in
      let guest = uniq "guest" in
      let board_id = make_board store ~operator in
      let scratch = scratch_branch store ~owner:guest in
      Fun.protect
        ~finally:(fun () ->
          cleanup store ~ids:[ board_id ]; drop_branch store ~branch_id:scratch)
        (fun () ->
          let t = make_session_on store ~username:guest ~branch_id:scratch in
          load_product t;
          (* guest has no Board in its own garden, but sees the shared one *)
          let hits =
            eval_int t
              (Printf.sprintf
                 "(count (list/filter (lambda (r) (string-eq (as-str \
                  (dict-get (cdr r) \"title\")) \"Board\")) (todo/list/all)))")
          in
          Alcotest.(check int) "guest sees exactly one Board" 1 hits;
          Printf.printf "[test] guest board hits=%d\n%!" hits;
          (* the id AND space resolve through board-list-id *)
          let (bid, space) = eval_pair t "(notes/issues/board-list-id)" in
          Alcotest.(check string) "board-list-id names the shared board"
            board_id bid;
          Alcotest.(check string) "board-list-id carries the shared space"
            "board" space))

let test_guest_files_one_shared_card_no_garden_fallback () =
  with_store (fun store ->
      point_pp_libs_dir ();
      let operator = uniq "op" in
      let guest = uniq "guest" in
      let board_id = make_board store ~operator in
      let scratch = scratch_branch store ~owner:guest in
      let card_ids = ref [] in
      Fun.protect
        ~finally:(fun () ->
          cleanup store ~ids:(board_id :: !card_ids);
          drop_branch store ~branch_id:scratch)
        (fun () ->
          let t = make_session_on store ~username:guest ~branch_id:scratch in
          load_product t;
          let cid =
            eval_str t
              "(notes/issues/add \"SMELL\" \"guest report\" \"from a guest\")"
          in
          card_ids := [ cid ];
          let conn = Option.get (S.conn_of store) in
          (* ONE card, on data.s.board.*, authored by the guest *)
          let n_board =
            let sql =
              "SELECT count(*)::text FROM tree_nodes WHERE kind='datum' \
               AND type_name='todo-item' AND tree_datum_id(path)=$1::uuid \
               AND path <@ ('data.s.' || tree_enc_label('board'))::ltree"
            in
            match Db.exec conn ~params:[ Pgx.Value.of_string cid ] sql with
            | [ [ c ] ] -> int_of_string (Option.get (Pgx.Value.to_string c))
            | _ -> Alcotest.fail "count shape"
          in
          Alcotest.(check int) "exactly ONE shared Board card" 1 n_board;
          let author =
            q_one conn ~params:[ Pgx.Value.of_string cid ]
              "SELECT author FROM tree_nodes WHERE kind='datum' \
               AND tree_datum_id(path)=$1::uuid"
          in
          Alcotest.(check string) "attributed to the guest" guest author;
          (* NO garden fallback: zero guest-authored todo-items under data.u.* *)
          let n_garden =
            let sql =
              "SELECT count(*)::text FROM tree_nodes WHERE kind='datum' \
               AND type_name='todo-item' AND path <@ \
               ('data.u.' || tree_enc_label($1))::ltree"
            in
            match Db.exec conn ~params:[ Pgx.Value.of_string guest ] sql with
            | [ [ c ] ] -> int_of_string (Option.get (Pgx.Value.to_string c))
            | _ -> Alcotest.fail "count shape"
          in
          Alcotest.(check int) "no garden card (no ensure-inbox fallback)" 0
            n_garden))

(* ids survive: the list id is stable and the card keeps its id + body. *)
let test_ids_survive () =
  with_store (fun store ->
      point_pp_libs_dir ();
      let operator = uniq "op" in
      let guest = uniq "guest" in
      let board_id = make_board store ~operator in
      let scratch = scratch_branch store ~owner:guest in
      let card_ids = ref [] in
      Fun.protect
        ~finally:(fun () ->
          cleanup store ~ids:(board_id :: !card_ids);
          drop_branch store ~branch_id:scratch)
        (fun () ->
          let t = make_session_on store ~username:guest ~branch_id:scratch in
          load_product t;
          let cid =
            eval_str t "(notes/issues/add \"BUG\" \"keep id\" \"body\")"
          in
          card_ids := [ cid ];
          (* the same id reads back with kind + notes intact *)
          let kind =
            eval_str t
              (Printf.sprintf "(as-str (dict-get (todo/item/get %S) \"kind\"))"
                 cid)
          in
          Alcotest.(check string) "card kind survives" "BUG" kind;
          let notes =
            eval_str t
              (Printf.sprintf "(as-str (dict-get (todo/item/get %S) \"notes\"))"
                 cid)
          in
          Alcotest.(check string) "card notes survive" "body" notes;
          (* a second board-list-id call still names the same list id *)
          let (bid1, _) = eval_pair t "(notes/issues/board-list-id)" in
          let (bid2, _) = eval_pair t "(notes/issues/board-list-id)" in
          Alcotest.(check string) "board-list-id is stable" bid1 bid2))

(* anonymous is denied the create primitive outright. *)
let test_anonymous_denied () =
  with_store (fun store ->
      point_pp_libs_dir ();
      let operator = uniq "op" in
      let board_id = make_board store ~operator in
      let scratch = scratch_branch store ~owner:"anonymous" in
      Fun.protect
        ~finally:(fun () ->
          cleanup store ~ids:[ board_id ]; drop_branch store ~branch_id:scratch)
        (fun () ->
          let t = make_session_on store ~username:"anonymous" ~branch_id:scratch in
          (match Session.load_library t "todo" with
           | Ok _ -> ()
           | Error e -> Alcotest.fail ("todo load: " ^ e));
          (* (create) itself denies anonymous *)
          let denied =
            match
              (try Ok (eval_v t "(create \"todo-item\" \"{}\")")
               with
               | Pricklypear_lisp.Ast.Interp_error e -> Error e)
            with
            | Error _ -> true
            | Ok _ -> false
          in
          Alcotest.(check bool) "anonymous create denied" true denied;
          (* and add-space with an empty Board (guest garden) files nothing *)
          let empty =
            eval_str t "(todo/item/add-space \"\" \"\" \"x\")"
          in
          Alcotest.(check string) "empty board files nothing" "" empty))

(* 5. a file ATTEMPT rows the ledger — no silent vanish. The register
   writes through tree/upsert (todo/item/add-space -> todo/tree/create),
   NOT through the (create) prim, so the ledger emit must live on the
   tree_upsert datum path. Drain the in-process ring and assert the guest's
   filing produced attributed data.write rows with PATH targets. *)
let test_file_attempt_rows_ledger () =
  with_store (fun store ->
      point_pp_libs_dir ();
      let operator = uniq "op" in
      let guest = uniq "guest" in
      let board_id = make_board store ~operator in
      let scratch = scratch_branch store ~owner:guest in
      let card_ids = ref [] in
      Fun.protect
        ~finally:(fun () ->
          cleanup store ~ids:(board_id :: !card_ids);
          drop_branch store ~branch_id:scratch)
        (fun () ->
          let t = make_session_on store ~username:guest ~branch_id:scratch in
          load_product t;
          ignore (Al.drain ());
          let cid =
            eval_str t
              "(notes/issues/add \"BUG\" \"trace me\" \"no silent vanish\")"
          in
          card_ids := [ cid ];
          let writes =
            List.filter
              (fun (e : Al.entry) ->
                e.Al.actor = guest && e.Al.cls = "data.write")
              (Al.drain ())
          in
          Alcotest.(check bool)
            "the file attempt emitted at least one attributed data.write" true
            (List.length writes >= 1);
          let has_path =
            List.exists
              (fun (e : Al.entry) ->
                match e.Al.target with
                | Some tg -> String.length tg > 0 && tg <> cid
                | None -> false)
              writes
          in
          Alcotest.(check bool) "targets are PATHS, not bodies" true has_path;
          Alcotest.(check bool)
            "the successful filing outcome is ok" true
            (List.exists (fun (e : Al.entry) -> e.Al.outcome = "ok") writes)))

let () =
  Alcotest.run "shared_register"
    [ ( "shared-register",
        [ Alcotest.test_case "non-operator sees shared Board" `Quick
            test_non_operator_sees_shared_board;
          Alcotest.test_case "guest files one shared card" `Quick
            test_guest_files_one_shared_card_no_garden_fallback;
          Alcotest.test_case "ids survive" `Quick test_ids_survive;
          Alcotest.test_case "anonymous denied" `Quick test_anonymous_denied;
          Alcotest.test_case "file attempt rows the ledger" `Quick
            test_file_attempt_rows_ledger;
        ] )
    ]
