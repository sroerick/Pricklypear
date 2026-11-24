(** Canonical todo-card writes reject malformed new data without freezing legacy rows.

    DB-backed: skips cleanly without Postgres. Guard observes the REAL raised
    (error ...) before tree/upsert; a tag returned under ok:true would not test
    the same behavior for callers. *)

module S = Pricklypear_image_lib.Store
module Session = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest
module Db = Pricklypear_image_lib.Store_db
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p =
  Printf.sprintf "cardinv_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

let point_pp_libs_dir () =
  let rec up n dir =
    if n > 10 then failwith "todo_card_invariants_test: libs/ not found"
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
      | None -> Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let scratch_branch store ~owner ~child_name =
  match S.fork_branch store ~parent_id:main_id ~owner ~child_name with
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

let make_session store ~username ~branch_id ~branch_name =
  let manifest =
    M.make ~user_id:username ~username ~branch_id ~branch_name
      ~boot_mode:M.Boot_on_connect ~image_version:"test" ()
  in
  let t = Session.make ~manifest ~store () in
  Session.checkout_to t ~branch_id ~branch_ref:branch_name;
  t

let load_lib t name =
  match Session.load_library t name with
  | Ok msg ->
      if String.length msg >= 14 && String.sub msg 0 14 = "load-library: "
      then Alcotest.fail (name ^ " partial load: " ^ msg)
  | Error e -> Alcotest.fail (name ^ " load: " ^ e)

let eval_raw (t : Session.t) src =
  let ctx =
    Pricklypear_image_lib.Request_context.make ~manifest:t.manifest
      ~env:t.eval_state.env ~is_admin:t.is_admin
      ~routes:(Pricklypear_image_lib.Request_context.copy_routes t.routes)
      ~source_ip:None
  in
  Pricklypear_image_lib.Request_context.with_current ctx (fun () ->
      Session.eval_string_rim t src)

let eval_v t src =
  try eval_raw t src with
  | Interp_error e ->
      Alcotest.fail (Printf.sprintf "%s: %s" src (pp_interp_error e))

let raised_bad_args t src =
  match eval_raw t src with
  | _ -> Alcotest.fail ("expected a real bad-args error: " ^ src)
  | exception Interp_error (User_error (_, "bad-args")) -> ()
  | exception Interp_error e -> Alcotest.fail (pp_interp_error e)

let eval_str t src =
  match eval_v t src with
  | VStr s -> s
  | v -> Alcotest.fail (Printf.sprintf "%s: expected string, got %s" src
                           (Pricklypear_lisp.Reader.pp_value v))

let eval_int t src =
  match eval_v t src with
  | VInt n -> Int64.to_int n
  | _ -> Alcotest.fail ("expected integer: " ^ src)

let refusal t src =
  match eval_v t (Printf.sprintf "(guard (lambda (e) e) %s)" src) with
  | VList [ VSymbol "error"; VStr msg; VSymbol "bad-args" ] -> msg
  | v -> Alcotest.fail (Printf.sprintf "%s: expected raised bad-args, got %s" src
                           (Pricklypear_lisp.Reader.pp_value v))

let with_cards f store =
  let owner = uniq "user" and child = uniq "scratch" in
  let branch_ref = owner ^ "/" ^ child in
  let branch_id = scratch_branch store ~owner ~child_name:child in
  Fun.protect ~finally:(fun () -> drop_branch store ~branch_id) (fun () ->
      let t = make_session store ~username:owner ~branch_id ~branch_name:branch_ref in
      load_lib t "todo";
      load_lib t "kanban";
      ignore (eval_str t
        "(if (null? (type-fields (quote todo-list))) \
          (deftype (quote todo-list) (quote title) (quote description) \
           (quote is_default)) \"ok\")");
      ignore (eval_str t
        "(if (null? (type-fields (quote todo-item))) \
          (deftype (quote todo-item) (quote list_id) (quote title) \
           (quote notes) (quote state) (quote due) (quote position) \
           (quote done)) \"ok\")");
      let lid = eval_str t (Printf.sprintf "(todo/list/create %S)" (uniq "list")) in
      f t lid)

let fields lid title state extra =
  Printf.sprintf
    "(dict-set* (dict-set* \"{}\" (list \"list_id\" %S \"title\" %S \
      \"notes\" \"\" \"state\" %S \"due\" \"\" \"position\" \"0\" \
      \"done\" \"false\")) %s)"
    lid title state extra

let create t lid title state extra =
  Printf.sprintf "(todo/tree/create \"todo-item\" %s)"
    (fields lid title state extra)

let card_count t lid = eval_int t (Printf.sprintf "(count (todo/items-for-list %S))" lid)

let test_create_refusals store =
  with_cards (fun t lid ->
      (* Not merely an (error ...) value under an ok envelope: the special form
         must raise through the evaluator and prevent the upsert. *)
      raised_bad_args t (create t lid "" "TODO" "(list)");
      List.iter (fun title ->
          let msg = refusal t (create t lid title "TODO" "(list)") in
          Alcotest.(check bool) "title named in refusal" true
            (String.length msg > 0))
        [ ""; "   \t  " ];
      let msg = refusal t (create t lid "bad state" "a prose body" "(list)") in
      Alcotest.(check bool) "pipeline named" true (String.length msg > 0);
      ignore (refusal t (create t lid "bad domain" "TODO"
                           "(list \"branch\" \"feature\" \"branch_kind\" \"guess\")"));
      ignore (refusal t (create t lid "orphan domain" "TODO"
                           "(list \"branch_kind\" \"git\")"));
      Alcotest.(check int) "no refused card was written" 0 (card_count t lid)) store

let test_create_allows_incomplete_and_extensions store =
  with_cards (fun t lid ->
      let id = eval_str t (create t lid "incomplete is valid" "TODO"
                             "(list \"x-extension\" \"kept\")") in
      let got = eval_str t (Printf.sprintf
        "(as-str (dict-get (todo/tree/get \"todo-item\" %S) \"x-extension\"))" id) in
      Alcotest.(check string) "extension retained" "kept" got;
      let no_domain = eval_str t (create t lid "domain may be filled later" "TODO"
                                    "(list \"branch\" \"feature\")") in
      Alcotest.(check string) "branch without domain remains editable legacy shape" ""
        (eval_str t (Printf.sprintf
          "(as-str (dict-get (todo/tree/get \"todo-item\" %S) \"branch_kind\"))" no_domain));
      Alcotest.(check int) "both incomplete cards created" 2 (card_count t lid)) store

let test_target_pipeline_and_kanban store =
  with_cards (fun t lid ->
      ignore (eval_v t (Printf.sprintf
        "(todo/list/set-columns %S (list \"TODO\" \"READY\" \"DONE\"))" lid));
      ignore (refusal t (create t lid "wrong pipeline" "CODE" "(list)"));
      ignore (refusal t (Printf.sprintf "(kanban/add-card %S \"wrong board state\" \"CODE\")" lid));
      Alcotest.(check int) "both canonical gates wrote nothing" 0 (card_count t lid);
      let id = eval_str t (Printf.sprintf "(kanban/add-card %S \"right board state\" \"READY\")" lid) in
      Alcotest.(check string) "kanban used canonical creation" "READY"
        (eval_str t (Printf.sprintf "(todo/item/state (todo/tree/get \"todo-item\" %S))" id));
      Alcotest.(check int) "one valid card" 1 (card_count t lid)) store

let test_save_checks_changes_but_allows_legacy_edits store =
  with_cards (fun t lid ->
      let legacy = eval_str t (Printf.sprintf
        "(todo/db/create \"todo-item\" %s)"
        (fields lid "legacy" "old prose state" "(list \"branch_kind\" \"unknown\")")) in
      ignore (eval_v t (Printf.sprintf
        "(todo/tree/save \"todo-item\" %S (dict-set (todo/tree/get \"todo-item\" %S) \"notes\" \"unrelated edit\"))"
        legacy legacy));
      Alcotest.(check string) "legacy bad state/domain survives unrelated save" "unrelated edit"
        (eval_str t (Printf.sprintf
          "(as-str (dict-get (todo/tree/get \"todo-item\" %S) \"notes\"))" legacy));
      ignore (refusal t (Printf.sprintf
        "(todo/tree/save \"todo-item\" %S (dict-set (todo/tree/get \"todo-item\" %S) \"title\" \"  \"))"
        legacy legacy));
      ignore (eval_v t (Printf.sprintf
        "(todo/tree/save \"todo-item\" %S (dict-set (todo/tree/get \"todo-item\" %S) \"state\" \"TODO\"))"
        legacy legacy));
      let blank = eval_str t (Printf.sprintf
        "(todo/db/create \"todo-item\" %s)" (fields lid "" "TODO" "(list)")) in
      ignore (eval_v t (Printf.sprintf
        "(todo/tree/save \"todo-item\" %S (dict-set (todo/tree/get \"todo-item\" %S) \"notes\" \"still editable\"))"
        blank blank));
      Alcotest.(check string) "legacy blank title does not freeze unrelated edits" "still editable"
        (eval_str t (Printf.sprintf
          "(as-str (dict-get (todo/tree/get \"todo-item\" %S) \"notes\"))" blank))) store

let () =
  point_pp_libs_dir ();
  with_store (fun store ->
      Alcotest.run "todo_card_invariants"
        [ ( "card-write-invariants",
            [ Alcotest.test_case "create rejects title/state/domain corruption" `Quick
                (fun () -> test_create_refusals store);
              Alcotest.test_case "incomplete cards and extension fields survive" `Quick
                (fun () -> test_create_allows_incomplete_and_extensions store);
              Alcotest.test_case "target pipeline and canonical kanban create" `Quick
                (fun () -> test_target_pipeline_and_kanban store);
              Alcotest.test_case "save validates changes without freezing legacy" `Quick
                (fun () -> test_save_checks_changes_but_allows_legacy_edits store) ] ) ])
