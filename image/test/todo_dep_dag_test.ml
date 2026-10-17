(** borge/workstreams.borg dependency DAG (beads-parity) — branch todo-dep-dag.

    DB-backed acceptance for todo/dep + todo/undep + todo/ready and the
    kanban surface:

      1. add/remove: todo/dep adds an edge, todo/dep/blocked? flips,
         todo/undep removes it ("removed") and reports "no such edge" on a
         second try;
      2. refusals: self edge, unknown from/to card, duplicate edge — refused
         as strings with NO write (the duplicate leaves exactly one edge);
      3. transitive ready: A<-B<-C all open => only the tail C is ready;
         closing C unblocks B only (readiness is a DIRECT-blocker predicate,
         no graph walk); closing B then unblocks A;
      4. a chain ending DONE/PARKED does not block;
      5. an open cycle A<->B keeps both members not-ready without hanging;
         closing one member frees the other;
      6. todo/ready + kanban/cards-ready list entry-column unblocked cards
         only (blocked and in-flight cards excluded);
      7. kanban/card-html marks a blocked card with .kb-blocked.

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
module Session = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest
module Db = Pricklypear_image_lib.Store_db
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p =
  Printf.sprintf "depdag_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

(* point libs at the repo root so load-library finds the working copies *)
let point_pp_libs_dir () : unit =
  let rec up n dir =
    if n > 10 then failwith "todo_dep_dag_test: libs/ not found"
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

(* Loading the product libs persists them onto the session's branch, so every
   test runs on a throwaway fork; system/main stays untouched. *)
let scratch_branch store ~owner ~child_name : string =
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

let make_session_on store ~username ~branch_id ~branch_name : Session.t =
  let manifest =
    M.make ~user_id:username ~username ~branch_id
      ~branch_name ~boot_mode:M.Boot_on_connect
      ~image_version:"test" ()
  in
  let t = Session.make ~manifest ~store () in
  (* Bind the store's current branch to the fork as a real checkout would,
     so deftype and datum writes land on the throwaway branch (and the
     ownership gate sees a branch the session user owns). *)
  Session.checkout_to t ~branch_id ~branch_ref:branch_name;
  t

let load_lib t name =
  match Session.load_library t name with
  | Ok msg ->
      Printf.printf "[test] %s load: %s\n%!" name msg;
      if String.length msg >= 14 && String.sub msg 0 14 = "load-library: "
      then Alcotest.fail (name ^ " partial load: " ^ msg)
  | Error e -> Alcotest.fail (name ^ " load: " ^ e)

(* eval under the per-request context: the datum write path stamps the
   ledger from Request_context (crud emit_write) and hard-crashes on None,
   so every test eval — writes AND the reads mixed with them — runs inside
   one (shared_register's as_identity pattern, applied uniformly). *)
let eval_v (t : Session.t) (src : string) : value =
  let ctx =
    Pricklypear_image_lib.Request_context.make
      ~manifest:t.manifest
      ~env:t.eval_state.env
      ~is_admin:t.is_admin
      ~routes:(Pricklypear_image_lib.Request_context.copy_routes t.routes)
      ~source_ip:None
  in
  Pricklypear_image_lib.Request_context.with_current ctx (fun () ->
      try Session.eval_string_rim t src with
      | Pricklypear_lisp.Ast.Interp_error e ->
          Alcotest.fail
            (Printf.sprintf "%s: %s" src
               (Pricklypear_lisp.Ast.pp_interp_error e)))

let eval_str t src =
  match eval_v t src with
  | VStr s -> s
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VStr, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_bool t src =
  match eval_v t src with
  | VBool b -> b
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VBool, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_int t src =
  match eval_v t src with
  | VInt n -> Int64.to_int n
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VInt, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let has_sub hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

(* One scratch branch + session with todo+kanban loaded; f gets the session
   and a fresh todo-list id. Data rows persist in the dev DB (per-card
   assertions only — never global counts); the branch is dropped after. *)
let with_cards f store =
  let owner = uniq "user" in
  let child = uniq "scratch" in
  let branch_ref = owner ^ "/" ^ child in
  let branch_id = scratch_branch store ~owner ~child_name:child in
  Fun.protect ~finally:(fun () -> drop_branch store ~branch_id) (fun () ->
      let t = make_session_on store ~username:owner ~branch_id ~branch_name:branch_ref in
      load_lib t "todo";
      load_lib t "kanban";
      (* Base todo types normally live on system/main (declared on the live
         habitat before this repo tracked them). Declare them on the scratch
         branch so the test is self-contained on a fresh dev DB. *)
      ignore
        (eval_str t
           "(if (null? (type-fields (quote todo-list))) \
            (deftype (quote todo-list) (quote title) (quote description) \
             (quote is_default)) \
            \"ok\")");
      ignore
        (eval_str t
           "(if (null? (type-fields (quote todo-item))) \
            (deftype (quote todo-item) (quote list_id) (quote title) \
             (quote notes) (quote state) (quote due) (quote position) \
             (quote done)) \
            \"ok\")");
      let lid =
        eval_str t (Printf.sprintf "(todo/list/create %S)" (uniq "list"))
      in
      f t lid)

let add_card t lid title =
  eval_str t (Printf.sprintf "(todo/item/add %S %S)" lid title)

let dep t a b = eval_str t (Printf.sprintf "(todo/dep %S %S)" a b)
let blocked t a = eval_bool t (Printf.sprintf "(todo/dep/blocked? %S)" a)
let readyq t a = eval_bool t (Printf.sprintf "(todo/ready? %S)" a)

let set_state t id st =
  ignore (eval_v t (Printf.sprintf "(todo/item/set-state %S %S)" id st))

let in_ready t lid id =
  eval_bool t
    (Printf.sprintf
       "(todo/dep/member? %S (list/map (lambda (r) (as-str (car r))) \
        (todo/ready %S)))"
       id lid)

let in_kanban_ready t lid id =
  eval_bool t
    (Printf.sprintf
       "(todo/dep/member? %S (list/map (lambda (r) (as-str (car r))) \
        (kanban/cards-ready %S)))"
       id lid)

let test_add_remove store =
  with_cards
    (fun t lid ->
      let a = add_card t lid "a" and b = add_card t lid "b" in
      let eid = dep t a b in
      Alcotest.(check bool) "edge id is a dashed uuid" true
        (String.length eid = 36);
      Alcotest.(check bool) "blocked after dep" true (blocked t a);
      Alcotest.(check bool) "blocker itself not blocked" false (blocked t b);
      Alcotest.(check string) "undep removes" "removed"
        (eval_str t (Printf.sprintf "(todo/undep %S %S)" a b));
      Alcotest.(check bool) "unblocked after undep" false (blocked t a);
      Alcotest.(check string) "second undep reports absence" "no such edge"
        (eval_str t (Printf.sprintf "(todo/undep %S %S)" a b)))
    store

let test_refusals store =
  with_cards
    (fun t lid ->
      let a = add_card t lid "a" and b = add_card t lid "b" in
      let ghost = "00000000-0000-0000-0000-0000000000ff" in
      Alcotest.(check string) "self edge refused" "self edge" (dep t a a);
      Alcotest.(check string) "unknown from refused"
        ("no such card: " ^ ghost)
        (dep t ghost b);
      Alcotest.(check string) "unknown to refused"
        ("no such card: " ^ ghost)
        (dep t a ghost);
      ignore (dep t a b);
      Alcotest.(check string) "duplicate refused" "duplicate edge"
        (dep t a b);
      Alcotest.(check int) "duplicate wrote nothing" 1
        (eval_int t (Printf.sprintf "(count (todo/dep/blockers %S))" a)))
    store

let test_transitive_chain store =
  with_cards
    (fun t lid ->
      let a = add_card t lid "a"
      and b = add_card t lid "b"
      and c = add_card t lid "c" in
      ignore (dep t a b);
      ignore (dep t b c);
      Alcotest.(check bool) "chain tail ready" true (readyq t c);
      Alcotest.(check bool) "mid blocked" false (readyq t b);
      Alcotest.(check bool) "head blocked" false (readyq t a);
      set_state t c "DONE";
      Alcotest.(check bool) "mid unblocks when tail closes" true (readyq t b);
      Alcotest.(check bool) "head still blocked by open mid" false (readyq t a);
      set_state t b "DONE";
      Alcotest.(check bool) "head unblocks when chain closes" true (readyq t a))
    store

let test_chain_to_parked store =
  with_cards
    (fun t lid ->
      let a = add_card t lid "a" and b = add_card t lid "b" in
      ignore (dep t a b);
      Alcotest.(check bool) "blocked while blocker open" false (readyq t a);
      set_state t b "PARKED";
      Alcotest.(check bool) "PARKED blocker does not block" true (readyq t a))
    store

let test_cycle store =
  with_cards
    (fun t lid ->
      let a = add_card t lid "a" and b = add_card t lid "b" in
      ignore (dep t a b);
      ignore (dep t b a);
      Alcotest.(check bool) "cycle member a not ready" false (readyq t a);
      Alcotest.(check bool) "cycle member b not ready" false (readyq t b);
      set_state t b "DONE";
      Alcotest.(check bool) "closing one member frees the other" true
        (readyq t a);
      Alcotest.(check bool) "closed member stays out of ready" false
        (readyq t b))
    store

let test_ready_list_verb store =
  with_cards
    (fun t lid ->
      let r1 = add_card t lid "r1" in
      let r2 = add_card t lid "r2" in
      let r3 = add_card t lid "r3" in
      ignore (dep t r2 r1);
      set_state t r3 "CODE";
      Alcotest.(check bool) "unblocked entry card ready" true
        (in_ready t lid r1);
      Alcotest.(check bool) "blocked entry card not ready" false
        (in_ready t lid r2);
      Alcotest.(check bool) "in-flight card not ready (not entry column)"
        false (in_ready t lid r3);
      Alcotest.(check bool) "kanban/cards-ready agrees (present)" true
        (in_kanban_ready t lid r1);
      Alcotest.(check bool) "kanban/cards-ready agrees (blocked)" false
        (in_kanban_ready t lid r2);
      Alcotest.(check bool) "kanban/cards-ready agrees (in-flight)" false
        (in_kanban_ready t lid r3))
    store

let test_marker_html store =
  with_cards
    (fun t lid ->
      let a = add_card t lid "a" and b = add_card t lid "b" in
      ignore (dep t a b);
      let card_html id =
        eval_str t
          (Printf.sprintf
             "(kanban/card-html %S (cons %S (todo/tree/get \"todo-item\" %S)) \
              (todo/state/columns-for-list %S))"
             lid id id lid)
      in
      Alcotest.(check bool) "blocked card carries kb-blocked" true
        (has_sub (card_html a) "kb-blocked");
      Alcotest.(check bool) "open blocker unmarked" false
        (has_sub (card_html b) "kb-blocked"))
    store

let () =
  point_pp_libs_dir ();
  with_store (fun store ->
      Alcotest.run "todo_dep_dag"
        [ ( "dep-dag",
            [ Alcotest.test_case "add/remove flips blocked?" `Quick
                (fun () -> test_add_remove store);
              Alcotest.test_case "self/unknown/duplicate refused" `Quick
                (fun () -> test_refusals store);
              Alcotest.test_case "transitive chain closes in order" `Quick
                (fun () -> test_transitive_chain store);
              Alcotest.test_case "PARKED blocker does not block" `Quick
                (fun () -> test_chain_to_parked store);
              Alcotest.test_case "open cycle: members not ready, close frees"
                `Quick (fun () -> test_cycle store);
              Alcotest.test_case "todo/ready + kanban/cards-ready" `Quick
                (fun () -> test_ready_list_verb store);
              Alcotest.test_case "kanban card marker" `Quick
                (fun () -> test_marker_html store) ] ) ])
