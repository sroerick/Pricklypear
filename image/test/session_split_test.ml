(** Session split regression (i-1788671109-1).

    session.ml used to be one 2.5k-line module mixing the interpreter
    driver, 498 lines of control prims, boot seeding, and the UI shell
    cache on a 25-field Session.t. The split extracted:

      - Session_control_prims (fork/merge/route/checkout/unbind/
        prune-synced/bench + private with-transaction/step rims +
        with-public-write, plus split_branch_ref/branch_owner_authorized),
        parameterized by a hooks record (rehydrate + checkout_to);
      - Boot_seed (additive libs seed, package discovery, agents-note
        seed, personal/agent branch bootstrap, first-load recipes),
        parameterized by an ops record (load_library / eval_one /
        rehydrate / checkout_to);
      - Session_types.t regrouped into ui_shell + eval_state sub-records
        (per-field mutation only; the env swap stays an inner-field
        assignment so captured closures keep reading the old env —
        invariant 4a).

    These tests pin the OBSERVABLE contract of the seams, no DB needed
    (Store.make without connect gives conn=None; Session.make is pure
    construction per BORGE-1788334666):

      1. make still wires the whole control surface through
         Session_control_prims (kernel_catalog lists every control prim).
      2. The regrouped state is live: eval against the session env/rim
         works, and the control prims' guards run through the hooks seam
         (anonymous denial, route-visibility memory read, bench's eval
         against eval_state.prims/env).
      3. The Boot_seed re-exports keep the Session.* API: recipe_* /
         seed package discovery stay reachable from Session (loader_test
         pins their behavior). *)

module S = Pricklypear_image_lib.Session
module Ast = Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let anon_session () =
  let store =
    Pricklypear_image_lib.Store.make
      ~cfg:(Pricklypear_image_lib.Store.config_from_env ())
      ~branch_id:main_id ()
  in
  let manifest =
    Pricklypear_common.Manifest.make
      ~user_id:"anonymous" ~username:"anonymous" ~branch_id:main_id
      ~branch_name:"system/main"
      ~boot_mode:Pricklypear_common.Manifest.Boot_on_connect
      ~image_version:"test" ()
  in
  S.make ~manifest ~store ()

(** (name) evals to VStr s. *)
let eval_str t src =
  match S.eval_string_rim t src with
  | Ast.VStr s -> s
  | v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected VStr, got %s" src
           (Pricklypear_lisp.Reader.pp_value v))

let eval_value t src = S.eval_string_rim t src

let test_make_wires_control_prims () =
  let t = anon_session () in
  let names = List.map fst (S.kernel_catalog t) in
  let present p = List.mem p names in
  let required =
    [ "fork"; "merge"; "route"; "public-route"; "route-visibility"; "checkout"
    ; "unbind"; "prune-synced"; "bench"; "__with_transaction__"; "__step__"
    ; "with-public-write" ]
  in
  List.iter
    (fun p ->
      Alcotest.(check bool)
        (Printf.sprintf "control prim %s registered through the split" p)
        true (present p))
    required;
  (* make also binds prims into the env as VPrim names (auto-bind loop). *)
  Alcotest.(check bool) "fork bound in env as VPrim"
    true
    (match Pricklypear_lisp.Env.lookup (S.env t) "fork" with
     | Some (Ast.VPrim "fork") -> true
     | _ -> false)

let test_subrecord_state_live () =
  let t = anon_session () in
  (* shell cache starts empty (ui_shell sub-record initial state) *)
  Alcotest.(check (option string)) "ui_shell.nav_html starts None" None
    (S.ui_nav_html t);
  Alcotest.(check (option string)) "ui_shell.css starts None" None
    (S.ui_css t);
  (* eval_state.env + eval_state.prims are live through the rim: pure
     eval against the made session works end to end. *)
  (match eval_value t "(+ 1 2)" with
   | Ast.VInt n -> Alcotest.(check int64) "(+ 1 2) through regrouped env/rim" 3L n
   | v -> Alcotest.fail (Printf.sprintf "(+ 1 2): expected VInt, got %s"
            (Pricklypear_lisp.Reader.pp_value v)));
  (match eval_value t "pi" with
   | Ast.VFloat f ->
       Alcotest.(check bool) "pi is the bound float value"
         true (Float.abs (f -. Float.pi) < 1e-9)
   | _ -> Alcotest.fail "pi: expected VFloat");
  Alcotest.(check string) "(whoami) reads the manifest through the prim" "anonymous"
    (eval_str t "(whoami)")

let test_control_prim_guards_through_seam () =
  let t = anon_session () in
  (* prune-synced: anonymous denial guard inside session_control_prims
     (the guards RAISE Interp_error; pp_interp_exn renders them) *)
  (match eval_value t "(prune-synced)" with
   | exception Ast.Interp_error e ->
       let msg = Ast.pp_interp_error e in
       let rec contains_sub s sub =
         String.length s >= String.length sub
         && (String.sub s 0 (String.length sub) = sub
             || contains_sub (String.sub s 1 (String.length s - 1)) sub)
       in
       Alcotest.(check bool) "(prune-synced) denied for anonymous"
         true (contains_sub msg "denied for anonymous")
   | v ->
       Alcotest.fail (Printf.sprintf "prune-synced: expected denial, got %s"
           (Pricklypear_lisp.Reader.pp_value v)));
  (* route-visibility: pure in-memory read through t.routes *)
  (match eval_value t "(route-visibility \"/nope\")" with
   | Ast.VNil -> Alcotest.(check bool) "(route-visibility unknown) is ()" true true
   | v ->
       Alcotest.fail (Printf.sprintf "route-visibility: expected (), got %s"
           (Pricklypear_lisp.Reader.pp_value v)));
  (* bench: runs the compiled loop through eval_state.prims/env from
     inside the control-prims module (hooks seam never blocks eval) *)
  (match eval_value t "(bench \"(+ 1 1)\" 3)" with
   | Ast.VFloat f -> Alcotest.(check bool) "bench returns a microsecond float"
       true (f >= 0.0)
   | _ -> Alcotest.fail "bench: expected VFloat")

let test_boot_seed_reexports () =
  (* The moved seed entry points stay on the Session.* API. Load-order
     behavior itself is pinned in loader_test via these same aliases. *)
  Alcotest.(check bool) "seed_package_priority keeps std first"
    true (List.hd S.seed_package_priority = "std");
  Alcotest.(check bool) "discover_seed_packages includes the preferred set"
    true
    (List.for_all
       (fun p -> List.mem p (S.discover_seed_packages ()))
       [ "std"; "notes"; "todo" ])

(** Regression (card d93d8292, "[ops] handler: unbound: marked"):
    call_handler must EVAL the handler in the same env it LOOKED it up in —
    the per-request snapshot (image.per-request-context). A concurrent
    rehydrate swaps t.eval_state.env mid-request; evaluating the found
    handler in the swapped live env used to die "unbound: <handler>"
    and auto-file a reliability incident. *)
let test_handler_evals_in_snapshot_env () =
  let t = anon_session () in
  ignore (eval_value t "(define (marked) \"ok\")");
  let snapshot_env = S.env t in
  (* Simulate the rehydrate: the live env moves to a fresh, handler-free
     record; the request context keeps holding the snapshot env. *)
  t.eval_state.env <- Pricklypear_lisp.Env.create ();
  let ctx =
    Pricklypear_image_lib.Request_context.make
      ~manifest:(S.manifest t)
      ~env:snapshot_env
      ~is_admin:false
      ~routes:(Pricklypear_image_lib.Request_context.copy_routes (S.routes t))
      ~source_ip:None
  in
  Pricklypear_image_lib.Request_context.with_current ctx (fun () ->
      match S.call_handler t "marked" with
      | S.Handler_ok _ -> ()
      | S.Handler_unbound ->
          Alcotest.fail "handler bound in the snapshot env resolved as unbound"
      | S.Handler_error msg ->
          Alcotest.fail (Printf.sprintf "handler died after env divergence: %s" msg))

(** Regression (card 0eaf31d5, "[ops] handler: unbound: todo/ensure-space"):
    the same request-snapshot divergence as d93d8292, but the unbound name
    is a CALLEE inside the handler body — todo/ensure-space tops every todo
    entry point (todo/page, kanban/page, workstreams/page) and
    notes/issues/page; no route dispatches it directly, so the incident's
    fingerprint names the callee, not the handler. The handler resolves in
    both envs; its body used to run in the swapped live env and died
    "unbound: todo/ensure-space". Dispatch must run the whole handler —
    body callees included — in the snapshot env it looked the handler up
    in. *)
let test_handler_body_callee_evals_in_snapshot_env () =
  let t = anon_session () in
  ignore (eval_value t "(define (todo/ensure-space) \"space-ok\")");
  ignore (eval_value t "(define (issues-page) (todo/ensure-space))");
  let snapshot_env = S.env t in
  (* Simulate the rehydrate: the live env moves to a fresh record holding
     neither the handler nor its callee; the request context keeps the
     snapshot env. Pre-77a3742 the body ran here and died unbound. *)
  t.eval_state.env <- Pricklypear_lisp.Env.create ();
  let ctx =
    Pricklypear_image_lib.Request_context.make
      ~manifest:(S.manifest t)
      ~env:snapshot_env
      ~is_admin:false
      ~routes:(Pricklypear_image_lib.Request_context.copy_routes (S.routes t))
      ~source_ip:None
  in
  Pricklypear_image_lib.Request_context.with_current ctx (fun () ->
      match S.call_handler t "issues-page" with
      | S.Handler_ok _ -> ()
      | S.Handler_unbound ->
          Alcotest.fail "handler bound in the snapshot env resolved as unbound"
      | S.Handler_error msg ->
          Alcotest.fail
            (Printf.sprintf "body callee died after env divergence: %s" msg))

(** Regression (card 1b76bdcf, "[ops] handler: not a function: 12"):
    dispatch must not eval a handler name blind. A handler name bound
    to a non-function (e.g. a page-size variable) returns a Handler_error
    that names the handler and the bound value - logged, not filed as a
    reliability incident. *)
let test_handler_non_function_binding () =
  let t = anon_session () in
  ignore (eval_value t "(define stream/page-size 12)");
  match S.call_handler t "stream/page-size" with
  | S.Handler_error msg ->
      let contains sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length msg
          && (String.sub msg i n = sub || go (i + 1))
        in
        go 0
      in
      Alcotest.(check bool) "error names the handler" true
        (contains "stream/page-size");
      Alcotest.(check bool) "error shows the bound value" true
        (contains "12")
  | S.Handler_ok _ ->
      Alcotest.fail "non-function binding dispatched as ok"
  | S.Handler_unbound ->
      Alcotest.fail "bound non-function name reported unbound"


(* Control_rim captures Eio effects, so eval-touching cases run inside
   the Eio scheduler (rls_floor_test precedent). *)
let in_eio f = Eio_main.run (fun _ -> f ())

let () =
  Alcotest.run "session_split"
    [ ("split seams", 
        [ Alcotest.test_case "make wires control prims" `Quick
            test_make_wires_control_prims
        ; Alcotest.test_case "sub-record state is live" `Quick
            (fun () -> in_eio test_subrecord_state_live)
        ; Alcotest.test_case "control prim guards through the hooks seam" `Quick
            (fun () -> in_eio test_control_prim_guards_through_seam)
          ; Alcotest.test_case "handler evals in the snapshot env" `Quick
              (fun () -> in_eio test_handler_evals_in_snapshot_env)
          ; Alcotest.test_case "handler body callee evals in the snapshot env" `Quick
              (fun () -> in_eio test_handler_body_callee_evals_in_snapshot_env)          ; Alcotest.test_case "handler non-function binding errors precisely" `Quick
              (fun () -> in_eio test_handler_non_function_binding)

        ; Alcotest.test_case "boot seed re-exports" `Quick
            test_boot_seed_reexports
        ]) ]
