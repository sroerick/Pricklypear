(** Tests for borge/remote-eval.borg client subsection. No network —
    the [Proxy_rim.transport] is a mock that records calls and returns
    canned values. Verifies: pure compute stays local (zero prim calls),
    a prim call proxies and the result binds locally for subsequent
    pure use, lazy def miss fetches + evals, genuinely-unbound fails
    cleanly, and the client makes no local store write (it is a cache +
    interpreter, never a source of truth). *)

open Pricklypear_lisp
open Ast

let pp_value_testable =
  Alcotest.testable
    (fun fmt v -> Format.pp_print_string fmt (Reader.pp_value v))
    (fun a b -> Reader.pp_value a = Reader.pp_value b)

(** A mock transport: records (name, args) calls and answers from a
    table; an optional fallback for calls not in the table. *)
let mock_transport
    ?(fallback : (string -> value list -> (value, exn) Stdlib.result) option)
    ()
    : Proxy_rim.transport * (string * value list) list ref
      * (string, value list -> (value, exn) Stdlib.result) Hashtbl.t =
  let calls = ref [] in
  let table = Hashtbl.create 8 in
  let fn name args =
    calls := !calls @ [ (name, args) ];
    match Hashtbl.find_opt table name with
    | Some f -> f args
    | None -> (
        match fallback with
        | Some g -> g name args
        | None ->
            Error
              (Interp_error
                 (Prim_error (name, "not in mock table", "prim_error"))))
  in
  (fn, calls, table)

(** Pure compute stays local: (+ 1 2) makes ZERO prim calls (the proxy
    rim services + from the local pure table). *)
let test_pure_local () =
  let transport, calls, _ = mock_transport () in
  let t = Remote.make ~transport () in
  let v = Remote.eval t "(+ 1 2)" in
  Alcotest.(check int) "pure result" 3
    (match v with VInt n -> Int64.to_int n | _ -> -1);
  Alcotest.(check int) "zero prim calls (pure stayed local)" 0 (List.length !calls)

(** A prim call proxies to /api/prim and the result binds locally for
    subsequent pure use. (get 'user 42) is NOT in the local pure table
    → forward → the mock returns a VStr JSON row → (dict-get … name)
    runs locally on it. *)
let test_prim_proxies () =
  let transport, calls, table = mock_transport () in
  Hashtbl.replace table "get"
    (fun args ->
      (* (get 'user 42) -> a JSON string row, as the image prim does *)
      Ok (VStr {|{"id":42,"name":"ada"}|}));
  let t = Remote.make ~transport () in
  let v = Remote.eval t "(dict-get (get 'user 42) \"name\")" in
  Alcotest.(check pp_value_testable)
    "proxied prim result usable locally" (VStr "ada") v;
  Alcotest.(check int) "exactly one prim call (get)" 1 (List.length !calls);
  Alcotest.(check string) "prim name was get" "get" (fst (List.hd !calls))

(** Lazy env miss: referencing an unbound function name triggers a
    fetch of its def source (proxied via the function-source prim) and
    evals it into the env, then retries — so the call succeeds and the
    function is cached (a second reference does NOT re-fetch). *)
let test_lazy_def_miss () =
  let transport, calls, table = mock_transport () in
  (* function-source returns the stored body for "inc" *)
  Hashtbl.replace table "function-source"
    (fun args ->
      match args with
      | [ VStr n ] when n = "inc" -> Ok (VStr "(define (inc x) (+ x 1))")
      | _ -> Ok VNil);
  let t = Remote.make ~transport () in
  let v = Remote.eval t "(inc 41)" in
  Alcotest.(check int) "lazy-loaded inc 41 = 42" 42
    (match v with VInt n -> Int64.to_int n | _ -> -1);
  (* one function-source fetch for the miss, then + runs local *)
  let fetches =
    List.length (List.filter (fun (n, _) -> n = "function-source") !calls)
  in
  Alcotest.(check int) "exactly one function-source fetch" 1 fetches;
  (* second reference: inc is now local, no new fetch *)
  let _ = Remote.eval t "(inc 100)" in
  let fetches2 =
    List.length (List.filter (fun (n, _) -> n = "function-source") !calls)
  in
  Alcotest.(check int) "second reference does not re-fetch" 1 fetches2

(** A genuinely unbound name (function-source returns nothing/empty)
    fails cleanly with Unbound_var, not an infinite retry loop. *)
let test_genuinely_unbound () =
  let transport, _calls, table = mock_transport () in
  Hashtbl.replace table "function-source" (fun _ -> Ok (VStr ""));
  let t = Remote.make ~transport () in
  (try
     ignore (Remote.eval t "(nope-such-fn 1)");
     Alcotest.fail "expected Unbound_var"
   with
  | Interp_error (Unbound_var n) ->
      Alcotest.(check string) "unbound name reported" "nope-such-fn" n
  | other -> Alcotest.fail ("wrong exn: " ^ Printexc.to_string other))

(** The client is a cache + interpreter, never a source of truth: it
    exposes NO prim that writes to a local store. There is no local
    store; the only write-ish prim is function-source (a read). Assert
    the local prim table contains no DB/branch/admin prims — those can
    only ever be proxied.

    NOTE: [route] is NOT in the forbidden set. The client owns a LOCAL
    route table (the agent-route-table host seam, ported so the
    Nopales-authored surface runs unchanged — borge/remote-eval.borg
    surfaces). (route path handler) writes to the client's local
    hashtbl, NOT the image's Postgres view_bindings; it is a surface
    prim, not a store write. The image's server-side [route] stays
    server-only (register_server_only) — a different prim with the same
    name on a different host. *)
let test_no_local_store_write () =
  let transport, _calls, _ = mock_transport () in
  let t = Remote.make ~transport () in
  let forbidden =
    [ "create"; "update"; "delete"; "fork"; "merge"; "checkout"; "create-user" ]
  in
  let local_names =
    Hashtbl.to_seq_keys t.seam.prims |> List.of_seq |> List.sort String.compare
  in
  List.iter
    (fun f ->
      Alcotest.(check bool)
        ("no local binding for store prim " ^ f) true
        (not (List.mem f local_names)))
    forbidden

(** Hybrid placement integration: with a whole-expression remote_eval
    hook attached, a (with-transaction …) expression is NOT run locally
    through the proxy rim — it is POSTed to the remote_eval hook
    wholesale (hard correctness: locks can't span HTTP). No local store
    write, no per-prim proxying of the transaction body. *)
let test_transaction_routed_remote () =
  let transport, calls, _ = mock_transport () in
  let remote_calls = ref [] in
  let t =
    Remote.make ~transport
      ~remote_eval:(fun src ->
        remote_calls := src :: !remote_calls;
        VStr "committed")
      ()
  in
  let v =
    Remote.eval t
      "(with-transaction (lambda () (create 'note 'n1)))"
  in
  Alcotest.(check pp_value_testable) "hook returned value" (VStr "committed") v;
  Alcotest.(check int) "remote_eval called exactly once" 1 (List.length !remote_calls);
  (match !remote_calls with
   | [ src ] ->
       Alcotest.(check bool) "WHOLE source sent to /api/eval" true
         (String.length src > 0)
   | _ -> Alcotest.fail "expected exactly one remote eval");
  (* the transaction body was NOT proxied prim-by-prim: no /api/prim calls *)
  Alcotest.(check int) "zero per-prim transport calls" 0 (List.length !calls)

(** A Remote placement with NO remote_eval hook raises instead of
    silently running locally — never pretend a transaction is local. *)
let test_transaction_without_hook_raises () =
  let transport, _calls, _ = mock_transport () in
  let t = Remote.make ~transport () in
  (try
     ignore (Remote.eval t "(with-transaction (lambda () (+ 1 2)))");
     Alcotest.fail "expected failwith (no remote-eval transport)"
   with
  | Failure msg ->
      Alcotest.(check bool) "raises a clear message" true
        (String.length msg > 0)
  | other -> Alcotest.fail ("wrong exn: " ^ Printexc.to_string other))

(** Stepping over a remote client (borge/remote-eval.borg stepping smoke:
    an agent loop drives a stepping eval against a remote branch — set
    bp, step, inspect frame, continue). Runs [Remote.stepper_of]
    (Explain.trace over the client's env + local pure prims + transport);
    a breakpoint on [get] fires; stepping never re-crosses the wire, so
    the server saw exactly the calls a fast eval would (stepping is a
    purely client concern). *)
let test_stepping_via_client () =
  let transport, calls, table = mock_transport () in
  Hashtbl.replace table "get" (fun _ -> Ok (VStr "ok"));
  let t = Remote.make ~transport () in
  let tr, s = Remote.stepper_of t "(+ 1 (get 'user 42))" in
  (* the trace recorded the get round trip (data plane crossed the wire
     and returned) *)
  Alcotest.(check bool) "trace has a get round trip" true
    (List.exists
       (function
         | Explain.Remote_ok (n, _) | Explain.Remote_err (n, _) -> n = "get"
         | Explain.Local _ -> false)
       tr.events);
  (* the fast eval crosses the wire exactly once for (get 'user 42): the
     server is unaware a breakpoint fired — one call, never re-crossed *)
  Alcotest.(check int) "one wire crossing" 1 (List.length !calls);
  (* drive: set a breakpoint on get, continue, continue to end, inspect
     the frame stack — set bp / step / inspect frame / continue *)
  Explain.add_breakpoint s (Explain.break_on_remote "get");
  let fired = Explain.continue s in
  Alcotest.(check bool) "breakpoint on get fires" true fired;
  let fired2 = Explain.continue s in
  Alcotest.(check bool) "continue past the end does not re-trigger" false fired2;
  let fr = Explain.frames "(+ 1 (get 'user 42))" in
  Alcotest.(check bool) "frames inspectable (source-level AST)" true (fr <> [])

(** Route table + template dispatch (agent-route-table host seam,
    ported to the client — borge/remote-eval.borg surfaces). The client
    owns a LOCAL route hashtbl; (route path handler) populates it;
    [lookup_route] / [dispatch_route] / [call_template] run handlers
    under the proxy rim so a handler's prim calls forward to /api/prim.
    This is what makes the Nopales-authored surface run on pp-remote
    unchanged. *)
let test_route_table_and_dispatch () =
  let transport, calls, table = mock_transport () in
  (* a handler that calls a proxy prim (whoami) + pure compute *)
  Hashtbl.replace table "whoami"
    (fun _args -> Ok (VStr "admin"));
  let t = Remote.make ~transport () in
  (* define a handler + bind it to a route, in Nopales (the surface
     boot-load path). *)
  ignore (Remote.eval t
    {|(define (hello) (render-html (string-append "<p>hi " (whoami) "</p>")))|} );
  ignore (Remote.eval t {|(route "/x" "hello")|});
  Alcotest.(check string) "lookup_route finds the handler"
    "hello"
    (match Remote.lookup_route ~rt:t "/x" with Some h -> h | None -> "");
  (* dispatch_route runs the handler under the proxy rim: whoami proxies,
     the result is rendered raw-HTML. *)
  let v = Remote.dispatch_route ~rt:t ~handler:"hello" ~method_:"GET"
            ~path:"/x" ~params:[] ~headers:[] ~body:"" in
  (match v with
   | VStr s ->
       Alcotest.(check string) "dispatch renders handler html" "<p>hi admin</p>" s
   | other -> Alcotest.fail ("dispatch wrong shape: " ^ Reader.pp_value other));
  Alcotest.(check bool) "dispatch proxied whoami" true
    (List.exists (fun (n, _) -> n = "whoami") !calls);
  (* call_template renders a named fn for the OCaml HTTP surface. *)
  (match Remote.call_template ~rt:t "hello" [] with
   | Some s -> Alcotest.(check string) "call_template renders" "<p>hi admin</p>" s
   | None -> Alcotest.fail "call_template returned None");
  (* query-param resolves inside a dispatched handler. *)
  ignore (Remote.eval t
    {|(define (qp) (render-html (string-append "<p>" (query-param "q") "</p>")))|});
  let v2 = Remote.dispatch_route ~rt:t ~handler:"qp" ~method_:"GET"
            ~path:"/y" ~params:[ ("q", "stuff") ] ~headers:[] ~body:"" in
  (match v2 with
   | VStr s -> Alcotest.(check string) "query-param resolves" "<p>stuff</p>" s
   | other -> Alcotest.fail ("qp wrong shape: " ^ Reader.pp_value other))

(** i-1788670799-f: the local table covers the shared pure catalog (the
    hand copy here registered string-split twice and skipped the
    math/render towers), and pure compute through it stays LOCAL and
    callable. *)
let test_pure_catalog_parity () =
  let transport, calls, _ = mock_transport () in
  let t = Remote.make ~transport () in
  List.iter
    (fun name ->
       if not (Hashtbl.mem t.seam.prims name) then
         Alcotest.fail ("remote local table missing pure prim: " ^ name))
    Prims_pure.catalog_names;
  Alcotest.check pp_value_testable "pure parity callable" (VInt 2L)
    (Remote.eval t "(length (string-split \"a,b\" \",\"))");
  Alcotest.(check int) "zero prim calls (pure stayed local)" 0
    (List.length !calls)

let () =
  Alcotest.run "remote"
    [
      ( "client",
        [
          Alcotest.test_case "pure compute stays local" `Quick test_pure_local;
          Alcotest.test_case "prim call proxies and binds locally" `Quick test_prim_proxies;
          Alcotest.test_case "lazy def miss fetches + evals" `Quick test_lazy_def_miss;
          Alcotest.test_case "genuinely unbound fails cleanly" `Quick test_genuinely_unbound;
          Alcotest.test_case "no local store-write prims" `Quick test_no_local_store_write;
          Alcotest.test_case "transaction routed to remote_eval wholesale" `Quick test_transaction_routed_remote;
          Alcotest.test_case "transaction without hook raises" `Quick test_transaction_without_hook_raises;
          Alcotest.test_case "stepping over a remote client" `Quick test_stepping_via_client;
          Alcotest.test_case "route table + template dispatch" `Quick test_route_table_and_dispatch;
          Alcotest.test_case "pure catalog parity" `Quick test_pure_catalog_parity;
        ] );
    ]
