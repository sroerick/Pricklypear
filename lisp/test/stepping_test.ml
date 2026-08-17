(** Tests for the remote-eval stepping/trace debugger (borge/
    remote-eval.borg stepping, campaign 7.3): Explain.trace /
    Explain.stepper / breakpoints. PURE tests — a mock transport stands
    in for the remote prim round trip, no Postgres, no HTTP.

    These verify the three stepping verify-stanzas:
      - a breakpoint on a def/prim pauses local eval mid-expression and
        the data plane (prim calls) still crosses the wire and returns
        (the breakpoint fires ON a Remote_ok/Remote_err event);
      - the server is unaware a breakpoint fired: the mock transport
        observes EXACTLY the calls the fast eval would make — one per
        proxied prim, never extra, and presentation-stepping never
        re-evals so the wire is never re-crossed;
      - a trace shows local pure steps interleaved with remote prim
        round trips, distinguished ([Local n] vs [Remote_ok]). *)
open Pricklypear_lisp

(* --- harness: a local pure prim table + a proxy-prim env + a mock
   transport that counts wire crossings --- *)

(** A proxy prim bound as VPrim in the env (so eval performs
    [Control.Prim_call] for it, which the trace's proxy-style rim
    forwards via the transport). *)
let bind_proxy (env : Ast.env) (name : string) : unit =
  Env.define env name (Ast.VPrim name)

(** A local pure prim: registered in [local_prims] AND bound as VPrim in
    the env — the trace handler services it locally (a local step, never
    a wire crossing). *)
let add_local (env : Ast.env) (tbl : Eval.prim_table) (name : string)
    (fn : Ast.value list -> Ast.value) : unit =
  Eval.register_prim tbl name fn;
  Env.define env name (Ast.VPrim name)

let test_env (tbl : Eval.prim_table) : Ast.env =
  let env = Env.create () in
  add_local env tbl "+"
    (function
      | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.add a b)
      | _ -> Ast.VNil);
  bind_proxy env "get";
  env

let mk_transport () =
  let calls = ref 0 in
  let transport name args =
    incr calls;
    match name with
    | "get" -> Ok (Ast.VStr "ok")
    | _ -> Error (Failure ("unexpected prim " ^ name))
  in
  (transport, calls)

(* --- test 1: trace distinguishes local pure steps from remote round trips --- *)

let test_trace_interleaved () =
  let tbl = Eval.make_prim_table () in
  let env = test_env tbl in
  let transport, calls = mk_transport () in
  let tr =
    Explain.trace ~local_prims:tbl ~transport ~init_env:env
      ~src:"(+ 1 (get 'user 42))"
  in
  (* the expression crossed the wire exactly once (one get) *)
  Alcotest.(check int) "one remote round trip" 1 !calls;
  (* a Remote_ok record for the get prim *)
  let has_get =
    List.exists
      (function
        | Explain.Remote_ok (n, _) -> n = "get"
        | _ -> false)
      tr.events
  in
  Alcotest.(check bool) "trace records the get round trip" true has_get;
  (* at least one Local span (the pure (+) work between wire crossings) *)
  let has_local =
    List.exists (function Explain.Local _ -> true | _ -> false) tr.events
  in
  Alcotest.(check bool) "trace records local pure steps" true has_local;
  (* the final value is present *)
  Alcotest.(check bool) "trace result recorded" true (tr.result <> "")

(* --- test 2: breakpoint on a remote prim pauses mid-expression --- *)

let test_breakpoint_on_remote () =
  let tbl = Eval.make_prim_table () in
  let env = test_env tbl in
  let transport, calls = mk_transport () in
  let tr =
    Explain.trace ~local_prims:tbl ~transport ~init_env:env
      ~src:"(+ 1 (get 'user 42))"
  in
  let s = Explain.stepper_of_trace tr in
  Explain.add_breakpoint s (Explain.break_on_remote "get");
  (* continue pauses at the first get round trip *)
  let fired = Explain.continue s in
  Alcotest.(check bool) "breakpoint on get fires" true fired;
  (* the wire was crossed exactly once during trace — the server saw the
     SAME single call a fast eval would; stepping never re-crossed it *)
  Alcotest.(check int) "server unaware of breakpoint (one call only)" 1 !calls

(* --- test 3: single-step advances event-by-event without re-eval --- *)

let test_single_step () =
  let tbl = Eval.make_prim_table () in
  let env = test_env tbl in
  let transport, calls = mk_transport () in
  let tr =
    Explain.trace ~local_prims:tbl ~transport ~init_env:env
      ~src:"(+ 1 (get 'user 42))"
  in
  let s = Explain.stepper_of_trace tr in
  (* step through every event one at a time *)
  let n = ref 0 in
  while not (Explain.done_p s) do
    let _ = Explain.step s in
    incr n
  done;
  Alcotest.(check int) "stepped all events"
    (List.length tr.events) !n;
  (* stepping over the trace never made another wire call *)
  Alcotest.(check int) "single-step never re-crosses the wire" 1 !calls

(* --- test 4: a trace with two remote round trips interleaves --- *)

let test_multiple_round_trips () =
  let tbl = Eval.make_prim_table () in
  let env = test_env tbl in
  let calls = ref 0 in
  let transport name args =
    incr calls;
    ignore args;
    match name with
    | "get" | "rows" -> Ok (Ast.VStr "ok")
    | _ -> Error (Failure ("unexpected " ^ name))
  in
  (* two wire crossings, pure arithmetic between them *)
  let tr =
    Explain.trace ~local_prims:tbl ~transport ~init_env:env
      ~src:"(get 'a 1) (+ 1 1) (get 'b 2)"
  in
  Alcotest.(check int) "two round trips" 2 !calls;
  let remote_count =
    List.fold_left
      (fun acc -> function
        | Explain.Remote_ok _ | Explain.Remote_err _ -> acc + 1
        | Explain.Local _ -> acc)
      0 tr.events
  in
  Alcotest.(check int) "two remote events recorded" 2 remote_count

let () =
  Alcotest.run "remote-stepping"
    [
      ( "stepping",
        [
          Alcotest.test_case "trace interleaves local/remote" `Quick
            test_trace_interleaved;
          Alcotest.test_case "breakpoint on remote prim" `Quick
            test_breakpoint_on_remote;
          Alcotest.test_case "single-step without re-eval" `Quick
            test_single_step;
          Alcotest.test_case "multiple round trips interleave" `Quick
            test_multiple_round_trips;
        ] );
    ]
