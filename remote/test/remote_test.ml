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
    only ever be proxied. *)
let test_no_local_store_write () =
  let transport, _calls, _ = mock_transport () in
  let t = Remote.make ~transport () in
  let forbidden =
    [ "create"; "update"; "delete"; "route"; "fork"; "merge"; "checkout"; "create-user" ]
  in
  let local_names =
    Hashtbl.to_seq_keys t.Remote.local_prims |> List.of_seq |> List.sort String.compare
  in
  List.iter
    (fun f ->
      Alcotest.(check bool)
        ("no local binding for store prim " ^ f) true
        (not (List.mem f local_names)))
    forbidden

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
        ] );
    ]
