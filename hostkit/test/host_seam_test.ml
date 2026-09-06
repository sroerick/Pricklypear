(* hostkit/host_seam_test.ml — the lifted host seam is the ONE home for
   the route table / request context / template / event / tool machinery
   both local hosts (agent + pp-remote) used to mirror (i-1788671304-d).
   These cases pin the seam's behavior over a synthetic rim so neither
   host can drift from it again:

   - route/dispatch renders a handler through the seam and binds the
     thin request context (query-param / request-method / hx-request?
     from BOTH the params alist and the headers list);
   - an unbound dispatch handler yields the "unbound handler" VStr
     contract (not an exception);
   - call_template unwraps raw-HTML VStrs and returns None for unbound;
   - fire contains a raising handler into {"error": ...};
   - eval_tool_body + register_builtin_tool round-trip the Loop.host
     seam through run_in_rim;
   - add_prim tracks registered local names (the placer feed) and the
     pure catalog fills the table + env. *)

open Pricklypear_lisp

let pp_value =
  Alcotest.testable
    (fun fmt v -> Format.pp_print_string fmt (Pricklypear_lisp.Reader.pp_value v))
    (fun a b -> Pricklypear_lisp.Reader.pp_value a = Pricklypear_lisp.Reader.pp_value b)

(* A synthetic rim: the LOCAL prim rim (no effects to forward — every
   prim the test uses is registered in the seam's table). *)
let make_seam () : Host_seam.t =
  let prims = Eval.make_prim_table () in
  let t =
    Host_seam.make ~prims ~run_in_rim:(Control.run_with_rim ~prims)
      ~cwd:(ref "/tmp") ()
  in
  (* The pure catalog + soup tree, as every real host boots. *)
  Host_seam.register_pure_prims t;
  Host_seam.register_soup_prims t;
  t

(* Direct eval into the seam's env — the same direct-style path
   Host_seam.apply_value_raw uses (pure compute needs no rim). *)
(* Eval ALL toplevel forms into the seam's env under the local prim
   rim — the same direct-style path the hosts' load_source uses
   (read_one does not skip leading whitespace, and these sources are
   multi-form; prim calls surface as Control.Prim_call effects that
   the rim services from the seam's table). *)
let eval_test (t : Host_seam.t) (src : string) : Ast.value =
  Control.run_with_rim ~prims:t.prims (fun () ->
      Eval.read_eval_all_direct ~prims:t.prims t.env src)

let test_route_dispatch () =
  let t = make_seam () in
  Host_seam.register_route_prims t;
  ignore
    (eval_test t
       {|
(define (home)
  (string-append "<h1>" (query-param "q") " " (request-method) "</h1>"))
(route "/" "home")|});
  (* dispatch through the OCaml wrapper (rim-establishing path). *)
  let v =
    Host_seam.dispatch_route ~rt:t ~handler:"home" ~method_:"GET" ~path:"/"
      ~params:[ ("q", "hi") ] ~headers:[] ~body:""
  in
  Alcotest.check pp_value "dispatch renders + binds request context"
    (Ast.VStr "<h1>hi GET</h1>") v;
  (* hx-request? resolves from BOTH headers and params (hyper/do-swap
     passes the header in params; the HTTP gate in headers). *)
  ignore (eval_test t {|(define (hx) (if (hx-request?) "hx" "no"))|});
  let v2 =
    Host_seam.dispatch_route ~rt:t ~handler:"hx" ~method_:"GET" ~path:"/hx"
      ~params:[] ~headers:[ ("hx-request", "true") ] ~body:""
  in
  Alcotest.check pp_value "hx-request? from headers" (Ast.VStr "hx") v2;
  let v3 =
    Host_seam.dispatch_route ~rt:t ~handler:"hx" ~method_:"GET" ~path:"/hx"
      ~params:[ ("hx-request", "true") ] ~headers:[] ~body:""
  in
  Alcotest.check pp_value "hx-request? from params" (Ast.VStr "hx") v3

let test_dispatch_unbound_handler () =
  let t = make_seam () in
  Host_seam.register_route_prims t;
  let v =
    Host_seam.dispatch_route ~rt:t ~handler:"nope" ~method_:"GET" ~path:"/"
      ~params:[] ~headers:[] ~body:""
  in
  match v with
  | Ast.VStr s when String.length s >= 31 && String.sub s 0 31 = "route/dispatch: unbound handler" ->
      ()
  | other -> Alcotest.fail ("expected unbound-handler VStr, got: " ^ Pricklypear_lisp.Reader.pp_value other)

let test_call_template () =
  let t = make_seam () in
  ignore (eval_test t {|(define (bubble) (render-html "<b>x</b>"))|});
  Alcotest.(check (option string)) "template bound -> rendered raw HTML"
    (Some "<b>x</b>") (Host_seam.call_template ~rt:t "bubble" []);
  Alcotest.(check (option string)) "template unbound -> None" None
    (Host_seam.call_template ~rt:t "nope" [])

let test_fire_error_containment () =
  let t = make_seam () in
  Host_seam.register_event_prims t;
  ignore
    (eval_test t
       {|(on "boom" (lambda (ev) (car 5)))
         (on "log" (lambda (ev) ev))|});
  let vs = Host_seam.fire t "boom" [ Ast.VStr "e1" ] in
  (match vs with
   | [ Ast.VStr s ] ->
       if not (String.length s >= 8 && String.sub s 0 8 = "{\"error\"") then
         Alcotest.fail ("raising handler must be contained into {\"error\": ..}, got " ^ s)
   | other ->
       Alcotest.fail
         ("expected one contained error value, got "
          ^ String.concat "," (List.map Pricklypear_lisp.Reader.pp_value other)));
  Alcotest.(check (list pp_value)) "non-raising handler passes payload through"
    [ Ast.VStr "e1" ] (Host_seam.fire t "log" [ Ast.VStr "e1" ]);
  Alcotest.(check int) "unfired event -> empty" 0
    (List.length (Host_seam.fire t "nope" []))

let test_tool_seam () =
  let t = make_seam () in
  Host_seam.register_event_prims t;
  (* plugin tool: closure evals under the rim; errors contained. *)
  ignore
    (eval_test t
       {|(register-tool "greet" (list) (lambda (args) "hi from nopales"))|});
  let v = Host_seam.eval_tool_body t "greet" "{\"x\":1}" in
  Alcotest.check pp_value "plugin tool body evals" (Ast.VStr "hi from nopales") v;
  Alcotest.check pp_value "no such tool -> error JSON"
    (Ast.VStr "{\"error\":\"no tool named ghost\"}")
    (Host_seam.eval_tool_body t "ghost" "{}");
  (* builtin tool: registry entry as a VPrim naming a __builtin__ prim. *)
  Host_seam.register_builtin_tool t "echo" ~run:(fun _json -> "echoed");
  let v2 =
    Host_seam.loop_host t
  in
  Alcotest.(check bool) "loop_host sees the builtin in tool order" true
    (List.mem "echo" (v2.Loop.tool_order ()));
  let entry =
    match v2.Loop.tool_entry "echo" with Some e -> e | None -> Alcotest.fail "builtin missing"
  in
  Alcotest.check pp_value "builtin dispatches through the rim's local table"
    (Ast.VStr "echoed") (v2.Loop.call_prim "__builtin__echo" [ Ast.VStr "{}" ]);
  ignore entry;
  (match v2.Loop.call_prim "__builtin__echo" [ Ast.VStr "not json" ] with
   | Ast.VStr s when String.length s >= 16 && String.sub s 0 16 = "{\"error\":\"bad js" -> ()
   | other ->
       Alcotest.fail
         ("builtin bad json must be contained into an error JSON, got: "
          ^ Pricklypear_lisp.Reader.pp_value other))

let test_registered_tracking_and_catalog () =
  let t = make_seam () in
  Host_seam.register_route_prims t;
  Host_seam.register_pure_prims t;
  Host_seam.register_soup_prims t;
  let locals = Host_seam.registered_local_prims t in
  List.iter
    (fun name ->
       if not (List.mem name locals) then
         Alcotest.fail ("registered list missing " ^ name))
    [ "route"; "route/dispatch"; "query-param" ];
  List.iter
    (fun name ->
       if not (List.mem name locals) then
         Alcotest.fail ("catalog prim not tracked as local: " ^ name))
    (Prims_pure.catalog_names @ [ "soup/parse"; "soup/select-one" ]);
  (* every catalog name is bound as a VPrim in the env AND in the table. *)
  List.iter
    (fun name ->
       match Env.lookup t.env name with
       | Some (Ast.VPrim p) when String.equal p name -> ()
       | _ -> Alcotest.fail ("catalog prim unbound: " ^ name))
    (List.filter_map
       (fun n -> if String.length n >= 5 && String.sub n 0 5 = "soup/" then None else Some n)
       Prims_pure.catalog_names)

let () =
  let open Alcotest in
  run "host_seam"
    [
      ( "route-table"
      , [ test_case "dispatch renders + binds the thin request context" `Quick
            test_route_dispatch
        ; test_case "unbound dispatch handler -> VStr contract" `Quick
            test_dispatch_unbound_handler
        ; test_case "call_template bound/unbound" `Quick test_call_template
        ] )
    ; ( "events-tools"
      , [ test_case "fire contains raising handlers" `Quick
            test_fire_error_containment
        ; test_case "tool seam: plugin + builtin + loop_host" `Quick
            test_tool_seam
        ] )
    ; ( "catalog"
      , [ test_case "registered tracking + pure/soup catalogs bound" `Quick
            test_registered_tracking_and_catalog
        ] )
    ]
