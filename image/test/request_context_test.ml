(* 2026-11-02: the active HTTP request is FIBER-LOCAL, not a field on the
   per-identity session.

   Regression for the "no active request" 500 on the slow /todos page: the
   binding used to be Session.t.current_request - one slot per identity,
   shared by every concurrent request of that identity - so whichever handler
   exited first restored its own previous value (usually None) and unbound a
   slower handler still running. These cases fail on that design and pass on
   an Eio fiber key. *)

open Pricklypear_image_lib

let mk (path : string) : Session_types.request =
  { method_ = `GET; path; params = []; body = ""; headers = [] }

let path_of (r : Session_types.request option) : string option =
  Option.map (fun (r : Session_types.request) -> r.path) r

(* The bug: two requests in flight on one identity, the later one finishing
   first. Each must still see its own request afterwards. *)
let test_parallel_requests_keep_their_own_binding () =
  Eio_main.run @@ fun _env ->
  let seen = ref [] in
  let worker path () =
    Request_context.with_request (mk path) (fun () ->
        Eio.Fiber.yield ();
        seen := (path, Request_context.get_request ()) :: !seen)
  in
  ignore (Eio.Fiber.both (worker "/slow") (worker "/fast"));
  Alcotest.(check int) "both handlers ran" 2 (List.length !seen);
  List.iter
    (fun (path, got) ->
      Alcotest.(check (option string))
        ("binding survives for " ^ path) (Some path) (path_of got))
    !seen

(* Nested dispatch: the inner handler's binding must be restored to the
   outer one on exit (card c58830ad's guarantee, kept by the fiber key). *)
let test_nested_dispatch_restores_the_outer_binding () =
  Eio_main.run @@ fun _env ->
  Request_context.with_request (mk "/outer") (fun () ->
      Request_context.with_request (mk "/inner") (fun () ->
          Alcotest.(check (option string)) "inner sees inner" (Some "/inner")
            (path_of (Request_context.get_request ())));
      Alcotest.(check (option string)) "outer restored" (Some "/outer")
        (path_of (Request_context.get_request ())))

(* Outside a handler (/api/eval, the REPL, a background fiber) there is no
   request, and request-* prims must still say so. *)
let test_no_request_outside_a_handler () =
  Eio_main.run @@ fun _env ->
  Alcotest.(check (option string)) "no binding" None
    (path_of (Request_context.get_request ()))

let () =
  Alcotest.run "request_context"
    [ ( "fiber-local request binding"
      , [ Alcotest.test_case "parallel requests keep their own binding" `Quick
            test_parallel_requests_keep_their_own_binding
        ; Alcotest.test_case "nested dispatch restores the outer binding" `Quick
            test_nested_dispatch_restores_the_outer_binding
        ; Alcotest.test_case "no request outside a handler" `Quick
            test_no_request_outside_a_handler
        ] )
    ]
