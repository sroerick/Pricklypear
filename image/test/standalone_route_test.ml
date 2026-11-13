(** Pricklypear Image — standalone public route contract (card e4fdcfda).

    A route published with the 'standalone annotation
    (`(public-route "/site" "site/page" 'standalone)` /
    `(route "/site" "site/page" "public" 'standalone)`, bumpers M3)
    serves the handler's document VERBATIM: no habitat chrome — no
    <header>/<main> wrapper, no brand meta, no injected CSS/htmx. That is
    the first-class mechanism a platform-hosted external site uses instead
    of hand-rolling a bytes-response with an explicit content-type.

    Pins [Http_core.standalone_body], the pure decision the HTML arm of
    [respond_handler_value] makes, plus the session-scoped declaration
    state behind it ([Session.is_standalone_route] /
    [Session.declare_standalone_route], written by the route prims).
    No DB / no live server. *)

module S = Pricklypear_image_lib.Session
module Ast = Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let session () =
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

(* The handler's own full document — what an external face returns. *)
let doc =
  "<!doctype html><html><head><title>external</title>\
   <meta name=\"viewport\" content=\"width=device-width\"></head>\
   <body><h1>external site</h1></body></html>"

let contains_sub s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.sub s i n = sub || go (i + 1))
  in
  n = 0 || go 0

(** The declaration alone flips the response contract: same handler value,
    declared-standalone path -> the document verbatim; any other path ->
    the ordinary chrome arm (None). *)
let test_standalone_serves_document_verbatim () =
  let t = session () in
  let value = Ast.VStr doc in
  (* Before the declaration: the ordinary habitat-chrome path. *)
  Alcotest.(check (option string))
    "undeclared path takes the chrome arm" None
    (Pricklypear_image_lib.Http_core.standalone_body ~session:t
       ~path:"/site" value);
  S.declare_standalone_route t ~route:"/site";
  Alcotest.(check bool) "declaration is visible on the session" true
    (S.is_standalone_route t "/site");
  (match
     Pricklypear_image_lib.Http_core.standalone_body ~session:t ~path:"/site"
       value
   with
   | None -> Alcotest.fail "declared-standalone route fell back to chrome"
   | Some served ->
       Alcotest.(check string) "handler document is served verbatim" doc served;
       (* No chrome wrapper, no habitat asset injection. *)
       Alcotest.(check bool) "no habitat <header>" false
         (contains_sub served "<header");
       Alcotest.(check bool) "no habitat <main>" false
         (contains_sub served "<main");
       Alcotest.(check bool) "no htmx bundle injected" false
         (contains_sub served "htmx.min.js");
       Alcotest.(check bool) "no brand logo injected" false
         (contains_sub served "pplogo.png");
       Alcotest.(check bool) "no session meta painted" false
         (contains_sub served "system/main"))

(** The declaration is per-path and session-scoped: sibling paths and
    fresh sessions keep the chrome arm. *)
let test_standalone_is_per_path_and_session () =
  let t = session () in
  S.declare_standalone_route t ~route:"/site";
  let value = Ast.VStr doc in
  Alcotest.(check (option string)) "sibling path keeps chrome" None
    (Pricklypear_image_lib.Http_core.standalone_body ~session:t
       ~path:"/other" value);
  (* Monotone within the session: re-declaring does not duplicate or
     un-declare. *)
  S.declare_standalone_route t ~route:"/site";
  Alcotest.(check bool) "re-declare keeps the route standalone" true
    (S.is_standalone_route t "/site");
  (* Session-scoped: a fresh session has no declarations (the annotation
     state is in-memory, bumpers M3 minimal mechanism). *)
  let fresh = session () in
  Alcotest.(check bool) "declaration does not leak across sessions" false
    (S.is_standalone_route fresh "/site")

let in_eio f = Eio_main.run (fun _ -> f ())

let () =
  Alcotest.run "standalone_route"
    [ ("standalone public route",
        [ Alcotest.test_case "serves the handler document verbatim" `Quick
            (fun () -> in_eio test_standalone_serves_document_verbatim)
        ; Alcotest.test_case "per-path and session-scoped" `Quick
            (fun () -> in_eio test_standalone_is_per_path_and_session)
        ]) ]
