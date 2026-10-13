(** Pricklypear Image — router table + one request-body cap tests
    (i-1788671135-a).

    Pins the http.ml router table contract: probe matrix of
    (method, path) → matched route NAME (exact / prefix /
    prefix-nonempty / guard / catch-all ordering), the table-shape
    invariants (unique names, catch-alls last), and the ONE cap table
    (Http_core.request_body_cap ~kind:`Form|`Media — was two drifted
    copies: http.ml route_body_cap vs Http_media.request_body_cap).
    No DB / no live server. *)

open Pricklypear_image_lib

(* ---- one request-body cap ---- *)

let mb = 1024 * 1024

let audio = Media.max_http_audio_bytes (* 64 MiB *)
let media_cap = Media.max_http_bytes (* 5 MiB *)
let form_cap = 64 * 1024

let cap_int_t : int Alcotest.testable = Alcotest.int

let test_cap_matrix () =
  let check msg ~kind ct expected =
    Alcotest.check cap_int_t msg expected (Http_core.request_body_cap ~kind ct)
  in
  (* `Form — the user-route pipeline (incl. anonymous public-route POSTs):
     only multipart escapes the 64 KiB form cap; media/JSON content types
     deliberately do NOT widen it. *)
  check "form: no content-type keeps form cap" ~kind:`Form None form_cap;
  check "form: urlencoded keeps form cap" ~kind:`Form
    (Some "application/x-www-form-urlencoded")
    form_cap;
  check "form: json does NOT inherit the media b64 ceiling" ~kind:`Form
    (Some "application/json")
    form_cap;
  check "form: raw image ct keeps form cap" ~kind:`Form (Some "image/png")
    form_cap;
  check "form: multipart gets the audio ceiling + slack" ~kind:`Form
    (Some "multipart/form-data; boundary=x")
    (audio + 65536);
  check "form: multipart case-insensitive" ~kind:`Form
    (Some "Multipart/Form-Data; boundary=y")
    (audio + 65536);
  (* `Media — the dedicated media endpoints keep their per-type ceilings. *)
  check "media: no content-type keeps 5 MiB read ceiling" ~kind:`Media None
    media_cap;
  check "media: raw image gets 5 MiB" ~kind:`Media (Some "image/png") media_cap;
  check "media: raw audio gets the audio ceiling" ~kind:`Media
    (Some "audio/mpeg")
    audio;
  check "media: audio alias normalized to the audio ceiling" ~kind:`Media
    (Some "audio/mp3")
    audio;
  check "media: json b64 rides the audio-inflated ceiling" ~kind:`Media
    (Some "application/json")
    (((audio * 4) / 3) + 4096);
  check "media: multipart gets audio + slack" ~kind:`Media
    (Some "multipart/form-data")
    (audio + 65536)

(* ---- router table ---- *)

let string_t : string Alcotest.testable = Alcotest.string

let test_route_probes () =
  (* (label, method, path, expected route name) — the load-bearing
     orderings: exacts before prefixes, narrow prefixes before broad
     ones, the guard-guarded media-delete falling through to the
     user-route catch-all, and the any-method not-found entry last. *)
  let probes :
      (string * (Httpun.Method.t * string * string)) list =
    [ ("home", (`GET, "/", "home"));
      ("login-get", (`GET, "/login", "login-form"));
      ("login-post", (`POST, "/login", "login-post"));
      ("welcome", (`GET, "/welcome", "welcome"));
      ("signup-get", (`GET, "/signup", "signup"));
      ("signup-post", (`POST, "/signup", "signup"));
      ("book-index", (`GET, "/book", "book-index"));
      ("book-chapter", (`GET, "/book/chapter/foo", "book-chapter"));
      (* /book/chapter/ exactly: the chapter arm needs a nonempty rest,
         so the /book/<pkg> arm owns it (pre-router behavior). *)
      ("book-chapter-exact-falls-to-spec", (`GET, "/book/chapter/", "book-spec"));
      ("book-code", (`GET, "/book/code/foo", "book-code"));
      ("book-course", (`GET, "/book/course/foo", "book-course"));
      ("book-verify", (`GET, "/book/verify", "book-verify"));
      ("book-spec", (`GET, "/book/foo", "book-spec"));
      ("book-pdf", (`GET, "/book.pdf", "book-pdf"));
      ("code", (`GET, "/code", "code"));
      ("code-raw", (`GET, "/code/raw/a.md", "code-raw"));
      ("code-raw-empty-rest", (`GET, "/code/raw/", "code-raw"));
      ("assets-cm-shadows-assets", (`GET, "/assets/cm/x.js", "assets-cm"));
      ("assets", (`GET, "/assets/x", "assets"));
      ("health", (`GET, "/health", "health"));
      ("agent-json", (`GET, "/.well-known/agent.json", "agent-json"));
      ("agent-txt", (`GET, "/agent.txt", "agent-txt"));
      ("agent-page", (`GET, "/agent", "agent-page"));
      ("api-eval", (`POST, "/api/eval", "api-eval"));
      ("api-prim", (`POST, "/api/prim", "api-prim"));
      ("api-media-list", (`GET, "/api/media", "api-media-list"));
      ("api-media-post", (`POST, "/api/media", "api-media-post"));
      ("api-media-delete", (`DELETE, "/api/media/x", "api-media-delete"));
      ("api-media-delete-empty-rest-404", (`DELETE, "/api/media/", "not-found"));
      ("media-page", (`GET, "/media", "media-page"));
      ("media-page-slash", (`GET, "/media/", "media-page"));
      ("media-get", (`GET, "/media/abc", "media-get"));
      ("media-upload", (`POST, "/media/upload", "media-upload"));
      ("media-delete", (`POST, "/media/abc/delete", "media-delete"));
      (* non-/delete POST under /media/ is a user route (guard fails) *)
      ("media-post-guard-falls-through", (`POST, "/media/abc", "user-route"));
      ("api-agent", (`POST, "/api/agent", "api-agent"));
      ("api-agent-task", (`POST, "/api/agent-task", "api-agent-task"));
      ("repl-llm", (`POST, "/repl-v2/llm", "repl-llm"));
      ("ai-propose", (`POST, "/api/ai/propose", "ai-propose"));
      ("analyze", (`POST, "/api/analyze/todo/page", "api-analyze"));
      ("explain", (`POST, "/api/explain/todo/page", "api-explain"));
      ("data-index", (`GET, "/data", "data-index"));
      ("data-index-slash", (`GET, "/data/", "data-index"));
      ("data-get", (`GET, "/data/note", "data-get"));
      ("data-get-new", (`GET, "/data/note/new", "data-get"));
      ("data-get-id", (`GET, "/data/note/123", "data-get"));
      ("data-post", (`POST, "/data/note", "data-post"));
      ("data-post-delete", (`POST, "/data/note/123/delete", "data-post"));
      ("tree", (`GET, "/tree", "tree"));
      ("tree-partial", (`GET, "/partials/tree", "tree-partial"));
      ("repl", (`GET, "/repl", "repl"));
      ("bindings", (`GET, "/bindings", "bindings"));
      ("binding", (`GET, "/binding/todo/page", "binding"));
      ("types", (`GET, "/types", "types"));
      ("timing", (`GET, "/timing", "timing"));
      ("diff", (`GET, "/diff", "diff"));
      ("branches", (`GET, "/branches", "branches"));
      ("branches-checkout", (`POST, "/branches/checkout", "branches-checkout"));
      ("page-index", (`GET, "/page", "page-index"));
      ("page-inspect-slash", (`GET, "/page/", "page-inspect"));
      ("page-inspect", (`GET, "/page/x", "page-inspect"));
      ("inspect-binding", (`GET, "/inspect/binding/todo/page", "inspect-binding"));
      ("inspect-row", (`GET, "/inspect/row/note/123", "inspect-row"));
      ("editor-new", (`GET, "/edit", "editor-new"));
      ("editor-new-slash", (`GET, "/edit/", "editor-new"));
      ("editor-get", (`GET, "/edit/todo/page", "editor-get"));
      ("edit-save", (`POST, "/edit/todo/page", "edit-save"));
      ("book-eval-example", (`POST, "/book/eval-example", "book-eval-example"));
      ("catch-all-get", (`GET, "/nonexistent", "user-route"));
      ("catch-all-post", (`POST, "/nonexistent", "user-route"));
      ("non-getpost-methods-404", (`PUT, "/", "not-found"));
      ("delete-unknown-404", (`DELETE, "/x", "not-found"));
      ("head-404", (`HEAD, "/", "not-found")) ]
  in
  List.iter
    (fun (label, (m, path, expected)) ->
      match Http.find_route m path with
      | Some r ->
          Alcotest.check string_t label expected r.Http.name
      | None ->
          Alcotest.fail (Printf.sprintf "%s: no route for %s" label path))
    probes

let test_table_shape () =
  let names = List.map (fun (r : Http.route) -> r.Http.name) Http.routes in
  let rec dups = function
    | [] -> []
    | x :: rest when List.mem x rest -> x :: dups (List.filter (( <> ) x) rest)
    | _ :: rest -> dups rest
  in
  Alcotest.check (Alcotest.list string_t) "unique route names" []
    (dups names);
  (* The catch-alls stay last: the GET/POST user-route entry, then the
     any-method not-found entry. Everything before them must be
     narrowing (exact or a real prefix), or the table is mis-ordered. *)
  let n = List.length Http.routes in
  let last = List.nth Http.routes (n - 1) in
  let second_last = List.nth Http.routes (n - 2) in
  Alcotest.check string_t "last entry is the any-method not-found"
    "not-found" last.Http.name;
  Alcotest.check Alcotest.bool "not-found answers any method"
    true (last.Http.meths = []);
  Alcotest.check string_t "second-to-last is the user-route catch-all"
    "user-route" second_last.Http.name;
  Alcotest.check Alcotest.bool "catch-all answers GET+POST"
    true
    (List.sort compare second_last.Http.meths = [ `GET; `POST ])

let () =
  let open Alcotest in
  run "http_router"
    [ ("cap", [ test_case "one request-body cap table" `Quick test_cap_matrix ]);
      ("router",
        [ test_case "probe matrix" `Quick test_route_probes;
          test_case "table shape" `Quick test_table_shape ])
    ]
