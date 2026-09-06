(* Wire codec round-trips (i-1788671698-b, no DB).

   pure/wire_codec.ml is the /api/eval error contract shared by the
   image and the proxy rims: [error_to_json] serializes ANY exn to the
   {ok:false, error, kind, …} wire shape, [error_of_json] re-raises the
   SAME exn so a remote client observes identical exceptions to an
   in-process eval. Untested since the prims_pure split (i-1788671384)
   — this is the pure/ test the review asked for. The round-trip matrix
   pins every reconstructible error class plus the generic fallback,
   and the curl -D header parser used by the http-get prim. *)

open Pricklypear_lisp.Ast

(* substring containment (contains_seq is not in this stdlib) *)
let contains_seq (hay : string) (needle : string) : bool =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  if m = 0 then true else go 0

let wire (exn : exn) : string =
  Yojson.Safe.to_string (Wire_codec.error_to_json exn)

let unwire (s : string) : exn =
  Wire_codec.error_of_json (Yojson.Safe.from_string s)

(* Same-interp-error comparison (exns have no structural equality). *)
let same_exn (a : exn) (b : exn) : bool =
  match (a, b) with
  | Interp_error x, Interp_error y -> (
      match (x, y) with
      | Unbound_var n, Unbound_var n' -> n = n'
      | Prim_error (n, r, k), Prim_error (n', r', k') -> n = n' && r = r' && k = k'
      | User_error (m, k), User_error (m', k') -> m = m' && k = k'
      | Budget_exceeded, Budget_exceeded -> true
      | Parse_error (m, l, c), Parse_error (m', l', c') -> m = m' && l = l' && c = c'
      | _ -> false)
  | Pricklypear_lisp.Reader.Reader_error (m, l, c), Pricklypear_lisp.Reader.Reader_error (m', l', c') ->
      m = m' && l = l' && c = c'
  | _ -> false

let roundtrips (name : string) (exn : exn) =
  let json = wire exn in
  (* the wire is a string: go through the actual text round-trip *)
  let back = unwire json in
  Alcotest.(check bool) (name ^ " reconstructs the SAME exn") true
    (same_exn exn back)

let test_unbound () =
  roundtrips "unbound var" (Interp_error (Unbound_var "foo"));
  (* the wire shape carries the kind so a remote rim can classify *)
  Alcotest.(check bool) "kind on the wire" true
    (contains_seq (wire (Interp_error (Unbound_var "foo"))) "\"kind\":\"unbound_var\"")

let test_prim_error () =
  (* raw_error + name ride the wire so the inverse re-raises the SAME
     Prim_error — the no-DB contract and prim names survive the hop *)
  roundtrips "prim error"
    (Interp_error (Prim_error ("tree/get", "failed: no DB", "db")));
  Alcotest.(check bool) "raw_error on the wire" true
    (contains_seq
       (wire (Interp_error (Prim_error ("tree/get", "failed: no DB", "db"))))
       "\"raw_error\":\"failed: no DB\"")

let test_user_error () =
  roundtrips "user error" (Interp_error (User_error ("boom", "denied")))

let test_budget () =
  roundtrips "budget exceeded" (Interp_error Budget_exceeded)

let test_reader_error () =
  roundtrips "reader error"
    (Pricklypear_lisp.Reader.Reader_error ("unbalanced paren", 3, 7))

let test_parse_error_shape () =
  (* Parse_error serializes to the parse_error shape and reconstructs
     as Reader_error (same parse failure, same wire — documented). The
     message slot carries the RENDERED pp_interp_error text (only
     Prim_error carries a raw_error field), plus line/col fields. *)
  let json = wire (Interp_error (Parse_error ("bad form", 2, 5))) in
  Alcotest.(check bool) "parse_error kind" true
    (contains_seq json "\"kind\":\"parse_error\"");
  match unwire json with
  | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
      Alcotest.(check bool) "message is the rendered error" true
        (contains_seq m "bad form");
      Alcotest.(check int) "line" 2 l;
      Alcotest.(check int) "col" 5 c
  | exn -> Alcotest.fail ("expected Reader_error, got: " ^ Printexc.to_string exn)

let test_generic_exn () =
  (* unknown exn → {ok:false, error} (no kind field). The inverse's
     no-kind path is the parse_error reconstruction, so a generic exn
     currently degrades to Reader_error (message, 0, 0): the MESSAGE
     survives the hop, the classification is lost. Pinned AS-IS — the
     kind loss is filed, not fixed, in this test-gap loop. *)
  let json = wire (Failure "disk gone") in
  Alcotest.(check bool) "generic shape has error" true
    (contains_seq json "disk gone");
  match unwire json with
  | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
      Alcotest.(check bool) "message survives the hop" true
        (contains_seq m "disk gone");
      Alcotest.(check int) "degraded loc is 0,0" 0 (l + c)
  | exn -> Alcotest.fail ("expected degraded Reader_error, got: " ^ Printexc.to_string exn)

let test_garbage_json () =
  (* a non-object payload degrades to User_error, never a crash *)
  match unwire "[1,2]" with
  | Interp_error (User_error (_, "unknown")) -> ()
  | exn -> Alcotest.fail ("expected unknown User_error, got: " ^ Printexc.to_string exn)

let str_field fields k =
  match List.assoc_opt k fields with
  | Some (`String s) -> Some s
  | _ -> None

let test_http_parse_headers () =
  let blob =
    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nX-A: b\r\n\r\n"
  in
  let json = Yojson.Safe.from_string (Wire_codec.http_parse_headers blob) in
  match json with
  | `Assoc fields ->
      Alcotest.(check string) "content-type (lowercased key)" "text/html"
        (Option.value ~default:"?" (str_field fields "content-type"));
      Alcotest.(check string) "x-a" "b"
        (Option.value ~default:"?" (str_field fields "x-a"));
      (* status line ignored, not a field *)
      Alcotest.(check bool) "no status field" true
        (not (List.mem_assoc "http/1.1 200 ok" fields))
  | _ -> Alcotest.fail "headers not a JSON object"

let test_http_parse_headers_url_value () =
  (* value with a ':' — split on the FIRST colon only *)
  let blob = "HTTP/2 200\r\nLocation: http://x/y?a=b\r\n" in
  let json = Yojson.Safe.from_string (Wire_codec.http_parse_headers blob) in
  match json with
  | `Assoc fields ->
      Alcotest.(check string) "location keeps colons" "http://x/y?a=b"
        (Option.value ~default:"?" (str_field fields "location"))
  | _ -> Alcotest.fail "headers not a JSON object"

let () =
  Alcotest.run "pricklypear-wire-codec"
    [ ( "wire-codec"
      , [ ("unbound-roundtrip", `Quick, test_unbound)
        ; ("prim-error-roundtrip", `Quick, test_prim_error)
        ; ("user-error-roundtrip", `Quick, test_user_error)
        ; ("budget-roundtrip", `Quick, test_budget)
        ; ("reader-error-roundtrip", `Quick, test_reader_error)
        ; ("parse-error-shape", `Quick, test_parse_error_shape)
        ; ("generic-exn", `Quick, test_generic_exn)
        ; ("garbage-json", `Quick, test_garbage_json)
        ; ("http-parse-headers", `Quick, test_http_parse_headers)
        ; ("http-parse-headers-url-value", `Quick, test_http_parse_headers_url_value)
        ] ) ]
