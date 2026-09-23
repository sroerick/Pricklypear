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
  (* unknown exn → {ok:false, error, kind:"exn"} (i-1788714788: the
     kind tag used to be missing and the inverse degraded the shape to
     Reader_error (message, 0, 0), misattributing arbitrary server
     failures as parse errors). The inverse maps kind:"exn" back to
     User_error(msg,"exn") — the MESSAGE survives the hop and the
     classification is a generic exception, never a Reader_error. *)
  let json = wire (Failure "disk gone") in
  Alcotest.(check bool) "generic shape has error" true
    (contains_seq json "disk gone");
  Alcotest.(check bool) "generic shape has kind exn" true
    (contains_seq json "\"kind\":\"exn\"");
  (match unwire json with
   | Interp_error (User_error (m, "exn")) ->
       Alcotest.(check bool) "message survives the hop" true
         (contains_seq m "disk gone")
   | exn ->
       Alcotest.fail ("expected User_error \"exn\", got: " ^ Printexc.to_string exn));
  (* the regression pin: a generic exn must NOT come back as a
     Reader_error, whatever its Printexc text contains *)
  match unwire (wire (Sys_error "connection refused: /db")) with
  | Pricklypear_lisp.Reader.Reader_error _ ->
      Alcotest.fail "generic exn degraded to Reader_error"
  | Interp_error (User_error (m, "exn")) ->
      Alcotest.(check bool) "Sys_error text survives" true
        (contains_seq m "connection refused")
  | exn -> Alcotest.fail ("expected User_error \"exn\", got: " ^ Printexc.to_string exn)

(* ---- bumpers M6 error hints (borge/bumpers.borg discovery-surfaces):
   known error classes carry an optional "hint" field (unbound var ->
   (apropos ...), prim bad-args -> (function-doc ...)); unknown classes
   carry NONE (must stay additive + never a false suggestion). *) 

let test_error_hint_unbound () =
  (* namespaced unbound var -> suggest the namespace prefix; plain name
     -> suggest the name itself (apropos matches substring) *)
  Alcotest.(check string) "namespaced stem" "(apropos \"todo/\")"
    (Option.value ~default:"?"
       (Wire_codec.error_hint
          (Interp_error (Unbound_var "todo/nonesuch"))));
  Alcotest.(check string) "plain name stem" "(apropos \"frobnic\")"
    (Option.value ~default:"?"
       (Wire_codec.error_hint (Interp_error (Unbound_var "frobnic"))));
  (* rides the wire JSON as an additive field *)
  let json = wire (Interp_error (Unbound_var "todo/nonesuch")) in
  Alcotest.(check bool) "hint on the wire" true
    (contains_seq json "\"hint\":\"(apropos \\\"todo/\\\")\"")

let test_error_hint_prim_bad_args () =
  Alcotest.(check string) "prim bad-args hint" "(function-doc \"tree/get\")"
    (Option.value ~default:"?"
       (Wire_codec.error_hint
          (Interp_error (Prim_error ("tree/get", "bad args (need type, id)", "bad-args")))));
  let json = wire
      (Interp_error (Prim_error ("tree/get", "bad args (need type, id)", "bad-args"))) in
  Alcotest.(check bool) "hint on the wire" true
    (contains_seq json "\"hint\":\"(function-doc \\\"tree/get\\\")\"")

let test_error_hint_absent_unknown () =
  (* unknown classes carry NO hint: User_error, Budget_exceeded, a
     non-bad-args Prim_error, Reader_error, and the generic exn *)
  let no_hint exn =
    Alcotest.(check bool) ("no hint for " ^ Printexc.to_string exn) true
      (Wire_codec.error_hint exn = None);
    Alcotest.(check bool) "no hint key on the wire" true
      (not (contains_seq (wire exn) "\"hint\":"))
  in
  no_hint (Interp_error (User_error ("boom", "denied")));
  no_hint (Interp_error Budget_exceeded);
  no_hint (Interp_error (Prim_error ("tree/get", "failed: no DB", "db")));
  no_hint (Pricklypear_lisp.Reader.Reader_error ("unbalanced paren", 3, 7));
  no_hint (Failure "disk gone")

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

(* ---- bumpers notices codec (borge/bumpers.borg notices-envelope, M1).

   The additive "notices" field on the /api/eval envelope: shared shape
   in pure (wire_codec.ml) so the image, proxy rims, lens, and pp-eval
   serialize/deserialize ONE shape. These tests pin the dict shape
   {kind,name?,effects?,text}, the additive-only rule (a full notice
   serializes with exactly those keys; a bare one omits the optional
   keys rather than emitting nulls), the empty-array default for a
   response with no notices, and the round-trip identity pp-eval uses
   to render a notice a peer sent it. *)

let assoc_str (fields : (string * Yojson.Safe.t) list) (k : string) : string =
  match List.assoc_opt k fields with
  | Some (`String s) -> s
  | _ -> ""

let test_notice_full_json () =
  let n =
    Wire_codec.
      { kind = "define"; name = Some "add"; effects = Some "pure"; text = "hi" }
  in
  match Wire_codec.notice_to_json n with
  | `Assoc fields ->
      (* additive-only: exactly the four keys, no nulls, ok/value untouched *)
      let keys = List.map fst fields in
      Alcotest.(check (list string)) "keys in spec order"
        [ "kind"; "name"; "effects"; "text" ] keys;
      Alcotest.(check string) "kind" "define" (assoc_str fields "kind");
      Alcotest.(check string) "name" "add" (assoc_str fields "name");
      Alcotest.(check string) "effects" "pure" (assoc_str fields "effects");
      Alcotest.(check string) "text" "hi" (assoc_str fields "text")
  | _ -> Alcotest.fail "notice_to_json not an object"

let test_notice_minimal_json () =
  let n = Wire_codec.{ kind = "tip"; name = None; effects = None; text = "doc me" } in
  match Wire_codec.notice_to_json n with
  | `Assoc fields ->
      let keys = List.map fst fields in
      Alcotest.(check (list string)) "optional keys omitted (no nulls)"
        [ "kind"; "text" ] keys
  | _ -> Alcotest.fail "notice_to_json not an object"

let test_notices_to_json_empty () =
  Alcotest.(check string) "no notices -> []" "[]"
    (Yojson.Safe.to_string (Wire_codec.notices_to_json []))

let test_notices_to_json_two () =
  let ns =
    [ Wire_codec.{ kind = "define"; name = Some "a"; effects = None; text = "t1" }
    ; Wire_codec.{ kind = "tip"; name = None; effects = None; text = "t2" }
    ]
  in
  match Wire_codec.notices_to_json ns with
  | `List [ _; _ ] -> Alcotest.(check bool) "two notices" true true
  | _ -> Alcotest.fail "expected a 2-element array"

let test_notice_of_json_roundtrip () =
  let full =
    Wire_codec.
      {
        kind = "route";
        name = Some "/x";
        effects = Some "read-db";
        text = "p";
      }
  in
  let minimal = Wire_codec.{ kind = "warn"; name = None; effects = None; text = "w" } in
  let rt n =
    match Wire_codec.notice_of_json (Wire_codec.notice_to_json n) with
    | Some n' -> n' = n
    | None -> false
  in
  Alcotest.(check bool) "full round-trips" true (rt full);
  Alcotest.(check bool) "minimal round-trips" true (rt minimal)

let test_notice_of_json_malformed () =
  (* malformed: missing kind / missing text / non-object *)
  Alcotest.(check bool) "missing kind" true
    (Wire_codec.notice_of_json (`Assoc [ ("text", `String "x") ]) = None);
  Alcotest.(check bool) "missing text" true
    (Wire_codec.notice_of_json (`Assoc [ ("kind", `String "tip") ]) = None);
  Alcotest.(check bool) "non-object" true
    (Wire_codec.notice_of_json (`String "nope") = None)

let test_notice_to_line () =
  Alcotest.(check string) "full line" "define add [pure]: hi"
    (Wire_codec.notice_to_line
       Wire_codec.{ kind = "define"; name = Some "add"; effects = Some "pure"; text = "hi" });
  Alcotest.(check string) "minimal line" "tip: doc me"
    (Wire_codec.notice_to_line
       Wire_codec.{ kind = "tip"; name = None; effects = None; text = "doc me" })

let () =
  Alcotest.run "pricklypear-wire-codec"
    [ ( "notices"
      , [ ("notice-full-json", `Quick, test_notice_full_json)
        ; ("notice-minimal-json", `Quick, test_notice_minimal_json)
        ; ("notices-to-json-empty", `Quick, test_notices_to_json_empty)
        ; ("notices-to-json-two", `Quick, test_notices_to_json_two)
        ; ("notice-of-json-roundtrip", `Quick, test_notice_of_json_roundtrip)
        ; ("notice-of-json-malformed", `Quick, test_notice_of_json_malformed)
        ; ("notice-to-line", `Quick, test_notice_to_line)
        ] )
    ; ( "wire-codec"
      , [ ("unbound-roundtrip", `Quick, test_unbound)
        ; ("prim-error-roundtrip", `Quick, test_prim_error)
        ; ("error-hint-unbound", `Quick, test_error_hint_unbound)
        ; ("error-hint-prim-bad-args", `Quick, test_error_hint_prim_bad_args)
        ; ("error-hint-absent-unknown", `Quick, test_error_hint_absent_unknown)
        ; ("user-error-roundtrip", `Quick, test_user_error)
        ; ("budget-roundtrip", `Quick, test_budget)
        ; ("reader-error-roundtrip", `Quick, test_reader_error)
        ; ("parse-error-shape", `Quick, test_parse_error_shape)
        ; ("generic-exn", `Quick, test_generic_exn)
        ; ("garbage-json", `Quick, test_garbage_json)
        ; ("http-parse-headers", `Quick, test_http_parse_headers)
        ; ("http-parse-headers-url-value", `Quick, test_http_parse_headers_url_value)
        ] ) ]
