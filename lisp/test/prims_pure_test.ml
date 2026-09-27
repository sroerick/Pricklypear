(** Tests for pure/prims_pure.ml prim behaviors that bit us once:
    the html-decode named-entity table must emit valid UTF-8 (a
    Latin-1 nbsp/copy/reg/deg/plusmn row once injected invalid UTF-8
    into rendered HTML), and html-escape . html-decode must round-trip
    over the whole table. Also locks quotient/remainder arg policies.
    PURE tests — no Postgres, no HTTP, no net. *)
open Pricklypear_lisp

let is_valid_utf8 s =
  let n = String.length s in
  let rec loop i =
    if i >= n then true
    else
      let d = String.get_utf_8_uchar s i in
      Uchar.utf_decode_is_valid d && loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let decode_str s =
  match Prims_pure.prim_html_decode [ Ast.VStr s ] with
  | Ast.VStr out -> out
  | v -> Alcotest.failf "html-decode returned %s" (Reader.pp_value v)

let escape_str s =
  match Prims_pure.prim_html_escape [ Ast.VStr s ] with
  | Ast.VStr out -> out
  | v -> Alcotest.failf "html-escape returned %s" (Reader.pp_value v)

(* Every named entity decodes to bytes that are (a) the table's own
   replacement, (b) valid UTF-8, and (c) closed under the pair:
   escaping the decoded char then re-decoding is the identity, and
   decoding the escaped entity source ("&nbsp;" -> "&amp;nbsp;") is
   the identity too. *)
let entity_tests =
  List.map
    (fun (name, repl) ->
      ( "entity " ^ name
      , `Quick
      , fun () ->
          Alcotest.(check string)
            ("decode &" ^ name ^ ";") repl (decode_str ("&" ^ name ^ ";"));
          if not (is_valid_utf8 repl) then
            Alcotest.failf "&%s; decodes to invalid UTF-8 (%d bytes)" name
              (String.length repl);
          Alcotest.(check string)
            ("escape-decode round-trip " ^ name) repl (decode_str (escape_str repl));
          Alcotest.(check string)
            ("escaped-source round-trip " ^ name)
            ("&" ^ name ^ ";")
            (decode_str (escape_str ("&" ^ name ^ ";"))) ))
    Prims_pure.named_entities

(* The original bug: nbsp decoded to the single Latin-1 byte 0xF0 (an
   invalid UTF-8 lead byte). Lock the correct two-byte form U+00A0. *)
let nbsp_regression =
  ( "nbsp is U+00A0 (C2 A0), not lone 0xF0"
  , `Quick
  , fun () -> Alcotest.(check string) "nbsp bytes" "\194\160" (decode_str "&nbsp;") )

(* quotient / remainder: int paths, zero divisor, non-int and
   wrong-arity args all reject through the single catch-all. *)
let raises_interp_error msg f =
  match f () with
  | v ->
      Alcotest.failf "expected Interp_error (%s), got %s" msg (Reader.pp_value v)
  | exception Ast.Interp_error (Ast.User_error (m, _)) ->
      if m <> msg then Alcotest.failf "expected %s, got %s" msg m
  | exception e -> Alcotest.failf "expected Interp_error, got %s" (Printexc.to_string e)

let as_int what v =
  match v with
  | Ast.VInt n -> Int64.to_int n
  | v -> Alcotest.failf "%s: expected VInt, got %s" what (Reader.pp_value v)

let quotient_tests =
  [ ( "quotient int path"
    , `Quick
    , fun () ->
        Alcotest.(check int) "7/2" 3
          (as_int "quotient"
             (Prims_pure.prim_quotient [ Ast.VInt 7L; Ast.VInt 2L ])) )
  ; ( "remainder int path"
    , `Quick
    , fun () ->
        Alcotest.(check int) "7 rem 2" 1
          (as_int "remainder"
             (Prims_pure.prim_remainder [ Ast.VInt 7L; Ast.VInt 2L ])) )
  ; ( "quotient by zero"
    , `Quick
    , fun () ->
        raises_interp_error "quotient by zero" (fun () ->
            Prims_pure.prim_quotient [ Ast.VInt 1L; Ast.VInt 0L ]) )
  ; ( "remainder by zero"
    , `Quick
    , fun () ->
        raises_interp_error "remainder by zero" (fun () ->
            Prims_pure.prim_remainder [ Ast.VInt 1L; Ast.VInt 0L ]) )
  ; ( "quotient rejects floats"
    , `Quick
    , fun () ->
        raises_interp_error "quotient needs two int args" (fun () ->
            Prims_pure.prim_quotient [ Ast.VFloat 1.5; Ast.VInt 2L ]) )
  ; ( "quotient rejects wrong arity"
    , `Quick
    , fun () ->
        raises_interp_error "quotient needs two int args" (fun () ->
            Prims_pure.prim_quotient [ Ast.VInt 1L ]) )
  ; ( "remainder rejects floats"
    , `Quick
    , fun () ->
        raises_interp_error "remainder needs two int args" (fun () ->
            Prims_pure.prim_remainder [ Ast.VFloat 1.5; Ast.VInt 2L ]) )
  ; ( "remainder rejects wrong arity"
    , `Quick
    , fun () ->
        raises_interp_error "remainder needs two int args" (fun () ->
            Prims_pure.prim_remainder [ Ast.VInt 1L ]) ) ]

(* --- i-1788671384: the ONE improper-tail list policy ---
   All list unwrapping goes through Eval.value_to_list: VList / VNil /
   proper VPair chains flatten; an improper (atom) tail is an
   Interp_error — the same decision apply and the args evaluator make
   (and standard Scheme: (length '(1 . 2)) is an error). The former
   five divergent copies accepted dotted tails (length/append/reverse/
   string-join/json-emit) or rejected even proper chains
   (render-tokens); these tests lock the unified policy. *)
let dotted = Ast.VPair (Ast.VInt 1L, Ast.VInt 2L)
let proper_chain = Ast.VPair (Ast.VInt 1L, Ast.VPair (Ast.VInt 2L, Ast.VNil))
let as_list what v =
  match v with
  | Ast.VList xs -> List.map (Reader.pp_value) xs |> String.concat ","
  | v -> Alcotest.failf "%s: expected VList, got %s" what (Reader.pp_value v)

let list_policy_tests =
  [ ( "length proper cons chain"
    , `Quick
    , fun () ->
        Alcotest.(check int) "len" 2
          (as_int "length" (Prims_pure.prim_length [ proper_chain ])) )
  ; ( "length rejects dotted tail"
    , `Quick
    , fun () -> raises_interp_error "list op needs a list, got 2" (fun () ->
          Prims_pure.prim_length [ dotted ]) )
  ; ( "string-join rejects dotted tail"
    , `Quick
    , fun () -> raises_interp_error "list op needs a list, got \"b\"" (fun () ->
          Prims_pure.prim_string_join
            [ Ast.VPair (Ast.VStr "a", Ast.VStr "b") ]) )
  ; ( "append rejects dotted tail"
    , `Quick
    , fun () -> raises_interp_error "list op needs a list, got 2" (fun () ->
          Prims_pure.prim_append [ Ast.VList [ Ast.VInt 1L ]; Ast.VInt 2L ]) )
  ; ( "reverse rejects dotted tail"
    , `Quick
    , fun () -> raises_interp_error "list op needs a list, got 2" (fun () ->
          Prims_pure.prim_reverse [ dotted ]) )
  ; ( "json-emit dotted pair keeps list wire form"
    , `Quick
    , fun () ->
        (* wire contract (codec_test): pairs travel as their JSON array;
           value_to_json is the wire-form rule, not a list-policy site *)
        let j = Prims_pure.value_to_json dotted in
        Alcotest.(check string) "wire" "[1,2]"
          (Yojson.Safe.to_string j) )
  ; ( "render-tokens accepts proper cons chain"
    , `Quick
    , fun () ->
        let tag s = Ast.VList [ Ast.VStr "__str__"; Ast.VStr s ] in
        let out =
          Prims_pure.prim_render_tokens
            [ tag "x"; proper_chain; Ast.VNil ]
        in
        (match out with Ast.VStr _ -> () | v -> Alcotest.failf "got %s" (Reader.pp_value v)) )
  ; ( "render-tokens rejects dotted tail"
    , `Quick
    , fun () -> raises_interp_error "list op needs a list, got 2" (fun () ->
          Prims_pure.prim_render_tokens
            [ Ast.VStr "x"; dotted; Ast.VNil ]) )
  ; ( "count still rejects pairs (VList/VNil only)"
    , `Quick
    , fun () -> raises_interp_error "count needs a list" (fun () ->
          Prims_pure.prim_count [ proper_chain ]) )
  ]

(* --- i-1788671384: the ONE string-coercion policy ---
   string-replace and the dict-* prims used to demand bare VStr while
   string-append/trim/split coerced; __str__ macro tags and symbols
   now coerce everywhere (string-length/trim/downcase semantics). *)
let tag s = Ast.VList [ Ast.VStr "__str__"; Ast.VStr s ]

let coercion_tests =
  [ ( "string-replace coerces int"
    , `Quick
    , fun () ->
        Alcotest.(check string) "5->6" "6"
          (match Prims_pure.prim_string_replace
                 [ Ast.VInt 5L; Ast.VStr "5"; Ast.VStr "6" ] with
           | Ast.VStr s -> s
           | v -> Alcotest.failf "got %s" (Reader.pp_value v)) )
  ; ( "string-replace coerces __str__ tag"
    , `Quick
    , fun () ->
        Alcotest.(check string) "ab->xb" "xb"
          (match Prims_pure.prim_string_replace
                 [ tag "ab"; Ast.VStr "a"; Ast.VStr "x" ] with
           | Ast.VStr s -> s
           | v -> Alcotest.failf "got %s" (Reader.pp_value v)) )
  ; ( "string-replace rejects non-stringy"
    , `Quick
    , fun () -> raises_interp_error
          "string-replace needs a string (got (1 . 2))"
        (fun () ->
           Prims_pure.prim_string_replace
             [ dotted; Ast.VStr "a"; Ast.VStr "b" ]) )
  ; ( "dict-get coerces __str__ tag"
    , `Quick
    , fun () ->
        Alcotest.(check string) "name" "ada"
          (match Prims_pure.prim_dict_get
                 [ tag "{\"name\":\"ada\"}"; Ast.VStr "name" ] with
           | Ast.VStr s -> s
           | v -> Alcotest.failf "got %s" (Reader.pp_value v)) )
  ; ( "dict-set coerces symbol key"
    , `Quick
    , fun () ->
        Alcotest.(check string) "json" "{\"k\":\"v\"}"
          (match Prims_pure.prim_dict_set
                 [ Ast.VStr "{}"; Ast.VSymbol "k"; Ast.VStr "v" ] with
           | Ast.VStr s -> s
           | v -> Alcotest.failf "got %s" (Reader.pp_value v)) )
  ; ( "dict-keys coerces __str__ tag"
    , `Quick
    , fun () ->
        (* as_list prints via pp_value, so the key renders quoted *)
        Alcotest.(check string) "keys" "\"a\""
          (as_list "dict-keys"
             (Prims_pure.prim_dict_keys [ tag "{\"a\":1}" ])) )
  ; ( "dict-get rejects non-stringy"
    , `Quick
    , fun () -> raises_interp_error "dict-get needs a string (got (1 . 2))"
        (fun () -> Prims_pure.prim_dict_get [ dotted; Ast.VStr "k" ]) )
  ; ( "dict-set keeps arity error"
    , `Quick
    , fun () -> raises_interp_error "dict-set needs 3 strings" (fun () ->
          Prims_pure.prim_dict_set [ Ast.VStr "{}"; Ast.VStr "k" ]) )
  ]

(* --- the ONE pure-prim catalog (i-1788670799-f) ---------------------
   Every host table (image / browser client / agent / remote) is built
   from [Prims_pure.catalog] via [install]; the per-host parity tests
   assert coverage. This group locks the catalog contract itself. *)

let catalog_tests =
  [ ( "catalog has no duplicate names"
    , `Quick
    , fun () ->
        let names = List.map fst Prims_pure.catalog in
        Alcotest.(check int) "uniq = raw" (List.length names)
          (List.length (List.sort_uniq String.compare names)) )
  ; ( "install registers every catalog name"
    , `Quick
    , fun () ->
        let t = Hashtbl.create 32 in
        Prims_pure.install t;
        List.iter
          (fun name ->
             Alcotest.(check bool) ("registered: " ^ name) true
               (Hashtbl.mem t name))
          Prims_pure.catalog_names )
  ; ( "the 8 prims the client hand-copy had dropped are catalogued"
    , `Quick
    , fun () ->
        List.iter
          (fun name ->
             Alcotest.(check bool) ("catalogued: " ^ name) true
               (List.mem_assoc name Prims_pure.catalog))
          [ "sort"; "string-join"; "string-split"; "prose-tokens"
          ; "render-tokens"; "err/shape"; "hx-get"; "hx-post" ];
        Alcotest.(check bool) "length catalogued" true
          (List.mem_assoc "length" Prims_pure.catalog) )
  ; ( "alias / is div; alias string=? is string-eq"
    , `Quick
    , fun () ->
        let t = Hashtbl.create 32 in
        Prims_pure.install t;
        let call n args = Hashtbl.find t n args in
        Alcotest.(check bool) "/ aliases div"
          (call "/" [ Ast.VInt 7L; Ast.VInt 2L ]
           = call "div" [ Ast.VInt 7L; Ast.VInt 2L ])
          true;
        Alcotest.(check bool) "string=? aliases string-eq"
          (call "string=?" [ Ast.VStr "a"; Ast.VStr "a" ]
           = call "string-eq" [ Ast.VStr "a"; Ast.VStr "a" ])
          true )
  ]

(* ---- json-values-alist (lisp-interpreter.json-values-alist) -------
   Objects decode to ALISTS (VList of VPair (VStr k, v)); the dict
   prims are polymorphic over alist | legacy VStr JSON; json-emit is
   the exact inverse. The old behavior (Assoc re-serialized back to a
   VStr) is the regression being locked out. *)
let parse = Prims_pure.prim_json_parse
let emit = Prims_pure.prim_json_emit

let alist_kvs what v =
  match v with
  | Ast.VList xs when xs <> [] ->
      List.map
        (fun p ->
          match p with
          | Ast.VPair (Ast.VStr k, x) -> (k, x)
          | other ->
              Alcotest.failf "%s: non-alist element %s" what
                (Reader.pp_value other))
        xs
  | v -> Alcotest.failf "%s: expected non-empty alist, got %s" what (Reader.pp_value v)

let emit_str what v =
  match emit [ v ] with
  | Ast.VStr s -> s
  | v -> Alcotest.failf "%s: emit returned %s" what (Reader.pp_value v)

let json_alist_tests =
  [ ( "object decodes to alist, not VStr JSON"
    , `Quick
    , fun () ->
        let v = parse [ Ast.VStr "{\"id\":\"x1\",\"title\":\"Notes\"}" ] in
        let kvs = alist_kvs "object" v in
        Alcotest.(check string) "key order" "id" (fst (List.nth kvs 0));
        Alcotest.(check string) "id value" "x1"
          (match snd (List.nth kvs 0) with Ast.VStr s -> s | v -> Alcotest.failf "got %s" (Reader.pp_value v));
        Alcotest.(check string) "title value" "Notes"
          (match snd (List.nth kvs 1) with Ast.VStr s -> s | v -> Alcotest.failf "got %s" (Reader.pp_value v)) )
  ; ( "nested objects decode recursively to alists"
    , `Quick
    , fun () ->
        let v = parse [ Ast.VStr "{\"body\":{\"slug\":\"a\"}}" ] in
        let kvs = alist_kvs "outer" v in
        (match snd (List.nth kvs 0) with
         | Ast.VStr _ -> Alcotest.fail "nested object still a VStr — boundary tax regression"
         | Ast.VList _ -> ()
         | v -> Alcotest.failf "nested object decoded to %s" (Reader.pp_value v)) )
  ; ( "arrays decode to VList of values (unchanged)"
    , `Quick
    , fun () ->
        let v = parse [ Ast.VStr "[\"a\",{\"k\":1}]" ] in
        match v with
        | Ast.VList [ Ast.VStr "a"; second ] ->
            (match second with
             | Ast.VList [ Ast.VPair (Ast.VStr "k", Ast.VInt n) ] ->
                 Alcotest.(check int64) "k" 1L n
             | v -> Alcotest.failf "elem %s" (Reader.pp_value v))
        | v -> Alcotest.failf "array decoded to %s" (Reader.pp_value v))
  ; ( "emit of alist round-trips to the object"
    , `Quick
    , fun () ->
        let src = "{\"id\":\"x1\",\"nested\":{\"a\":true},\"arr\":[1,2]}" in
        Alcotest.(check string) "round-trip"
          "{\"id\":\"x1\",\"nested\":{\"a\":true},\"arr\":[1,2]}"
          (emit_str "parsed" (parse [ Ast.VStr src ])) )
  ; ( "emit of alist-shaped VList is an object"
    , `Quick
    , fun () ->
        let alist =
          Ast.VList
            [ Ast.VPair (Ast.VStr "a", Ast.VStr "1")
            ; Ast.VPair (Ast.VStr "b", Ast.VInt 2L) ]
        in
        Alcotest.(check string) "object" "{\"a\":\"1\",\"b\":2}" (emit_str "alist" alist) )
  ; ( "dotted pair keeps the list wire form"
    , `Quick
    , fun () ->
        Alcotest.(check string) "wire" "[1,2]"
          (Yojson.Safe.to_string (Prims_pure.value_to_json (Ast.VPair (Ast.VInt 1L, Ast.VInt 2L)))) )
  ; ( "dict-get on alist returns the cdr as-is"
    , `Quick
    , fun () ->
        let node = parse [ Ast.VStr "{\"title\":\"Register\",\"body\":{\"slug\":\"log\"}}" ] in
        (match Prims_pure.prim_dict_get [ node; Ast.VStr "title" ] with
         | Ast.VStr "Register" -> ()
         | v -> Alcotest.failf "title %s" (Reader.pp_value v));
        (match Prims_pure.prim_dict_get [ node; Ast.VStr "body" ] with
         | Ast.VStr _ -> Alcotest.fail "body should be an alist, not VStr JSON"
         | Ast.VList _ -> ()
         | v -> Alcotest.failf "body %s" (Reader.pp_value v));
        match Prims_pure.prim_dict_get [ node; Ast.VStr "missing" ] with
        | Ast.VNil -> ()
        | v -> Alcotest.failf "missing %s" (Reader.pp_value v) )
  ; ( "dict-get on legacy VStr JSON unchanged"
    , `Quick
    , fun () ->
        (match Prims_pure.prim_dict_get
                 [ Ast.VStr "{\"name\":\"ada\"}"; Ast.VStr "name" ] with
         | Ast.VStr "ada" -> ()
         | v -> Alcotest.failf "legacy %s" (Reader.pp_value v)) )
  ; ( "dict-keys on alist returns the keys"
    , `Quick
    , fun () ->
        let node = parse [ Ast.VStr "{\"a\":1,\"b\":2}" ] in
        (match Prims_pure.prim_dict_keys [ node ] with
         | Ast.VList [ Ast.VStr "a"; Ast.VStr "b" ] -> ()
         | v -> Alcotest.failf "keys %s" (Reader.pp_value v)) )
  ; ( "dict-set on alist conses/replaces and stays an alist"
    , `Quick
    , fun () ->
        let node = parse [ Ast.VStr "{\"a\":\"1\"}" ] in
        let out = Prims_pure.prim_dict_set [ node; Ast.VStr "b"; Ast.VStr "2" ] in
        (match out with
         | Ast.VList [ Ast.VPair (Ast.VStr "b", Ast.VInt 2L); _ ] -> ()
         | v -> Alcotest.failf "set %s" (Reader.pp_value v));
        (* emit gives the object back — "2" rode as a JSON number, the
           legacy dict-set value policy (numbers detected, emitted numeric) *)
        Alcotest.(check string) "emit" "{\"b\":2,\"a\":\"1\"}" (emit_str "set" out) )
  ; ( "assoc is the idiomatic accessor on parsed nodes"
    , `Quick
    , fun () ->
        let node = parse [ Ast.VStr "{\"slug\":\"log/issues\"}" ] in
        match node with
        | Ast.VList [ Ast.VPair (Ast.VStr "slug", Ast.VStr "log/issues") ] -> ()
        | v -> Alcotest.failf "assoc shape %s" (Reader.pp_value v) )
  ; ( "empty object decodes to empty list (accepted edge)"
    , `Quick
    , fun () ->
        match parse [ Ast.VStr "{}" ] with
        | Ast.VList [] -> ()
        | v -> Alcotest.failf "{} -> %s" (Reader.pp_value v))
  ; ( "json_body: VStr passes through unchanged (legacy JSON or plain string)"
    , `Quick
    , fun () ->
        Alcotest.(check string) "json str" "{\"a\":\"b\"}"
          (Prims_pure.json_body (Ast.VStr "{\"a\":\"b\"}"));
        Alcotest.(check string) "plain str" "not json at all"
          (Prims_pure.json_body (Ast.VStr "not json at all")) )
  ; ( "json_body: alist (dict-set* output) emits as a JSON object"
    , `Quick
    , fun () ->
        (* (json-parse "{\"a\":\"b\"}") then dict-set "c" "d" — the
           documented mutation idiom's body. This is exactly the value
           that used to reach tree/upsert and raise bad-args. *)
        let parsed = parse [ Ast.VStr "{\"a\":\"b\"}" ] in
        let mutated = Prims_pure.prim_dict_set [ parsed; Ast.VStr "c"; Ast.VStr "d" ] in
        let out = Prims_pure.json_body mutated in
        (match Yojson.Safe.from_string out with
         | `Assoc kvs ->
             Alcotest.(check (option string)) "kept a" (Some "b")
               (List.assoc_opt "a" kvs
                |> Option.map (function `String s -> s | _ -> "<non-string>"));
             Alcotest.(check (option string)) "added c" (Some "d")
               (List.assoc_opt "c" kvs
                |> Option.map (function `String s -> s | _ -> "<non-string>"))
         | other -> Alcotest.failf "expected object, got %s" (Yojson.Safe.to_string other)) )
  ; ( "json_body: a hand-built VList emits as a JSON array"
    , `Quick
    , fun () ->
        Alcotest.(check string) "array" "[\"x\",\"y\"]"
          (Prims_pure.json_body (Ast.VList [ Ast.VStr "x"; Ast.VStr "y" ])) )
  ; ( "json_body: nested alist round-trips through the body boundary"
    , `Quick
    , fun () ->
        let v = parse [ Ast.VStr "{\"outer\":{\"inner\":[1,2]}}" ] in
        let out = Prims_pure.json_body v in
        Alcotest.(check string) "canonical" "{\"outer\":{\"inner\":[1,2]}}" out )
  ]

let () =
  Alcotest.run "prims_pure"
    [ ("entities", entity_tests @ [ nbsp_regression ])
    ; ("quotient-remainder", quotient_tests)
    ; ("list-policy", list_policy_tests)
    ; ("coercion-policy", coercion_tests)
    ; ("json-alist", json_alist_tests)
    ; ("catalog", catalog_tests) ]
