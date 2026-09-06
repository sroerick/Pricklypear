(** Pricklypear Image — Row codec + Http_kit leaf-helper tests (i-1788671505).

    Pins the one-implementation contracts that replaced the per-module
    copies: Row.cell_* (was duplicated in ai_store/auth_store/jobs/media),
    Http_kit.header_ci (was http_media + agent_discovery),
    Http_kit.pct_decode / query_param (was http_core.url_decode/query_param
    + http_code.pct_decode/query_param), and the single string-level HTML
    escaper (Prims_pure.html_escape_str; Http_code/crud copies were
    deletions). No DB / no live server. *)

open Pricklypear_image_lib

let row_of (cells : string option list) : Pgx.row =
  List.map
    (function Some s -> Pgx.Value.of_string s | None -> Pgx.Value.null)
    cells

let test_cell_string () =
  Alcotest.(check (option string) "text" (Some "abc")
              (Row.cell_string (row_of [ Some "abc" ]) 0));
  Alcotest.(check (option string) "null cell" None
              (Row.cell_string (row_of [ None ]) 0));
  Alcotest.(check (option string) "absent index" None
              (Row.cell_string (row_of []) 3));
  Alcotest.(check (option string) "empty text is text" (Some "")
              (Row.cell_string (row_of [ Some "" ]) 0))

let test_cell_bool () =
  let b cells = Row.cell_bool (row_of cells) 0 in
  List.iter
    (fun s -> Alcotest.(check bool) ("true spelling " ^ s) true (b [ Some s ]))
    [ "t"; "true"; "TRUE"; "1" ];
  List.iter
    (fun s -> Alcotest.(check bool) ("false spelling " ^ s) false (b [ Some s ]))
    [ "f"; "false"; "FALSE"; "0" ];
  Alcotest.(check bool "other text" false (b [ Some "yes" ]));
  Alcotest.(check bool "null" false (b [ None ]))

let test_cell_int () =
  Alcotest.(check (option int) "int" (Some 42)
              (Row.cell_int (row_of [ Some "42" ]) 0));
  Alcotest.(check (option int) "not an int" None
              (Row.cell_int (row_of [ Some "x" ]) 0));
  Alcotest.(check (option int) "null" None (Row.cell_int (row_of [ None ]) 0));
  Alcotest.(check (option Alcotest.int64) "int64" (Some 9000000000L)
              (Row.cell_int64 (row_of [ Some "9000000000" ]) 0));
  Alcotest.(check (option Alcotest.int64) "int64 junk" None
              (Row.cell_int64 (row_of [ Some "nope" ]) 0))

let test_cell_uuid_opt () =
  Alcotest.(check (option string) "uuid" (Some "abc-123")
              (Row.cell_uuid_opt (row_of [ Some "abc-123" ]) 0));
  (* empty text is treated as absent for uuid-ish columns *)
  Alcotest.(check (option string) "empty" None
              (Row.cell_uuid_opt (row_of [ Some "" ]) 0));
  Alcotest.(check (option string) "null" None
              (Row.cell_uuid_opt (row_of [ None ]) 0))

let test_header_ci () =
  let headers = [ ("Content-Type", "text/plain"); ("X-Filename", "a.txt") ] in
  Alcotest.(check (option string) "exact" (Some "text/plain")
              (Http_kit.header_ci headers "Content-Type"));
  Alcotest.(check (option string) "case-insensitive" (Some "a.txt")
              (Http_kit.header_ci headers "x-filename"));
  Alcotest.(check (option string) "missing" None
              (Http_kit.header_ci headers "x-nope"))

let test_pct_decode () =
  Alcotest.(check string) "plain" "a/b c" (Http_kit.pct_decode "a/b c");
  Alcotest.(check string) "hex" "A" (Http_kit.pct_decode "%41");
  Alcotest.(check string) "lower hex" "a/b" (Http_kit.pct_decode "a%2Fb");
  Alcotest.(check string) "utf-8 bytes" "\195\169" (Http_kit.pct_decode "%C3%A9");
  Alcotest.(check string) "plus is space" "a b" (Http_kit.pct_decode "a+b");
  (* malformed escapes are preserved verbatim, never silently eaten *)
  Alcotest.(check string) "bad hex kept" "%zz" (Http_kit.pct_decode "%zz");
  Alcotest.(check string) "truncated kept" "%4" (Http_kit.pct_decode "%4");
  Alcotest.(check string) "lone pct kept" "100%_test" (Http_kit.pct_decode "100%_test")

let test_query_param () =
  Alcotest.(check (option string) "basic" (Some "foo.pp")
              (Http_kit.query_param "/x?f=foo.pp" "f"));
  Alcotest.(check (option string) "no query" None
              (Http_kit.query_param "/x" "f"));
  Alcotest.(check (option string) "second key" (Some "10")
              (Http_kit.query_param "/x?limit=5&offset=10" "offset"));
  Alcotest.(check (option string) "first occurrence wins" (Some "1")
              (Http_kit.query_param "/x?a=1&a=2" "a"));
  Alcotest.(check (option string) "plus in value" (Some "a b")
              (Http_kit.query_param "/x?q=a+b" "q"));
  Alcotest.(check (option string) "decoded key" (Some "c")
              (Http_kit.query_param "/x?a%20b=c" "a b"));
  Alcotest.(check (option string) "value with =" (Some "b=c")
              (Http_kit.query_param "/x?a=b=c" "a"));
  (* bare flag pins the predecessors' shared behavior: name matches with "" *)
  Alcotest.(check (option string) "bare flag" (Some "")
              (Http_kit.query_param "/x?all" "all"));
  Alcotest.(check (option string) "empty value" (Some "")
              (Http_kit.query_param "/x?a=" "a"));
  Alcotest.(check (option string) "missing key" None
              (Http_kit.query_param "/x?a=1" "b"))

let meta_t : Auth_store.llm_key_meta Alcotest.testable =
  let pp fmt (m : Auth_store.llm_key_meta) =
    Format.fprintf fmt "(last4=%s label=%s base=%s model=%s)" m.last4
      (match m.label with Some l -> l | None -> "-")
      (match m.base with Some b -> b | None -> "-")
      (match m.model with Some s -> s | None -> "-")
  in
  let eq (a : Auth_store.llm_key_meta) (b : Auth_store.llm_key_meta) =
    a.last4 = b.last4 && a.label = b.label && a.base = b.base && a.model = b.model
  in
  Alcotest.testable pp eq

(* Pins the ONE llm_key credential parser (was duplicated ~79 lines between
   get_llm_key_meta_direct and get_llm_key_secret_direct). *)
let test_llm_key_parsing () =
  let m last4 label base model =
    { Auth_store.last4; label; base; model }
  in
  (* full public blob *)
  Alcotest.check meta_t "full blob"
    (m "1234" (Some "work") (Some "https://x") (Some "gpt-9"))
    (match
       Auth_store.llm_key_meta_of_public
         (Some {| {"last4":"1234","label":"work","base":"https://x","model":"gpt-9"} |})
     with Some m -> m | None -> failwith "expected Some");
  (* missing last4 falls back to the caller's default; present fields pass through *)
  Alcotest.check meta_t "default last4"
    (m "????" (Some "l") None None)
    (match Auth_store.llm_key_meta_of_public (Some {| {"label":"l"} |}) with
     | Some m -> m | None -> failwith "expected Some");
  (* empty optional strings are None, not Some "" *)
  Alcotest.check meta_t "empty optionals"
    (m "9999" None None None)
    (match
       Auth_store.llm_key_meta_of_public
         (Some {| {"last4":"9999","label":"","base":"","model":""} |})
     with Some m -> m | None -> failwith "expected Some");
  (* absent / garbage / non-object blobs yield None *)
  Alcotest.(check bool) "absent blob" false
    (match Auth_store.llm_key_meta_of_public None with Some _ -> true | None -> false);
  Alcotest.(check bool) "garbage blob" false
    (match Auth_store.llm_key_meta_of_public (Some "not json{") with
     | Some _ -> true | None -> false);
  Alcotest.(check bool) "non-object blob" false
    (match Auth_store.llm_key_meta_of_public (Some "[1,2]") with
     | Some _ -> true | None -> false);
  (* secret blob: api_key extraction incl. the empty-key rejection *)
  Alcotest.(check (option string)) "secret key" (Some "sk-live-abcd")
    (Auth_store.llm_key_of_secret {| {"api_key":"sk-live-abcd"} |});
  Alcotest.(check bool) "empty secret" false
    (match Auth_store.llm_key_of_secret {| {"api_key":""} |} with
     | Some _ -> true | None -> false);
  Alcotest.(check bool) "garbage secret" false
    (match Auth_store.llm_key_of_secret "{{{" with Some _ -> true | None -> false);
  (* default record derives last4 from the key itself *)
  Alcotest.check meta_t "default meta"
    (m "abcd" None None None)
    (Auth_store.default_llm_key_meta "sk-live-abcd")

let test_html_escape_single_source () =
  (* the escaped set is amp, lt, gt, quot, apos — the one string-level escaper is
     Prims_pure.html_escape_str; Http_code.html_escape / Crud's renderer /
     Http_core's decoders all delegate to the shared implementations now *)
  Alcotest.(check string) "full set" "&amp;&lt;&gt;&quot;&#39;"
    (Prims_pure.html_escape_str "&<>\"'");
  Alcotest.(check string) "passthrough" "plain text 123"
    (Prims_pure.html_escape_str "plain text 123");
  Alcotest.(check string) "Http_code delegates" "&lt;tag&gt;"
    (Http_code.html_escape "<tag>")

let () =
  let open Alcotest in
  run "row_kit"
    [ ( "row"
      , [ test_case "cell_string" `Quick test_cell_string
        ; test_case "cell_bool" `Quick test_cell_bool
        ; test_case "cell_int" `Quick test_cell_int
        ; test_case "cell_uuid_opt" `Quick test_cell_uuid_opt ] )
    ; ( "http_kit"
      , [ test_case "header_ci" `Quick test_header_ci
        ; test_case "pct_decode" `Quick test_pct_decode
        ; test_case "query_param" `Quick test_query_param ] )
    ; ( "escaper"
      , [ test_case "single_source" `Quick test_html_escape_single_source ] )
    ; ( "llm_key"
      , [ test_case "parsing" `Quick test_llm_key_parsing ] ) ]
