(* Pure Nopales primitives (no DB, no Session.t) — composition root.

   Registered by [Primitives.make_with_defaults]. The implementations
   live in per-topic modules split out of this file (i-1788671384):

     prims_common  — value layer: numeric + string coercion policies,
                     the ONE list-unwrapping policy, reflection prims
     prims_num     — arithmetic, comparison, math tower, parsing, RNG,
                     uuid, FSRS
     prims_list    — list/pair ops, equality, alist lookup
     prims_string  — string ops (all operands through coerce_str),
                     percent-encoding, date parsing
     prims_json    — JSON <-> value codecs, dict-* prims, read-string
     prims_html    — html render/escape/decode, wiki+bible rendering,
                     prose tokens, xml-parse, hx helpers
     prims_mime    — part/multipart (multipart.mime-framing)
     wire_codec    — shared /api/eval error codec + http header parser

   This module INCLUDES them all so the historical [Prims_pure.*]
   surface is unchanged for every host (image/lib/primitives.ml's
   registration table, client/bin, agent/lib/runtime, remote/lib —
   see prims_soup for the soup/* half). *)

include Prims_common
include Prims_num
include Prims_list
include Prims_string
include Prims_json
include Prims_html
include Prims_mime
include Wire_codec

(* ------------------------------------------------------------------
   THE pure-prim catalog (i-1788670799-f).

   ONE hand-enumerated (name, fn) list — the frozen pure surface —
   consumed by every host that registers pure prims:

     image/lib/primitives.ml  Primitives.make_with_defaults (image-only
                              extras layer on top: wall-clock, sha1, the
                              Mirage_crypto random-uuid shadow)
     client/lib/pp_client_table.ml  the browser boot table (the catalog
                              IS the browser's capability boundary: no
                              DB / IO / net / wall-clock prim here)
     agent/lib/runtime.ml     Runtime.register_pure_prims (via add_prim:
                              table + VPrim env binding)
     remote/lib/remote.ml     Remote.register_pure_prims (via add_prim,
                              which also tracks the placer's local
                              pure-prim list)

   The four per-host hand copies this replaces had already diverged:
   the browser client was missing sort / string-join / string-split /
   prose-tokens / render-tokens / err/shape / hx-get / hx-post (so any
   browser eval reaching libs/std failed with unbound var on
   string-split), the agent lacked length and the whole math tower,
   and agent+remote each registered string-split twice.

   Host-specific extras re-register AFTER [install] — Hashtbl.replace
   semantics, last writer wins. *)

let catalog :
  (string * (Pricklypear_lisp.Ast.value list -> Pricklypear_lisp.Ast.value))
    list =
  [ ("+", prim_add); ("-", prim_sub); ("*", prim_mul)
  ; ("div", prim_div); ("/", prim_div)  (* Scheme-style alias *)
  ; ("mod", prim_mod); ("quotient", prim_quotient)
  ; ("remainder", prim_remainder); ("abs", prim_abs)
  ; ("min", prim_min); ("max", prim_max)
  ; ("expt", prim_expt); ("sqrt", prim_sqrt); ("exp", prim_exp)
  ; ("log", prim_log); ("sin", prim_sin); ("cos", prim_cos)
  ; ("tan", prim_tan); ("asin", prim_asin); ("acos", prim_acos)
  ; ("atan", prim_atan)
  ; ("number?", prim_number_p); ("integer?", prim_integer_p)
  ; ("zero?", prim_zero_p); ("positive?", prim_positive_p)
  ; ("negative?", prim_negative_p); ("even?", prim_even_p)
  ; ("odd?", prim_odd_p); ("random", prim_random)
  ; ("random-seed", prim_random_seed)
  ; ("=", prim_num_eq); ("<", prim_lt); (">", prim_gt)
  ; ("<=", prim_le); (">=", prim_ge); ("not", prim_not)
  ; ("list", prim_list); ("car", prim_car); ("cdr", prim_cdr)
  ; ("cons", prim_cons); ("null?", prim_null_p); ("pair?", prim_pair_p)
  ; ("string-append", prim_string_append)
  ; ("string-join", prim_string_join)
  ; ("string-length", prim_string_length)
  ; ("substring", prim_substring)
  ; ("string-contains?", prim_string_contains)
  ; ("string-split", prim_string_split)
  ; ("prose-tokens", prim_prose_tokens)
  ; ("render-tokens", prim_render_tokens)
  ; ("string-eq", prim_string_eq)
  ; ("string=?", prim_string_eq)  (* Scheme-style alias *)
  ; ("string<?", prim_string_lt); ("string<=?", prim_string_le)
  ; ("string>?", prim_string_gt); ("string>=?", prim_string_ge)
  ; ("string-trim", prim_string_trim)
  ; ("html-escape", prim_html_escape)
  ; ("json-parse", prim_json_parse); ("json-emit", prim_json_emit)
  ; ("read-string", prim_read_string)
  ; ("read-all-string", prim_read_all_string)
  ; ("type-of", prim_type_of); ("symbol?", prim_symbol_p)
  ; ("string?", prim_string_p); ("symbol=?", prim_symbol_eq)
  ; ("symbol->string", prim_symbol_to_string)
  ; ("string->symbol", prim_string_to_symbol)
  ; ("str", prim_str); ("as-str", prim_as_str)
  ; ("append", prim_append); ("reverse", prim_reverse)
  ; ("sort", prim_sort); ("nth", prim_nth)
  ; ("hx-get", prim_hx_get); ("hx-post", prim_hx_post)
  ; ("err/shape", prim_err_shape)
  ; ("count", prim_count); ("length", prim_length)
  ; ("equal?", prim_equal_p)
  ; ("cadr", prim_cadr); ("caar", prim_caar); ("caddr", prim_caddr)
  ; ("cddr", prim_cddr); ("assoc", prim_assoc); ("member", prim_member)
  ; ("dict-get", prim_dict_get); ("dict-set", prim_dict_set)
  ; ("parse-int", prim_parse_int); ("parse-float", prim_parse_float)
  ; ("floor", prim_floor); ("round", prim_round)
  ; ("fsrs-schedule", prim_fsrs_schedule)
  ; ("dict-keys", prim_dict_keys)
  ; ("render-html", prim_render_html)
  ; ("bytes-response", Prims_html.prim_bytes_response)
  ; ("status-response", prim_status_response)
  ; ("part", prim_part); ("json-part", prim_json_part)
  ; ("multipart", prim_multipart)
  ; ("extract-links", prim_extract_links)
  ; ("render-links", prim_render_links)
  ; ("budget-remaining", prim_budget_remaining)
  ; ("string-replace", prim_string_replace)
  ; ("string-downcase", prim_string_downcase)
  ; ("string-upcase", prim_string_upcase)
  ; ("string-starts-with?", prim_string_starts_with_p)
  ; ("string-ends-with?", prim_string_ends_with_p)
  ; ("url-encode", prim_url_encode); ("url-decode", prim_url_decode)
  ; ("html-decode", prim_html_decode); ("parse-date", prim_parse_date)
  ; ("xml-parse", prim_xml_parse)
  ; ("random-uuid", prim_random_uuid)
  ]

(** Install the whole catalog into [tbl]. *)
let install
    (tbl : (string, Pricklypear_lisp.Ast.value list ->
                        Pricklypear_lisp.Ast.value) Hashtbl.t) : unit =
  List.iter (fun (name, fn) -> Hashtbl.replace tbl name fn) catalog

(** Sorted catalog names — the parity tripwire: every host table must
    cover these (image / client / agent / remote parity tests). *)
let catalog_names : string list =
  List.sort_uniq String.compare (List.map fst catalog)
