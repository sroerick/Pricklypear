(** Pricklypear browser evaluator runtime.

    A js_of_ocaml build of the pure Nopales interpreter (lisp/lib) plus
    the shared pure-prims table (pure/prims_pure.ml). The same .pp
    package source the server serves is evaluated verbatim here — see
    borge/structural-editing.borg client-eval.

    Built with `js_of_ocaml --effects=cps` (OCaml 5 effects require the
    CPS backend; the default backend fatally aborts). Exposes a small
    JSON-bound surface to JavaScript via [Js.export]:

      boot()                 -> initializes the prim table + env
      load_source(string)    -> evaluates a multi-form .pp source string
      apply_op(string, json) -> runs a pure op fn over a JSON tree,
                                returns the result tree as JSON
      eval(string)           -> evaluates one Nopales expression,
                                returns its printed value (for REPL/debug)

    The pure prim table EXCLUDES every DB / IO / net prim — the
    capability boundary IS the prim table. Wall-clock prims (now,
    date-today, date-weekday, etc.) are absent too: they live in
    image-only prims_time.ml because they read Unix. *)
open Pricklypear_lisp

(* --- the runtime state, initialized by boot --- *)
let prims : Eval.prim_table option ref = ref None
let env : Ast.env option ref = ref None

let boot () =
  let t = Eval.make_prim_table () in
  (* Register every pure prim by name. Prims_pure is the shared Unix-free
     library; it has no DB/IO/net/wall-clock prims. *)
  let register name fn = Eval.register_prim t name fn in
  register "+" Prims_pure.prim_add;
  register "-" Prims_pure.prim_sub;
  register "*" Prims_pure.prim_mul;
  register "div" Prims_pure.prim_div;
  register "/" Prims_pure.prim_div;
  register "mod" Prims_pure.prim_mod;
  register "quotient" Prims_pure.prim_quotient;
  register "remainder" Prims_pure.prim_remainder;
  register "abs" Prims_pure.prim_abs;
  register "min" Prims_pure.prim_min;
  register "max" Prims_pure.prim_max;
  register "expt" Prims_pure.prim_expt;
  register "sqrt" Prims_pure.prim_sqrt;
  register "exp" Prims_pure.prim_exp;
  register "log" Prims_pure.prim_log;
  register "sin" Prims_pure.prim_sin;
  register "cos" Prims_pure.prim_cos;
  register "tan" Prims_pure.prim_tan;
  register "asin" Prims_pure.prim_asin;
  register "acos" Prims_pure.prim_acos;
  register "atan" Prims_pure.prim_atan;
  register "number?" Prims_pure.prim_number_p;
  register "integer?" Prims_pure.prim_integer_p;
  register "zero?" Prims_pure.prim_zero_p;
  register "positive?" Prims_pure.prim_positive_p;
  register "negative?" Prims_pure.prim_negative_p;
  register "even?" Prims_pure.prim_even_p;
  register "odd?" Prims_pure.prim_odd_p;
  register "random" Prims_pure.prim_random;
  register "random-seed" Prims_pure.prim_random_seed;
  register "=" Prims_pure.prim_num_eq;
  register "<" Prims_pure.prim_lt;
  register ">" Prims_pure.prim_gt;
  register "<=" Prims_pure.prim_le;
  register ">=" Prims_pure.prim_ge;
  register "not" Prims_pure.prim_not;
  register "list" Prims_pure.prim_list;
  register "car" Prims_pure.prim_car;
  register "cdr" Prims_pure.prim_cdr;
  register "cons" Prims_pure.prim_cons;
  register "null?" Prims_pure.prim_null_p;
  register "pair?" Prims_pure.prim_pair_p;
  register "string-append" Prims_pure.prim_string_append;
  register "string-length" Prims_pure.prim_string_length;
  register "substring" Prims_pure.prim_substring;
  register "string-contains?" Prims_pure.prim_string_contains;
  register "string-eq" Prims_pure.prim_string_eq;
  register "string<?" Prims_pure.prim_string_lt;
  register "string<=?" Prims_pure.prim_string_le;
  register "string>?" Prims_pure.prim_string_gt;
  register "string>=?" Prims_pure.prim_string_ge;
  register "string-trim" Prims_pure.prim_string_trim;
  register "html-escape" Prims_pure.prim_html_escape;
  register "json-parse" Prims_pure.prim_json_parse;
  register "json-emit" Prims_pure.prim_json_emit;
  register "read-string" Prims_pure.prim_read_string;
  register "read-all-string" Prims_pure.prim_read_all_string;
  register "type-of" Prims_pure.prim_type_of;
  register "symbol?" Prims_pure.prim_symbol_p;
  register "string?" Prims_pure.prim_string_p;
  register "symbol=?" Prims_pure.prim_symbol_eq;
  register "symbol->string" Prims_pure.prim_symbol_to_string;
  register "string->symbol" Prims_pure.prim_string_to_symbol;
  register "str" Prims_pure.prim_str;
  register "as-str" Prims_pure.prim_as_str;
  register "append" Prims_pure.prim_append;
  register "reverse" Prims_pure.prim_reverse;
  register "nth" Prims_pure.prim_nth;
  register "count" Prims_pure.prim_count;
  register "length" Prims_pure.prim_length;
  register "equal?" Prims_pure.prim_equal_p;
  register "cadr" Prims_pure.prim_cadr;
  register "caar" Prims_pure.prim_caar;
  register "caddr" Prims_pure.prim_caddr;
  register "cddr" Prims_pure.prim_cddr;
  register "assoc" Prims_pure.prim_assoc;
  register "member" Prims_pure.prim_member;
  register "dict-get" Prims_pure.prim_dict_get;
  register "dict-set" Prims_pure.prim_dict_set;
  register "dict-keys" Prims_pure.prim_dict_keys;
  register "parse-int" Prims_pure.prim_parse_int;
  register "parse-float" Prims_pure.prim_parse_float;
  register "floor" Prims_pure.prim_floor;
  register "round" Prims_pure.prim_round;
  register "fsrs-schedule" Prims_pure.prim_fsrs_schedule;
  register "render-html" Prims_pure.prim_render_html;
  register "status-response" Prims_pure.prim_status_response;
  register "part" Prims_pure.prim_part;
  register "json-part" Prims_pure.prim_json_part;
  register "multipart" Prims_pure.prim_multipart;
  register "extract-links" Prims_pure.prim_extract_links;
  register "render-links" Prims_pure.prim_render_links;
  register "budget-remaining" Prims_pure.prim_budget_remaining;
  register "string-replace" Prims_pure.prim_string_replace;
  register "string-downcase" Prims_pure.prim_string_downcase;
  register "string-upcase" Prims_pure.prim_string_upcase;
  register "string-starts-with?" Prims_pure.prim_string_starts_with_p;
  register "string-ends-with?" Prims_pure.prim_string_ends_with_p;
  register "url-encode" Prims_pure.prim_url_encode;
  register "url-decode" Prims_pure.prim_url_decode;
  register "html-decode" Prims_pure.prim_html_decode;
  register "parse-date" Prims_pure.prim_parse_date;
  register "xml-parse" Prims_pure.prim_xml_parse;
  register "random-uuid" Prims_pure.prim_random_uuid;
  (* Bind each prim name as VPrim in env, mirroring session.ml. *)
  let e = Env.create () in
  Hashtbl.iter
    (fun name _ -> Env.define e name (Ast.VPrim name))
    t;
  (* Route prim dispatch through the direct table, bypassing the
     Effect.Deep handler. Under js_of_ocaml's CPS backend the
     one-shot deep-match continuations resume twice on nested prim
     calls (Continuation_already_resumed); direct_prims avoids
     performing Prim_call effects entirely. Spec: structural-editing.client-eval. *)
  Pricklypear_lisp.Control.direct_prims := Some t;
  prims := Some t;
  env := Some e

let get_prims () =
  match !prims with Some t -> t | None -> failwith "pp: boot() not called"
let get_env () =
  match !env with Some e -> e | None -> failwith "pp: boot() not called"

(* Evaluate a multi-form .pp source string (load-library shape). Mirrors
   image/lib/loader.ml: read_all + eval each form under the effect
   handler. The reader handles ;; line comments natively. *)
let load_source (src : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t =
  let s = Js_of_ocaml.Js.to_string src in
  let t = get_prims () in
  let e = get_env () in
  (try
     Control.run_sync ~prims:t (fun () ->
         let forms = Reader.read_all s in
         List.iter (fun form ->
             let budget = Eval.make_budget () in
             ignore (Eval.eval ~budget ~prims:t e form))
           forms)
   with _ -> ());
  Js_of_ocaml.Js.string "ok"

(* Evaluate one expression, return its printed value (REPL/debug). *)
let eval (src : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t =
  let s = Js_of_ocaml.Js.to_string src in
  let t = get_prims () in
  let e = get_env () in
  try
    let v = Eval.eval_string ~prims:t e s in
    Js_of_ocaml.Js.string (Reader.pp_value v)
  with
  | Ast.Interp_error err ->
      Js_of_ocaml.Js.string
        ("ERROR: " ^ Ast.pp_interp_error err)
  | Reader.Reader_error (m, l, c) ->
      Js_of_ocaml.Js.string (Printf.sprintf "ERROR: %s (%d:%d)" m l c)
  | e -> Js_of_ocaml.Js.string ("ERROR: " ^ Printexc.to_string e)

(* set_tree(json): parse the outline tree JSON into a Nopales value and
   bind it as `pp-tree` in the env. json-parse of a JSON array yields a
   VList, matching the server's tree shape. Spec: structural-editing.
   client-eval OPTIMISTIC EXECUTION MODEL step 1. *)
let set_tree (json : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) : unit =
  let s = Js_of_ocaml.Js.to_string json in
  let t = get_prims () in
  let e = get_env () in
  let v = Eval.eval_string ~prims:t e ("(json-parse " ^ Printf.sprintf "%S" s ^ ")") in
  Env.define e "pp-tree" v

(* apply_op(op, nid, slug): run the optimistic preview in the browser —
   dispatch the pure op over pp-tree (same outline/structed fns the
   server runs) and render via outline/html-lite. Returns the preview
   HTML, or "" if the op is not previewable (set-*, eval, no-op). *)
let apply_op
    (op : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)
    (nid : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)
    (slug : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)
  : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t =
  let op_s = Js_of_ocaml.Js.to_string op in
  let nid_s = Js_of_ocaml.Js.to_string nid in
  let slug_s = Js_of_ocaml.Js.to_string slug in
  let t = get_prims () in
  let e = get_env () in
  let expr =
    Printf.sprintf "(pp/preview %S pp-tree %S %S)" slug_s op_s nid_s
  in
  try
    let v = Eval.eval_string ~prims:t e expr in
    (match v with
     | Ast.VStr s -> Js_of_ocaml.Js.string s
     | _ -> Js_of_ocaml.Js.string "")
  with _ -> Js_of_ocaml.Js.string ""

let () =
  let open Js_of_ocaml.Js in
  let obj =
    Unsafe.obj
      [| "boot", Unsafe.inject (wrap_callback boot)
       ; "load_source", Unsafe.inject (wrap_callback load_source)
       ; "eval", Unsafe.inject (wrap_callback eval)
       ; "set_tree", Unsafe.inject (wrap_callback set_tree)
       ; "apply_op", Unsafe.inject (wrap_callback apply_op)
      |]
  in
  export "pp" obj
