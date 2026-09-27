(* JSON <-> value codecs and the dict-* prims over VStr JSON, plus the
   read-string/read-all-string reader bridge. dict-* operands go
   through Prims_common.coerce_str (i-1788671384). Split from
   prims_pure.ml. *)
open Pricklypear_lisp.Ast
open Prims_common


(* ---- JSON <-> value codecs + dict primitives ---------------------

   json-values-alist ordering: is_alist / json_to_value /
   value_to_json (the codecs) come first; alist_of + the dict prims
   consume them. Section header retained for the iteration-14 trail
   below.

   Iteration-14 decision: don't introduce a new VDict value type yet
   (that would ripple through ast.ml / reader.ml / eval.ml / value
   JSON round-trip). Instead, (get 'type id) returns a VStr holding
   the fields JSON; these prims operate on VStr JSON directly so Lisp
   code can do:
       (define u (get 'user id))
       (dict-get u "name")
       (dict-set u "active" "false")
   The primitive returns VStr JSON (or VNil if the input wasn't a valid
   object, or key wasn't found). The underlying parse-and-emit is
   Yojson; the canonical round-trip preserves well-formed objects.

   2026-12 SUPERSEDED IN PART (lisp-interpreter.json-values-alist):
   json_to_value's `Assoc case re-serializing objects to VStr was
   self-defeating — parse in OCaml, re-emit the same JSON as a string,
   then every dict-get re-finds the memo and re-stringifies the leaf.
   The boundary tax (parse → re-serialize → re-extract, per node per
   field) pushed the notes outline fold to ~380k ticks and the live
   read path into the jobs runner. Objects now decode to ALISTS (VList
   of VPair (VStr key, value)) — still no VDict; assoc is the idiomatic
   accessor and the dict-* prims are polymorphic over alist | legacy
   VStr JSON, so every (dict-get (json-parse s) k) call site keeps
   working unmodified. Still true: no new value type; VStr stays text.
   Code that json-parses an OBJECT and then as-str's / re-parses a
   structured field must migrate to reading the value directly (the
   only such package was lookbook, fixed in the same pass). *)

(* ---- JSON parse memo (log/issues "dict-get re-parses per call") ----

   dict-get / dict-set / dict-keys / json-parse re-parse the SAME fields
   JSON per call in product folds — the todo nav ratio fold paid ~1.5k
   ticks per item and blew the 400k route budget at 50 lists. The parse
   result (Yojson.Safe.t) and the converted Lisp value are immutable, so
   a CONTENT-keyed cache is sound: same bytes -> same tree, no hash
   collision risk (full-string keys, structural equality). Bounds: 32
   entries, 1MiB per string, clear-on-overflow — no LRU machinery on a
   hot prim path; invalid JSON is not cached (re-parse per call, error
   path stays cold). Single-domain image (no Domain.spawn) and these
   functions never yield, so plain mutable state is fiber-atomic. *)

let memo_max_entries = 32
let memo_max_bytes = 1 lsl 20  (* 1MiB — big outlines skip the memo *)

let memo_json_tbl : (string, Yojson.Safe.t) Hashtbl.t = Hashtbl.create 32
let memo_val_tbl : (string, value) Hashtbl.t = Hashtbl.create 32
let memo_json_n = ref 0
let memo_val_n = ref 0

(* Bounded insert shared by both tables: clear-all on overflow (simple
   and predictable; a fold touches a handful of distinct strings). *)
let memo_put (tbl : ('k, 'v) Hashtbl.t) (n : int ref) ~(k : 'k) ~(v : 'v) =
  if !n >= memo_max_entries then begin
    Hashtbl.reset tbl;
    n := 0
  end;
  Hashtbl.replace tbl k v;
  incr n

(** Parse [s] once per distinct content (memoized). None on invalid JSON. *)
let parse_cached (s : string) : Yojson.Safe.t option =
  match Hashtbl.find_opt memo_json_tbl s with
  | Some j -> Some j
  | None ->
      (match Yojson.Safe.from_string s with
       | exception _ -> None
       | j ->
           if String.length s <= memo_max_bytes then
             memo_put memo_json_tbl memo_json_n ~k:s ~v:j;
           Some j)

(* (dict-get json-str key) -> VStr value | VNil (legacy VStr JSON path);
   on an alist operand the cdr value is returned as-is.

   json-values-alist emit/operand predicates (shared by json_to_value /
   value_to_json / the dict prims):

   [is_alist xs] — a VList ALL of whose elements are VPair (VStr _, _)
   is an alist and emits as a JSON object (the exact inverse of the
   decode in json_to_value). The ambiguity with a user list of
   string-keyed pairs is accepted by the spec — same answer either
   way. A bare VPair (dotted pair) keeps the LIST wire form (codec
   contract, unchanged). *)
let is_alist (xs : value list) : bool =
  match xs with
  | [] -> false
  | _ ->
      List.for_all
        (function VPair (VStr _, _) -> true | _ -> false)
        xs

let rec json_to_value (j : Yojson.Safe.t) : value =
  match j with
  | `Null -> VNil
  | `Bool b -> VBool b
  | `Int n -> VInt (Int64.of_int n)
  | `Intlit s -> (try VInt (Int64.of_string s) with _ -> VStr s)
  | `Float f -> VFloat f
  | `String s -> VStr s
  | `List xs -> VList (List.map json_to_value xs)
  | `Assoc kvs ->
      (* json-values-alist: an object decodes to an ALIST — VList of
         VPair (VStr key, value), recursive. Parsed data crosses the
         prim boundary WALKABLE: assoc is the accessor, quote/match see
         real structure, and no caller pays a re-parse. Edge accepted
         by the spec: {} and [] both decode to (VList []) — the shapes
         are indistinguishable without a new value type, and every
         dict-get/dict-keys answer is the empty either way. *)
      VList (List.map (fun (k, v) -> VPair (VStr k, json_to_value v)) kvs)

(* json-values-alist emit side: the guarded VList clause above turns
   alists back into objects. *)
let rec value_to_json (v : value) : Yojson.Safe.t =
  match v with
  | VNil -> `Null
  | VBool b -> `Bool b
  | VInt n -> `Intlit (Int64.to_string n)
  | VStr s ->
      (* Nested JSON object/array strings stay structured when emitted. *)
      let len = String.length s in
      if len >= 2
         && ((s.[0] = '{' && s.[len - 1] = '}')
             || (s.[0] = '[' && s.[len - 1] = ']'))
      then (try Yojson.Safe.from_string s with _ -> `String s)
      else `String s
  | VList xs when is_alist xs ->
      (* alist -> JSON object (json-values-alist); guarded so a plain
         list of pairs still emits as an array of arrays and dotted
         pairs keep the list wire form *)
      `Assoc
        (List.filter_map
           (fun p ->
             match p with
             | VPair (VStr k, v) -> Some (k, value_to_json v)
             | _ -> None)
           xs)
  | VList xs -> `List (List.map value_to_json xs)
  | VFloat f ->
      (* Real JSON float so it round-trips back to VFloat (not a VStr).
         Integral floats still come back as VFloat via [json_to_value]
         (Float f -> VFloat f), keeping the type distinct from VInt. *)
      `Float f
  | VPair (a, b) ->
      (* WIRE-FORM rule, not a list-policy site (i-1788671384): pairs
         travel as their JSON array (codec contract, lisp/test/codec_test.ml
         "VPair (dotted) via list wire form"). A proper chain unwinds
         element-wise; a cons-onto-list tail splices; a DOTTED (atom)
         tail is serialized as wire data by the recursion — value_to_json
         must not raise on any value. The list-op policy lives in
         list_of_value alone. *)
      let rec unwind acc = function
        | VPair (x, y) -> unwind (x :: acc) y
        | VNil -> List.rev acc
        | VList ys -> List.rev_append acc ys
        | tail -> List.rev (tail :: acc)
      in
      `List (List.map value_to_json (unwind [] (VPair (a, b))))
  | other -> `String (Pricklypear_lisp.Reader.pp_value other)

(* alist operand detection: returns the (key, value) pairs in
   insertion order when [v] is alist-shaped, else None (fall through
   to the legacy VStr JSON path). *)
let alist_of (v : value) : (string * value) list option =
  match v with
  | VList xs when is_alist xs ->
      Some
        (List.filter_map
           (fun p ->
             match p with
             | VPair (VStr k, v) -> Some (k, v)
             | _ -> None)
           xs)
  | _ -> None

let prim_dict_get args =
  match args with
  | [ json_v; key_v ] ->
      (* Shared string-coercion policy (i-1788671384): __str__ tags and
         symbols coerce like the string-append family. *)
      let key = coerce_str ~ctx:"dict-get" key_v in
      (match alist_of json_v with
       | Some kvs ->
           (* alist input (json-values-alist): assoc walk, the cdr
              returned AS-IS — nested objects arrive as alists, arrays
              as VLists, no re-parse at the call site *)
           (match List.assoc_opt key kvs with Some v -> v | None -> VNil)
       | None ->
           let json = coerce_str ~ctx:"dict-get" json_v in
           (match parse_cached json with
            | Some (`Assoc kvs) ->
                (match List.assoc_opt key kvs with
                 | Some (`String s) -> VStr s
                 | Some (`Int n) -> VStr (string_of_int n)
                 | Some (`Intlit s) -> VStr s
                 | Some (`Float f) -> VStr (string_of_float f)
                 | Some (`Bool b) -> VStr (string_of_bool b)
                 | Some (`Null) -> VNil
                 | Some (#Yojson.Safe.t as v) ->
                     VStr (Yojson.Safe.to_string v)
                 | None -> VNil)
            | _ -> VNil))
  | _ -> raise (Interp_error (User_error ("dict-get needs 2 strings", "bad-args")))

(* (dict-set json-str key value) -> VStr new-json.
   Builds a JSON object with [key] = [value] (a string; numbers
   detected and emitted as JSON numeric where parseable). Returns the
   updated JSON as a VStr. If [json-str] isn't a JSON object, starts
   a fresh {} and adds the new key. *)
let prim_dict_set args =
  match args with
  | [ json_v; key_v; value_v ] ->
      let key = coerce_str ~ctx:"dict-set" key_v in
      let value = coerce_str ~ctx:"dict-set" value_v in
      let v_json =
        (* Try parsing the value as JSON (so callers can pass nested
           objects/arrays); if it doesn't parse, emit as a JSON string.
           Shared by both shapes below. *)
        try Yojson.Safe.from_string value
        with _ -> `String value
      in
      (match alist_of json_v with
       | Some kvs ->
           (* alist input: cons/replace the pair, return the new alist
              (emits back as an object via value_to_json) *)
           VList
             (VPair (VStr key, json_to_value v_json)
              :: List.filter_map
                   (fun (k, v) ->
                     if k <> key then Some (VPair (VStr k, v)) else None)
                   kvs)
       | None ->
           let json = coerce_str ~ctx:"dict-set" json_v in
           let base =
             match parse_cached json with
             | Some (`Assoc kvs) -> kvs
             | _ -> []
           in
           let updated =
             (key, v_json) ::
             (List.filter (fun (k, _) -> k <> key) base)
           in
           VStr (Yojson.Safe.to_string (`Assoc updated)))
  | _ -> raise (Interp_error (User_error ("dict-set needs 3 strings", "bad-args")))

(* (dict-keys json-str) -> VList of VStr keys.
   Returns the keys of the JSON object at [json-str] (insertion order
   preserved by Yojson's `Assoc). Empty list if input isn't an object. *)
let prim_dict_keys args =
  match args with
  | [ json_v ] ->
      (match alist_of json_v with
       | Some kvs -> VList (List.map (fun (k, _) -> VStr k) kvs)
       | None ->
           let json = coerce_str ~ctx:"dict-keys" json_v in
           (match parse_cached json with
            | Some (`Assoc kvs) ->
                VList (List.map (fun (k, _) -> VStr k) kvs)
            | _ -> VList []))
  | _ -> raise (Interp_error (User_error ("dict-keys needs 1 string", "bad-args")))

(** (json-parse s) result memoized per distinct content: the converted
    Lisp value tree is immutable and deterministic for fixed bytes, and
    the conversion (not the parse) dominates for big outlines. *)
let json_parse_cached (s : string) : value option =
  match Hashtbl.find_opt memo_val_tbl s with
  | Some v -> Some v
  | None ->
      (match Yojson.Safe.from_string s with
       | exception _ -> None
       | j ->
           let v = json_to_value j in
           if String.length s <= memo_max_bytes then
             memo_put memo_val_tbl memo_val_n ~k:s ~v;
           Some v)

let prim_json_parse args =
  match args with
  | [ VStr s ] ->
      (match json_parse_cached s with
       | Some v -> v
       | None ->
           raise (Interp_error (User_error ("json-parse: invalid JSON", "bad-args"))))
  | _ -> raise (Interp_error (User_error ("json-parse needs 1 string", "bad-args")))

let prim_json_emit args =
  match args with
  | [ v ] -> VStr (Yojson.Safe.to_string (value_to_json v))
  | _ -> raise (Interp_error (User_error ("json-emit needs 1 value", "bad-args")))

(* (read-string s) -> quoted value tree. Same reader as load-string / the
   REPL, then quote_to_value — NO eval. Pair with (function-ast name):
   both yield walkable s-expr data. Clojure-style twin of load-string.
   (read-all-string s) -> VList of forms (multi top-level). *)
let prim_read_string args =
  match args with
  | [ VStr s ] ->
      (try
         let e = Pricklypear_lisp.Reader.read_one s in
         Pricklypear_lisp.Eval.quote_to_value e
       with
       | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
           raise
             (Interp_error
                (User_error
                   (Printf.sprintf "read-string: %s (%d:%d)" msg line col, "bad-args")))
       | exn ->
           raise
             (Interp_error
                (User_error
                   ("read-string: " ^ Printexc.to_string exn, "bad-args"))))
  | _ -> raise (Interp_error (User_error ("read-string needs 1 string", "bad-args")))

let prim_read_all_string args =
  match args with
  | [ VStr s ] ->
      (try
         let es = Pricklypear_lisp.Reader.read_all s in
         VList (List.map Pricklypear_lisp.Eval.quote_to_value es)
       with
       | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
           raise
             (Interp_error
                (User_error
                   (Printf.sprintf "read-all-string: %s (%d:%d)" msg line col, "bad-args")))
       | exn ->
           raise
             (Interp_error
                (User_error
                   ("read-all-string: " ^ Printexc.to_string exn, "bad-args"))))
  | _ -> raise (Interp_error (User_error ("read-all-string needs 1 string", "bad-args")))
