(** Http_kit — leaf HTTP text helpers shared by the http_* modules.

    One implementation each of case-insensitive header lookup, percent
    decoding and target-query parsing; previously duplicated across
    http_core / http_code / http_media / agent_discovery (i-1788671505).
    No dependencies beyond the stdlib — safe to link anywhere. *)

let hex_nibble (c : char) : int option =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

(* Strict percent-decoding: [+]->space, %XX -> byte when both hex digits are
   valid; malformed escapes are preserved verbatim (never silently drop
   characters — a filename like "100%_test" must survive a decode round).
   This is the semantics the /code faces need for path segments, and the
   lenient variant it replaces (http_core.url_decode, which ate the two hex
   chars of a malformed escape) only ever differed on malformed input. *)
let pct_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '%' && i + 2 < len then (
      match hex_nibble s.[i + 1], hex_nibble s.[i + 2] with
      | Some a, Some b ->
          Buffer.add_char buf (Char.chr ((a lsl 4) lor b));
          loop (i + 3)
      | _ ->
          Buffer.add_char buf s.[i];
          loop (i + 1))
    else if s.[i] = '+' then (
      Buffer.add_char buf ' ';
      loop (i + 1))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

(* First value of [key] in a request target's query component. Keys are
   decoded like values; a bare flag with no '=' (e.g. "?all") matches its
   own name with the empty string as value (both predecessors did this).
   First occurrence wins on repeated keys. *)
let query_param (target : string) (key : string) : string option =
  match String.index_opt target '?' with
  | None -> None
  | Some i ->
      let q = String.sub target (i + 1) (String.length target - i - 1) in
      let pairs = String.split_on_char '&' q in
      List.find_map
        (fun pair ->
          match String.split_on_char '=' pair with
          | k :: rest when pct_decode k = key ->
              let v = String.concat "=" rest in
              Some (pct_decode v)
          | _ -> None)
        pairs

(* Case-insensitive header lookup over a name/value list. *)
let header_ci (headers : (string * string) list) (name : string) : string option
    =
  let lower = String.lowercase_ascii name in
  List.find_map
    (fun (k, v) -> if String.lowercase_ascii k = lower then Some v else None)
    headers
