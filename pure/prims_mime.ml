(* Multipart/part primitives (multipart.mime-framing): the __pp_part__/
   __pp_multipart__ value shapes, kwargs parsing, part/multipart
   constructors and decoders. Split from prims_pure.ml (i-1788671384). *)
open Pricklypear_lisp.Ast


(* ------------------------------------------------------------------
   multipart/mixed values (multipart.mime-framing)

   Structured as VLists with symbolic tags (not string sentinels) so
   parts keep headers as data:
     part  = ( __pp_part__  body content-type target-or-() swap-or-() id-or-() )
     multi = ( __pp_multipart__  env-target env-swap  part… )
   ------------------------------------------------------------------ *)

let part_tag = "__pp_part__"
let multipart_tag = "__pp_multipart__"

type mime_part =
  { body : string
  ; content_type : string
  ; target : string option
  ; swap : string option
  ; id : string option
  }

type mime_multipart =
  { envelope_target : string option
  ; envelope_swap : string option
  ; parts : mime_part list
  }

let opt_str = function
  | VNil | VList [] -> None
  | VStr s -> Some s
  | VSymbol s -> Some s
  | other -> Some (Pricklypear_lisp.Reader.pp_value other)

let str_or_raw = function
  | VStr s ->
      (match Prims_html.is_raw_html (VStr s) with Some raw -> raw | None -> s)
  | VSymbol s -> s
  | other -> Pricklypear_lisp.Reader.pp_value other

let kw_name = function
  | VSymbol s when String.length s > 1 && s.[0] = ':' ->
      Some (String.sub s 1 (String.length s - 1))
  | VStr s when String.length s > 1 && s.[0] = ':' ->
      Some (String.sub s 1 (String.length s - 1))
  | _ -> None

(** Split args into (kwargs, positionals).
    Supports leading kwargs, trailing kwargs, or positionals-then-kwargs:
      (part html :target "#x")  (multipart :target "#f" p1 p2) *)
let split_kwargs (args : value list) : (string * value) list * value list =
  let rec go kws pos = function
    | k :: v :: rest ->
        (match kw_name k with
         | Some name when String.length name > 0 ->
             go ((name, v) :: kws) pos rest
         | _ -> go kws (k :: pos) (v :: rest))
    | [ last ] -> go kws (last :: pos) []
    | [] -> (List.rev kws, List.rev pos)
  in
  go [] [] args

let kw_get (kws : (string * value) list) (name : string) : string option =
  match List.assoc_opt name kws with
  | None -> None
  | Some v -> opt_str v

let make_part_value ~(body : string) ~(content_type : string)
    ~(target : string option) ~(swap : string option) ~(id : string option)
  : value =
  let o = function None -> VNil | Some s -> VStr s in
  VList
    [ VSymbol part_tag
    ; VStr body
    ; VStr content_type
    ; o target
    ; o swap
    ; o id
    ]

let part_of_value = function
  | VList
      [ VSymbol t; VStr body; VStr ct; target; swap; id ]
    when t = part_tag ->
      Some
        { body
        ; content_type = ct
        ; target = opt_str target
        ; swap = opt_str swap
        ; id = opt_str id
        }
  | _ -> None

let is_part (v : value) : mime_part option = part_of_value v

let is_multipart (v : value) : mime_multipart option =
  match v with
  | VList (VSymbol t :: env_target :: env_swap :: rest) when t = multipart_tag
    ->
      let parts =
        List.filter_map part_of_value rest
      in
      if parts = [] && rest <> [] then None
      else
        Some
          { envelope_target = opt_str env_target
          ; envelope_swap = opt_str env_swap
          ; parts
          }
  | _ -> None

(* (part html [:target t] [:swap s] [:id i]) *)
let prim_part args =
  let kws, pos = split_kwargs args in
  match pos with
  | [ body ] ->
      make_part_value
        ~body:(str_or_raw body)
        ~content_type:"text/html; charset=utf-8"
        ~target:(kw_get kws "target")
        ~swap:(kw_get kws "swap")
        ~id:(kw_get kws "id")
  | _ ->
      raise
        (Interp_error
           (User_error
              ("part needs (part html [:target t] [:swap s] [:id i])", "bad-args")))

(* (json-part json-string [:id i]) *)
let prim_json_part args =
  let kws, pos = split_kwargs args in
  match pos with
  | [ body ] ->
      make_part_value
        ~body:(str_or_raw body)
        ~content_type:"application/json"
        ~target:(kw_get kws "target")
        ~swap:(kw_get kws "swap")
        ~id:(kw_get kws "id")
  | _ ->
      raise
        (Interp_error
           (User_error ("json-part needs (json-part json [:id i])", "bad-args")))

(* (multipart [:target t] [:swap s] part…)
   Also accepts a single list of parts: (multipart :target "#x" parts-list) *)
let prim_multipart args =
  let kws, pos = split_kwargs args in
  let pos =
    match pos with
    | [ VList items ] when items <> [] && List.for_all (fun v -> part_of_value v <> None) items
      -> items
    | [ VList items ] when items <> [] -> items
    | _ -> pos
  in
  let parts =
    List.map
      (fun v ->
        match part_of_value v with
        | Some _ -> v
        | None ->
            (* Bare string / raw-html becomes an html part. *)
            make_part_value
              ~body:(str_or_raw v)
              ~content_type:"text/html; charset=utf-8"
              ~target:None ~swap:None ~id:None)
      pos
  in
  let o = function None -> VNil | Some s -> VStr s in
  VList
    ( VSymbol multipart_tag
      :: o (kw_get kws "target")
      :: o (kw_get kws "swap")
      :: parts )
