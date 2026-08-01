(** Mechanical inside-out (innermost-first) peel of a Nopales AST.

    Pure structure + light annotations. LLM glosses are image-layer. *)
open Ast
open Recursion

type layer = {
  depth : int;
  kind : string;
  annotation : string;
  expr : expr;
  path : int list;
}

let kind_of = function
  | Var _ -> "var"
  | Lit _ -> "lit"
  | App (Var n, _) -> "call:" ^ n
  | App _ -> "app"
  | Let _ -> "let"
  | Letrec _ -> "letrec"
  | If _ -> "if"
  | Lambda _ -> "lambda"
  | Prim (n, _) -> "prim:" ^ n
  | Seq _ -> "seq"
  | Quote _ -> "quote"
  | Define _ -> "define"

let annotate ~(fn_name : string) ~(info : recursion_info) (e : expr)
    ~(path : int list) : string =
  let bits = ref [] in
  (match e with
   | App (Var n, _) when n = fn_name ->
       bits := "recursive call" :: !bits;
       if is_tail_self_path info path then bits := "tail" :: !bits
       else if is_nontail_self_path info path then bits := "non-tail" :: !bits
   | Prim (n, _) -> bits := ("prim " ^ n) :: !bits
   | App (Var n, _) -> bits := ("calls " ^ n) :: !bits
   | Lit _ -> bits := "literal" :: !bits
   | Var n -> bits := ("var " ^ n) :: !bits
   | If _ -> bits := "conditional" :: !bits
   | Lambda _ -> bits := "closure" :: !bits
   | _ -> ());
  match !bits with
  | [] -> kind_of e
  | xs -> String.concat "; " (List.rev xs)

(** Depth of an expression (1 = leaf). *)
let rec depth_of = function
  | Var _ | Lit _ | Quote _ -> 1
  | App (f, args) ->
      1 + List.fold_left max (depth_of f) (List.map depth_of args)
  | Let (_, a, b) | Seq (a, b) -> 1 + max (depth_of a) (depth_of b)
  | Letrec (bs, body) ->
      let d =
        List.fold_left max (depth_of body)
          (List.map (fun (_, e) -> depth_of e) bs)
      in
      1 + d
  | If (a, b, c) ->
      1 + max (depth_of a) (max (depth_of b) (depth_of c))
  | Lambda (_, body) | Define (_, body) -> 1 + depth_of body
  | Prim (_, args) ->
      1
      + List.fold_left max 0 (List.map depth_of args)

(** All subexpressions with path and depth, leaves first (stable). *)
let collect_nodes (e : expr) : (int * int list * expr) list =
  let acc = ref [] in
  let rec go path e =
    acc := (depth_of e, List.rev path, e) :: !acc;
    match e with
    | Var _ | Lit _ | Quote _ -> ()
    | App (f, args) ->
        go (0 :: path) f;
        List.iteri (fun i a -> go ((i + 1) :: path) a) args
    | Let (_, v, body) ->
        go (0 :: path) v;
        go (1 :: path) body
    | Letrec (bs, body) ->
        List.iteri (fun i (_, e') -> go (i :: path) e') bs;
        go (List.length bs :: path) body
    | If (t, th, el) ->
        go (0 :: path) t;
        go (1 :: path) th;
        go (2 :: path) el
    | Lambda (_, body) | Define (_, body) -> go (0 :: path) body
    | Prim (_, args) -> List.iteri (fun i a -> go (i :: path) a) args
    | Seq (a, b) ->
        go (0 :: path) a;
        go (1 :: path) b
  in
  go [] e;
  (* Innermost first: higher depth first; then left-to-right by path. *)
  !acc
  |> List.sort (fun (d1, p1, _) (d2, p2, _) ->
         let c = compare d2 d1 in
         if c <> 0 then c else compare p1 p2)

let peel ~(fn_name : string) (e : expr) : layer list =
  let body = unwrap_fn_body e in
  let info = analyze_self ~fn_name body in
  collect_nodes body
  |> List.mapi (fun i (_d, path, expr) ->
         {
           depth = i;
           kind = kind_of expr;
           annotation = annotate ~fn_name ~info expr ~path;
           expr;
           path;
         })

let layers_to_strings (layers : layer list) : string list =
  List.map
    (fun (l : layer) ->
      Printf.sprintf "[%d] %s — %s\n  %s" l.depth l.kind l.annotation
        (Reader.pp_expr l.expr))
    layers
