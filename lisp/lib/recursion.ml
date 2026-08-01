(** Static recursion / tail-position analysis over Nopales ASTs.

    Pure: no I/O. Used by binding-page HTML and complexity heuristics.
    Tail context mirrors the eval trampoline (lisp-interpreter.eval TCO). *)
open Ast

type tail_ctx = Tail | Non_tail

type site = {
  path : int list;
  tail : bool;
  callee : string;
}

type recursion_info = {
  is_recursive : bool;
  tail_recursive : bool;
  (** True when there is at least one self-call and every self-call is tail. *)
  self_sites : site list;
  (** Direct App-Var callees excluding self (for mutual annotation). *)
  callees : string list;
}

let rec unwrap_fn_body (e : expr) : expr =
  match e with
  | Define (_, body) -> unwrap_fn_body body
  | Lambda (_, body) -> body
  (* Surface reader keeps define/lambda as App until eval. *)
  | App (Var "define", [ _name; body ]) -> unwrap_fn_body body
  | App (Var "define", [ _name; body; _doc ]) -> unwrap_fn_body body
  | App (Var "lambda", [ _params; body ]) -> body
  | App (Var "lambda", _params :: body :: _) -> body
  | other -> other

let rec collect_sites ~(fn_name : string) ~(ctx : tail_ctx) ~(path : int list)
    (e : expr) : site list =
  match e with
  | Var _ | Lit _ | Quote _ -> []
  (* Surface special forms stay App(Var …) until eval — treat like If/Seq/Let. *)
  | App (Var "if", test :: then_e :: else_e :: _) ->
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) test
      @ collect_sites ~fn_name ~ctx ~path:(1 :: path) then_e
      @ collect_sites ~fn_name ~ctx ~path:(2 :: path) else_e
  | App (Var "if", test :: then_e :: []) ->
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) test
      @ collect_sites ~fn_name ~ctx ~path:(1 :: path) then_e
  | App (Var sf, args)
    when sf = "seq" || sf = "begin" ->
      (match List.rev args with
       | [] -> []
       | last :: rev_rest ->
           let early =
             List.concat
               (List.mapi
                  (fun i a ->
                    collect_sites ~fn_name ~ctx:Non_tail ~path:(i :: path) a)
                  (List.rev rev_rest))
           in
           let last_i = List.length args - 1 in
           early
           @ collect_sites ~fn_name ~ctx ~path:(last_i :: path) last)
  | App (Var "and", args) | App (Var "or", args) ->
      (match List.rev args with
       | [] -> []
       | last :: rev_rest ->
           let early =
             List.concat
               (List.mapi
                  (fun i a ->
                    collect_sites ~fn_name ~ctx:Non_tail ~path:(i :: path) a)
                  (List.rev rev_rest))
           in
           let last_i = List.length args - 1 in
           early @ collect_sites ~fn_name ~ctx ~path:(last_i :: path) last)
  | App (Var "lambda", args) ->
      (* New frame — body is not tail of the outer function. *)
      (match args with
       | _params :: body :: _ ->
           collect_sites ~fn_name ~ctx:Non_tail ~path:(1 :: path) body
       | _ -> [])
  | App (Var "define", args) ->
      (match args with
       | _name :: body :: _ ->
           collect_sites ~fn_name ~ctx:Non_tail ~path:(1 :: path) body
       | _ -> [])
  | App (Var callee, args) ->
      let here =
        if callee = fn_name then
          [ { path = List.rev path; tail = ctx = Tail; callee } ]
        else []
      in
      let arg_sites =
        List.concat
          (List.mapi
             (fun i a ->
               collect_sites ~fn_name ~ctx:Non_tail ~path:(i :: path) a)
             args)
      in
      here @ arg_sites
  | App (f, args) ->
      let fs = collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) f in
      let arg_sites =
        List.concat
          (List.mapi
             (fun i a ->
               collect_sites ~fn_name ~ctx:Non_tail ~path:((i + 1) :: path) a)
             args)
      in
      fs @ arg_sites
  | If (t, th, el) ->
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) t
      @ collect_sites ~fn_name ~ctx ~path:(1 :: path) th
      @ collect_sites ~fn_name ~ctx ~path:(2 :: path) el
  | Seq (a, b) ->
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) a
      @ collect_sites ~fn_name ~ctx ~path:(1 :: path) b
  | Let (_, v, body) ->
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) v
      @ collect_sites ~fn_name ~ctx ~path:(1 :: path) body
  | Letrec (binds, body) ->
      let bs =
        List.concat
          (List.mapi
             (fun i (_, e') ->
               collect_sites ~fn_name ~ctx:Non_tail ~path:(i :: path) e')
             binds)
      in
      bs @ collect_sites ~fn_name ~ctx ~path:(List.length binds :: path) body
  | Lambda (_, body) ->
      (* New frame — body is not tail of the outer function. *)
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) body
  | Prim (_, args) ->
      List.concat
        (List.mapi
           (fun i a ->
             collect_sites ~fn_name ~ctx:Non_tail ~path:(i :: path) a)
           args)
  | Define (_, body) ->
      collect_sites ~fn_name ~ctx:Non_tail ~path:(0 :: path) body

let collect_callees (e : expr) : string list =
  let rec go acc = function
    | Var _ | Lit _ | Quote _ -> acc
    | App (Var f, args) ->
        List.fold_left go (f :: acc) args
    | App (f, args) -> List.fold_left go (go acc f) args
    | Let (_, a, b) -> go (go acc a) b
    | Letrec (bs, body) ->
        let acc = List.fold_left (fun a (_, e) -> go a e) acc bs in
        go acc body
    | If (a, b, c) -> go (go (go acc a) b) c
    | Lambda (_, body) -> go acc body
    | Prim (_, args) -> List.fold_left go acc args
    | Seq (a, b) -> go (go acc a) b
    | Define (_, body) -> go acc body
  in
  let seen = Hashtbl.create 16 in
  go [] e
  |> List.fold_left
       (fun acc n ->
         if Hashtbl.mem seen n then acc
         else begin
           Hashtbl.add seen n ();
           n :: acc
         end)
       []
  |> List.rev

let analyze_self ~(fn_name : string) (e : expr) : recursion_info =
  let body = unwrap_fn_body e in
  let self_sites = collect_sites ~fn_name ~ctx:Tail ~path:[] body in
  let is_recursive = self_sites <> [] in
  let tail_recursive =
    is_recursive && List.for_all (fun s -> s.tail) self_sites
  in
  let callees =
    List.filter (fun c -> c <> fn_name) (collect_callees body)
  in
  { is_recursive; tail_recursive; self_sites; callees }

(** Path predicates: [is_tail path] / [is_self path] for HTML decoration. *)
let site_index (info : recursion_info) : (int list, site) Hashtbl.t =
  let h = Hashtbl.create 8 in
  List.iter (fun s -> Hashtbl.replace h s.path s) info.self_sites;
  h

let is_self_path (info : recursion_info) (path : int list) : bool =
  List.exists (fun s -> s.path = path) info.self_sites

let is_tail_self_path (info : recursion_info) (path : int list) : bool =
  List.exists (fun s -> s.path = path && s.tail) info.self_sites

let is_nontail_self_path (info : recursion_info) (path : int list) : bool =
  List.exists (fun s -> s.path = path && not s.tail) info.self_sites
