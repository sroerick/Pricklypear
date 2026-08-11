(** Prim registration extracted from session.ml (Eio-floor Phase 4). *)
open Session_types

let method_to_string (m : http_method) : string =
  match m with `GET -> "GET" | `POST -> "POST"

let require_request (t : t) : request =
  match t.current_request with
  | Some r -> r
  | None ->
      raise
        (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.User_error ("no active request", "bad-args")))

let str_name = function
  | Pricklypear_lisp.Ast.VStr s | Pricklypear_lisp.Ast.VSymbol s -> Some s
  | _ -> None

let depth_arg = function
  | None -> 1
  | Some (Pricklypear_lisp.Ast.VInt n) -> Int64.to_int n
  | Some (Pricklypear_lisp.Ast.VStr s) ->
      (try int_of_string s with _ -> 1)
  | Some _ -> 1

let vlist_strs xs =
  Pricklypear_lisp.Ast.VList (List.map (fun s -> Pricklypear_lisp.Ast.VStr s) xs)

let impact_to_json (e : impact_event) : string =
  let open Yojson.Safe in
  to_string
    (`Assoc
       [ ("name", `String e.name)
       ; ("at", `Float e.at)
       ; ("branch", `String e.branch)
       ; ("author", `String e.author)
       ; ("dependents", `List (List.map (fun s -> `String s) e.dependents))
       ; ("routes", `List (List.map (fun s -> `String s) e.routes))
       ; ("status", `String e.status)
       ])

let change_to_json (e : change_event) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("op", `String e.op)
       ; ("name", `String e.name)
       ; ("at", `Float e.at)
       ; ("branch", `String e.branch)
       ; ("author", `String e.author)
       ])

let record_define_side_effects = Call_graph.record_define_side_effects
let impact_summary_text = Call_graph.impact_summary_text


let register (t : t) (tbl : Primitives.t) : unit =
  Primitives.register_sync tbl "request-method" (fun _args ->
      let r = require_request t in
      Pricklypear_lisp.Ast.VStr (method_to_string r.method_));
  Primitives.register_sync tbl "request-path" (fun _args ->
      let r = require_request t in
      Pricklypear_lisp.Ast.VStr r.path);
  Primitives.register_sync tbl "request-body" (fun _args ->
      let r = require_request t in
      Pricklypear_lisp.Ast.VStr r.body);
  Primitives.register_sync tbl "request-param" (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr key ] ->
          let r = require_request t in
          (match List.assoc_opt key r.params with
           | Some v -> Pricklypear_lisp.Ast.VStr v
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error ("request-param needs 1 string", "bad-args"))));
  Primitives.register_sync tbl "request-header" (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr key ] ->
          let r = require_request t in
          let k = String.lowercase_ascii key in
          (match List.assoc_opt k r.headers with
           | Some v -> Pricklypear_lisp.Ast.VStr v
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error ("request-header needs 1 string", "bad-args"))));
  Primitives.register_sync tbl "hx-request?" (fun _args ->
      let r = require_request t in
      match List.assoc_opt "hx-request" r.headers with
      | Some v when v <> "" && v <> "false" -> Pricklypear_lisp.Ast.VBool true
      | _ -> Pricklypear_lisp.Ast.VBool false);
  Primitives.register_sync tbl "request-params" (fun _args ->
      let r = require_request t in
      let j =
        `Assoc
          (List.map (fun (k, v) -> (k, `String v)) r.params)
      in
      Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j));
  Primitives.register_sync tbl "redirect" (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          if String.length path = 0 || path.[0] <> '/' then
            raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.User_error
                    ("redirect path must be absolute on host (start with /)", "bad-args")))
          else if String.contains path ':' then
            raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.User_error
                    ("redirect rejects off-site URLs", "bad-args")))
          else Primitives.redirect_value path
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error ("redirect needs 1 string path", "bad-args"))))

