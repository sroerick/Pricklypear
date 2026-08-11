(** Prim registration extracted from session.ml (Eio-floor Phase 4). *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  let deny_anon name =
    (* Raise, don't return a VStr: a returned string is silently dropped
       by (seq …), so a multi-write handler could partially succeed past
       an anonymous denial. Raising aborts the whole eval. (BORGE-1785213376) *)
    raise
      (Pricklypear_lisp.Ast.Interp_error
         (Pricklypear_lisp.Ast.User_error
            (name ^ ": denied for anonymous", "bad-args")))
  in
  let actor () =
    match Request_context.get () with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  (* (media-meta id) -> VStr json | VNil *)
  Primitives.register tbl "media-meta"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
          let m = Media.get_meta t.store ~id in
          (match m with
           | None -> Pricklypear_lisp.Ast.VNil
           | Some meta ->
               (Pricklypear_lisp.Ast.VStr (Media.meta_to_json_string meta)))
      | _ ->
          (Pricklypear_lisp.Ast.VStr "media-meta: need id"));
  (* (media-list [limit]) -> VList of meta json strings *)
  Primitives.register tbl "media-list"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "media-list"
      else
        let limit =
          match args with
          | [] -> 50
          | [ Pricklypear_lisp.Ast.VInt n ] -> Int64.to_int n
          | _ -> 50
        in
        let rows =
          Media.list t.store ~author:(Some uname) ~limit ()
        in
        let items =
          List.map
            (fun m ->
              Pricklypear_lisp.Ast.VStr (Media.meta_to_json_string m))
            rows
        in
        (Pricklypear_lisp.Ast.VList items));
  (* (media-delete id) -> VStr id | VNil | denial *)
  Primitives.register tbl "media-delete"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "media-delete"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr id ] ->
            let res =
              Media.delete t.store ~id ~actor:uname ~is_admin:t.is_admin
            in
            (match res with
             | Ok (`Ok id') ->
                 (Pricklypear_lisp.Ast.VStr id')
             | Ok `Not_found -> Pricklypear_lisp.Ast.VNil
             | Ok `Forbidden ->
                 (Pricklypear_lisp.Ast.VStr
                      "media-delete: not author or admin")
             | Error e ->
                 (Pricklypear_lisp.Ast.VStr ("media-delete: " ^ e)))
        | _ ->
            (Pricklypear_lisp.Ast.VStr "media-delete: need id"));
  (* (media-put content-type data-b64 [filename]) -> VStr id | error
     Tiny payloads only (icons/fixtures). Real pushes: POST /api/media. *)
  Primitives.register tbl "media-put"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "media-put"
      else
        let parse =
          match args with
          | [ Pricklypear_lisp.Ast.VStr ct
            ; Pricklypear_lisp.Ast.VStr b64 ] ->
              Ok (ct, b64, None)
          | [ Pricklypear_lisp.Ast.VStr ct
            ; Pricklypear_lisp.Ast.VStr b64
            ; Pricklypear_lisp.Ast.VStr filename ] ->
              Ok (ct, b64, Some filename)
          | _ -> Error "media-put: need content-type, data-b64 [, filename]"
        in
        match parse with
        | Error e -> (Pricklypear_lisp.Ast.VStr e)
        | Ok (ct, b64, filename) ->
            (match Media.b64_decode_opt (String.trim b64) with
             | None ->
                 (Pricklypear_lisp.Ast.VStr "media-put: invalid data-b64")
             | Some data ->
                 let res =
                   Media.put t.store ~author:uname ~content_type:ct ~data
                     ~filename ~max_bytes:Media.max_eval_put_bytes ()
                 in
                 (match res with
                  | Ok r -> (Pricklypear_lisp.Ast.VStr r.id)
                  | Error e ->
                      (Pricklypear_lisp.Ast.VStr ("media-put: " ^ e)))));
  (* (media-url id) -> VStr "/media/<id>" *)
  Primitives.register tbl "media-url"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
          (Pricklypear_lisp.Ast.VStr (Media.url_of_id id))
      | _ ->
          (Pricklypear_lisp.Ast.VStr "media-url: need id"))

