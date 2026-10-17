(* Per-user LLM keys + chat prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  (* Per-user LLM keys (auth.user-llm-keys) *)
  Primitives.register tbl "set-llm-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr key ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", e, "failed"))))))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", e, "failed"))))))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base;
            Pricklypear_lisp.Ast.VStr model ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ~model:(Some model) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", e, "failed"))))))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", "need key-str [, base] [, model]", "bad-args")))));

  Primitives.register tbl "clear-llm-key"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("clear-llm-key", "denied for anonymous", "denied"))))
      else
        let r =
          Auth_store.clear_llm_key t.store ~user_id:t.manifest.user_id
        in
        match r with
        | Ok () -> Pricklypear_lisp.Ast.VNil
        | Error e ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("clear-llm-key", e, "failed"))))));

  Primitives.register tbl "llm-key-status"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-key-status", "denied for anonymous", "denied"))))
      else
        let meta =
          Auth_store.get_llm_key_meta t.store ~user_id:t.manifest.user_id
        in
        match meta with
        | None ->
            let env_set = Llm_client.api_key () <> "" in
            (Pricklypear_lisp.Ast.VStr
                 (if env_set then
                    "configured=false env_fallback=true"
                  else "configured=false env_fallback=false"))
        | Some m ->
            let base =
              match m.base with Some b -> " base=" ^ b | None -> ""
            in
            let model =
              match m.model with Some m -> " model=" ^ m | None -> ""
            in
            (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "configured=true last4=%s%s%s"
                    m.last4 base model)));

  (* (llm-chat system user [temperature] [max-tokens]) -> VStr content.
     Raw one-shot chat completion — NOT the agentic install loop. Returns
     the model's content string. Used by forge spec-graph drafters and any
     caller that wants a plain completion. See llm_client.call. *)
  Primitives.register tbl "llm-chat"
    (fun args ->
      let temperature, max_tokens, system, user =
        match args with
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user ] ->
            (0.2, 2048, system, user)
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user;
            Pricklypear_lisp.Ast.VFloat t ] ->
            (t, 2048, system, user)
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user;
            Pricklypear_lisp.Ast.VInt n ] ->
            (0.2, Int64.to_int n, system, user)
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user;
            Pricklypear_lisp.Ast.VFloat t;
            Pricklypear_lisp.Ast.VInt n ] ->
            (t, Int64.to_int n, system, user)
        | _ ->
            (0.2, 2048, "", "need system-str, user-str")
      in
      let r =
        Llm_client.call ~temperature ~max_tokens ~system ~user ()
      in
      (match r with
       | Ok s -> (Pricklypear_lisp.Ast.VStr s)
       | Error e -> (Pricklypear_lisp.Ast.VStr ("llm-chat: " ^ e))));
        (* lint-exempt-prim-error: forge/llm-error? pattern-matches this error
           prefix; raising needs the paired Nopales guard
           change on the live package. Tracked on the habitat issue register. *)

  (* Coding tools: freeze AST rewrites into store *)
  Primitives.register tbl "llm-inner"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_inner t ~name ~concept
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-inner", "need name-str, concept-str", "bad-args")))));

  Primitives.register tbl "llm-outer"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_outer t ~name ~concept
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-outer", "need name-str, concept-str", "bad-args")))));

  Primitives.register tbl "llm-rewrite"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_rewrite t ~mode:"replace" ~name ~concept
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept;
          Pricklypear_lisp.Ast.VStr mode ] ->
          Agent_tools.llm_rewrite t ~mode ~name ~concept
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-rewrite", "need name, concept [, mode replace|wrap]", "bad-args"))))));

  Primitives.register tbl "llm-seed"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr requirement ] ->
          Agent_tools.llm_seed t ~requirement ()
      | [ Pricklypear_lisp.Ast.VStr requirement;
          Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.llm_seed t ~requirement ~max_steps:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-seed", "need requirement-str [, max-steps int]", "bad-args"))))));
