(* Durable agent sessions prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  (* Durable agent sessions (ai-authoring.agent-portal). *)
  Primitives.register tbl "agent-session-new"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed ] ->
          Agent_tools.agent_session_new t ~seed ~tools:true ()
      | [ Pricklypear_lisp.Ast.VStr seed; Pricklypear_lisp.Ast.VBool tools ] ->
          Agent_tools.agent_session_new t ~seed ~tools ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-session-new", "need seed-str [, tools-bool]", "bad-args"))))));

  Primitives.register tbl "agent-say"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent_say t ~session_id:sid ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt;
          Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_say t ~session_id:sid ~prompt ~max_steps:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-say", "need session-id, prompt [, max-steps]", "bad-args"))))));

  Primitives.register tbl "agent-history"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] -> Agent_tools.agent_history t ~session_id:sid
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-history", "need session-id", "bad-args")))));

  Primitives.register tbl "agent-sessions"
    (fun args ->
      match args with
      | [] -> Agent_tools.agent_sessions t ()
      | [ Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_sessions t ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-sessions", "optional limit int", "bad-args"))))));

  Primitives.register tbl "agent-session-delete"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] ->
          Agent_tools.agent_session_delete t ~session_id:sid
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-session-delete", "need session-id", "bad-args")))));

  (* Temporary aliases: new session + one say. Prefer agent-session-new/say. *)
  Primitives.register tbl "agent"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed;
          Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent t ~seed ~prompt ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent", "need 2 strings (seed, prompt)", "bad-args"))))));

  Primitives.register tbl "agent-task"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent_task t ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr prompt; Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_task t ~max_steps:(Int64.to_int n) ~prompt ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-task", "need prompt-str [, max-steps int]", "bad-args"))))));
