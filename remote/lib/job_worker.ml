(** remote/lib/job_worker.ml (borge/jobs.borg, THE FLEET QUESTION).

    The out-of-process worker: claim a job from the image's queue, decide
    whether this process can FAITHFULLY run it, and either run it locally or
    hand it back.

    WHY A GATE AT ALL. The claim protocol travels; the evaluation context does
    not. The in-image runner evaluates a job under a synthetic Request_context
    pinned to job.branch and job.submitted_by (boot.ml eval_job). This client
    evaluates under its own connection's branch and identity. That difference
    is invisible for most bodies and fatal for some:

      - Txn bodies. (with-transaction …) and (with-step …) hand a LAMBDA to a
        server-only prim. A lambda cannot cross a wire — prims_json renders it
        as printed text and reconstructs nothing — so proxying one fails loudly
        ("need a lambda"). Worse, (with-step …)'s checkpoint must land in the
        SAME transaction as the thunk's writes, which is the entire
        exactly-once mechanism and rides Store.current_conn. Exactly-once is
        honorable in ONE place: inside the image. So a Txn body must never be
        claimed by an external worker.

      - Unresolvable defs. If the job's fn is not materialized locally or
        fetchable, running it would fail for a reason unrelated to the job.

    PLACEMENT DERIVES FROM THE BODY, NOT FROM A PACKAGE LIST. The classifier is
    Effect.infer — the same analysis (with-transaction) already uses to refuse
    bodies carrying Net/Ai (session_control_prims.ml). Reusing it means the
    rule is stated once, and a job that GROWS a with-transaction becomes
    ineligible automatically. A curated allowlist of package names would rot
    the first time someone edited a body.

    A DECLINED JOB IS NOT A FAILED JOB. The worker releases it
    (job-release), which refunds the attempt, so declining cannot march a job
    to 'dead'. The row becomes immediately claimable by the in-image runner —
    that handoff is the design, not a fallback. *)

open Pricklypear_lisp

(** What this worker decided to do with a claimed row. *)
type verdict =
  | Run
  | Decline of string
      (** [Decline reason] — hand it back unrun. The reason is for the log;
          it deliberately does NOT get written to the row's error column,
          because nothing failed. *)

(** Should this worker run a body?

    [src] is the job fn's stored source, or [None] when it could not be
    resolved locally. The conservative direction is Decline: an unrunnable job
    handed back costs one claim cycle, while a wrongly-run one produces a
    WRONG RESULT that looks authoritative. *)
let gate ?(known_fn : Effect.known_fn_lookup = fun _ -> None)
    (src : string option) : verdict =
  match src with
  | None -> Decline "def-not-resolvable-locally"
  | Some source ->
    (* A parse failure declines rather than raising: the worker's whole value
       is that it keeps draining, and one malformed def must not stop it. *)
    (match (try Some (Reader.read_one source) with _ -> None) with
     | None -> Decline "def-unparseable"
     | Some expr ->
       (match (try Some (Effect.infer ~known_fn expr) with _ -> None) with
        | None -> Decline "effects-unknown"
        | Some fx ->
          if Effect.EffSet.mem Effect.Txn fx then Decline "txn-body-in-image-only"
          else if Effect.EffSet.mem Effect.Branch fx then
            (* Branch/fork/merge/checkout mutate shared server state that this
               client cannot see consistently; those stay in-image for the
               same reason Txn does. *)
            Decline "branch-effect-in-image-only"
          else Run))

(** Escape a string as a Nopales string literal. Only the two characters that
    can terminate or corrupt a literal are handled, which is all that is
    needed for the tiny set of ids/tags this client ever splices. *)
let lisp_quote (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(** The claim expression. Sent as a WHOLE EXPRESSION over /api/eval, never as a
    proxied prim call: claim-job is register_server_only, and
    Primitives.dispatch (the /api/prim path) refuses those. *)
let claim_expr ~(tag : string) ~(branch : string) : string =
  Printf.sprintf "(claim-job %s %s)" (lisp_quote tag) (lisp_quote branch)

let heartbeat_expr ~(id : string) ~(tag : string) : string =
  Printf.sprintf "(job-heartbeat %s %s)" (lisp_quote id) (lisp_quote tag)

let release_expr ~(id : string) ~(tag : string) : string =
  Printf.sprintf "(job-release %s %s)" (lisp_quote id) (lisp_quote tag)

(** Finish with a result that is already JSON text. job-finish's value argument
    is passed to as_json as a VStr, i.e. the string IS the stored jsonb. *)
let finish_done_expr ~(id : string) ~(tag : string) ~(json : string) : string =
  Printf.sprintf "(job-finish %s %s \"done\" %s)" (lisp_quote id)
    (lisp_quote tag) (lisp_quote json)

let finish_failed_expr ~(id : string) ~(tag : string) ~(err : string) : string =
  Printf.sprintf "(job-finish %s %s \"failed\" %s)" (lisp_quote id)
    (lisp_quote tag) (lisp_quote err)

(** A claimed row, as far as the worker cares. *)
type claim = {
  id : string;
  fn : string;
  args : string;  (** raw jsonb text, spliced into the call *)
  branch : string;
}

let claim_of_json (s : string) : claim option =
  try
    let j = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    match (j |> member "id" |> to_string_option, j |> member "fn" |> to_string_option) with
    | Some id, Some fn ->
      let args =
        match j |> member "args" with
        | `String s -> s
        | `Null -> "null"
        | other -> Yojson.Safe.to_string other
      in
      Some
        { id; fn; args
        ; branch = (j |> member "branch" |> to_string_option |> Option.value ~default:"") }
    | _ -> None
  with _ -> None

(** The call source for a claim, matching image Jobs.eval_src byte-for-byte so
    local and remote runs of the same row agree. *)
let call_src (c : claim) : string =
  let a = String.trim c.args in
  if a = "" || a = "[]" || a = "null" then Printf.sprintf "(%s)" c.fn
  else Printf.sprintf "(%s %s)" c.fn a

(** Result text, mirroring boot.ml eval_job's convention exactly: a job's value
    is JSON TEXT. [VStr s] passes through unquoted because it is assumed
    already-encoded jsonb; a non-string value is rendered as a JSON scalar, and
    anything structured becomes a JSON string of its printed form. Diverging
    here would make the same row store different jsonb locally vs in-image. *)
let result_text (v : Ast.value) : string =
  match v with
  | Ast.VStr s -> s
  | Ast.VNil -> "null"
  | Ast.VInt n -> Int64.to_string n
  | Ast.VBool b -> if b then "true" else "false"
  | other -> Printf.sprintf "%S" (Reader.pp_value other)

(** Everything the loop needs, injected so a tick is testable with no server. *)
type env = {
  tag : string;
  branch : string;
  interval : float;
  server_eval : string -> Ast.value;
      (** Raw /api/eval. The ONLY path that can reach claim-job /
          job-heartbeat / job-release / job-finish, because they are
          register_server_only and /api/prim refuses those. *)
  local_eval : string -> Ast.value;
      (** Hybrid placement for the body: defs materialize locally, prims
          proxy. This is the half that must NOT be used for the protocol
          verbs — it would try to proxy them and be refused. *)
  def_source : string -> string option;
  sleep : float -> unit;
  log : string -> unit;
}

(** One turn of the loop. Returns [true] when a job was handled, [false] when
    the queue was empty (so a caller can distinguish idle from busy in tests;
    the forever-loop ignores it).

    ORDER MATTERS. Claim, then decide, then either release or run+finish. The
    release path exists so a decline never spends an attempt; the finish path
    returns a bool that reports LEASE OWNERSHIP, and a false there means this
    worker's result is no longer authoritative. *)
let tick (e : env) : bool =
  let claimed = e.server_eval (claim_expr ~tag:e.tag ~branch:e.branch) in
  let claim =
    match claimed with
    | Ast.VStr s -> claim_of_json s
    | _ -> None
  in
  match claim with
  | None -> false
  | Some c ->
    let src = e.def_source c.fn in
    (match gate src with
     | Decline reason ->
       (* Nothing ran, nothing failed: hand it back with the attempt refunded.
          A false here is not an error either — someone else already owns it. *)
       let released = e.server_eval (release_expr ~id:c.id ~tag:e.tag) in
       e.log
         (Printf.sprintf "declined %s (%s): release=%s" c.fn reason
            (match released with Ast.VBool true -> "ok" | _ -> "lost"));
       true
     | Run ->
       let outcome =
         try Ok (result_text (e.local_eval (call_src c)))
         with exn -> Error (Printexc.to_string exn)
       in
       let verdict =
         match outcome with
         | Ok json ->
           e.server_eval (finish_done_expr ~id:c.id ~tag:e.tag ~json)
         | Error err ->
           e.server_eval (finish_failed_expr ~id:c.id ~tag:e.tag ~err)
       in
       (match verdict with
        | Ast.VBool true ->
          e.log
            (Printf.sprintf "%s %s"
               (match outcome with Ok _ -> "finished" | Error _ -> "failed")
               c.fn)
        | _ ->
          (* The lease lapsed or was reclaimed. Anything we computed is
             already superseded — say so loudly rather than dropping it
             silently. *)
          e.log
            (Printf.sprintf "%s: lease lost, result discarded" c.fn));
       true)

(** Drain until cancelled. An idle queue sleeps a full interval; a handled job
    loops immediately so a backlog drains without waiting out the timer — the
    same asymmetry the in-image runner uses. *)
let run_forever (e : env) : unit =
  while true do
    let busy = try tick e with exn ->
      e.log ("tick failed: " ^ Printexc.to_string exn);
      false
    in
    if not busy then e.sleep e.interval
  done
