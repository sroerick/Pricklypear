(* Shared helpers for the split DB-prim families (i-1788671035-8 fix pass).

   The old prims_db.register was one ~2,850-line closure registering 137
   prims across unrelated domains; its helper closures coupled those domains
   through hidden state. These are the same helpers, made explicit over
   (t, tbl) so each family module (prims_data/sync/jobs/auth/code/agent/
   llm/ops) re-binds them as locals and its prim bodies stay verbatim. *)
open Session_types

(* Quoted type/field names are VSymbol after Phase 1; CRUD still wants
   string payloads for SQL. Coerce symbols->strings at the CRUD boundary
   only (do not collapse in symbol=? / general prims). *)
let coerce_sym_args (args : Pricklypear_lisp.Ast.value list) =
  List.map
    (function
      | Pricklypear_lisp.Ast.VSymbol s -> Pricklypear_lisp.Ast.VStr s
      | v -> v)
    args

let register_crud ~(tbl : Primitives.t) name fn =
  Primitives.register tbl name (fun args ->
      Pricklypear_lisp.Eval.note_db_hit ();
      let t0 = Unix.gettimeofday () in
      let result = fn (coerce_sym_args args) in
      let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000. in
      (* accumulate DB wall time into the eval budget for page-timing;
         per-prim breakdown for the db-panel (image.db-panel) *)
      Pricklypear_lisp.Eval.note_db_ms elapsed_ms;
      Pricklypear_lisp.Eval.note_prim_db name elapsed_ms;
      result)

let deny_anon name =
  (* Raise, don't return a VStr: a returned string is silently dropped
     by (seq ...), so a multi-write handler could partially succeed past
     an anonymous denial. Raising aborts the whole eval. (BORGE-1785213376) *)
  raise
    (Pricklypear_lisp.Ast.Interp_error
       (Pricklypear_lisp.Ast.User_error
          (name ^ ": denied for anonymous", "bad-args")))

(* json-values-alist boundary policy for the fields/body argument of the
   data prims (create / update / tree/upsert). The policy itself lives in
   the pure codec (Prims_json.json_body) so it is unit-testable without a
   DB; this alias keeps the image prim bodies reading naturally. *)
let json_body = Prims_pure.json_body

(* Per-request identity snapshot (avoids the cotenant race where the
   live t.manifest is mid-swap). Falls back to t.manifest outside HTTP. *)
let actor ~(t : t) () =
  match Request_context.get () with
  | Some c -> (Request_context.manifest c).username
  | None -> t.manifest.username

let caller_username ~(t : t) () =
  match Request_context.get () with
  | Some c -> (Request_context.manifest c).username
  | None -> t.manifest.username

(* Admin gating for Lisp auth surface (auth.user-management /
   auth.bootstrap-admin). Prefer the per-request snapshot username
   and re-check users.is_admin in PG so a concurrent attach cannot
   flip t.is_admin under an in-flight /api/eval. *)
let require_admin ~(t : t) (op : string) (k : unit -> Pricklypear_lisp.Ast.value)
  : Pricklypear_lisp.Ast.value =
  let uname = caller_username ~t () in
  if uname = "anonymous" then
    (Pricklypear_lisp.Ast.VStr (op ^ ": denied for anonymous"))
  else
    let u = Auth_store.get_user_by_username t.store uname in
    match u with
    | Some u when u.Auth_store.is_admin -> k ()
    | _ ->
        (Pricklypear_lisp.Ast.VStr (op ^ ": denied: admin only"))

(** DB-verified admin consult for SECURITY GATES (the flag is not enough).
    require_admin answers "may this prim run"; this answers "is the
    identity REALLY an admin right now" for authority gates that catch
    stale or boot-derived [t.is_admin]:: the jobs runner and the
    loopback REPL eval on the SHARED BOOT session, whose t.is_admin is
    true on prod (Session.make derives it from username="admin" when
    PP_USERNAME is the default) and NEVER changes when the runner flips
    manifest.username to the submitter. Gates that trusted the flag
    alone (branch_owner_authorized, promote, reseed, media visibility)
    let any submitted job eval as admin — checkout of any tenant's
    garden, promote into system/main, cross-tenant media reads.

    Shape mirrors require_admin's rationale ("a concurrent attach
    cannot flip t.is_admin"): read the per-request snapshot username,
    re-check users.is_admin in PG, fail closed when the row is missing
    or the DB is unreachable. Cheap on the common path — a non-admin
    session (t.is_admin=false) answers false with NO db round trip. *)
let is_verified_admin ~(t : t) () : bool =
  if not t.is_admin then false
  else
    let uname = caller_username ~t () in
    if uname = "anonymous" then false
    else
      match Auth_store.get_user_by_username t.store uname with
      | Some u -> u.Auth_store.is_admin
      | None -> false
