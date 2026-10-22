(** One permission model for the OCaml layer (specs/authz-matrix.md).

    Migration STEP 1 (2026-10-21): identity classes + capability table +
    [authorize], with NO call sites yet. Families migrate onto it in
    steps 2-6 (branch ops, reseed/load-library/deftype, data, auth prims,
    HTTP edge); until then every existing inline gate stays the live
    enforcement and these rows are documentation, not behavior.

    Documented simplifications (deliberate):
    - Admin is computed from the session flag [t.is_admin]. The live
      [is_verified_admin] re-check stays at call sites until step 2
      folds it in here.
    - Self_agent is detected with the same username heuristic
      [branch_owner_authorized] uses (username ends in "/agent"); the
      manifest carries no user_type field today.
    - The Reseed_branch x Anonymous row is the INTENDED policy (deny).
      Current mainframe behavior ALLOWS anonymous non-main reseeds (no
      anon gate on that path); the row takes effect only when the
      reseed family migrates in step 3, never before. *)

type identity_class =
  | Anonymous
  | Self_human
  | Self_agent
  | Delegated
  | Tenant
  | Admin

let string_of_identity_class = function
  | Anonymous -> "anonymous"
  | Self_human -> "self-human"
  | Self_agent -> "self-agent"
  | Delegated -> "delegated"
  | Tenant -> "tenant"
  | Admin -> "admin"

let ends_with ~(suffix : string) (s : string) : bool =
  let n = String.length s and m = String.length suffix in
  n >= m && String.sub s (n - m) m = suffix

(** The ONE identity-class computation. Every gate site should end up
    calling this instead of re-deriving its own idea of "who am I" -
    three admin idioms and two owner idioms already drifted apart
    (spec section 1). *)
let identity_class_of_session (t : Session_types.t) : identity_class =
  if t.is_admin then Admin
  else if t.delegated then Delegated
  else if t.manifest.username = "anonymous" then Anonymous
  else
    (match Profile.name () with
     | Some _ -> Tenant
     | None ->
         if ends_with ~suffix:"/agent" t.manifest.username then Self_agent
         else Self_human)

(** A capability is a gated surface from the matrix (spec section 3).
    A variant, not a string, so rule rows stay compiler-checked. *)
type capability =
  | Reseed_main
  | Reseed_branch
  | Branch_move  (** checkout / fork / merge *)
  | Branch_read
  | Code_persist  (** define / route / package persistence *)
  | Deftype
  | Load_library
  | Token_own  (** issue/list/revoke/rotate the caller's own credentials *)
  | Token_other  (** third-party credential admin *)
  | Delegated_token_manage
  | User_admin
  | Retention_admin
  | Data_write
  | Data_read
  | Promote_own

let string_of_capability = function
  | Reseed_main -> "reseed-main"
  | Reseed_branch -> "reseed-branch"
  | Branch_move -> "branch-move"
  | Branch_read -> "branch-read"
  | Code_persist -> "code-persist"
  | Deftype -> "deftype"
  | Load_library -> "load-library"
  | Token_own -> "token-own"
  | Token_other -> "token-other"
  | Delegated_token_manage -> "delegated-token-manage"
  | User_admin -> "user-admin"
  | Retention_admin -> "retention-admin"
  | Data_write -> "data-write"
  | Data_read -> "data-read"
  | Promote_own -> "promote-own"

type decision =
  | Allow
  | Deny of string  (** centralized denial string *)
    | Owner_scope of (string -> string -> string)
    (** the owner-namespace rule (909f295); the function takes the
        ASKED-FOR owner first, then the session's own username, and
        returns the denial string *)

(** The matrix as data. Rows encode the CURRENT mainframe behavior of
    each gate family (spec section 3) unless marked otherwise. *)
let decision (c : capability) (k : identity_class) : decision =
  match (c, k) with
  | Reseed_main, Admin -> Allow
  | Reseed_main, (Delegated | Tenant) ->
      Deny "reseed-package: denied for delegated sessions and tenants"
  | Reseed_main, _ -> Deny "reseed-package: requires admin"
  | Reseed_branch, (Admin | Self_human | Self_agent) -> Allow
    | Reseed_branch, (Delegated | Tenant) ->
        Owner_scope (fun ref own ->
            Printf.sprintf
              "reseed-package: '%s' is outside your namespace (you may only reseed branches of '%s')"
              ref own)
  | Reseed_branch, Anonymous ->
      Deny "reseed-package: requires authentication"
  | Branch_move, (Admin | Self_human | Self_agent | Tenant) ->
      Allow  (* per-branch owner rules stay at the call sites *)
  | Branch_move, Delegated ->
      Deny "branch move blocked: agent is locked to the current branch"
  | Branch_move, Anonymous -> Deny "requires authentication"
  | Branch_read, _ ->
      Allow  (* system/* public + branch_readable stay at the call sites *)
  | Code_persist, (Admin | Self_human | Self_agent) -> Allow
  | Code_persist, Delegated -> Deny "delegation is for data, never for code"
  | Code_persist, Tenant -> Deny "denied for tenants (code persistence)"
  | Code_persist, Anonymous -> Deny "denied for anonymous"
  | Deftype, (Admin | Self_human | Self_agent) -> Allow
  | Deftype, (Delegated | Tenant) ->
      Deny "deftype: denied for delegated sessions and tenants"
  | Deftype, Anonymous -> Deny "deftype: denied for anonymous"
  | Load_library, _ ->
      Allow  (* the seeded-only rule for delegated stays at the call site *)
  | Token_own, (Admin | Self_human | Self_agent | Tenant | Delegated) ->
      Allow
  | Token_own, Anonymous -> Deny "requires authentication"
  | Token_other, Admin -> Allow
  | Token_other, _ -> Deny "admin only"
  | Delegated_token_manage, (Admin | Self_human | Self_agent | Tenant | Delegated) ->
      Allow  (* owner checks stay at the call sites *)
  | Delegated_token_manage, Anonymous -> Deny "requires authentication"
  | User_admin, Admin -> Allow
  | User_admin, _ -> Deny "admin only"
  | Retention_admin, Admin -> Allow
  | Retention_admin, _ -> Deny "admin only"
  | Data_write, (Admin | Self_human | Self_agent | Tenant | Delegated) ->
      Allow  (* RLS + space guards stay at the call sites *)
  | Data_write, Anonymous -> Deny "denied for anonymous"
  | Data_read, _ -> Allow  (* RLS + vis floors stay at the call sites *)
  | Promote_own, (Admin | Self_agent) -> Allow
  | Promote_own, _ -> Deny "promote requires an agent user"

(** The one entry point call sites migrate onto. [owner] is the resource
    namespace owner for Owner_scope rows (the branch owner prefix). *)
let authorize (t : Session_types.t) ~(capability : capability)
    ?(owner : string option) () : (unit, string) result =
  let k = identity_class_of_session t in
  match decision capability k with
  | Allow -> Ok ()
  | Deny msg -> Error msg
    | Owner_scope f -> (
        match owner with
        | Some o when o = t.manifest.username -> Ok ()
        | Some o -> Error (f o t.manifest.username)
        | None -> Error (f "?" t.manifest.username))

(** Bare namespace predicate for call sites that need it directly
    (reseed_package's 909f295 gate). *)
let owner_namespace_ok (t : Session_types.t) (owner : string) : bool =
  owner = t.manifest.username