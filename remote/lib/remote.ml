(** remote/lib/remote.ml (borge/remote-eval.borg client).

    The client host: a pure-lisp env + a small LOCAL table of pure
    prims (shared Prims_pure code, so semantics are identical to the
    image's) + a [Proxy_rim.transport] to the image. All evaluation
    runs under the proxy rim; pure compute stays local, everything
    else proxies.

    LAZY ENV (spec): function resolution is data (the server's CTE
    over function_defs). The client materializes defs on first
    reference instead of at boot: an [Interp_error (Unbound_var name)]
    during eval triggers a fetch of the def source (via the
    [function-source] prim, proxied) and evals it into the env under
    the proxy rim, then retries the expression once. The env is a
    CACHE of definitions — never a source of truth.

    IDENTITY: the client passes basic-auth per prim call through the
    transport. It never claims an author it does not hold; identity is
    server-asserted (whoami/current-author come back from the server). *)

open Pricklypear_lisp
open Ast

type transport = Proxy_rim.transport

type t = {
  env : Ast.env;
  local_prims : Eval.prim_table;
  transport : transport;
  (* functions already materialized locally, to bound how often we
     ask the server on a miss (and to break infinite retry). *)
  mutable loaded : (string, unit) Hashtbl.t;
  mutable prim_names : string list; (* for diagnostics, not used for lookup *)
}

let value_to_json = Prims_pure.value_to_json
let json_to_value = Prims_pure.json_to_value

(** Register a prim BOTH in the dispatch table and as a VPrim binding
    in the env (Nopales resolves prims via the env, like the image
    session binds its catalog). *)
let add_prim (t : t) (name : string) (fn : value list -> value) : unit =
  Eval.register_prim t.local_prims name fn;
  Env.define t.env name (VPrim name)

(** Bind a prim as VPrim in the env WITHOUT a local handler. Calling it
    performs [Control.Prim_call], which the proxy rim forwards to
    /api/prim (it is absent from [local_prims]). This is how server
    prims (get/create/rows/…) reach the wire: they must be bound as
    VPrim so eval performs Prim_call, but must NOT be in the local
    table or they'd be serviced locally. Server-only prims
    (route/fork/merge/…) are intentionally NOT bound here — the server
    refuses them with a `server_only` error, and leaving them unbound
    surfaces that locally as Unbound_var rather than a round-trip refusal. *)
let bind_proxy_prim (t : t) (name : string) : unit =
  Env.define t.env name (VPrim name)

(** Register the frozen pure prim surface locally. Same code as the
    image links (Prims_pure), so local semantics are identical — the
    proxy rim's local table is the "pure compute stays local" half of
    hybrid placement. Options that keep a prim OUT of this table
    simply proxy it; nothing depends on the split being exhaustive. *)
let register_pure_prims (t : t) : unit =
  let open Prims_pure in
  let r n f = add_prim t n f in
  r "+" prim_add;
  r "-" prim_sub;
  r "*" prim_mul;
  r "/" prim_div;
  r "div" prim_div;
  r "mod" prim_mod;
  r "quotient" prim_quotient;
  r "remainder" prim_remainder;
  r "abs" prim_abs;
  r "min" prim_min;
  r "max" prim_max;
  r "=" prim_num_eq;
  r "<" prim_lt;
  r ">" prim_gt;
  r "<=" prim_le;
  r ">=" prim_ge;
  r "not" prim_not;
  r "list" prim_list;
  r "car" prim_car;
  r "cdr" prim_cdr;
  r "cons" prim_cons;
  r "null?" prim_null_p;
  r "pair?" prim_pair_p;
  r "append" prim_append;
  r "reverse" prim_reverse;
  r "nth" prim_nth;
  r "count" prim_count;
  r "length" prim_length;
  r "string-append" prim_string_append;
  r "string-length" prim_string_length;
  r "substring" prim_substring;
  r "string-contains?" prim_string_contains;
  r "string-trim" prim_string_trim;
  r "string-replace" prim_string_replace;
  r "string-downcase" prim_string_downcase;
  r "string-upcase" prim_string_upcase;
  r "string-starts-with?" prim_string_starts_with_p;
  r "string-ends-with?" prim_string_ends_with_p;
  r "string=?" prim_string_eq;
  r "string-eq" prim_string_eq;
  r "type-of" prim_type_of;
  r "symbol?" prim_symbol_p;
  r "string?" prim_string_p;
  r "symbol->string" prim_symbol_to_string;
  r "string->symbol" prim_string_to_symbol;
  r "str" prim_str;
  r "as-str" prim_as_str;
  r "assoc" prim_assoc;
  r "member" prim_member;
  r "json-parse" prim_json_parse;
  r "json-emit" prim_json_emit;
  r "dict-get" prim_dict_get;
  r "dict-set" prim_dict_set;
  r "dict-keys" prim_dict_keys;
  r "equal?" prim_equal_p;
  r "random-uuid" prim_random_uuid

(** Bind the server-side prims a client is allowed to proxy. These are
    the proxyable DB CRUD + read/identity prims (NOT the server-only
    branch/admin prims — those stay unbound; see [bind_proxy_prim]). The
    set is deliberately a known list: it is the client's declaration of
    which server prims exist for it. A server prim not in this list is
    simply unbound on the client (Unbound_var), which is honest — the
    client doesn't pretend to know about prims it wasn't built against. *)
let register_proxy_prims (t : t) : unit =
  List.iter (bind_proxy_prim t)
    [ "get"; "create"; "update"; "delete"; "rows"
    ; "function-source"; "function-doc"; "apropos"; "apropos-docs"
    ; "list-bindings"; "whoami"; "current-author"; "render-html" ]

(** Fetch the stored source for [name] by proxying the [function-source]
    read prim (registered on the image as a plain, proxyable prim — see
    image/lib/prims_db.ml). Returns Some source when the server has a
    def for [name], None when it does not. *)
let fetch_def (t : t) (name : string) : string option =
  match t.transport "function-source" [ VStr name ] with
  | Ok (VStr src) when String.trim src <> "" -> Some src
  | Ok (VStr _) -> None
  | Ok _ -> None
  | Error _ -> None

(** Materialize a def into the client env: parse + eval the fetched
    source (a toplevel (define …)) under the proxy rim in [env]. The
    def body may reference prims — those proxy/run locally on call,
    not at define time. *)
let load_def (t : t) (name : string) (src : string) : unit =
  Proxy_rim.run ~local_prims:t.local_prims ~transport:t.transport (fun () ->
      ignore (Eval.read_eval_all_direct ~prims:t.local_prims t.env src));
  Hashtbl.replace t.loaded name ()

(** Lazy env miss inlined into [eval]: on [Unbound_var name], fetch
    the def source and materialize it, then retry the expression. Bounds
    retries per name so a genuinely unbound name fails cleanly. *)
let eval (t : t) (src : string) : value =
  let seen = Hashtbl.create 4 in
  let rec go () =
    try
      Proxy_rim.run ~local_prims:t.local_prims ~transport:t.transport (fun () ->
          Eval.eval_string_direct ~prims:t.local_prims t.env src)
    with
    | Interp_error (Unbound_var name) ->
        if Hashtbl.mem seen name then raise (Interp_error (Unbound_var name))
        else (
          Hashtbl.replace seen name ();
          if Hashtbl.mem t.loaded name then raise (Interp_error (Unbound_var name))
          else
            match fetch_def t name with
            | Some def_src ->
                load_def t name def_src;
                go ()
            | None -> raise (Interp_error (Unbound_var name)))
  in
  go ()

(** Create a client bound to a [transport] against one image. No
    network is touched here (boot is lazy), so construction is cheap
    and pure. *)
let make ~(transport : transport) : t =
  let t =
    {
      env = Env.create ();
      local_prims = Eval.make_prim_table ();
      transport;
      loaded = Hashtbl.create 16;
      prim_names = [];
    }
  in
  register_pure_prims t;
  register_proxy_prims t;
  t
