(* Code (generations, function introspection, forge, packages, history) prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register ?(rehydrate_fn : t -> unit = fun _ -> ()) (t : t) (tbl : Primitives.t) : unit =
  let register_crud name fn = Prims_db_common.register_crud ~tbl name fn in
  let deny_anon name = Prims_db_common.deny_anon name in
  let actor () = Prims_db_common.actor ~t () in

  (* Resolve an optional "owner/branch" read ref to a branch id without
     checking out (branching.branch-resolved-reads). Mirrors code-history's
     parsing, but also enforces the read-side owner rule via
     [Session_control_prims.branch_readable]: system/* is public, personal
     gardens are tenant-confined. Returns a clean error STRING the caller
     surfaces (never raises) — reads must not crash on a bad ref. *)
  let branch_read_scope = function
    | None -> `Current (Store.current_branch_id t.store)
    | Some branch_ref ->
        let owner, name = Session_control_prims.split_branch_ref branch_ref in
        if not (Session_control_prims.branch_readable t owner) then
          `Denied (branch_ref, owner)
        else
          (match Store.lookup_branch_id_by_name t.store ~owner ~name with
           | Some id -> `Found (id, branch_ref)
           | None -> `Missing branch_ref)
  in

  (* (list-branches) -> VList of "owner/name" strings *)
  Primitives.register tbl "list-branches"
    (fun _args ->
      let bs = Store.list_branches t.store in
      let names =
        List.map
          (fun (b : Pricklypear_common.Branch.t) ->
            Pricklypear_lisp.Ast.VStr
              (b.Pricklypear_common.Branch.owner ^ "/" ^
               b.Pricklypear_common.Branch.name))
          bs
      in
      (Pricklypear_lisp.Ast.VList names));

  (* (gen-now) -> current tree gen (int). Spec: tree-time.reads-as-of-gen.
     Returns the current branch's generation (migration 017 ancestry-walked
     counter) = the code-path head gen coordinate for this session's branch
     (what a :gen read-as-of coordinate on code paths compares against). A
     write anywhere in the branch's ancestry bumps it, so a descendant sees
     its own writes via inheritance, consistent with how a session resolves
     code-path tree nodes. Unknown/unreachable -> 0 (caller treats as
     "at head / reload"). Pure read, no gen allocation. *)
  Primitives.register tbl "gen-now"
    (fun _args ->
      let bid = Store.current_branch_id t.store in
      let g =
        match Store.current_generation t.store ~branch_id:bid with
        | Some n -> n
        | None -> 0L
      in
      (Pricklypear_lisp.Ast.VInt g));

  (* (gen-read path gen) -> node body at gen, crossed over the same branch
     lens rules head reads use (tree-time.reads-as-of-gen). path may be an
     absolute b.*/data.* path or a RELATIVE logical suffix (kind.name)
     resolved closest-wins through the branch lens chain. gen is the as-of
     coordinate; 0/omitted means head. Returns version JSON (the version_to_json
     surface: id/path/kind/type/body/author/hash/created_at/gen/tombstone) or
     nil when the path is unversioned at that gen. ReadDb cap. *)
  register_crud "gen-read"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt g
        ] ->
          Crud.tree_gen_read t.store ~path ~gen:g
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("gen-read", "need path gen", "bad-args")))));

  (* (gen-diff path gen-a gen-b) -> node-granular diff: reads both leaves
     via the versioned store (hashes first, walk on mismatch) and returns
     "same" when hash-equal, else a JSON {path,a,b,same,delta}. ReadDb. *)
  register_crud "gen-diff"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt ga
        ; Pricklypear_lisp.Ast.VInt gb
        ] ->
          Crud.tree_gen_diff t.store ~path ~gen_a:ga ~gen_b:gb
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("gen-diff",
                        "need path gen-a gen-b", "bad-args")))));

  (* (undo path [n]) -> restore value at head-n as a NEW write at head+1
     (restore-by-write, so the undo itself is versioned and redoable).
     Supports datum paths (data-prefixed; the documents' first consumer is notes
     undo). Code paths (b-prefixed) report the code-restore consumer deferral.
     WriteDb cap (it appends a version row). Returns the written path or an
     error string. *)
  register_crud "undo"
    (fun args ->
      (* i-1788669842: undo is a write (restore-by-write appends a
         version row) and joins the create/update/delete/tree-* anon
         gate. RLS floors the datum path via pp.user, but that floor
         opens whenever pp.user is unset (scripts/admin paths) - the
         explicit gate keeps the write surface consistent. *)
      if (actor ()) = "anonymous" then deny_anon "undo"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr path ] ->
            Crud.tree_undo t.store ~path ()
        | [ Pricklypear_lisp.Ast.VStr path
          ; Pricklypear_lisp.Ast.VInt n
          ] ->
            Crud.tree_undo t.store ~path ~n ()
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error
                      (Pricklypear_lisp.Ast.Prim_error
                         ("undo", "need path [, n]", "bad-args")))));

  (* (code-restore name [n] [owner/branch]) -> restore a code node (fn/type/
     route) at head-n as a NEW write at head+1 (restore-by-write). The
     code-path analogue of (undo) (which is datum-path only): gen-read + write
     the same value back via the code write surface (upsert_fn / upsert_type /
     upsert_route), so the reverse mirror re-populates function_defs /
     type_defs / view_bindings and the restore itself is versioned + redoable
     (the restored value becomes the new head). n default 1 = the state just
     before the latest change. Clean error strings (never a crash). WriteDb
     cap. Consumers: tree-time.consumers — code-restore = gen-read + define. *)
  register_crud "code-restore"
    (fun args ->
      (* i-1788669791: code-restore writes fn/type/route rows, and RLS
         deliberately does NOT cover non-datum kinds (migration 013),
         so it is gated like the rest of the code write surface: no
         anonymous calls, and the target branch ref must pass the
         branch-owner rule (admin-only for system/*, own tenant
         otherwise) BEFORE the lookup. Closes the promote bypass: an
         anonymous or cross-tenant identity can no longer time-travel
         a promoted handler on system/main. Bare refs default to
         system/<name>, matching checkout/fork. *)
      if (actor ()) = "anonymous" then deny_anon "code-restore"
      else
      let current_branch_id () = Store.current_branch_id t.store in
      let resolve_ref (branch_ref : string) =
        let owner, name =
          Session_control_prims.split_branch_ref branch_ref
        in
        if not (Session_control_prims.branch_owner_authorized t owner)
        then `Denied (branch_ref, owner)
        else
          (match
             Store.lookup_branch_id_by_name t.store ~owner ~name
           with
           | Some id -> `Found id
           | None -> `Missing branch_ref)
      in
      let err = "code-restore: need name [n] [owner/branch]" in
      let bad_branch ref_opt =
        match ref_opt with
        | None -> (Pricklypear_lisp.Ast.VStr err)
        | Some branch_ref ->
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "code-restore: no such branch: %s" branch_ref))
      in
      let denied_branch ref owner =
        (Pricklypear_lisp.Ast.VStr
           (Printf.sprintf "denied: branch %s is owned by %s" ref owner))
      in
      (* Attribute to the per-request identity, not the live manifest
         (mid-swap under the cotenant race), same as create/update. *)
      let default_author = actor () in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let bid = current_branch_id () in
          if bid = "" then bad_branch None
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.code_restore t.store ~branch_id:bid ~name
                  ~author:default_author ()))
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64 ] ->
          let bid = current_branch_id () in
          if bid = "" then bad_branch None
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.code_restore t.store ~branch_id:bid ~name
                  ~author:default_author ~n:(Int64.to_int n64) ()))
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VStr branch_ref
        ] ->
          (match resolve_ref branch_ref with
           | `Found bid ->
               (Pricklypear_lisp.Ast.VStr
                  (Store.code_restore t.store ~branch_id:bid ~name
                     ~author:default_author ()))
           | `Denied (r, o) -> denied_branch r o
           | `Missing r -> bad_branch (Some r))
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64
        ; Pricklypear_lisp.Ast.VStr branch_ref
        ] ->
          (match resolve_ref branch_ref with
           | `Found bid ->
               (Pricklypear_lisp.Ast.VStr
                  (Store.code_restore t.store ~branch_id:bid ~name
                     ~author:default_author ~n:(Int64.to_int n64) ()))
           | `Denied (r, o) -> denied_branch r o
           | `Missing r -> bad_branch (Some r))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("code-restore", err, "bad-args")))));

  (* (propose-function 'name "spec text") -> VStr name | error string.
     Calls OpenAI-compat LLM, defines the function on the CURRENT branch
     with author = Agent_tools.agent:<model> (AI provenance). Never merges to system. *)
  Primitives.register tbl "propose-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec ] ->
          Agent_tools.propose_function t ~name ~spec
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("propose-function", "bad args (need name-str, spec-str)", "bad-args"))))));

  (* (install-function name body-src) -> install AI-authored body (effect gate). *)
  Primitives.register tbl "install-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr body ] ->
          Agent_tools.install_ai_function t ~name ~body_src:body
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("install-function", "need name-str, body-str", "bad-args"))))));

  (* (function-source name [owner/branch]) -> VStr pretty body | VNil
     Reads the branch's effective body over its own chain WITHOUT checkout
     (branching.branch-resolved-reads). A denied/missing ref returns a clean
     error string. *)
  Primitives.register tbl "function-source"
    (fun args ->
      let fetch name = function
        | None ->
            (match Agent_tools.function_source t name with
             | Some s -> (Pricklypear_lisp.Ast.VStr s)
             | None -> Pricklypear_lisp.Ast.VNil)
        | Some branch_ref ->
            (match branch_read_scope (Some branch_ref) with
             | `Found (bid, _) ->
                 (match Agent_tools.function_source_at t ~branch_id:bid name with
                  | Some s -> (Pricklypear_lisp.Ast.VStr s)
                  | None -> Pricklypear_lisp.Ast.VNil)
             | `Denied (r, o) ->
                 (Pricklypear_lisp.Ast.VStr
                    (Printf.sprintf "denied: branch %s is owned by %s" r o))
             | `Missing r ->
                 (Pricklypear_lisp.Ast.VStr
                    (Printf.sprintf "function-source: no such branch: %s" r))
             | `Current _ -> assert false)
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] -> fetch name None
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          fetch name (Some branch_ref)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("function-source", "need name-str [owner/branch]", "bad-args")))));

  (* (find-code needle [limit] [owner/branch]) -> VList of name strings.
     Postgres ILIKE over effective function_defs.body::text (branch chain).
     The optional branch ref reads a fork's chain without checkout; a
     non-numeric string arg is taken as the ref (a ref contains '/'). *)
  Primitives.register tbl "find-code"
    (fun args ->
      let names_of ~branch_id needle limit =
        let ns =
          match branch_id with
          | None -> Store.search_function_bodies t.store ~needle ~limit
          | Some bid ->
              Store.search_function_bodies_at t.store ~branch_id:bid ~needle ~limit
        in
        Pricklypear_lisp.Ast.VList
          (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) ns)
      in
      let with_ref needle limit branch_ref =
        match branch_read_scope branch_ref with
        | `Current bid -> names_of ~branch_id:(Some bid) needle limit
        | `Found (bid, _) -> names_of ~branch_id:(Some bid) needle limit
        | `Denied (r, o) ->
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "denied: branch %s is owned by %s" r o))
        | `Missing r ->
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "find-code: no such branch: %s" r))
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] -> names_of ~branch_id:None needle 200
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VInt n ] ->
          names_of ~branch_id:None needle (Int64.to_int n)
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VStr n_s ] ->
          (match int_of_string_opt n_s with
           | Some n -> names_of ~branch_id:None needle n
           | None -> with_ref needle 200 (Some n_s))
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VInt n
        ; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          with_ref needle (Int64.to_int n) (Some branch_ref)
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VStr n_s
        ; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          (match int_of_string_opt n_s with
           | Some n -> with_ref needle n (Some branch_ref)
           | None ->
               (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-code", "limit must be int", "bad-args")))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-code", "need needle-str [limit] [owner/branch]", "bad-args")))));

  (* (function-doc name) -> VStr docstring | VNil
     Store metadata first; special-form / prim blurbs as fallback. *)
  Primitives.register tbl "function-doc"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let doc = Store.lookup_function_doc t.store name in
          (match doc with
           | Some s -> (Pricklypear_lisp.Ast.VStr s)
           | None ->
               (match Pricklypear_lisp.Special_forms.find name with
                | Some e ->
                    (Pricklypear_lisp.Ast.VStr e.summary)
                | None ->
                    let blurb = Primitives.prim_summary name in
                    if blurb = "built-in primitive" then
                      Pricklypear_lisp.Ast.VNil
                    else (Pricklypear_lisp.Ast.VStr blurb)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("function-doc", "need name-str", "bad-args")))));

  (* (function-ast name [owner/branch]) -> quoted body value tree | VNil.
     Does not eval the body; reifies the stored AST via quote_to_value.
     Optional ref reads a fork without checkout. *)
  Primitives.register tbl "function-ast"
    (fun args ->
      let fetch name = function
        | None ->
            (match Agent_tools.function_ast t name with
             | Some v -> v
             | None -> Pricklypear_lisp.Ast.VNil)
        | Some branch_ref ->
            (match branch_read_scope (Some branch_ref) with
             | `Found (bid, _) ->
                 (match Agent_tools.function_ast_at t ~branch_id:bid name with
                  | Some v -> v
                  | None -> Pricklypear_lisp.Ast.VNil)
             | `Denied (r, o) ->
                 (Pricklypear_lisp.Ast.VStr
                    (Printf.sprintf "denied: branch %s is owned by %s" r o))
             | `Missing r ->
                 (Pricklypear_lisp.Ast.VStr
                    (Printf.sprintf "function-ast: no such branch: %s" r))
             | `Current _ -> assert false)
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] -> fetch name None
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          fetch name (Some branch_ref)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("function-ast", "need name-str [owner/branch]", "bad-args")))));

  (* ===== Forge prims ===== *)

  (* (forge-effects "src-or-name") -> VStr badge string.
     Exposes lisp/lib/effect.ml infer over a parsed source expression
     (or a named binding's body). The SAME analysis install-function
     gates on; this prim merely exposes it for the forge mechanical
     flaggers (capability escalation check). Read-only, pure-ish.
     Spec: borge/forge.borg mechanical-flags. *)
  Primitives.register tbl "forge-effects"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] ->
          (try
             let forms = Pricklypear_lisp.Reader.read_all s in
             (match forms with
              | [] -> (Pricklypear_lisp.Ast.VStr "pure")
              | expr :: _ ->
                  let defs = Store.list_function_names t.store in
                  (* Opaque-body contract (i-1788671083-c): parse at the
                     eval boundary; malformed rows skipped as before. *)
                  let known_fn =
                    Pricklypear_lisp.Effect.known_fn_of_bindings
                      (List.filter_map
                         (fun (n, body, _) ->
                           match Pricklypear_lisp.Ast.expr_of_body_json body with
                           | Some e -> Some (n, e)
                           | None -> None)
                         defs)
                  in
                  let badge =
                    Pricklypear_lisp.Effect.EffSet.badge
                      (Pricklypear_lisp.Effect.infer ~known_fn expr)
                  in
                  (Pricklypear_lisp.Ast.VStr badge))
           with exn ->
             raise (Pricklypear_lisp.Ast.Interp_error
                      (Pricklypear_lisp.Ast.Prim_error
                         ("forge-effects",
                          "parse/infer failed: " ^ Printexc.to_string exn,
                          "failed"))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("forge-effects", "need src-or-name-str", "bad-args")))));

  (* (package-exports pkg) -> VList of public names (package-surface).
     Resolution: declared registry exports -> kernel_std_exports for
     std/kernel -> shallow heuristic. *)
  let kernel_std_exports =
    [ "list/map"; "list/filter"; "list/foldl"; "reverse"; "append"; "nth"
    ; "null?"; "pair?"; "car"; "cdr"; "cons"; "list"; "count"
    ; "string-append"; "string-length"; "substring"; "string-eq"
    ; "string-contains?"; "string-split"; "string-trim"; "html-escape"; "as-str"; "str"
    ; "string-join"
    ; "string<?"; "string<=?"; "string>?"; "string>=?"
    ; "symbol?"; "string?"; "symbol=?"; "symbol->string"; "string->symbol"
    ; "not"; "type-of"; "read-string"; "read-all-string"; "function-ast"
    ; "function-source"; "function-doc"; "apropos"; "apropos-docs"
    ; "describe"; "list-packages"; "package-bindings"; "package-exports"
    ; "packages"; "package-enable"; "package-disable"; "prefix-of"
    ]
  in
  Primitives.register tbl "package-exports"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let pkg = String.lowercase_ascii (String.trim pkg) in
          if pkg = "std" || pkg = "kernel" || pkg = "(kernel)" then
            (Pricklypear_lisp.Ast.VList
                 (List.map
                    (fun n -> Pricklypear_lisp.Ast.VStr n)
                    kernel_std_exports))
          else begin
            let reg = Store.lookup_package t.store pkg in
            match reg with
            | Some r when r.exports <> [] ->
                (Pricklypear_lisp.Ast.VList
                     (List.map
                        (fun n -> Pricklypear_lisp.Ast.VStr n)
                        r.exports))
            | _ ->
                let names = Agent_tools.package_export_names t pkg in
                (Pricklypear_lisp.Ast.VList
                     (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
          end
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-exports", "need package-str", "bad-args")))));

  (* (package-bindings pkg) -> VList of name strings.
     MEMBERSHIP = function_defs.package provenance (the (package …)
     form's stamp) when present, else fallback prefix pkg/ + exact.
     Provenance is required because a package's defines may not share
     its name as a prefix (std defines list/…, html/…, css/…; each
     theme-asNNN defines theme/…), and prefix matching returned []. *)
  Primitives.register tbl "package-bindings"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let names = Agent_tools.package_binding_names t pkg in
          (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-bindings", "need package-str", "bad-args")))));

  (* (list-packages) -> VList of top-level package prefix strings *)
  Primitives.register tbl "list-packages"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          let names = Agent_tools.list_package_names t in
          (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("list-packages", "takes no args", "bad-args"))))));

  (* (packages) -> VList of (name enabled exports-count) rows from registry +
     prefix-discovered packages (enabled=true, exports=0 if unregistered). *)
  Primitives.register tbl "packages"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          let reg = Store.list_packages t.store in
          let prefixes = Agent_tools.list_package_names t in
          let seen = Hashtbl.create 32 in
          let rows = ref [] in
          List.iter
            (fun (r : Store.package_row) ->
              Hashtbl.replace seen r.name ();
              rows :=
                Pricklypear_lisp.Ast.VList
                  [ Pricklypear_lisp.Ast.VStr r.name
                  ; Pricklypear_lisp.Ast.VBool r.enabled
                  ; Pricklypear_lisp.Ast.VInt
                      (Int64.of_int (List.length r.exports))
                  ]
                :: !rows)
            reg;
          List.iter
            (fun p ->
              if not (Hashtbl.mem seen p) then
                rows :=
                  Pricklypear_lisp.Ast.VList
                    [ Pricklypear_lisp.Ast.VStr p
                    ; Pricklypear_lisp.Ast.VBool true
                    ; Pricklypear_lisp.Ast.VInt 0L
                    ]
                  :: !rows)
            prefixes;
          let sorted =
            List.sort
              (fun a b ->
                match a, b with
                | Pricklypear_lisp.Ast.VList (Pricklypear_lisp.Ast.VStr na :: _),
                  Pricklypear_lisp.Ast.VList (Pricklypear_lisp.Ast.VStr nb :: _) ->
                    String.compare na nb
                | _ -> 0)
              !rows
          in
          (Pricklypear_lisp.Ast.VList sorted)
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("packages", "takes no args", "bad-args"))))));

  let protected_pkg name =
    let n = String.lowercase_ascii (String.trim name) in
    n = "std" || n = "kernel" || n = "(kernel)"
  in

  (* (package-enable name) — flip enabled=true on current branch + rehydrate *)
  Primitives.register tbl "package-enable"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ]
      | [ Pricklypear_lisp.Ast.VSymbol name ] ->
          let name = String.trim name in
          if name = "" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", "empty name", "bad-args")))))
          else if protected_pkg name then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", name ^ " is always enabled", "denied")))))
          else if t.manifest.username = "anonymous" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", "auth required", "denied")))))
          else begin
            let () =
              Store.set_package_enabled t.store ~name ~enabled:true
                ~author:t.manifest.username
            in
            let () = rehydrate_fn t in
            (Pricklypear_lisp.Ast.VStr ("enabled " ^ name))
          end
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", "need package-str", "bad-args")))));

  (* (package-disable name) — hide package bindings on current branch *)
  Primitives.register tbl "package-disable"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ]
      | [ Pricklypear_lisp.Ast.VSymbol name ] ->
          let name = String.trim name in
          if name = "" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "empty name", "bad-args")))))
          else if protected_pkg name then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "cannot disable " ^ name, "denied")))))
          else if t.manifest.username = "anonymous" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "auth required", "denied")))))
          else begin
            let () =
              Store.set_package_enabled t.store ~name ~enabled:false
                ~author:t.manifest.username
            in
            let () = rehydrate_fn t in
            (Pricklypear_lisp.Ast.VStr ("disabled " ^ name))
          end
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "need package-str", "bad-args")))));

  (* (apropos-docs pat) -> VList of (name . doc) pairs *)
  Primitives.register tbl "apropos-docs"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pat ] ->
          let pairs = Agent_tools.apropos_docs t pat in
          let rows =
            List.map
              (fun (n, d) ->
                Pricklypear_lisp.Ast.VList
                  [ Pricklypear_lisp.Ast.VStr n
                  ; Pricklypear_lisp.Ast.VStr d
                  ])
              pairs
          in
          (Pricklypear_lisp.Ast.VList rows)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("apropos-docs", "need pattern-str", "bad-args")))));

  (* (Agent_tools.describe name-or-pkg) -> VStr selective reveal text *) 
  Primitives.register tbl "describe"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          let s = Agent_tools.describe t needle in
          (Pricklypear_lisp.Ast.VStr s)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "describe: need name-or-package-str", "bad-args")))));

  (* (docs-missing [pkg]) -> VList of visible bindings without docstrings.
     bumpers M5 discovery-surfaces; function-doc () means missing. Related to
     (function-doc name) / (apropos-docs pat): resolves the doc string the same
     way (store doc, else special-form summary, else prim blurb). *) 
  Primitives.register tbl "docs-missing"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          Pricklypear_lisp.Ast.VList
            (List.map (fun n -> Pricklypear_lisp.Ast.VStr n)
               (Agent_tools.docs_missing t ()))
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          Pricklypear_lisp.Ast.VList
            (List.map (fun n -> Pricklypear_lisp.Ast.VStr n)
               (Agent_tools.docs_missing t ~pkg ()))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("docs-missing", "need optional package-str", "bad-args")))));

  (* (app-check "pkg") -> VStr per-route fragment-fit report (bumpers M5,
     fragment-by-default). Advisory classification only - route_gate stays
     about legality, never about shape. *) 
  Primitives.register tbl "app-check"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          (Pricklypear_lisp.Ast.VStr (Agent_tools.app_check t ()))
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          (Pricklypear_lisp.Ast.VStr (Agent_tools.app_check t ~pkg ()))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("app-check", "need package-str", "bad-args")))));

  (* (export-package pkg) -> VStr lib.pp text *)
  Primitives.register tbl "export-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let text = Agent_tools.export_package t pkg in
          (Pricklypear_lisp.Ast.VStr text)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("export-package", "need package-str", "bad-args")))));

  (* (code-history "name") / (code-history "name" "owner/branch")
     → VStr JSON list newest-first. Spec: branching.code-history.
     Reads honor branch_read_scope like code-diff: refs outside the
     session's readable set are denied BEFORE the lookup (authz-matrix
     gap 3, 2026-10-21). *)
  Primitives.register tbl "code-history"
    (fun args ->
      let run name branch_ref_opt =
        (match branch_read_scope branch_ref_opt with
         | `Current bid | `Found (bid, _) ->
             (Pricklypear_lisp.Ast.VStr
                (Store.list_code_history_json t.store ~branch_id:bid ~name))
         | `Denied (r, o) ->
             (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "denied: branch %s is owned by %s" r o))
         | `Missing r ->
             (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "code-history: no such branch: %s" r)))
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] -> run name None
      | [ Pricklypear_lisp.Ast.VStr name
        ; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          run name (Some branch_ref)
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-history", "need name [owner/branch]", "bad-args"))))));

  (* (code-diff name n [owner/branch]) -> VStr text diff of branch-head
     vs nth-newest history. Optional ref reads a fork without checkout. *)
  Primitives.register tbl "code-diff"
    (fun args ->
      let run name n branch_ref =
        (match branch_read_scope branch_ref with
         | `Current bid | `Found (bid, _) ->
             (match Store.code_diff t.store ~branch_id:bid ~name ~n with
              | Ok s -> (Pricklypear_lisp.Ast.VStr s)
              | Error e -> (Pricklypear_lisp.Ast.VStr e))
         | `Denied (r, o) ->
             (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "denied: branch %s is owned by %s" r o))
         | `Missing r ->
             (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "code-diff: no such branch: %s" r)))
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64 ] ->
          run name (Int64.to_int n64) None
      | [ Pricklypear_lisp.Ast.VStr name
        ; Pricklypear_lisp.Ast.VStr n_s ] ->
          (match int_of_string_opt n_s with
           | None ->
               (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-diff", "n must be int", "bad-args"))))
           | Some n -> run name n None)
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64
        ; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          run name (Int64.to_int n64) (Some branch_ref)
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VStr n_s
        ; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          (match int_of_string_opt n_s with
           | None ->
               (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-diff", "n must be int", "bad-args"))))
           | Some n -> run name n (Some branch_ref))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-diff", "need name n [owner/branch]", "bad-args")))));

  (* (diff source target) -> VList of VStr change summaries.
     Spec: branching.diff.  Args: two branch specs (owner/name strings, same
     parsing as checkout/merge/promote).  Returns one summary string per
     divergent row across function_defs + type_defs + view_bindings:
       "<kind>\t<status>\t<name>\t<from-updated>\t<to-updated>"
     Summary level ONLY — the structured AST delta is consumed in-process by
     the /diff page renderer, not surfaced here (a programmatic (diff-ast …)
     prim is explicit v2). *)
  Primitives.register tbl "diff"
    (fun args ->
      let resolve ref_ =
        (* authz-matrix gap 3: diff reads honor branch_read_scope. *)
        match branch_read_scope (Some ref_) with
        | `Found (id, _) -> `Ok id
        | `Denied (r, o) -> `Denied (r, o)
        | `Missing r -> `Missing r
        | `Current _ -> `Missing ref_
      in
      let to_ref = function
        | Pricklypear_lisp.Ast.VStr s -> Some s
        | Pricklypear_lisp.Ast.VSymbol s -> Some s
        | _ -> None
      in
      match args with
      | [ a; b ] ->
          (match to_ref a, to_ref b with
           | Some from_ref, Some to_ref ->
               (match resolve from_ref, resolve to_ref with
                | `Ok from_id, `Ok to_id ->
                    let rows = Store.diff_branches t.store ~from_id ~to_id in
                    let kind_s = function
                      | Store.DiffFn -> "fn"
                      | Store.DiffType -> "type"
                      | Store.DiffRoute -> "route"
                    in
                    let status_s = function
                      | Store.DiffAdded -> "added"
                      | Store.DiffRemoved -> "removed"
                      | Store.DiffModified -> "modified"
                    in
                    let line (r : Store.branch_diff_row) =
                      let fu = Option.value ~default:"-" r.Store.from_updated in
                      let tu = Option.value ~default:"-" r.Store.to_updated in
                      Printf.sprintf "%s\t%s\t%s\t%s\t%s"
                        (kind_s r.Store.diff_kind) (status_s r.Store.diff_status)
                        r.Store.diff_name fu tu
                    in
                    (Pricklypear_lisp.Ast.VList
                       (List.map (fun r -> Pricklypear_lisp.Ast.VStr (line r)) rows))
              | `Denied (r, o), _ | _, `Denied (r, o) ->
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "denied: branch %s is owned by %s" r o))
              | _ ->
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "diff: no such branch pair (%s, %s)"
                        from_ref to_ref)))
           | _ ->
               (raise
                  (Pricklypear_lisp.Ast.Interp_error
                     (Pricklypear_lisp.Ast.Prim_error
                        ("diff", "need source target", "bad-args")))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("diff", "need source target", "bad-args")))));
