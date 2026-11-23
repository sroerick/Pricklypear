(** Boot seeding + branch bootstrap — extracted verbatim from session.ml
    (i-1788671109-1): the additive libs/<pkg> seed (seed_stdlib),
    package discovery (discover_seed_packages / seed_package_priority),
    the living agents-note seed, the personal/agent branch bootstrap,
    and the first-load recipes (recipe_hello/todo/bible/dabar/notes)
    that http.ml's on-demand package faces call.

    Seam: this module cannot depend on Session (Session's make/boot
    funnel calls into it), so the driver operations the seed needs —
    [load_library], [eval_one], [rehydrate], [checkout_to] — arrive as
    an [ops] record supplied by Session. ensure_personal_branch /
    ensure_agent_branch are Store-only and need no ops.

     Behavior contract: byte-identical relocation; seed order, failure
     reporting, and stderr lines are unchanged. branching.seed-policy
     MIRROR (roerick 2026-10-22): the seed now UPSERTS onto system/main
     (persist_function / register_route) instead of insert-if-absent —
     system/main mirrors the code; there is no second system branch. *)

open Session_types

(** The driver seam (see module comment). Session passes its own
    load_library / eval_one / rehydrate / checkout_to here. *)
type ops = {
  load_library : t -> string -> (string, string) result;
  eval_one : t -> string -> Pricklypear_lisp.Ast.value;
  rehydrate : t -> unit;
  checkout_to : t -> branch_id:string -> branch_ref:string -> unit;
}

let seed_one ~(ops : ops) (t : t) (name : string) : string option =
  (* promote-window task 7 (2026-09-04): per-package ms on the existing
     load line — the boot-phase table drills into seed_stdlib here.
     image.seed-failure-loud (i-1788670156): the error is also RETURNED
     so seed_stdlib can list every failed package by name in the boot
     summary, instead of the failure hiding as one stderr line among
     boot noise (libs/list shipped broken for weeks that way). *)
  let t0 = Unix.gettimeofday () in
  let r = (ops.load_library) t name in
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  (match r with
   | Ok msg -> Printf.eprintf "[session] %s: %s (%.0f ms)\n%!" name msg ms
   | Error e ->
       Printf.eprintf
         "[session] SEED FAILURE: %s did not load: %s (%.0f ms) — its \
          defines/routes are missing on this branch\n%!"
         name e ms);
  (match r with Error e -> Some e | Ok _ -> None)

(** Boot seed order (BORGE-1785211333).
    Preferred prefix preserves dependency-ish order (std/ui before notes,
    outline before notes, …). Any other [libs/<name>/lib.pp] is appended
    alphabetically so adding a package no longer requires an OCaml edit. *)
let seed_package_priority : string list =
  (* css before notes: notes/tree-css evaluates (css/sheet …) at define
     time; if css is unbound the notes load aborts mid-file and routes
     like /partials/notes-search never seed (notes.search-modal). *)
  (* theme before ui: default theme/* tokens exist before ui/css first runs. *)
  [ "std"; "theme"; "weather"; "ui"; "layout"; "welcome"; "ai"; "css"; "html"
  ; "lookbook" (* before chat: libs/chat redefines lookbook/elements against
                  the captured lib seeds, so lookbook must load first. *)
  ; "outline"; "structed"; "notes"; "signup"; "todo"; "contacts"
  ; "plan"; "chat"; "bookmarks"; "calendar"; "workout"; "flashcards"
  ; "bible"; "dabar"; "bi"; "ref"; "verify" ]

(** Pure: apply a profile to the full discovery list (spec_town_profile).
    Canonical priority order is kept for preferred names (std first,
    css before notes, theme before ui); profile-only extras follow in
    FILE order, so the profile author controls extra dep order. *)
let apply_profile ~(profile : string list) : string list =
  let in_profile n = List.mem n profile in
  let preferred = List.filter in_profile seed_package_priority in
  let extras = List.filter (fun n -> not (List.mem n preferred)) profile in
  preferred @ extras

let packages_in_root (root : string) : string list =
  try
    Sys.readdir root
    |> Array.to_list
    |> List.filter (fun name ->
           name <> ""
           && name.[0] <> '_'
           && name.[0] <> '.'
           (* Alternate skins (theme-paper, …) load on demand via
              theme/set → load-library; seeding them would only
              thrash in-memory theme/* before rehydrate. *)
           && not (String.starts_with ~prefix:"theme-" name)
           &&
           let pp =
             Filename.concat root (Filename.concat name "lib.pp")
           in
           Sys.file_exists pp)
    |> List.sort String.compare
  with _ -> []

let full_seed_packages () : string list =
  let preferred = seed_package_priority in
  (* Vendored packages are a second root (see Loader.libs_dirs): core
     names keep their explicit priority, everything else is appended
     alphabetically across all roots. *)
  let discovered =
    List.concat_map packages_in_root (Loader.libs_dirs ())
    |> List.sort_uniq String.compare
  in
  let extras =
    List.filter (fun n -> not (List.mem n preferred)) discovered
  in
  preferred @ extras

(** Resolved boot order from package manifests.

    [candidates] is discovery order (the preferred list, then extras
    alphabetically). Each candidate's [lib.pp] may carry a
    [(package …)] manifest with [(requires …)]. We topologically sort,
    using candidate order as the tiebreak, and return failures for
    packages that cannot be seeded safely: a manifest that does not
    parse or names a different package, a missing requirement (and its
    dependents, transitively), or a dependency cycle. A package with no
    manifest is a leaf with no requirements — backward compatible with
    the packages that predate the manifest. *)
type seed_plan = {
  order : string list;
  failures : (string * string) list;
}

let manifest_for (name : string)
    : (Pricklypear_lisp.Package_manifest.t, string) result =
  match Loader.read_file (Loader.library_path name) with
  | Error e -> Error e
  | Ok src -> (
      match Pricklypear_lisp.Package_manifest.parse_source src with
      | Ok None -> Ok (Pricklypear_lisp.Package_manifest.empty name)
      | Ok (Some m) -> Ok m
      | Error e -> Error e)

let resolve_seed_order (candidates : string list) : seed_plan =
  let rank = Hashtbl.create 128 in
  List.iteri (fun i n -> Hashtbl.replace rank n i) candidates;
  let cmp a b =
    let ra = try Hashtbl.find rank a with Not_found -> max_int in
    let rb = try Hashtbl.find rank b with Not_found -> max_int in
    if ra <> rb then compare ra rb else String.compare a b
  in
  let man = Hashtbl.create 128 in
  let failures = ref [] in
  let fail name msg = failures := (name, msg) :: !failures in
  List.iter
    (fun name ->
       match manifest_for name with
       | Error e -> fail name ("manifest: " ^ e)
       | Ok m ->
           if m.Pricklypear_lisp.Package_manifest.name <> name then
             fail name
               (Printf.sprintf "manifest names %s, directory is %s"
                  m.Pricklypear_lisp.Package_manifest.name name)
           else Hashtbl.replace man name m)
    candidates;
  (* Fixed point: remove packages with missing/self requirements, then
     remove their dependents. A package that cannot be seeded must not
     leave a caller in the order behind. *)
  let present = Hashtbl.create 128 in
  Hashtbl.iter (fun n _ -> Hashtbl.replace present n ()) man;
  let changed = ref true in
  while !changed do
    changed := false;
    let doomed = ref [] in
    Hashtbl.iter
      (fun name (m : Pricklypear_lisp.Package_manifest.t) ->
         if Hashtbl.mem present name then
           List.iter
             (fun dep ->
                if dep = name then
                  doomed := (name, "package requires itself") :: !doomed
                else if not (Hashtbl.mem present dep) then
                  doomed :=
                    (name,
                     Printf.sprintf "requires unavailable package %s" dep)
                    :: !doomed)
             m.requires)
      man;
    List.iter
      (fun (name, msg) ->
         if Hashtbl.mem present name then begin
           Hashtbl.remove present name;
           fail name msg;
           changed := true
         end)
      !doomed
  done;
  (* Kahn's algorithm, with candidate order as the ready-set tiebreak. *)
  let indeg = Hashtbl.create 128 in
  let dependents = Hashtbl.create 128 in
  Hashtbl.iter
    (fun name (m : Pricklypear_lisp.Package_manifest.t) ->
       if Hashtbl.mem present name then begin
         let deps =
           List.filter (fun d -> d <> name && Hashtbl.mem present d) m.requires
           |> List.sort_uniq String.compare
         in
         Hashtbl.replace indeg name (List.length deps);
         List.iter
           (fun d ->
              let xs =
                try Hashtbl.find dependents d with Not_found -> []
              in
              Hashtbl.replace dependents d (name :: xs))
           deps
       end)
    man;
  let ready = ref [] in
  let add_ready n =
    if Hashtbl.mem indeg n && Hashtbl.find indeg n = 0 then begin
      Hashtbl.remove indeg n;
      let rec ins = function
        | [] -> [ n ]
        | x :: xs -> if cmp n x < 0 then n :: x :: xs else x :: ins xs
      in
      ready := ins !ready
    end
  in
  Hashtbl.iter (fun n _ -> add_ready n) (Hashtbl.copy indeg);
  let order = ref [] in
  while !ready <> [] do
    match !ready with
    | [] -> ()
    | n :: rest ->
        ready := rest;
        order := n :: !order;
        List.iter
          (fun d ->
             if Hashtbl.mem indeg d then begin
               let k = Hashtbl.find indeg d - 1 in
               if k <= 0 then begin
                 Hashtbl.replace indeg d 0;
                 add_ready d
               end else Hashtbl.replace indeg d k
             end)
          (try Hashtbl.find dependents n with Not_found -> [])
  done;
  let order = List.rev !order in
  (* Anything left has indegree > 0: a cycle, or a package that depends
     on one. Exclude the whole unresolved set and name it. *)
  let remaining =
    Hashtbl.fold (fun n _ acc -> n :: acc) indeg [] |> List.sort cmp
  in
  if remaining <> [] then
    List.iter
      (fun n ->
         fail n
           (Printf.sprintf "dependency cycle/unresolved (with: %s)"
              (String.concat " " (List.filter (fun x -> x <> n) remaining))))
      remaining;
  { order; failures = List.rev !failures }

(** Boot seed set. No PP_PROFILE: full discovery, byte-identical to the
    pre-profile behavior. PP_PROFILE set: the profile file IS the seed
    set (and the tenant load-library allowlist). A profile that cannot
    be read or validates to an empty list FAILS CLOSED — nothing is
    seeded (a public box must never fall back to the full organ set);
    admin sessions keep load-library and can reseed after a fix. *)
let discover_seed_packages () : string list =
  match Profile.packages () with
  | Error e ->
      Printf.eprintf
        "[session] PROFILE FAILURE: %s — seeding NOTHING (fix profiles/ or \
         unset PP_PROFILE)\n%!"
        e;
      []
  | Ok profile -> (
      match Profile.name () with
      | None -> full_seed_packages ()
      | Some n ->
          if profile = [] then (
            Printf.eprintf "[session] PROFILE: %s is empty — seeding \
                           nothing\n%!"
              n;
            [])
          else apply_profile ~profile)

(** Ensure living agents note exists on system/main. Called from
    Session.boot_personal AFTER checkout_personal's rehydrate: with the
    load-skip map active (unchanged defines bind via rehydrate, not the
    seed eval), the seed needs notes/doc/get + roam/reindex actually
    bound — mid-seed placement failed with 'unbound: notes/doc/get' on
    the 2026-09-13 town boot. Idempotent: skip if slug agents is
    already present. *)
let seed_agents_note ~(ops : ops) (t : t) : unit =
  let body =
    String.concat "\n"
      [ "# Agents (living AGENTS.md)"
      ; ""
      ; "This note is the habitat AGENTS.md. Edit with notes/doc/upsert."
      ; ""
      ; "## Entry path"
      ; "1. Public bootstrap (no secrets): GET /agent.txt and GET /.well-known/agent.json"
      ; "2. Those docs explain HTTP Basic auth and how to build a REPL client (POST /api/eval)."
      ; "3. Local helper: pp-eval CLI or curl with Basic auth."
      ; "4. Then explore live:"
      ; "   (whoami)"
      ; "   (list-packages)"
      ; "   (load-library \"todo\")"
      ; "   (describe \"todo\")"
      ; "   (apropos-docs \"todo/item\")"
      ; "   (function-doc \"todo/item/add\")"
      ; "   (function-source \"todo/item/add\")"
      ; ""
      ; "## Selective discovery"
      ; "- (list-packages) — top-level package prefixes"
      ; "- (package-bindings \"pkg\") — all names under pkg/ (any depth)"
      ; "- (package-exports \"pkg\") — shallow pkg or pkg/local names"
      ; "- (apropos \"pat\") / (apropos-docs \"pat\") — search names (+ docs)"
      ; "- (describe \"name-or-pkg\") — one-shot orientation"
      ; "- Prefer DATA builders from /api/eval; */page handlers need a real HTTP request."
      ; ""
      ; "## Libraries"
      ; "Load before use: std ui notes todo outline bible dabar chat calendar"
      ; "flashcards bookmarks contacts plan bi weather ref verify ai layout welcome."
      ]
  in
  (* Escape body for a Lisp string literal. *)
  let escaped =
    let b = Buffer.create (String.length body + 32) in
    String.iter
      (function
        | '\\' -> Buffer.add_string b "\\\\"
        | '"' -> Buffer.add_string b "\\\""
        | '\n' -> Buffer.add_string b "\\n"
        | c -> Buffer.add_char b c)
      body;
    Buffer.contents b
  in
  (* i-20260914-promote-rollback: probe EXISTENCE with notes/doc/row (one
     tree find-by), never notes/doc/get — the full decode is the known
     155-query walk (~60-80s on prod's big notes under DB degradation),
     and this expression runs SYNCHRONOUSLY in boot_personal. The 2026-09-14
     promote died at the 30s health gate exactly here: town-p1 moved
     seed_agents_note after checkout rehydrate, so the notes/doc/get call
     ran for real for the first time (before, mid-seed it short-circuited
     'unbound: notes/doc/get'), boot-personal hit 78s, gate rolled back.
     Contract with gregor's move: notes/doc/row is in the notes package,
     so it is bound after checkout rehydrate exactly like notes/doc/get.
     Only the upsert branch decodes nothing — doc/get is not needed for
     existence. *)
  let expr =
    Printf.sprintf
      {| (if (null? (notes/doc/row "agents"))
            (notes/doc/upsert "agents" "Agents (living AGENTS.md)"
              (outline/node* "root" "Agents (living AGENTS.md)" "%s" "" (list))
              "" "note" "")
            "agents-note-exists") |} 
      escaped
  in
  (try
      let v = (ops.eval_one) t expr in
      Printf.eprintf "[session] agents-note: %s\n%!"
        (Pricklypear_lisp.Reader.pp_value v);
      ()
    with exn ->
      Printf.eprintf "[session] agents-note seed: %s\n%!"
        (Agent_tools.pp_interp_exn exn);
      ())

  let seed_stdlib ~(ops : ops) (t : t) : unit =
    (* Package defaults always land on system/main — NEVER on the user's
       personal branch. Personal inherits main via the recursive CTE; user
       (define)s write overrides on personal and survive reseed/deploy.

       branching.seed-policy MIRROR (roerick 2026-10-22): boot seed
       UPSERTS every define/route from libs/<pkg>/lib.pp onto system/main
       (persist_function / register_route — the old OPTION A
       insert-if-absent policy and its seed_additive flag are retired),
       so a deploy's code changes land on main at the next boot without
       an admin (reseed-package). There is no second system branch.
       Personal branches stay the override layer: persist_function's
       no-diff-no-pin rule keeps an identical body from pinning, and a
       user's own define wins by closest-chain.

       Warm-boot cost: load_skip_names (the same fingerprint map
       interactive load-library uses) is built ONCE below from the
       chain-visible rows on system/main; eval_form skips defines whose
       normalized body + doc match, so a warm boot re-evals only what a
       deploy changed. This replaces the additive era's body-agnostic
       seed_names skip (begin_seed_names / end_seed_names are gone).

       suppress_refresh_on_define: the eval_one define-interceptor would
       otherwise fire refresh_ui_shell when ui/nav is defined mid-seed
       (before notes/capture/nav is bound) and fail with
       `unbound: notes/capture/nav`. The boot sequence does one explicit
       refresh_ui_shell AFTER all packages are loaded. *)
    Store.with_branch t.store ~branch_id:Store.system_main_id (fun () ->
      t.ui_shell.suppress_refresh_on_define <- true;
      t.seed_boot <- true;
      (* Mirror-skip map: name -> fingerprint (pp_expr body ^ "\\x00" ^
         doc) of every chain-visible fn row on system/main, built ONCE
         for the seed window. load_library leaves it alone while
         seed_boot is set (session.ml) and our finally restores it. *)
      let tbl = Hashtbl.create 1024 in
      List.iter
        (fun (name, body_json, _author, _pkg, doc, _branch, _updated) ->
          match Pricklypear_lisp.Ast.expr_of_body_json body_json with
          | None -> ()  (* malformed row: keep full eval *)
          | Some body ->
              Hashtbl.replace tbl name
                (Pricklypear_lisp.Reader.pp_expr body ^ "\\x00"
                 ^ (match doc with Some d -> d | None -> "")))
        (Store.list_function_names_for_load t.store);
      let prev_skip = t.eval_state.load_skip_names in
      t.eval_state.load_skip_names <- Some tbl;
      let started = Unix.gettimeofday () in
      (* image.seed-failure-loud (i-1788670156): collect per-package load
         failures so the boot-seed summary names them loudly. *)
      let failed : string list ref = ref [] in
      let record_failure name = function
        | None -> ()
        | Some e -> failed := (name ^ ": " ^ e) :: !failed
      in
      (* Resolve manifests before the finally closure so the summary
         names the actual seed set and dependency failures land in the
         same loud BOOT SEED FAILURES line as load failures. *)
      let candidates = discover_seed_packages () in
      let plan =
        try resolve_seed_order candidates
        with exn ->
          (* Never leave seed_boot / load-skip state dirty because a
             manifest read blew up; fall back to discovery order and let
             load_library report the package failure. *)
          Printf.eprintf
            "[session] seed order failed: %s — using discovery order\n%!"
            (Printexc.to_string exn);
          { order = candidates; failures = [] }
      in
      let finally () =
        t.eval_state.load_skip_names <- prev_skip;
        t.ui_shell.suppress_refresh_on_define <- false;
        t.seed_boot <- false;
        let elapsed_ms =
          int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
        Printf.eprintf "[session] boot seed: %d packages, %d ms%s\n%!"
          (List.length plan.order) elapsed_ms
          (match !failed with
           | [] -> ""
           | xs ->
               Printf.sprintf
                 "\n[session] *** BOOT SEED FAILURES (%d): %s — those \
                  packages' defines/routes are MISSING; fix libs/<pkg> \
                  or plugins/<pkg> and reseed ***"
                 (List.length xs) (String.concat " | " xs))
      in
      Fun.protect ~finally (fun () ->
          List.iter
            (fun (name, e) -> record_failure name (Some e))
            plan.failures;
          Printf.eprintf "[session] seed_stdlib packages (%d/%d): %s\n%!"
            (List.length plan.order) (List.length candidates)
            (String.concat " " plan.order);
          let rec go = function
            | [] -> ()
            | name :: rest ->
                let () = record_failure name (seed_one ~ops t name) in
                go rest
          in
          go plan.order))

(** Ensure username/personal exists as an EMPTY child of system/main.
    Inheritance (not copy) means main defaults stay visible; personal
    only stores the user's overrides. *)
let ensure_personal_branch (t : t) : string option =
  let owner = t.manifest.username in
  let main_id_opt =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  let parent_id =
    match main_id_opt with
    | Some id -> id
    | None -> Store.system_main_id
  in
  Store.ensure_branch t.store ~parent_id ~owner ~name:"personal"

let checkout_personal ~(ops : ops) (t : t) : unit =
  let owner = t.manifest.username in
  let branch_ref = owner ^ "/personal" in
  let found =
    Store.lookup_branch_id_by_name t.store ~owner ~name:"personal"
  in
  match found with
  | None ->
      Printf.eprintf
        "[session] personal branch missing; staying on %s\n%!"
        t.manifest.branch_name;
      (* Still rehydrate whatever branch we have. *)
      (ops.rehydrate) t
  | Some id ->
      Printf.eprintf "[session] checkout %s (%s)\n%!" branch_ref id;
      (ops.checkout_to) t ~branch_id:id ~branch_ref

(** Agent branch: owner = parent human's username, name = "agent".
    Sibling of the parent's personal; agent boots here, not in personal. *)
let ensure_agent_branch (t : t) ~(parent_username : string)
  : string option =
  let main_id_opt =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  let parent_id =
    match main_id_opt with
    | Some id -> id
    | None -> Store.system_main_id
  in
  Store.ensure_branch t.store ~parent_id ~owner:parent_username ~name:"agent"

let checkout_agent ~(ops : ops) (t : t) ~(parent_username : string) : unit =
  let branch_ref = parent_username ^ "/agent" in
  let found =
    Store.lookup_branch_id_by_name t.store ~owner:parent_username
      ~name:"agent"
  in
  match found with
  | None ->
      Printf.eprintf
        "[session] agent branch missing; staying on %s\n%!"
        t.manifest.branch_name;
      (ops.rehydrate) t
  | Some id ->
      Printf.eprintf "[session] checkout %s (%s)\n%!" branch_ref id;
      (ops.checkout_to) t ~branch_id:id ~branch_ref

let recipe_hello ~(ops : ops) (t : t) : (string, string) result =
  (ops.load_library) t "hello"

let recipe_todo ~(ops : ops) (t : t) : (string, string) result =
  (ops.load_library) t "todo"

let recipe_bible ~(ops : ops) (t : t) : (string, string) result =
  (* Types + API + /bible only — corpus via scripts/seed_bible_bsb.py *)
  (ops.load_library) t "bible"

let recipe_dabar ~(ops : ops) (t : t) : (string, string) result =
  (* Study layer — requires bible/*; Strong's/interlinear via seed scripts *)
  let r = (ops.load_library) t "bible" in
  (match r with
   | Error e -> (Error e)
   | Ok _ -> (ops.load_library) t "dabar")

let recipe_notes ~(ops : ops) (t : t) : (string, string) result =
  (* Outline-first notes — outline pure lib then notes package *)
  let r = (ops.load_library) t "outline" in
  (match r with
   | Error e -> (Error e)
   | Ok _ -> (ops.load_library) t "notes")
