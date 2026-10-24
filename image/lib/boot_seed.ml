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

let full_seed_packages () : string list =
  let preferred = seed_package_priority in
  let root = Loader.libs_dir () in
  let discovered =
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
  in
  let extras =
    List.filter (fun n -> not (List.mem n preferred)) discovered
  in
  preferred @ extras

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
      let finally () =
        t.eval_state.load_skip_names <- prev_skip;
        t.ui_shell.suppress_refresh_on_define <- false;
        t.seed_boot <- false;
        let elapsed_ms =
          int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
        Printf.eprintf "[session] boot seed: %d packages, %d ms%s\n%!"
          (List.length (discover_seed_packages ())) elapsed_ms
          (match !failed with
           | [] -> ""
           | xs ->
               Printf.sprintf
                 "\n[session] *** BOOT SEED FAILURES (%d): %s — those \
                  packages' defines/routes are MISSING; fix libs/<pkg> \
                  and reseed ***"
                 (List.length xs) (String.concat " | " xs))
      in
      Fun.protect ~finally (fun () ->
          let pkgs = discover_seed_packages () in
          Printf.eprintf "[session] seed_stdlib packages (%d): %s\n%!"
            (List.length pkgs) (String.concat " " pkgs);
          let rec go = function
            | [] -> ()
            | name :: rest ->
                let () = record_failure name (seed_one ~ops t name) in
                go rest
          in
          go pkgs))

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
