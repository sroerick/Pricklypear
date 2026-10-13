(** Capability effects for Pricklypear Lisp programs.

    Separate from Lwt async plumbing and from OCaml 5 algebraic effects.
    See spec [lisp-interpreter.effects].

    Effects label what a term *needs* (capabilities), not when it runs. *)

open Ast

(** [effect] is an OCaml 5 keyword; use [eff]. *)
type eff =
  | Pure
  | ReadDb
  | WriteDb
  | Branch
  | Publish
  | Ai
  | Html
  | Txn
  | Net

(** Canonical string name for an [eff]. Inverse of [eff_of_string]. *)
let eff_to_string = function
  | Pure -> "pure"
  | ReadDb -> "read-db"
  | WriteDb -> "write-db"
  | Branch -> "branch"
  | Publish -> "publish"
  | Ai -> "ai"
  | Html -> "html"
  | Txn -> "txn"
  | Net -> "net"

(** Parse a string into an [eff]; [None] if the name is unknown. Inverse of [eff_to_string]. *)
let eff_of_string = function
  | "pure" -> Some Pure
  | "read-db" -> Some ReadDb
  | "write-db" -> Some WriteDb
  | "branch" -> Some Branch
  | "publish" -> Some Publish
  | "ai" -> Some Ai
  | "html" -> Some Html
  | "txn" -> Some Txn
  | "net" -> Some Net
  | _ -> None

(** Total order on effects (0..8) used to sort sets: higher rank = stronger capability. *)
let eff_rank = function
  | Pure -> 0
  | Html -> 1
  | ReadDb -> 2
  | WriteDb -> 3
  | Txn -> 4
  | Publish -> 5
  | Branch -> 6
  | Ai -> 7
  | Net -> 8

module EffSet = struct
  type t = eff list  (* unique, sorted by rank then name *)

  (** The empty effect set (no capabilities). *)
  let empty : t = []

  (** Sort and dedupe an effect list: drop [Pure], order by rank then name. *)
  let normalize (xs : eff list) : t =
    let xs = List.filter (function Pure -> false | _ -> true) xs in
    let xs =
      List.sort_uniq
        (fun a b ->
          let c = compare (eff_rank a) (eff_rank b) in
          if c <> 0 then c else String.compare (eff_to_string a) (eff_to_string b))
        xs
    in
    xs

  (** Effect set containing exactly [e] (normalized). *)
  let singleton e = normalize [ e ]

  (** Set union, dropping [Pure] and deduping by rank. *)
  let union a b = normalize (a @ b)

  (** Fold [union] over a list of effect sets. *)
  let unions sets = List.fold_left union empty sets

  (** Membership test (linear scan over the normalized list). *)
  let mem e set = List.mem e set

  (** [true] when the set holds no effects. *)
  let is_empty = function [] -> true | _ -> false

  (** [true] when the set is empty (no capabilities required). *)
  let is_pure = is_empty

  (** [true] when every effect of [a] is also in [b]. *)
  let subset (a : t) (b : t) : bool =
    List.for_all (fun e -> mem e b) a

  (** The normalized effect list backing this set. *)
  let to_list s = s

  (** Human-readable rendering: ["pure"] or effects joined by ["+"]. *)
  let pp (s : t) : string =
    if is_pure s then "pure"
    else String.concat "+" (List.map eff_to_string s)

  (** Short UI badge string; collapses to a coarse capability label when one effect dominates. *)
  let badge (s : t) : string =
    if is_pure s then "pure"
    else if mem Ai s then "ai"
    else if mem Publish s then "page"
    else if mem Branch s then "branch"
    else if mem Net s then "net"
    else if mem WriteDb s then "db-write"
    else if mem ReadDb s then "db-read"
    else if mem Html s then "html"
    else pp s

  (** Strongest possible set: denied by every capability allowlist.
      Used to fail closed on unclassified prim names in policy gates. *)
  let top : t = normalize [ ReadDb; WriteDb; Branch; Publish; Ai; Html; Txn; Net ]
end

type effect_set = EffSet.t

(** Default capability buckets (image may add more).
    H0.1 audit 2026-07-18: every registered image prim must appear in
    exactly one capability bucket (or the special-case arms in
    [effects_for_prim]). Guest-visible only — not host control effects.

    2026-09-10 FULL SWEEP (i-1788669715 / i-1788670318): the old
    hand-maintained lists had drifted from the registered catalog —
    eval-string, tree/upsert, tree/delete, link/unlink, sync/push,
    lens/fork, fork-data, merge-data, abandon-data, type-fields, unbind,
    space/*, media/*, promote*, tokens, streams, tcp-connect, … inferred
    as PURE, so publish-time and AI gates sailed them through. Every
    prim registered by the image is classified below, and
    Session.make asserts it at boot (fail boot on drift). *)

let pure_prims = [
  "+"; "-"; "*"; "div"; "/"; "mod"; "quotient"; "remainder";
  "abs"; "min"; "max";
  "expt"; "sqrt"; "exp"; "log";
  "sin"; "cos"; "tan"; "asin"; "acos"; "atan";
  "number?"; "integer?"; "zero?"; "positive?"; "negative?";
  "even?"; "odd?";
  "random"; "random-seed";
  "="; "<"; ">"; "<="; ">="; "not";
  "list"; "car"; "cdr"; "cons"; "null?"; "pair?"; "count";
  "append"; "reverse"; "nth"; "list-ref";
  "string-append"; "string-join"; "str"; "type-of"; "as-str";
  "symbol?"; "string?"; "symbol=?"; "symbol->string"; "string->symbol";
  "string-eq"; "string-trim"; "string-length"; "substring"; "string-contains?";
  "string<?"; "string<=?"; "string>?"; "string>=?";
  "html-escape"; "json-parse"; "json-emit";
  "read-string"; "read-all-string";
  "dict-get"; "dict-set"; "dict-keys";
  "hx-get"; "hx-post"; "hx-request?";
  "request-header"; "request-method"; "request-path";
  "request-param"; "request-params"; "request-body"; "request-multipart";
  "date-today"; "date-weekday"; "date-format";
  "date-add-days"; "date-diff-days";
  "now"; "now-ms"; "now-iso";
  "parse-int"; "parse-float"; "floor"; "round";
  "fsrs-schedule";
  "length"; "equal?";
  "cadr"; "caar"; "caddr"; "cddr"; "assoc"; "member";
  "string-replace"; "string-downcase"; "string-upcase";
  "string-starts-with?"; "string-ends-with?";
  "url-encode"; "url-decode"; "sha1"; "html-decode";
  "parse-date"; "xml-parse"; "random-uuid";
  "verify-ssh-key";  (* pure crypto check on args; no store *)
  "media-url";       (* builds /media/<id> string; no store read *)
  (* prims_pure shared-catalog extras + hypermedia harness (2026-09-10
     boot-assertion catch): string/list utils, request-body shaping,
     soup/* in-memory DOM ops, impact session-memory queue writes. *)
  "budget-remaining"; "err/shape"; "extract-links"; "render-links";
  "render-tokens"; "prose-tokens"; "status-response"; "sort";
  "bytes-response";    (* tagged __bytes__ value; no store, no net *)
  "string-split"; "string=?"; "json-part"; "multipart"; "part";
  "soup/parse"; "soup/page-copy"; "soup/select"; "soup/select-one";
  "soup/serialize"; "soup/text"; "soup/attr"; "soup/has-attr";
  "soup/swap!"; "soup/create"; "soup/replace"; "soup/closest-attr";
  "soup/parent"; "soup/diff"; "soup/apply-parts!";
  "impact-clear"; "impact-mark";  (* session-memory impact queue *)
  "route/lookup";    (* session-memory route table lookup *)
]

let read_db_prims = [
  "get"; "rows"; "select-for-update";
  "find-row"; "find-rows"; "find-text"; "find-random";
  "list-branches"; "current-branch"; "list-bindings";
  "whoami"; "current-author";
  "current-actor";  (* auth.delegation-tokens: session identity provenance *)
  "function-source"; "function-doc"; "function-ast"; "package-bindings"; "package-exports";
  "export-package";
  "route-visibility";
  "branch/status";  (* branch-ops status row: read-only fold of Store.branch_status *)
  "branch/cp-list"; "branch/cp-diff"; "branch/log";  (* branching.checkpoints reads: cp listing, cp diff, the oplog fold *)
  "list-usernames"; "list-ssh-keys";
  "llm-key-status";
  "log-tail"; "ops/summary"; "ops/incidents"; "ops/activity";
  "ops/presence";
  "agent-history"; "agent-sessions";
  "job-status"; "jobs"; "schedules";  (* borge/jobs.borg ReadDb *)
  "gen-now";  (* borge/tree-time.borg reads-as-of-gen ReadDb *)
  "gen-read"; "gen-diff";  (* borge/tree-time.borg reads-as-of-gen ReadDb *)
  "data-history";  (* borge/tree-time.borg S3 datum history ReadDb *)
  "data-scope";  (* read-only effective data-scope probe (marcus/codex ask, 2f6dacd) *)
  (* code introspection / call graph (prims_code, prims_graph) +
     bumpers M5 discovery surfaces (borge/bumpers.borg discovery-surfaces):
     docs-missing lists visible bindings without docstrings, app-check
     reports per-route fragment-fit — both read branch-visible defs/routes
     (ReadDb), never write. *) 
  "analyze"; "explain"; "describe"; "apropos-docs"; "find-code";
  "docs-missing"; "app-check";
  "code-history"; "code-diff"; "diff";
  "callees"; "callers"; "dependents"; "used-by-routes"; "holes";
  "backlinks"; "out-links"; "branch-changes"; "change-transcript";
  "forge-effects";
  (* tree reads (prims_data) — the read/write split mirrors
     example_eval.ml's read_extra_prims *)
  "tree/get"; "tree/get-many"; "tree/list"; "tree/list-kind"; "tree/children-of";
  "tree/child"; "tree/parent"; "tree/ancestors"; "tree/subtree";
  "tree/leaf"; "tree/siblings"; "tree/next"; "tree/prev";
  "tree/find-by"; "tree/search-text"; "tree/note-index";
  "tree/note-index-full"; "tree/folder-index"; "tree/children-html";
  "tree/path-label"; "tree/segments"; "tree/get-uuid"; "tree/garden-root";
  "tree/lens-get"; "tree/lens-children"; "tree/lens-subtree";
  "node-path";
  (* author-visible reads (agent-native-m2): rows-author/find-rows-author
     return [id; author; fields] triples with the same RLS visibility as
     rows/find-rows *)
  "rows-author"; "find-rows-author";
  (* data spaces reads *)
  "space-exists?"; "space-list"; "space-members";
  "list-data-lenses";
  (* media reads *)
  "media-list"; "media-meta";
  (* sync planning is read-only over the caller's own lens unit *)
  "sync/plan";
  (* delta exchange negotiate + base commit are read/classify over the
     caller's own sync_bases row (the write arm is sync/push2 below) *)
  "sync/plan2"; "sync/commit2";
  (* impact queue read arm (impact-mark/clear write below) *)
  "impact-queue";
  (* retention reads *)
  "retention-policies";
  (* admin/agent token reads *)
  "list-tokens";
  (* delegated on-behalf-of token reads (auth.delegation-tokens) *)
  "list-delegated-tokens";
  (* replication unit diagnostics *)
  "replication/unit";
  "list-packages"; "packages";
  "is-admin?";

]

let write_db_prims = [
  "create"; "update"; "update-field"; "delete"; "deftype";
  "load-string"; "load-library";  (* persist defs/routes onto branch *)
  "add-ssh-key"; "revoke-ssh-key";
  "set-llm-key"; "clear-llm-key";
  (* bumpers M6 (borge/bumpers.borg notices-envelope quiet tiers):
     (agent-profile 'quiet) persists a per-identity datum (type
     "profile") — it CAN write, so conservative WriteDb like
     create/update. *)
  "agent-profile";
  "with-public-write";  (* elevates anon → may write data_rows *)
  "submit-job"; "submit-job-at"; "cancel-job";  (* borge/jobs.borg WriteDb *)
  "with-step"; "__step__";  (* checkpoint write; composes with Txn for exactly-once *)
  "schedule"; "unschedule";  (* borge/jobs.borg schedules WriteDb *)
  "undo";  (* borge/tree-time.borg reads-as-of-gen restore-by-write WriteDb *)
  "ops/knock";  (* deterministic knock scan files security incidents (admin) *)
  "ops/hold";   (* reversible operator containment: revoke/cancel/disable (admin) *)
  "code-restore";  (* borge/tree-time.borg consumers code-restore WriteDb *)
  (* META-EVAL family (i-1788669715): evaluates code in the live env / a
     fresh budget / a timing harness loop — capability or resource reset
     in one prim. Classified WriteDb so the public-route and AI gates
     deny them by default; runtime dispatch also denies eval-string et
     al in public handlers. *)
  "eval-string"; "bench"; "example-eval";
  (* tree/data writes (prims_data) *)
  "tree/upsert"; "tree/delete"; "tree/move"; "tree/move-before";
  "tree/move-after"; "tree/reorder"; "tree/insert-child";
  "link"; "unlink";
  "type-fields";
  "fork-data"; "merge-data"; "abandon-data";
  "lens/fork";
  (* data spaces writes *)
  "space-create"; "space-add-member"; "space-admit"; "space-remove-member";
  (* media writes *)
  "media-put"; "media-put-raw"; "media-delete";
  (* branch-time writes *)
  "prune-synced"; "stub-function"; "unbind";
  "branch/pull";  (* branch-ops pull row: bulk unbind of behind pins (store writes) *)
    "branch/push";  (* branch-ops push row: dep-closure promote of own pins onto the owner's personal (store writes) *)
    "branch/cp";  (* branch-ops checkpoint: name a point on the gen stream (store write) *)
    "branch/cp-restore";  (* branch-ops checkpoint restore: re-land captured bodies as new writes (store writes) *)
  (* retention writes *)
  "retention-delete-policy"; "retention-policy-set"; "retention-run";
  (* auth writes *)
  "issue-token"; "revoke-token"; "rotate-token";
  (* delegated on-behalf-of token writes (auth.delegation-tokens) *)
  "mint-delegated-token"; "revoke-delegated-token";
  "set-user-type"; "set-allow-agent-promote";
  "set-password";
  "create-user"; "delete-user"; "create-agent";
  (* package/config writes *)
  "package-enable"; "package-disable"; "reseed-package";
  (* sync push writes both sides (server-side merge authority) *)
  "sync/push"; "sync/push2";
  (* route/dispatch executes a named handler — conservative WriteDb so
     public routes can never call it *)
  "route/dispatch";
  (* bumpers M4 verify-route (borge/bumpers.borg verify-route): invokes a
     named route handler once to measure it. SAME executor semantics as
     route/dispatch — it RUNS arbitrary handler code, so it is
     conservatively WriteDb (a public handler must never be able to call
     it and trigger another route's writes). read-db is only the "honest
     floor" if it were purely static; it is not (see M4 note). *)
  "verify-route";
]

let branch_prims = [
  "fork"; "merge"; "checkout";
  (* promote upserts code onto a target branch (prims_auth) *)
  "promote"; "promote-to-main";
]
let publish_prims = [ "route"; "public-route" ]
let html_prims = [ "render-html"; "redirect" ]
(* Outbound network (HTTP fetch). Gated to admin/agent at the prim level
   (session.ml); tracked as a capability badge so public routes and AI install
   policy can deny it by default. sleep + the tcp stream prims are here too:
   they BLOCK a fiber for their duration, so any allowlist that denies net
   also denies them (i-1788669886: sleep used to be "pure", letting public
   handlers and txn bodies hold fibers/txns open for a minute at a time). *)
let net_prims = [
  "http-get"; "http-post";
  "sleep";
  "tcp-connect"; "stream-read"; "stream-write"; "stream-close"; "stream-list";
]
(* Author tools that freeze into function_defs or drive the model (Ai + WriteDb). *)
let ai_write_prims = [
  "propose-function"; "install-function"; "agent-task";
  "llm-inner"; "llm-outer"; "llm-rewrite"; "llm-seed";
  "agent-session-new"; "agent-say"; "agent-session-delete";
]
(* Portal that only talks to the model (Ai only) — legacy one-shot + raw completion. *)
let ai_only_prims = [ "agent"; "llm-chat" ]
let ai_prims = ai_only_prims @ ai_write_prims

(** Heads the EVALUATOR dispatches as special forms before any prim
    lookup (Eval.special_form_names, i-1788671219). They can therefore
    reach [infer] as App (Var name, …) nodes — stored ASTs from before
    the If-node codec, macro templates round-tripped through
    value_to_expr — and must never fall through to a prim lookup:
    strict lookups would raise Unresolved_name (prod /guestbook denied
    on "if", 2026-09-11) and default lookups would silently drop the
    body's effects. Special forms are not user-bindable
    (assert_bindable_name) and carry no capability of their own; infer
    recurses into the argument exprs, which for control flow
    over-approximates (both arms), for delay captures the recipe's
    static effects, and for the OCaml-loop list ops includes the mapped
    function's effects. Parity with Eval.special_form_names is asserted
    in lisp/test/eval_test.ml. *)
let special_form_heads : string list =
  [ "lambda"; "let"; "letrec"; "if"; "define"; "quote"; "seq"; "begin";
    "match"; "guard"; "error"; "prim";
    "with-transaction"; "with-step"; "quasiquote"; "unquote"; "unquote-splicing";
    "defmacro"; "macroexpand-1"; "macroexpand"; "gensym";
    "cond"; "and"; "or";
    "apply"; "list/map"; "list/filter"; "list/foldl"; "list/foldr";
    "delay"; "force" ]

let effects_for_prim (name : string) : effect_set =
  if List.mem name pure_prims then EffSet.empty
  else if List.mem name read_db_prims then EffSet.singleton ReadDb
  else if List.mem name write_db_prims then EffSet.singleton WriteDb
  else if List.mem name branch_prims then EffSet.singleton Branch
  else if List.mem name publish_prims then EffSet.singleton Publish
  else if List.mem name html_prims then EffSet.singleton Html
  else if List.mem name net_prims then EffSet.singleton Net
  else if List.mem name ai_only_prims then EffSet.singleton Ai
  else if List.mem name ai_write_prims then
    EffSet.union (EffSet.singleton Ai) (EffSet.singleton WriteDb)
  else if name = "with-transaction" || name = "__with_transaction__" then EffSet.singleton Txn
  else
    (* Unknown name: not a classified prim. User fns resolved via
       known_fn; bare unknowns contribute nothing HERE (avoid painting
       every call as db-write). POLICY GATES must not use this function
       raw — go through [strict_prim_effects], which fails closed for
       unclassified names (EffSet.top). Drift between the registered
       catalog and these lists fails boot via Session.make's assertion. *)
    EffSet.empty

(** [true] when [name] is in exactly one capability bucket (or a
    special-case arm). Classification-complete names are the only ones a
    policy gate should trust. *)
let is_classified (name : string) : bool =
  List.mem name pure_prims
  || List.mem name read_db_prims
  || List.mem name write_db_prims
  || List.mem name branch_prims
  || List.mem name publish_prims
  || List.mem name html_prims
  || List.mem name net_prims
  || List.mem name ai_prims
  || name = "with-transaction" || name = "__with_transaction__"

(** Strict lookup for policy gates (public-route publish + dispatch,
    AI install): unclassified names infer as [EffSet.top], denied by
    every allowlist. User-fn names never reach this raw — inference
    resolves them via known_fn first. *)
(** Raised by [strict_prim_effects] for an applied name that is neither a
    visible user function nor a classified prim. Policy gates catch it to
    NAME the offender — "effects = EffSet.top" told you nothing about
    which symbol the walk could not resolve (the /guestbook prod hunt,
    2026-09-11). Every raise site previously inferred top and was denied
    anyway, so this only changes the message, never the verdict. *)
exception Unresolved_name of string

let strict_prim_effects (name : string) : effect_set =
  if is_classified name then effects_for_prim name
  else raise (Unresolved_name name)

(** Boot-time classification audit (i-1788670318). Given the registered
    prim catalog, returns drift errors: registered names missing from
    every bucket, and names listed in more than one bucket. Empty list
    = clean. Session.make fails boot on non-empty. *)
let classification_errors (catalog : string list) : string list =
  let buckets =
    [ ("pure", pure_prims); ("read-db", read_db_prims);
      ("write-db", write_db_prims); ("branch", branch_prims);
      ("publish", publish_prims); ("html", html_prims);
      ("net", net_prims); ("ai", ai_prims);
      ("txn", [ "with-transaction"; "__with_transaction__" ]) ]
  in
  let bucket_of = Hashtbl.create 256 in
  let errs = ref [] in
  List.iter
    (fun (label, names) ->
      List.iter
        (fun n ->
          match Hashtbl.find_opt bucket_of n with
          | None -> Hashtbl.replace bucket_of n [ label ]
          | Some prev -> Hashtbl.replace bucket_of n (label :: prev))
        names)
    buckets;
  List.iter
    (fun name ->
      match Hashtbl.find_opt bucket_of name with
      | None ->
          errs :=
            Printf.sprintf "unclassified prim %S (registered, in no bucket)" name
            :: !errs
      | Some labels ->
          if List.length labels > 1 then
            errs :=
              Printf.sprintf "prim %S in multiple buckets (%s)" name
                (String.concat "," (List.rev labels))
              :: !errs)
    catalog;
  List.rev !errs

type prim_effect_lookup = string -> effect_set
type known_fn_lookup = string -> effect_set option

(** Default [effect_set] resolution for prim names — [effects_for_prim]. *)
let default_prim_lookup = effects_for_prim

(** Build a [known_fn] lookup from visible bindings [(name, body)].
    Memoized; cycles break by treating the in-progress name as pure so
    mutual recursion does not diverge. Used by badges, AI install gate,
    and route publish policy (H0.2 / H0.3). *)
let rec known_fn_of_bindings (bindings : (string * expr) list) : known_fn_lookup =
  let bodies : (string, expr) Hashtbl.t =
    Hashtbl.create (max 16 (List.length bindings))
  in
  List.iter (fun (n, e) -> Hashtbl.replace bodies n e) bindings;
  let cache : (string, effect_set) Hashtbl.t =
    Hashtbl.create (max 16 (List.length bindings))
  in
  let rec lookup (name : string) : effect_set option =
    match Hashtbl.find_opt cache name with
    | Some s -> Some s
    | None ->
        (match Hashtbl.find_opt bodies name with
         | None -> None
         | Some body ->
             (* cycle break *)
             Hashtbl.replace cache name EffSet.empty;
             let s = infer ~known_fn:lookup body in
             Hashtbl.replace cache name s;
             Some s)
  in
  lookup

(** Conservative AST effect inference.
    [known_fn] resolves user function names when known; unknown globals
    contribute no extra effects (overapprox only on prims). Higher-order
    unknown application does not explode to top — callers may wrap. *)
and infer
    ?(prims = default_prim_lookup)
    ?(known_fn : known_fn_lookup = fun _ -> None)
    (e : expr) : effect_set =
  let go = infer ~prims ~known_fn in
  match e with
  | Var name ->
      (match known_fn name with
       | Some s -> s
       | None ->
           (* bare var might be a prim bound in env *)
           if List.mem name pure_prims
              || List.mem name read_db_prims
              || List.mem name write_db_prims
              || List.mem name branch_prims
              || List.mem name publish_prims
              || List.mem name html_prims
              || List.mem name ai_prims
              || List.mem name net_prims
           then prims name
           else EffSet.empty)
  | Lit _ -> EffSet.empty
  | Quote _ -> EffSet.empty
  | Lambda (_params, body) -> go body
  | If (t, a, b) -> EffSet.unions [ go t; go a; go b ]
  | Let (_n, v, body) -> EffSet.union (go v) (go body)
  | Letrec (bindings, body) ->
      let bs = List.map (fun (_, e') -> go e') bindings in
      EffSet.unions (go body :: bs)
  | Seq (a, b) -> EffSet.union (go a) (go b)
  | Define (_n, v) -> go v
  | Prim (name, args) ->
      EffSet.union (prims name) (EffSet.unions (List.map go args))
  | App (head, args) ->
      (match head with
       | Var name when name = "let" || name = "letrec" ->
           (* let/letrec arrive as App nodes: the reader encodes the
              binding list as App (first_pair, [rest pairs]) where each
              pair is App (Var name, [val]) — parse_binding_pairs' exact
              shape. The pair heads are BINDING NAMES (syntax), not
              calls: strict prim lookups on them denied every
              multi-binding-let public handler (prod /guestbook,
              2026-09-11). Walk the VALUE exprs and the body — and ONLY
              those: the generic tail below re-walks args verbatim,
              which would hit the pair heads a second time. *)
           (match args with
            | [ App (App (Var _, [ v ]), rest_pairs); body ] ->
                let pair_val p =
                  match p with App (Var _, [ v ]) -> go v | x -> go x
                in
                EffSet.unions (go v :: List.map pair_val rest_pairs @ [ go body ])
            | _ -> EffSet.unions (List.map go args))
       | _ ->
           let head_fx =
             match head with
             | Var name when List.mem name special_form_heads ->
                 (* Mirror eval's App dispatch: special forms never reach
                    prim dispatch, so do not consult [prims] (strict
                    would raise, default would drop the body).
                    Capabilities these forms carry themselves:
                    with-transaction holds the txn; with-step writes a
                    checkpoint (composes with Txn). Everything else is
                    pure dispatch — the args carry the effects. *)
                 let extra =
                   if name = "with-transaction" then EffSet.singleton Txn
                   else if name = "with-step" then
                     EffSet.unions [ EffSet.singleton Txn;
                                     EffSet.singleton WriteDb ]
                   else EffSet.empty
                 in
                 EffSet.unions (extra :: List.map go args)
             | Var name ->
                 (match known_fn name with
                  | Some s -> s
                  | None -> prims name)
             | _ -> go head
           in
           let special =
             match head with
             | Var "with-transaction" -> EffSet.singleton Txn
             | _ -> EffSet.empty
           in
           EffSet.unions (head_fx :: special :: List.map go args))

(** Default AI allowlist for generated code (install_function bodies).

    WriteDb is allowed so agents can build real apps (create/update/delete/
    deftype on data_rows). Publish (route) stays denied here — use the
    dedicated register_route tool so route registration is explicit and
    not smuggled inside a lambda body. Elevate with PP_AI_ALLOW_WRITE for
    Branch/Publish/Ai extras. *)
let ai_default_allow : effect_set =
  EffSet.normalize [ ReadDb; WriteDb; Html ]

(** [true] when env var [name] is set to a truthy value (1/true/yes/on). *)
let env_truthy (name : string) : bool =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "TRUE" | "yes" | "on") -> true
  | _ -> false

(** Public user-route allowlist (capability-enforcement).
    Default: pure | read-db | html | txn — no WriteDb/Branch/Ai/Publish.
    Set PP_PUBLIC_ALLOW_WRITE=1 to permit WriteDb (signup forms, public
    capture). Prefer (with-public-write …) inside those handlers so the
    write runs as elevated identity, not the anonymous user. *)
let public_route_allow () : effect_set =
  let base = [ ReadDb; Html; Txn ] in
  let base =
    if env_truthy "PP_PUBLIC_ALLOW_WRITE" then WriteDb :: base else base
  in
  EffSet.normalize base

(** String form of the current public-route allowlist. *)
let public_route_allow_pp () : string = EffSet.pp (public_route_allow ())

(** Capability policy check: [true] when [got] is a subset of [allow], or when
    [got] is pure (no capabilities required). *)
let policy_allows ~(allow : effect_set) (got : effect_set) : bool =
  EffSet.subset got allow
  || (EffSet.is_pure got)

(** Soft transactional hygiene hints (H0.4). Advisory only — does not
    reject eval. Looks for multi-write bodies and [select-for-update]
    without a surrounding [with-transaction] / Txn effect. *)
let count_write_sites (e : expr) : int =
  let rec go = function
    | Prim (name, args) ->
        let here = if List.mem name write_db_prims then 1 else 0 in
        here + List.fold_left (fun n a -> n + go a) 0 args
    | App (Var name, args) when List.mem name write_db_prims ->
        1 + List.fold_left (fun n a -> n + go a) 0 args
    | App (h, args) -> go h + List.fold_left (fun n a -> n + go a) 0 args
    | Lambda (_, body) | Define (_, body) -> go body
    | If (t, a, b) -> go t + go a + go b
    | Let (_, v, body) | Seq (v, body) -> go v + go body
    | Letrec (bs, body) ->
        List.fold_left (fun n (_, e') -> n + go e') (go body) bs
    | Var _ | Lit _ | Quote _ -> 0
  in
  go e

(** [true] if [e] contains a [select-for-update] call anywhere; used by txn linting. *)
let has_select_for_update (e : expr) : bool =
  let rec go = function
    | Prim ("select-for-update", _) -> true
    | App (Var "select-for-update", _) -> true
    | Prim (_, args) -> List.exists go args
    | App (h, args) -> go h || List.exists go args
    | Lambda (_, body) | Define (_, body) -> go body
    | If (t, a, b) -> go t || go a || go b
    | Let (_, v, body) | Seq (v, body) -> go v || go body
    | Letrec (bs, body) -> go body || List.exists (fun (_, e') -> go e') bs
    | Var _ | Lit _ | Quote _ -> false
  in
  go e

(** Advisory transactional-hygiene hints for [e] (soft, non-blocking).
    Flags multi-write bodies and [select-for-update] outside a [with-transaction]. *)
let txn_lint ?(known_fn : known_fn_lookup = fun _ -> None) (e : expr)
  : string list =
  let fx = infer ~known_fn e in
  let has_txn = EffSet.mem Txn fx in
  let writes = count_write_sites e in
  let hints = ref [] in
  if writes >= 2 && not has_txn then
    hints :=
      (Printf.sprintf
         "soft txn: %d write-db sites without with-transaction — consider wrapping multi-step mutations"
         writes)
      :: !hints;
  if has_select_for_update e && not has_txn then
    hints :=
      "soft txn: select-for-update without with-transaction — row lock will not span later prims"
      :: !hints;
  List.rev !hints

(* ---- bumpers M2: define-notices (borge/bumpers.borg notices-envelope) ----

   Pure, advisory generation for a top-level (define …). Given the
   parsed define (name, doc?, peeled body) + effect inference, it emits
   the candidate notices: a {kind:define} (name + effect badge + bench
   pointer when the body has effects) and, when the docstring is
   missing, a {kind:tip} (the docstring nudge). A well-formed, doc'd,
   pure define yields exactly ONE notice (the define kind) — the M2
   "quiet define".

   Resides HERE in effect.ml rather than a new lisp/lib/notices.ml
   deliberately: every lisp/lib module is spec-declared and borge drift
   would flag an undeclared file as unspecified-module; effect.ml
   already hosts advisory AST lint (txn_lint) on this same infer, so
   the generator sits beside its input (docs/plans/bumpers-m1-notices.md
   M1 progress note left this home decision to the M2 loop). The image
   define path and the lens local-eval path call this SAME function so
   both emit identical notice text (bumpers M2 wire bullet). *)

type define_notice = {
  kind : string;  (* "define" | "tip" | "warn" (bumpers notices-envelope) *)
  name : string;
  effects : string option;  (* effect badge when the kind carries one *)
  text : string;
}

(** Hard budget: at most [notice_budget] notices per eval response
    (bumpers.notice-budget — the channel dies if it nags). The pure
    generator never exceeds it per define; the session drain caps at the
    same constant. *)
let notice_budget = 2

(** Cap a list at the notice budget (first N win). Shared by the pure
    generator callers and the session drain so the budget is ONE
    constant. *)
let cap_notices (ns : define_notice list) : define_notice list =
  let rec go n acc = function
    | _ when n = 0 -> List.rev acc
    | x :: rest -> go (n - 1) (x :: acc) rest
    | [] -> List.rev acc
  in
  go notice_budget [] ns

(** Candidate notices for one top-level define. [body] is the peeled
    body (lambda leading-doc already stripped, as parse_top_define
    returns); [doc] None triggers the docstring tip. Effect inference
    uses [known_fn] for user-callee resolution when provided; default is
    prim-only (conservative floor). *)
let define_notices ?(known_fn : known_fn_lookup = fun _ -> None)
    ~(name : string) ~(doc : string option) (body : expr) :
    define_notice list =
  let fx = infer ~known_fn body in
  let badge = EffSet.badge fx in
  let define_text =
    if EffSet.is_pure fx then "defined"
    else
      Printf.sprintf
        "bench: (bench \"(%s 1)\" 100); watch /timing after deploy" name
  in
  let notice =
    { kind = "define"; name; effects = Some badge; text = define_text }
  in
  match doc with
  | Some _ -> [ notice ]
  | None ->
      let tip_text =
        Printf.sprintf
          "no docstring — (define %s ... \"doc\") makes (function-doc %s) work"
          name name
      in
      [ notice; { kind = "tip"; name; effects = None; text = tip_text } ]

(** Session taper (bumpers.notice-budget): each tip KIND fires at most
    once per session. Pure fold — [fired] is the session's set of
    tip-kinds already emitted (a list of kind strings). Returns the
    kept notices plus the updated fired set, so the session mutates
    exactly one value. *)
let taper_tips ~(fired : string list) (ns : define_notice list) :
    define_notice list * string list =
  let kept, fired' =
    List.fold_left
      (fun (kept, fired) n ->
        match n.kind with
        | "tip" when List.mem "tip" fired -> (kept, fired)
        | "tip" -> (n :: kept, "tip" :: fired)
        | _ -> (n :: kept, fired))
      ([], fired) ns
  in
  (List.rev kept, fired')


(* ---- bumpers M3: route-notices (borge/bumpers.borg notices-envelope /
       fragment-by-default) ----

   Pure route-publish notice generation + shape classification. The image
   route prim (session_control_prims register_route_checked) calls this
   on a successful publish; the classifier is exposed separately so the
   M5 (app-check "pkg") per-route report can reuse it. Resides HERE the
   same reason define_notices does: every lisp/lib module is
   spec-declared and effect.ml already hosts the advisory AST lensing on
   this same [infer]. The notice is a [define_notice]-shaped dict with
   kind "route" — the SAME wire codec as M1/M2, no new shape
   (prim-seam shared-codec: an additive "notices" field, out of band). *)

type route_shape = Fragment | Shell | NotHtml

(** Canonical shape name (used by M5 app-check reports). *)
let route_shape_string = function
  | Fragment -> "fragment"
  | Shell -> "shell"
  | NotHtml -> "not-html"

(** Full-shell constructor heads (fragment-by-default): a handler that
    applies any of these renders a COMPLETE HTML document shell.
    layout/page (+ variants) is the neocities full page; html/page is the
    plain full page. This is the chapter's "layout/page, html/html, or a
    head tag". Static set — false negatives acceptable, false positives
    not: calling a fragment a shell would nag a legitimate fragment, and
    a full-shell page is a deliberate citizen, never a publish block. *)
let full_shell_constructors : string list =
  [ "layout/page"; "layout/page-with-css"; "html/page" ]

(** Name prefixes/tags that mean a handler produces HTML output rather
    than plain text/JSON — distinguishes NotHtml from Fragment. *)
let html_generation_prefixes : string list =
  [ "html/"; "ui/"; "layout/"; "css/"; "theme/" ]

(** [true] when [name] is a head that generates HTML (a fragment or a
    page) — as opposed to a plain-string/JSON endpoint. *)
let is_html_generation_name (name : string) : bool =
  name = "render-html" || name = "redirect"
  || List.exists
       (fun p ->
         String.length name >= String.length p
         && String.sub name 0 (String.length p) = p)
       html_generation_prefixes

(** Pure AST walk: does [e] apply any head matching [pred]? Walks
    App/Prim heads and recurses arguments, lambdas, and bodies. Static —
    no eval, no effect inference. *)
let rec expr_uses_head (pred : string -> bool) (e : expr) : bool =
  match e with
  | Var _ | Lit _ | Quote _ -> false
  | Lambda (_, body) | Define (_, body) -> expr_uses_head pred body
  | If (t, a, b) ->
      expr_uses_head pred t || expr_uses_head pred a
      || expr_uses_head pred b
  | Let (_, v, body) | Seq (v, body) ->
      expr_uses_head pred v || expr_uses_head pred body
  | Letrec (bs, body) ->
      expr_uses_head pred body
      || List.exists (fun (_, e') -> expr_uses_head pred e') bs
  | Prim (name, args) -> pred name || List.exists (expr_uses_head pred) args
  | App (Var name, args) -> pred name || List.exists (expr_uses_head pred) args
  | App (h, args) -> expr_uses_head pred h || List.exists (expr_uses_head pred) args

(** Static fragment/shape classification of a route handler body.
    [Shell]: calls a full-shell constructor. [NotHtml]: produces no HTML
    (a plain-text or JSON endpoint). [Fragment]: HTML that is NOT a full
    shell (fits glochid/habitat panes). Advisory only — route_gate stays
    about legality (handler exists, effects permitted), never about
    shape (bumpers fragment-by-default: the lane sensor, not the wall). *)
let classify_route_shape (body : expr) : route_shape =
  let shell n = List.mem n full_shell_constructors in
  if expr_uses_head shell body then Shell
  else if expr_uses_head is_html_generation_name body then Fragment
  else NotHtml

(** htmx-harness pointer (docs/agent-ui-harness.md tier ladder): the same
    short text rides every HTML-bearing route notice — cheapest
    observation first, graduate green flows into the package smoke. *)
let htmx_harness_pointer : string =
  "verify tiered: wget content -> scripted flow -> CDP snapshot+vision; "
  ^ "graduate green flows into scripts/smoke_<pkg>_ui.sh"

(** Full-shell remark (fragment-by-default): a full-shell page is
    legitimate, just declared. *)
let full_shell_remark : string =
  "renders a full shell; apps fit glochid/habitat panes as fragments "
  ^ "built from libs/ui + libs/layout; standalone page? declare it"

(** Facts the image route prim has at publish time. [effects] and
    [shape] are [None]/[NotHtml] when the handler body is not (yet) a
    visible function — define-after-route is still allowed, so a publish
    without a body cannot claim a badge or a shape. *)
type route_facts = {
  path : string;
  effects : effect_set option;
  shape : route_shape;
  standalone : bool;
}

(** Candidate notice for one route publish. EXACTLY one notice (kind
    "route" — budget-friendly): path + effect badge, plus the htmx-
    harness pointer on every HTML-bearing route, the full-shell remark on
    a full-shell page, "published" on a plain endpoint, and the
    standalone silence (fragment family off, route_gate legality
    unchanged). *)
let route_notice (f : route_facts) : define_notice list =
  let text =
    if f.standalone then
      "declared standalone - fragment family silenced (route_gate legality unchanged)"
    else
      match f.effects with
      | None ->
          "published (handler not yet defined; define it then re-verify)"
      | Some _ -> (
          match f.shape with
          | Shell -> full_shell_remark ^ " | " ^ htmx_harness_pointer
          | Fragment -> htmx_harness_pointer
          | NotHtml -> "published")
  in
  [ { kind = "route"; name = f.path;
      effects = Option.map EffSet.badge f.effects; text } ]

(* ---- bumpers M4: verify-route warnings (borge/bumpers.borg
   verify-route) ---------------------------------------------------------

   Static advisory lint over a route handler body for the
   (verify-route "/path") dry-run report. FIRST named warning pattern
   (prod 2026-12-30 layout stylesheet lambda incident): the handler
   embeds a large literal constant that it would rebuild on EVERY
   invocation (big inline CSS/JSON string, or a big (list ...) of literal
   strings fed to html/join/string-append), where a delay-define would
   pay it once.

   Conservative by construction: only LITERAL constants in the AST count
   (a handler that builds its page from request-dependent data has no
   such literals, so it can never be flagged — false negatives
   acceptable, false positives not), and the thresholds are deliberately
   high so a small legitimate constant is never nagged. Deliberately
   per-handler-batch only: it does NOT chase calls into sibling
   functions, so a handler that merely CALLS a heavy constant-builder is
   a false-miss by design (cross-function cost analysis is a later
   bumpers tier, bumpers sequencing). *)

(** Payload of a literal value in estimated chars (strings + symbols +
    nested list literals). *) 
let literal_payload (v : value) : int =
  let rec go acc = function
    | VStr s -> acc + String.length s
    | VSymbol s -> acc + String.length s
    | VList xs -> List.fold_left (fun a x -> go a x) acc xs
    | _ -> acc
  in
  go 0 v

(** A literal big enough to deserve the rebuild warning: >= 1024 chars of
    payload, or a literal list with >= 24 elements (the stylesheet was a
    ~30-element CSS string list). *) 
let large_const_threshold = 1024
let large_const_min_items = 24

let is_large_literal (v : value) : bool =
  literal_payload v >= large_const_threshold
  || (match v with
      | VList xs -> List.length xs >= large_const_min_items
      | _ -> false)

(** [true] when [e] is a pure literal atomic (Lit or Quote) — the only
    things our constant walk treats as constant leaves. *) 
let is_literal_atom (e : expr) : bool =
  match e with
  | Lit _ | Quote _ -> true
  | _ -> false

(** Sum of literal payload over an expr (Lit leaves + (list ...) app
    literal args); non-literals contribute 0. *) 
let rec const_payload (e : expr) : int =
  match e with
  | Lit v -> literal_payload v
  | Quote e' -> const_payload e'
  | App (Var "list", args) ->
      List.fold_left (fun acc a -> acc + const_payload a) 0 args
  | _ -> 0

(** Does the body embed a large literal constant on the per-invocation
    path? Flags: a large single literal, a (list ...) whose args are ALL
    literals with >= 24 elements or >= 1024 chars payload (the
    rebuild-worthy constant-list case), or any large literal nested
    deeper (recurse into args/lambdas/seqs/defines). App heads and
    lambda bodies all recurse so "(html/join (list <30 str>))" — the
    actual layout incident shape — is caught. *) 
let rec has_large_const_on_path (e : expr) : bool =
  match e with
  | Lit v -> is_large_literal v
  | Quote e' -> has_large_const_on_path e'
  | App (Var "list", args) ->
      (List.for_all is_literal_atom args
       && (List.length args >= large_const_min_items
           || const_payload e >= large_const_threshold))
      || List.exists has_large_const_on_path args
  | Lambda (_, body) | Define (_, body) -> has_large_const_on_path body
  | If (a, b, c) ->
      has_large_const_on_path a || has_large_const_on_path b
      || has_large_const_on_path c
  | Let (_, v, body) ->
      has_large_const_on_path v || has_large_const_on_path body
  | Letrec (bs, body) ->
      has_large_const_on_path body
      || List.exists (fun (_, e') -> has_large_const_on_path e') bs
  | Seq (a, b) -> has_large_const_on_path a || has_large_const_on_path b
  | Prim (_, args) -> List.exists has_large_const_on_path args
  | App (h, args) ->
      has_large_const_on_path h || List.exists has_large_const_on_path args
  | Var _ -> false

(** Warnings for a (verify-route "/path") dry run of the handler body.
    At most one warning today (the report stays sparse): the large-
    constant-rebuild nudge. *) 
let verify_route_warnings (body : expr) : string list =
  if has_large_const_on_path body then
    [ "large constant literal rebuilt per invocation — consider (delay ...) "
      ^ "so a delay-define pays it once" ]
  else []
