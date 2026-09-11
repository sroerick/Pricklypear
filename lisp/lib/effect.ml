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
  "append"; "reverse"; "nth";
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
  "now"; "now-ms";
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
  "function-source"; "function-doc"; "function-ast"; "package-bindings"; "package-exports";
  "export-package";
  "route-visibility";
  "list-usernames"; "list-ssh-keys";
  "llm-key-status";
  "log-tail"; "ops/summary"; "ops/incidents";
  "agent-history"; "agent-sessions";
  "job-status"; "jobs"; "schedules";  (* borge/jobs.borg ReadDb *)
  "gen-now";  (* borge/tree-time.borg reads-as-of-gen ReadDb *)
  "gen-read"; "gen-diff";  (* borge/tree-time.borg reads-as-of-gen ReadDb *)
  "data-history";  (* borge/tree-time.borg S3 datum history ReadDb *)
  (* code introspection / call graph (prims_code, prims_graph) *)
  "analyze"; "explain"; "describe"; "apropos-docs"; "find-code";
  "code-history"; "code-diff"; "diff";
  "callees"; "callers"; "dependents"; "used-by-routes"; "holes";
  "backlinks"; "out-links"; "branch-changes"; "change-transcript";
  "forge-effects";
  (* tree reads (prims_data) — the read/write split mirrors
     example_eval.ml's read_extra_prims *)
  "tree/get"; "tree/list"; "tree/list-kind"; "tree/children-of";
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
  (* impact queue read arm (impact-mark/clear write below) *)
  "impact-queue";
  (* retention reads *)
  "retention-policies";
  (* admin/agent token reads *)
  "list-tokens";
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
  "with-public-write";  (* elevates anon → may write data_rows *)
  "submit-job"; "submit-job-at"; "cancel-job";  (* borge/jobs.borg WriteDb *)
  "with-step"; "__step__";  (* checkpoint write; composes with Txn for exactly-once *)
  "schedule"; "unschedule";  (* borge/jobs.borg schedules WriteDb *)
  "undo";  (* borge/tree-time.borg reads-as-of-gen restore-by-write WriteDb *)
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
  (* retention writes *)
  "retention-delete-policy"; "retention-policy-set"; "retention-run";
  (* auth writes *)
  "issue-token"; "revoke-token"; "rotate-token";
  "set-user-type"; "set-allow-agent-promote";
  "create-user"; "create-agent";
  (* package/config writes *)
  "package-enable"; "package-disable"; "reseed-package";
  (* sync push writes both sides (server-side merge authority) *)
  "sync/push";
  (* route/dispatch executes a named handler — conservative WriteDb so
     public routes can never call it *)
  "route/dispatch";
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
