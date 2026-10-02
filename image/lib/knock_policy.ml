(** Pricklypear Image — deterministic knock policy
    (product-experience.borg §activity-ledger KNOCK SCHEMA,
    security.borg §the-watched-machine).

    LAW 1: detection never depends on a model noticing. This module is a
    PURE function over structure-only ledger events ([Activity_ledger.entry])
    — no DB, no content, no LLM. The caller scans raw rows and files one
    security incident per [knock] through the existing ops_incidents
    fingerprint queue (dup bumps count, never a second row).

    Thresholds + classes only. The trifecta row is likewise computed from
    CLASS COUNTS ONLY: does one actor simultaneously hold a private-data
    read, an untrusted-content ingest, and an effect channel.

    v1 rules (all deterministic):
      - [admin_denials]        N denials on admin surfaces in a window
      - [rls_violations]       repeated rls.violation rows
      - [privilege_escalation] any denied token.mint / promote.attempt
    Documented seams (need history/rollups, not a window): first-login-
    from-new-IP and cost-outliers vs the 7-day median. *)

type knock = {
  fingerprint : string;   (* stable per (kind, actor): dedup key *)
  kind : string;
  actor : string;
  title : string;
  count : int;            (* matching events observed in the window *)
}

type trifecta_row = {
  actor : string;
  private_data : bool;
  untrusted_ingest : bool;
  effect_channel : bool;
  flag : bool;            (* all three legs live *)
}

(* The admin-surface classes: a denial of any of these is an attempt at a
   privileged act. auth.deny is the unauthenticated edge of the same. *)
let admin_surface_classes : string list =
  [
    "admin.content-read"; "admin.hold"; "admin.knock"; "admin.revoke";
    "admin.create"; "admin.delete"; "promote.attempt"; "token.mint";
    "auth.deny"; "auth.revoke";
  ]

let is_admin_surface cls = List.mem cls admin_surface_classes

(* Trifecta leg maps — CLASS COUNTS ONLY, never event content.
   SPEC GAP (filed as a borge issue): the closed ledger grammar has NO
   untrusted-content-ingest class (the ledger records structure events,
   not reads; only cross-identity reads emit admin.content-read). So the
   ingest leg cannot be computed from ledger classes today. The leg is
   parameterized (defaults empty = never fires) rather than guessed; a
   future `egress.ingest` / `content.ingest` class closes it. *)
let private_data_classes = [ "admin.content-read" ]

let untrusted_ingest_classes : string list = []

let effect_channel_classes =
  [ "data.write"; "tree.write"; "token.mint"; "promote.attempt";
    "agent.spawn"; "egress.http" ]

let list_mem cls xs = List.mem cls xs

(* Count entries by actor for a predicate. *)
let count_by_actor (events : Activity_ledger.entry list)
    (p : Activity_ledger.entry -> bool) : (string * int) list =
  let tbl : (string, int) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (e : Activity_ledger.entry) ->
      if p e then
        Hashtbl.replace tbl e.actor
          (1 + (match Hashtbl.find_opt tbl e.actor with Some n -> n | None -> 0)))
    events;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

let mk_knock ~(kind : string) ~(actor : string) ~(count : int) ~(title : string)
    : knock =
  { fingerprint = Printf.sprintf "security|%s|%s" kind actor; kind; actor;
    title; count }

(** [detect ~min_denials ~min_rls events] -> the deterministic knock list.
    Stable order (denials, rls, privilege) so callers/tests are
    reproducible. *)
let detect ?(min_denials = 5) ?(min_rls = 2)
    (events : Activity_ledger.entry list) : knock list =
  let denials =
    count_by_actor events (fun e ->
        e.outcome = "denied" && is_admin_surface e.cls)
    |> List.filter (fun (_, n) -> n >= min_denials)
    |> List.map (fun (actor, n) ->
           mk_knock ~kind:"admin.denials" ~actor ~count:n
             ~title:
               (Printf.sprintf
                  "%d denied admin-surface attempts by %s in window" n actor))
  in
  let rls =
    count_by_actor events (fun e -> e.cls = "rls.violation")
    |> List.filter (fun (_, n) -> n >= min_rls)
    |> List.map (fun (actor, n) ->
           mk_knock ~kind:"rls.violation" ~actor ~count:n
             ~title:
               (Printf.sprintf
                  "%d cross-boundary write refusals (rls.violation) by %s" n
                  actor))
  in
  let privilege =
    count_by_actor events (fun e ->
        e.outcome = "denied"
        && (e.cls = "token.mint" || e.cls = "promote.attempt"))
    |> List.map (fun (actor, n) ->
           mk_knock ~kind:"privilege.escalation" ~actor ~count:n
             ~title:
               (Printf.sprintf
                  "denied privileged act (token.mint / promote.attempt) by %s"
                  actor))
  in
  denials @ rls @ privilege

(** Trifecta rows per actor, from class counts only. Agents with all three
    legs live are exactly the lethal-trifecta shape the spec names.
    [?ingest_classes] lets the caller supply an ingest leg once the
    grammar has a class for it; defaults to the module list (currently
    empty — see the SPEC GAP note above). *)
let trifecta ?(ingest_classes = untrusted_ingest_classes)
    (events : Activity_ledger.entry list) : trifecta_row list =
  let actors =
    List.sort_uniq compare
      (List.map (fun (e : Activity_ledger.entry) -> e.actor) events)
  in
  List.map
    (fun actor ->
      let has classes =
        List.exists
          (fun (e : Activity_ledger.entry) ->
            e.actor = actor && list_mem e.cls classes)
          events
      in
      let private_data = has private_data_classes in
      let untrusted_ingest = has ingest_classes in
      let effect_channel = has effect_channel_classes in
      { actor; private_data; untrusted_ingest; effect_channel;
        flag = private_data && untrusted_ingest && effect_channel })
    actors
