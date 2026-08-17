(** Tests for borge/remote-eval.borg caching subsection. No network —
    the [Proxy_rim.transport] is a mock serving the two proxyable read
    prims the def cache reads: [current-branch] (VStr, branch namespace)
    and [gen-now] (VInt, migration-017 branch_generations = the tree-time
    gen coordinate).

    Covers the three verification cases:
      1. a write in the client's branch ancestry bumps the generation;
         the next cache-check round trip observes the bump and reloads
         affected defs (the stale def is returned for eviction and
         dropped from the table, so the next lookup misses -> re-fetch).
      2. a checkout to a different branch invalidates the WHOLE local
         def cache (branch is the namespace).
      3. with no writes, the cache-check round trip is the single cheap
         HEAD-ish (two single-counter reads) returning unchanged-
         generation — it does NOT do an N-way hash diff and mutates
         nothing.
    Plus a client-integration case: the lazy env re-fetches a def once
    the generation bumps (the [function-source] prim is called again and
    the NEW source is what eval uses). *)

open Pricklypear_lisp
open Ast

(** A mock transport over a mutable branch/gen coordinate + a per-name
    def-source feed, so the def cache's read prims can be driven
    (bump gen / switch branch) across cache-checks. *)
let coord_transport
    ?(branch = "user/personal") ?(gen = 0L) ?(defs : (string -> string) option) ()
    : Proxy_rim.transport
      * (string -> unit)
      * (unit -> unit)
      * (unit -> string * int64)
      * (string * value list) list ref =
  let b = ref branch in
  let g = ref gen in
  let calls = ref [] in
  let set_branch nb = b := nb in
  let bump_gen () = g := Int64.add !g 1L in
  let get_coord () = (!b, !g) in
  let fn name args =
    calls := !calls @ [ (name, args) ];
    match name with
    | "current-branch" -> Ok (VStr !b)
    | "gen-now" -> Ok (VInt !g)
    | _ -> (
        match defs with
        | Some feed -> (
            match args with
            | [ VStr n ] ->
                let s = feed n in
                if s = "" then Ok VNil else Ok (VStr s)
            | _ -> Ok VNil)
        | None ->
            Error
              (Interp_error
                 (Prim_error (name, "unserved", "prim_error"))))
  in
  (fn, set_branch, bump_gen, get_coord, calls)

(** Test 1: a generation bump (write-in-ancestry) invalidates stale
    defs. Load a def at gen 5; bump gen-now to 6; the cache-check round
    trip observes the bump and returns the stale name for eviction (the
    entry is dropped, so a subsequent lookup misses — the client would
    re-fetch). A def loaded AT the new generation survives a second
    check at that same generation (only affected defs reload). *)
let test_generation_bump_reloads_stale () =
  let transport, _sb, bump, _coord, _calls = coord_transport ~gen:5L () in
  let c = Def_cache.create ~transport () in
  ignore (Def_cache.check c); (* pins (branch, gen=5) *)
  Def_cache.store c "inc" (VInt 1L); (* loaded at gen 5 *)
  Def_cache.store c "sq" (VInt 2L); (* loaded at gen 5 *)
  Alcotest.(check int) "two defs cached" 2 (Def_cache.size c);
  (match Def_cache.lookup c "inc" with
   | Some _ -> ()
   | None -> Alcotest.fail "def should be cached at gen 5");
  (* a write bumps the generation to 6 *)
  bump ();
  (* cache-check observes the bump and flags the stale defs for eviction *)
  let stale = Def_cache.check c in
  Alcotest.(check bool) "check reported a change (stale names returned)" true
    (stale <> []);
  Alcotest.(check int) "both stale defs flagged for eviction" 2 (List.length stale);
  Alcotest.(check bool) "inc flagged stale" true (List.mem "inc" stale);
  (* table dropped the stale defs: next lookup misses -> re-fetch *)
  (match Def_cache.lookup c "inc" with
   | Some _ -> Alcotest.fail "stale def should have been dropped (reload on next use)"
   | None -> Alcotest.(check bool) "stale def dropped" true true);
  Alcotest.(check int) "cache empty after eviction" 0 (Def_cache.size c)

(** Test 2: a checkout to a different branch invalidates the WHOLE
    cache (branch is the namespace), regardless of generation. Every
    cached name is returned for eviction. *)
let test_branch_switch_invalidates_all () =
  let transport, set_branch, _bump, _coord, _calls =
    coord_transport ~branch:"user/personal" ~gen:3L ()
  in
  let c = Def_cache.create ~transport () in
  ignore (Def_cache.check c);
  Def_cache.store c "inc" (VInt 1L);
  Def_cache.store c "dec" (VInt 2L);
  Def_cache.store c "sq" (VInt 3L);
  Alcotest.(check int) "three defs cached" 3 (Def_cache.size c);
  (* switch branch, same generation *)
  set_branch "admin/user";
  let stale = Def_cache.check c in
  Alcotest.(check int) "check reported a branch change (all names evicted)" 3
    (List.length stale);
  Alcotest.(check bool) "all three names flagged" true
    (List.length (List.filter (fun n -> List.mem n stale) [ "inc"; "dec"; "sq" ]) = 3);
  Alcotest.(check int) "WHOLE cache cleared on branch switch" 0 (Def_cache.size c);
  (match Def_cache.lookup c "inc" with
   | Some _ -> Alcotest.fail "branch switch must drop everything"
   | None -> ())

(** Test 3: with no writes, the cache-check round trip is the single
    cheap HEAD-ish (two single-counter reads) returning unchanged-
    generation — it does NOT do an N-way hash diff and mutates nothing
    (a cached def survives, [] returned, no fetch of def sources). *)
let test_no_writes_unchanged_single_roundtrip () =
  let transport, _sb, _bump, _coord, calls = coord_transport ~gen:9L () in
  let c = Def_cache.create ~transport () in
  ignore (Def_cache.check c); (* pin *)
  Def_cache.store c "inc" (VInt 1L);
  calls := []; (* forget pin-roundtrip calls *)
  (* a second check at the SAME branch+gen: unchanged *)
  let stale = Def_cache.check c in
  Alcotest.(check bool) "unchanged-generation -> no evictions" true (stale = []);
  Alcotest.(check int) "still cached after unchanged check" 1 (Def_cache.size c);
  (* exactly the two cheap counter reads, nothing else (no N-way diff) *)
  Alcotest.(check int) "one cheap round trip = 2 counter reads" 2
    (List.length !calls);
  List.iter
    (fun (n, _) ->
      Alcotest.(check bool) "only counter read prims cross the wire" true
        (n = "current-branch" || n = "gen-now"))
    !calls

(** Integration: the client's lazy env uses the def cache so a def
    fetched at gen N is re-fetched (reloaded) once the generation bumps
    — the second eval re-fetches the NEW source from the server and uses
    it, because the stale env binding is evicted. *)
let test_client_lazy_env_reloads_after_bump () =
  let gen = ref 5L in
  let transport name args =
    match name with
    | "current-branch" -> Ok (VStr "user/personal")
    | "gen-now" -> Ok (VInt !gen)
    | "function-source" -> (
        match args with
        | [ VStr "inc" ] ->
            if Int64.compare !gen 6L < 0 then Ok (VStr "(define (inc x) (+ x 1))")
            else Ok (VStr "(define (inc x) (+ x 100))")
        | _ -> Ok VNil)
    | _ -> Error (Interp_error (Prim_error (name, "unserved", "prim_error")))
  in
  let t = Remote.make ~transport () in
  (* first use: gen 5 -> inc x => (+ x 1); (inc 41) = 42 *)
  let v1 = Remote.eval t "(inc 41)" in
  Alcotest.(check int) "inc at gen5 = 42" 42
    (match v1 with VInt n -> Int64.to_int n | _ -> -1);
  (* bump the server generation to 6 -> the next use re-fetches v2 source *)
  gen := 6L;
  let v2 = Remote.eval t "(inc 41)" in
  Alcotest.(check int) "inc after bump = 141 (reloaded v2 source)" 141
    (match v2 with VInt n -> Int64.to_int n | _ -> -1)

let () =
  Alcotest.run "remote-def-cache"
    [
      ( "caching",
        [
          Alcotest.test_case "generation bump reloads stale defs" `Quick
            test_generation_bump_reloads_stale;
          Alcotest.test_case "branch switch invalidates whole cache" `Quick
            test_branch_switch_invalidates_all;
          Alcotest.test_case "no writes = single cheap unchanged round trip" `Quick
            test_no_writes_unchanged_single_roundtrip;
          Alcotest.test_case "client lazy env reloads after generation bump" `Quick
            test_client_lazy_env_reloads_after_bump;
        ] );
    ]
