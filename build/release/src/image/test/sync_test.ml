(** local-first sync authority tests (local-first.sync, campaign 8.3).

    The heart of sync is PURE (no Postgres): manifest classification
    (the exchange's conflict DETECTION at node granularity) and the
    three-way merge planner (base = last-synced, ours = server,
    theirs = client — the campaign-3 diff philosophy). These tests
    exercise the pure core exhaustively, plus the JSON report rendering
    the sync/plan prim returns. The DB-backed harness
    (Sync.server_manifest / Sync.apply_client_node / Sync.commit) is
    exercised against the live dev image / shared PG when reachable, and
    skipped cleanly otherwise.

    Each test uses synthetic hashes (h1..hn) — content identity is
    opaque to the planner; only equality/membership matters.
*)

module S = Pricklypear_image_lib.Sync
open Alcotest

let h (n : int) : string = Printf.sprintf "h%d" n

(* ---- classify (two-way negotiation) ---- *)

let classify_same () =
  let server = [ ("data.u.x.a", h 1); ("data.u.x.b", h 2) ] in
  let client = [ ("data.u.x.a", h 1); ("data.u.x.b", h 2) ] in
  let r = S.classify server client in
  check (int) "same_len" 2 r.same_len;
  check (int) "client_only" 0 (List.length r.client_only);
  check (int) "server_only" 0 (List.length r.server_only);
  check (int) "diverge" 0 (List.length r.diverge)

let classify_client_new () =
  let server = [ ("data.u.x.a", h 1) ] in
  let client = [ ("data.u.x.a", h 1); ("data.u.x.b", h 2) ] in
  let r = S.classify server client in
  check (int) "same_len" 1 r.same_len;
  check (int) "client_only" 1 (List.length r.client_only);
  check (int) "server_only" 0 (List.length r.server_only);
  check (bool) "b on client" true (List.mem_assoc "data.u.x.b" r.client_only)

let classify_server_new () =
  let server = [ ("data.u.x.a", h 1); ("data.u.x.c", h 3) ] in
  let client = [ ("data.u.x.a", h 1) ] in
  let r = S.classify server client in
  check (int) "same_len" 1 r.same_len;
  check (int) "client_only" 0 (List.length r.client_only);
  check (int) "server_only" 1 (List.length r.server_only);
  check (bool) "c on server" true (List.mem_assoc "data.u.x.c" r.server_only)

let classify_diverge () =
  let server = [ ("data.u.x.a", h 1) ] in
  let client = [ ("data.u.x.a", h 9) ] in
  let r = S.classify server client in
  check (int) "same_len" 0 r.same_len;
  check (int) "diverge" 1 (List.length r.diverge);
  check (string) "div path" "data.u.x.a" (fst (List.hd r.diverge))

(* ---- three-way plan (base = last-synced, ours = server, theirs =
   client) ---- *)

let plan_one (plans : S.plan list) (path : string) : S.outcome =
  match List.find_opt (fun { S.path = p; _ } -> p = path) plans with
  | Some { S.outcome; _ } -> outcome
  | None -> failwith ("no plan for " ^ path)

let plan_unchanged () =
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 1) ] [ ("a", h 1) ] in
  (match plan_one p "a" with
   | S.Unchanged -> ()
   | _ -> failwith "expected Unchanged")

let plan_theirs_add () =
  (* client added a node neither base nor server have -> server applies it *)
  let p = S.plan_of [] [ ("a", h 1) ] [ ("a", h 1); ("b", h 2) ] in
  (match plan_one p "b" with
   | S.Theirs_add -> ()
   | _ -> failwith "expected Theirs_add")

let plan_ours_add () =
  (* server added a node; client never had it -> keep, no transfer *)
  let p = S.plan_of [] [ ("a", h 1); ("c", h 3) ] [ ("a", h 1) ] in
  (match plan_one p "c" with
   | S.Ours_keep -> ()
   | _ -> failwith "expected Ours_keep")

let plan_theirs_mod () =
  (* client edited a node the server never moved from base -> apply client
     body (a clean client edit). *)
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 1) ] [ ("a", h 9) ] in
  (match plan_one p "a" with
   | S.Theirs_mod -> ()
   | _ -> failwith "expected Theirs_mod")

let plan_ours_mod () =
  (* server edited a node the client never moved from base -> keep server *)
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 9) ] [ ("a", h 1) ] in
  (match plan_one p "a" with
   | S.Ours_keep -> ()
   | _ -> failwith "expected Ours_keep")

let plan_identical () =
  (* both moved to the SAME new hash -> one change, keep either *)
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 9) ] [ ("a", h 9) ] in
  (match plan_one p "a" with
   | S.Identical -> ()
   | _ -> failwith "expected Identical")

let plan_conflict () =
  (* base present, both changed to different hashes -> report conflict *)
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 8) ] [ ("a", h 9) ] in
  (match plan_one p "a" with
   | S.Conflict (path, b, o, t) ->
       check (string) "path" "a" path;
       check (string) "base" (h 1) b;
       check (string) "ours" (h 8) o;
       check (string) "theirs" (h 9) t
   | _ -> failwith "expected Conflict")

let plan_theirs_del () =
  (* client deleted a node the server still holds -> delete server node *)
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 1) ] [] in
  (match plan_one p "a" with
   | S.Theirs_del -> ()
   | _ -> failwith "expected Theirs_del")

let plan_ours_del () =
  (* server deleted a node the client still holds -> client pulls *)
  let p = S.plan_of [ ("a", h 1) ] [] [ ("a", h 1) ] in
  (match plan_one p "a" with
   | S.Ours_del -> ()
   | _ -> failwith "expected Ours_del")

let actionable_only_transfers () =
  (* only transfer-or-conflict rows are actionable *)
  let p =
    S.plan_of
      [ ("a", h 1); ("b", h 1); ("c", h 1); ("d", h 1); ("e", h 1) ]
      [ ("a", h 1); ("b", h 1); ("c", h 8); ("d", h 1); ("e", h 1) ]
      [ ("a", h 1); ("b", h 9); ("c", h 1); ("d", h 9); ("e", h 1) ]
  in
  let act = S.actionable p in
  let paths = List.map (fun { S.path; _ } -> path) act in
  check (bool) "unchanged a filtered" true (not (List.mem "a" paths));
  check (bool) "theirs_mod b actionable" true (List.mem "b" paths);
  check (bool) "ours_keep c filtered" true (not (List.mem "c" paths));
  check (bool) "conflict d actionable" true (List.mem "d" paths);
  check (bool) "unchanged e filtered" true (not (List.mem "e" paths))

(* ---- JSON rendering (the sync/plan prim surface) ---- *)

let json_report_roundtrip () =
  let r = S.classify [ ("a", h 1) ] [ ("a", h 9); ("b", h 2) ] in
  let j = Yojson.Safe.from_string (S.json_of_report r) in
  (match j with
   | `Assoc kv ->
       check (int) "same" 0 (match List.assoc_opt "same" kv with Some (`Int n) -> n | _ -> -1);
       check (int) "client_only" 1 (match List.assoc_opt "client_only" kv with Some (`Int n) -> n | _ -> -1);
       check (int) "diverge" 1 (match List.assoc_opt "diverge" kv with Some (`Int n) -> n | _ -> -1)
   | _ -> failwith "expected Assoc")

let json_plan_roundtrip () =
  let p = S.plan_of [ ("a", h 1) ] [ ("a", h 8) ] [ ("a", h 9) ] in
  let j = Yojson.Safe.from_string (S.json_of_plan p) in
  (match j with
   | `Assoc kv ->
       (match List.assoc_opt "rows" kv with
        | Some (`List [ `Assoc row ]) ->
            (match List.assoc_opt "outcome" row with
             | Some (`String "conflict") -> ()
             | _ -> failwith "expected conflict outcome in JSON")
        | _ -> failwith "expected one row")
   | _ -> failwith "expected Assoc")

(* ---- delta exchange (merge_delta: the client's manifest implied by
   its delta against the stored base — the chunked protocol's server
   half, 2026-09-11) ---- *)

let merge_delta_empty () =
  let base = [ ("a", h 1); ("b", h 2) ] in
  Alcotest.(check (list (pair string string)))
    "empty delta = the base" (S.normalize base)
    (S.merge_delta ~base ~changes:[] ~deleted:[])

let merge_delta_change () =
  let base = [ ("a", h 1); ("b", h 2) ] in
  let got = S.merge_delta ~base ~changes:[ ("a", h 9) ] ~deleted:[] in
  Alcotest.(check (list (pair string string)))
    "changed path carries the new hash"
    (S.normalize [ ("a", h 9); ("b", h 2) ]) got

let merge_delta_add () =
  let base = [ ("a", h 1) ] in
  let got = S.merge_delta ~base ~changes:[ ("new", h 7) ] ~deleted:[] in
  Alcotest.(check (list (pair string string)))
    "delta addition lands in the reconstructed manifest"
    (S.normalize [ ("a", h 1); ("new", h 7) ]) got

let merge_delta_delete () =
  let base = [ ("a", h 1); ("b", h 2) ] in
  let got = S.merge_delta ~base ~changes:[] ~deleted:[ "a" ] in
  Alcotest.(check (list (pair string string)))
    "deleted path drops from the reconstructed manifest"
    (S.normalize [ ("b", h 2) ]) got

let merge_delta_roundtrip_plan () =
  (* the point of merge_delta: reconstructed theirs + stored base feeds
     the SAME plan_of as v1. Server adds x, client edits a, client adds
     n, client deletes b → plan must show ours_add/theirs_mod/theirs_add/
     theirs_del and NOTHING else *)
  let base = [ ("a", h 1); ("b", h 2) ] in
  let server = [ ("a", h 1); ("b", h 2); ("x", h 5) ] in
  let theirs =
    S.merge_delta ~base ~changes:[ ("a", h 9); ("n", h 4) ] ~deleted:[ "b" ]
  in
  let plans = S.plan_of base server theirs in
  let outcomes =
    List.filter_map
      (fun { S.path; outcome } ->
        match outcome with
        | S.Unchanged -> None
        | o -> Some (path, o))
      plans
  in
  Alcotest.(check int) "four actionable rows" 4 (List.length outcomes);
  List.iter
    (fun (path, o) ->
      match path, o with
      | "x", S.Ours_keep -> ()
      | "a", S.Theirs_mod -> ()
      | "n", S.Theirs_add -> ()
      | "b", S.Theirs_del -> ()
      | p, _ -> failwith ("unexpected plan row " ^ p))
    outcomes

let () =
  Alcotest.run "sync" [
    ("classify", [
        test_case "same" `Quick classify_same;
        test_case "client-new" `Quick classify_client_new;
        test_case "server-new" `Quick classify_server_new;
        test_case "diverge" `Quick classify_diverge;
      ]);
    ("three-way", [
        test_case "unchanged" `Quick plan_unchanged;
        test_case "theirs-add" `Quick plan_theirs_add;
        test_case "ours-add" `Quick plan_ours_add;
        test_case "theirs-mod" `Quick plan_theirs_mod;
        test_case "ours-mod" `Quick plan_ours_mod;
        test_case "identical" `Quick plan_identical;
        test_case "conflict" `Quick plan_conflict;
        test_case "theirs-del" `Quick plan_theirs_del;
        test_case "ours-del" `Quick plan_ours_del;
        test_case "actionable-subset" `Quick actionable_only_transfers;
      ]);
    ("json", [
        test_case "report" `Quick json_report_roundtrip;
        test_case "plan" `Quick json_plan_roundtrip;
      ]);
    ("delta", [
        test_case "empty-delta-is-base" `Quick merge_delta_empty;
        test_case "change" `Quick merge_delta_change;
        test_case "add" `Quick merge_delta_add;
        test_case "delete" `Quick merge_delta_delete;
        test_case "roundtrip-plan" `Quick merge_delta_roundtrip_plan;
      ]);
  ]
