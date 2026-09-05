(* upsets-sim — the bench CLI (borge/upsets.borg simulator subsection).

   usage:
     upsets-sim --snapshot S.json --rule R.json
                [--window train|validate|holdout]
                [--out manifest.json] [--manifests-dir DIR]

   Windows: the CLI label overrides the snapshot header when given.
   Holdout discipline is enforced by the manifest trail: a second run
   of the same (dataset, rule) on a holdout window is refused. *)

open Upsets_bench

let usage () =
  prerr_endline
    "usage: upsets-sim --snapshot S.json --rule R.json [--window train|validate|holdout] [--out FILE] [--manifests-dir DIR]";
  exit 2

let () =
  let snapshot_file = ref None in
  let rule_file = ref None in
  let window = ref None in
  let out = ref None in
  let dump = ref None in
  let mdir = ref "results" in
  let argv = Sys.argv in
  let rec go i =
    if i >= Array.length argv then ()
    else
      let next () =
        if i + 1 >= Array.length argv then usage ();
        argv.(i + 1)
      in
      match argv.(i) with
      | "--snapshot" ->
          snapshot_file := Some (next ());
          go (i + 2)
      | "--rule" ->
          rule_file := Some (next ());
          go (i + 2)
      | "--window" ->
          window := Some (next ());
          go (i + 2)
      | "--out" ->
          out := Some (next ());
          go (i + 2)
      | "--dump" ->
          dump := Some (next ());
          go (i + 2)
      | "--manifests-dir" ->
          mdir := next ();
          go (i + 2)
      | _ -> go (i + 1)
  in
  go 1;
  match (!snapshot_file, !rule_file) with
  | Some sf, Some rf -> (
      let snap = Types.snapshot_of_json (Yojson.Basic.from_file sf) in
      let snap =
        match !window with
        | Some w -> { snap with Types.s_window_kind = w }
        | None -> snap
      in
      let rule = Types.rule_of_json (Yojson.Basic.from_file rf) in
      match Replay.run ~snapshot:snap ~rule ~manifests_dir:!mdir with
      | Error e ->
          prerr_endline ("error: " ^ e);
          exit 3
      | Ok rr ->
          let m = rr.rr_metrics in
          Printf.printf "snapshot   %s (%s)\n" snap.Types.s_name snap.Types.s_window_kind
            ;
          Printf.printf "rule_hash  %s\n" (Rules.rule_hash rule);
          Printf.printf
            "signals    %d (filled %d, unfilled %d, open-at-end %d)\n"
            m.Replay.mt_n_signals m.Replay.mt_n_filled m.Replay.mt_n_unfilled
            m.Replay.mt_n_open;
          Printf.printf "hit_rate   %.3f  avg_entry %.3f\n" m.Replay.mt_hit_rate
            m.Replay.mt_avg_entry;
          Printf.printf "pnl        %.2f  expectancy %.2f  max_dd %.2f\n"
            m.Replay.mt_total_pnl m.Replay.mt_expectancy m.Replay.mt_max_dd;
          Printf.printf "base-rate tax by entry band (L-1 — the number to beat):\n";
          List.iter
            (fun (b, tx, n) ->
              Printf.printf "  band %.2f  tax %.3f  (n=%d)\n" b tx n)
            m.Replay.mt_entry_tax;
          (match !out with
           | Some o ->
               Yojson.Basic.to_file o rr.rr_manifest;
               Printf.printf "manifest   %s\n" o
           | None -> ());
          (match !dump with
          | Some dp ->
              let ord_json (o : Fills.order) =
                `Assoc
                  [ ("market", `String o.ord_market);
                    ("side", `String (Types.string_of_side o.ord_side));
                    ("kind", `String (Types.string_of_kind o.ord_kind));
                    ("signal_ts", `Int o.ord_signal_ts);
                    ("fill_ts", `Int o.ord_fill_ts);
                    ("entry", `Float o.ord_entry);
                    ("size", `Float o.ord_size);
                  ]
              in
              let st_keys =
                List.map (fun s -> s.Replay.st_order.Fills.ord_fill_ts) rr.Replay.rr_settled
                |> List.sort compare
              in
              Yojson.Basic.to_file dp
                (`Assoc
                  [ ("signals",
                     `List
                       (List.map
                          (fun (s : Types.signal) ->
                            `Assoc
                              [ ("ts", `Int s.Types.sig_ts);
                                ("market", `String s.Types.sig_market);
                                ("side", `String (Types.string_of_side s.Types.sig_side));
                                ("kind", `String (Types.string_of_kind s.Types.sig_kind));
                                ("score", `Float s.Types.sig_score);
                              ])
                          rr.Replay.rr_signals));
                    ("orders", `List (List.map ord_json rr.Replay.rr_orders));
                    ("settled_fill_ts", `List (List.map (fun t -> `Int t) st_keys));
                  ]);
               Printf.printf "dump       %s\n" dp
           | None -> ()))
  | _ -> usage ()
