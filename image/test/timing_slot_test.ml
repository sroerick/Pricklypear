(** Timing fiber-slot (i-1788670103-5): [measure] binds a fiber-keyed
    slot; the session eval paths publish their budget into it via
    [note_eval_budget]; the sample reads THIS fiber's budget instead of
    the cross-request [Eval.last_budget] delta that raced when two
    requests' evals interleaved across DB awaits. No DB needed. *)

let () =
  Eio_main.run @@ fun _env ->
  (* A budget published inside the measured window is sampled. *)
  let b = Pricklypear_lisp.Eval.make_budget () in
  b.db_ms <- 42.5;
  b.db_hits <- 7;
  let _result, _total =
    Pricklypear_image_lib.Timing.measure ~route:"test/pub"
      (fun () ->
         Pricklypear_image_lib.Timing.note_eval_budget b;
         "ok")
  in
  let agg =
    List.find
      (fun a -> a.Pricklypear_image_lib.Timing.route = "test/pub")
      (Pricklypear_image_lib.Timing.aggregates ())
  in
  if abs_float (agg.Pricklypear_image_lib.Timing.db_ms -. 42.5) > 1e-9 then
    failwith (Printf.sprintf "db_ms expected 42.5, got %g" agg.db_ms);
  if agg.Pricklypear_image_lib.Timing.db_hits < 1.0 then
    failwith "db_hits expected >= 1";
  (* No publish inside the window -> zero-DB sample, not a foreign
     request's budget (the old race). *)
  let _r, _t =
    Pricklypear_image_lib.Timing.measure ~route:"test/empty" (fun () -> "x")
  in
  let agg =
    List.find
      (fun a -> a.Pricklypear_image_lib.Timing.route = "test/empty")
      (Pricklypear_image_lib.Timing.aggregates ())
  in
  if agg.Pricklypear_image_lib.Timing.db_ms <> 0.0
     || agg.Pricklypear_image_lib.Timing.db_hits <> 0.0
  then
    failwith
      (Printf.sprintf "empty window expected 0 db, got %g/%g" agg.db_ms
         agg.db_hits);
  (* [note_eval_budget] outside any Eio fiber / measure window is a
     no-op (the key read degrades to None on Unhandled). *)
  (match Pricklypear_image_lib.Timing.note_eval_budget b with
   | () -> ()
   | exception _ -> failwith "note_eval_budget must not raise outside Eio");
  (* perf 1104 item 1: X-Timing-Ms carries the WHOLE budget split, always,
     with no env var. Format: "<total> db=<db_ms> hits=<db_hits>
     steps=<steps>". The total is wall time so only the three accumulator
     fields are asserted exactly. *)
  let b2 = Pricklypear_lisp.Eval.make_budget () in
  b2.db_ms <- 12.3;
  b2.db_hits <- 4;
  b2.steps <- 91000;
  ignore
    (Pricklypear_image_lib.Timing.measure ~route:"test/hdr"
       (fun () ->
          Pricklypear_image_lib.Timing.note_eval_budget b2;
          "ok"));
  let hdr = Pricklypear_image_lib.Timing.timing_header () in
  (match hdr with
   | [ (name, v) ] ->
       if not (String.equal name "x-timing-ms") then
         failwith (Printf.sprintf "header name %s" name);
       let has sub =
         let n = String.length sub and m = String.length v in
         let rec go i = i + n <= m && (String.equal (String.sub v i n) sub || go (i + 1)) in
         n = 0 || go 0
       in
       List.iter
         (fun sub ->
            if not (has sub) then
              failwith (Printf.sprintf "header %S missing %S" v sub))
         [ "db=12.3"; "hits=4"; "steps=91000"; " " ]
   | _ -> failwith "expected exactly one x-timing-ms header");
  (* Consumed once: a second response in the same request must not
     re-report the previous route's sample. *)
  (match Pricklypear_image_lib.Timing.timing_header () with
   | [] -> ()
   | _ -> failwith "timing_header must be consumed by the first read");
  print_endline "timing_slot_test: OK"
