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
  print_endline "timing_slot_test: OK"
