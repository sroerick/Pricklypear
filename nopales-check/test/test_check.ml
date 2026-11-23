(** CHECK-1: the static checker's contract on a small corpus.

    arity mismatch and a package-qualified unbound name are ERRORs; an
    unresolved qualified call is a WARN; let*/cond/guard/defmacro must produce
    nothing (the binding forms the corpus actually uses). *)

let () =
  let dir = Filename.concat (Filename.dirname Sys.executable_name) "fixtures" in
  let r = Pp_nopales_check.Check.run [ dir ] in
  let has code name =
    List.exists
      (fun (f : Pp_nopales_check.Check.finding) ->
        f.code = code && f.name = name)
      r.findings
  in
  let fail what =
    Printf.eprintf "check-test: FAIL %s (files=%d defines=%d errors=%d warns=%d)\n"
      what r.files r.defines r.errors r.warns;
    List.iter
      (fun (f : Pp_nopales_check.Check.finding) ->
        Printf.eprintf "  %s:%d %s %s\n" f.file f.line
          (Pp_nopales_check.Check.severity_name f.severity) f.message)
      r.findings;
    exit 1
  in
  if not (has "arity" "f") then fail "the arity mismatch on f is not reported";
  if not (has "unbound" "missing/lib") then fail "the unbound qualified name is not reported";
  if not (has "unknown-callable" "plot/chart") then
    fail "the unresolved qualified call is not reported";
  if not (has "unbound" "missing-name") then
    fail "the unbound bare name is not reported (macros are expanded)";
  let line_of code name =
    List.find_map
      (fun (f : Pp_nopales_check.Check.finding) ->
        if f.code = code && f.name = name then Some f.line else None)
      r.findings
  in
  if line_of "arity" "f" <> Some 3 then
    fail "the arity finding must point at the call line (3), not the define";
  if line_of "unbound" "missing-name" <> Some 1 then
    fail "a bare unbound name reports its enclosing define's line";
  if r.errors <> 2 then fail "expected exactly 2 errors";
  if r.warns <> 2 then fail "expected exactly 2 warnings";
  if Pp_nopales_check.Check.exit_code r <> 1 then
    fail "exit_code should be 1 with an error present";
  (* the same corpus with the two bad files removed is clean *)
  let clean = Pp_nopales_check.Check.run [ Filename.concat dir "clean.pp" ] in
  if clean.errors <> 0 || clean.warns <> 0 then
    fail "clean.pp produced a finding";
  if Pp_nopales_check.Check.exit_code clean <> 0 then
    fail "exit_code should be 0 when clean";
  (* the macro fixture proves expansion: without it, x would be unbound *)
  let mac = Pp_nopales_check.Check.run
      [ Filename.concat dir "macro.pp" ] in
  if mac.findings <> [] then fail "macro-introduced bindings produced a finding";
  (* the capability pass: pure, db, transitive db, tx, and the unknown class *)
  let effects_of report name =
    List.find_opt
      (fun (e : Pp_nopales_check.Check.define_effect) -> e.de_name = name)
      report.Pp_nopales_check.Check.effects
  in
  let classes report name =
    match effects_of report name with
    | Some e -> e.de_classes
    | None -> fail ("no effect row for " ^ name)
  in
  let eff =
    Pp_nopales_check.Check.run ~effects:true
      [ Filename.concat dir "effects.pp" ]
  in
  if classes eff "eff-pure" <> [ "pure" ] then
    fail "eff-pure must classify as pure";
  if classes eff "eff-read" <> [ "db" ] then
    fail "eff-read must classify as db";
  if classes eff "eff-via" <> [ "db" ] then
    fail "a wrapper around a DB reader must classify as db (transitive)";
  if not (List.mem "tx" (classes eff "eff-txn")) then
    fail "with-transaction must add the tx class";
  let unk =
    Pp_nopales_check.Check.run ~effects:true
      [ Filename.concat dir "unknown.pp" ]
  in
  if classes unk "u" <> [ "unknown" ] then
    fail "an unclassified qualified call must fail closed as unknown";
  (* txn hints are opt-in: two writes outside with-transaction only *)
  let has_code (report : Pp_nopales_check.Check.report) code name =
    List.exists
      (fun (f : Pp_nopales_check.Check.finding) ->
        f.code = code && f.name = name)
      report.Pp_nopales_check.Check.findings
  in
  let tx =
    Pp_nopales_check.Check.run ~txn:true
      [ Filename.concat dir "effects.pp" ]
  in
  if not (has_code tx "txn" "eff-write") then
    fail "two writes outside with-transaction must produce a txn hint";
  if has_code tx "txn" "eff-txn" then
    fail "with-transaction must suppress the txn hint";
  (* baseline: adopt the gate while the existing list is burned down *)
  let with_temp_pp text f =
    let path = Filename.temp_file "nopales-check" ".pp" in
    let oc = open_out path in
    output_string oc text;
    close_out oc;
    Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ())
      (fun () -> f path)
  in
  with_temp_pp "(define (f a b) a)\n(define (g) (f 1))\n" (fun path ->
      let base = Filename.temp_file "nopales-check" ".json" in
      Fun.protect ~finally:(fun () -> try Sys.remove base with _ -> ())
        (fun () ->
          let r1 = Pp_nopales_check.Check.run [ path ] in
          Pp_nopales_check.Check.write_baseline base r1.findings;
          let r2 = Pp_nopales_check.Check.run ~baseline:base [ path ] in
          if r2.findings <> [] || r2.suppressed <> 1 then
            fail "the baseline did not suppress the recorded arity finding";
          let oc = open_out_gen [ Open_append ] 0o600 path in
          output_string oc "(define (h) (f 2))\n";
          close_out oc;
          let r3 = Pp_nopales_check.Check.run ~baseline:base [ path ] in
          if r3.errors <> 1 || r3.suppressed <> 1 then
            fail "a second identical call site must not be suppressed";
          let oc = open_out path in
          output_string oc "(define (f a b) a)\n(define (g) (f 1 2 3))\n";
          close_out oc;
          let r4 = Pp_nopales_check.Check.run ~baseline:base [ path ] in
          if r4.errors <> 1 || r4.suppressed <> 0 then
            fail "a changed arity message must not match the baseline"));
  (* package manifests: --packages resolves requires, catches cycles, a
     missing dependency, duplicate package names and undeclared calls. *)
  let pkgs = Filename.concat (Filename.dirname dir) "package_fixtures" in
  let pr = Pp_nopales_check.Check.run ~packages:true [ pkgs ] in
  if not (has_code pr "package-missing" "missing") then
    fail "a missing (requires nope) was not reported";
  if not (has_code pr "package-cycle" "cycle-a") then
    fail "cycle-a was not reported as a package cycle";
  if not (has_code pr "package-undeclared" "c/z") then
    fail "undeclared's call to c/z was not reported";
  if not (has_code pr "package-duplicate" "dup") then
    fail "the duplicate dup manifest was not reported";
  if not (has_code pr "package-manifest" "bad.pp") then
    fail "the invalid manifest in bad.pp was not reported";
  if has_code pr "package-missing" "a" then
    fail "a's (requires b) was resolved but reported missing";
  let plain = Pp_nopales_check.Check.run [ pkgs ] in
  let is_pkg (f : Pp_nopales_check.Check.finding) =
    String.length f.code >= 8 && String.sub f.code 0 8 = "package-"
  in
  if List.exists is_pkg plain.Pp_nopales_check.Check.findings then
    fail "package checks must be opt-in (--packages)";
  Printf.printf "check-test: ok (%d files, %d defines, %d errors, %d warns)\n"
    r.files r.defines r.errors r.warns
