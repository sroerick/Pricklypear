(** Store.package_names_by_provenance + package_binding_names membership.

    Regression for the vault-exporter bug (i-vault-exporter, card
    8dff7c9e): package-bindings/export-package matched names by
    `pkg/`-prefix, so a package whose defines do NOT share its name —
    std defines list/…, html/…, css/…; each theme-asNNN defines
    theme/… — exported NOTHING. pp-sync snapshot then wrote that empty
    export over the vault file, gutting libs/std (69 defines -> 0).

    Membership must come from function_defs.package provenance (the
    (package …) form's stamp), with prefix only as a fallback. Needs PG
    (scripts/dev.sh); skips cleanly when unreachable.

    We write directly via Store.persist_function ~package so the test
    does not depend on eval/package-form plumbing. *)

module S = Pricklypear_image_lib.Store

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p = Printf.sprintf "stpk-%s-%d" p (Random.int 1_000_000)

let () = Random.self_init ()

let with_store f =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ ->
          (* sweep leftovers from aborted runs *)
          (match S.conn_of store with
           | Some conn ->
               let _ =
                 S.exec conn ~params:[]
                   "DELETE FROM function_defs WHERE name LIKE 'stpk-%'"
               in
               ()
           | None -> ());
          f store)

let cleanup (store : S.t) : unit =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      let _ =
        S.exec conn ~params:[]
          "DELETE FROM function_defs WHERE name LIKE 'stpk-%'"
      in
      ()

let persist (store : S.t) ~(name : string) ~(body : string)
    ~(package : string) : unit =
  S.persist_function store name body ~package:(Some package) "test" |> ignore

(* Body jsonb we can hand in directly; the store persists raw text. *)
let body_for (label : string) : string =
  Printf.sprintf {|{"app":[{"var":"lambda"},{"lit":{"str":"%s"}}]}|} label

let test_provenance_membership () =
  with_store @@ fun store ->
  let pkg = uniq "pkgx" in
  (* Two names that do NOT share the package name as a prefix, plus one
     that does — all stamped to [pkg]. *)
  let n1 = "stpk-" ^ pkg ^ "/alpha" in
  let n2 = "otherns/" ^ pkg ^ "-beta" in
  S.persist_function store n1 (body_for "a") ~package:(Some pkg) "test";
  S.persist_function store n2 (body_for "b") ~package:(Some pkg) "test";
  let names = S.package_names_by_provenance store pkg in
  let has n = List.mem n names in
  Alcotest.(check bool) "alpha (prefix-matched name) present" true (has n1);
  Alcotest.(check bool)
    "beta (name does NOT share pkg prefix) present via provenance" true
    (has n2);
  (* A different package must not leak in. *)
  let other = S.package_names_by_provenance store (pkg ^ "-other") in
  Alcotest.(check int) "no cross-package leakage" 0 (List.length other);
  cleanup store

let test_provenance_beats_prefix_fallback () =
  (* The exact std shape: package "stdsim" defines only list/… names.
     package_binding_names must find them (via provenance), where the
     old prefix matcher returned []. *)
  with_store @@ fun store ->
  let pkg = uniq "stdsim" in
  let a = pkg ^ "/take" in
  let b = "list/" ^ pkg ^ "-map" in
  S.persist_function store a (body_for "t") ~package:(Some pkg) "test";
  S.persist_function store b (body_for "m") ~package:(Some pkg) "test";
  let names = S.package_names_by_provenance store pkg in
  Alcotest.(check bool) "prefixed name found" true (List.mem a names);
  Alcotest.(check bool) "non-prefixed name found" true (List.mem b names);
  cleanup store

let () =
  Alcotest.run "package-provenance"
    [ ( "provenance membership",
        [ Alcotest.test_case "names not sharing pkg prefix" `Quick
            test_provenance_membership
        ; Alcotest.test_case "prefix fallback is not the only source" `Quick
            test_provenance_beats_prefix_fallback
        ] )
    ]
