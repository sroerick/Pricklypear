(** spec_delegation loader enforcement (2026-09-11, no DB).

    A delegated session (t.delegated; the token resolve path will set
    it once it lands — no producer yet, so every session is self) is a
    DATA session: it may load SEEDED packages only and never persists
    code. These tests pin the enforcement points, no DB needed
    (Store.make without connect, session_split_test precedent):

      D1. load-library denies a non-seeded package (loader gate).
      D2. load-library of a seeded package (std) still loads env-only
          and its defines bind — persist skipped, not failed.
      D3. (reseed-package) is denied before the admin check.
      D4. (deftype) is denied (schema is code-adjacent).
      D5. A self session is unchanged: the same non-seeded load fails
          with the plain loader error, not the delegation denial. *)

module S = Pricklypear_image_lib.Session
module Ast = Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

(* substring containment (contains_seq is not in this stdlib) *)
let contains_seq (hay : string) (needle : string) : bool =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  if m = 0 then true else go 0

(* Walk up from the dune cwd until [rel] exists (repo root), like
   loader_test's find_up, then point PP_LIBS_DIR there. *)
let point_pp_libs_dir () : unit =
  let rec up n dir =
    if n > 10 then failwith "delegation_test: libs/ not found"
    else
      let p = Filename.concat dir "libs/std/lib.pp" in
      if Sys.file_exists p then Unix.putenv "PP_LIBS_DIR"
                                (Filename.concat dir "libs")
      else up (n + 1) (Filename.dirname dir)
  in
  up 0 (Sys.getcwd ())

let make_session_on ~branch_id ~branch_name ~delegated =
  let store =
    Pricklypear_image_lib.Store.make
      ~cfg:(Pricklypear_image_lib.Store.config_from_env ())
      ~branch_id
      ()
  in
  let manifest =
    Pricklypear_common.Manifest.make
      ~user_id:"tester" ~username:"tester" ~branch_id
      ~branch_name
      ~boot_mode:Pricklypear_common.Manifest.Boot_on_connect
      ~image_version:"test" ()
  in
  let t = S.make ~manifest ~store () in
  t.delegated <- delegated;
  t

(* Existing call sites: default branch (main_id / tester/personal — the
   manifest lies about the branch on purpose; W2 pins that shape). *)
let make_session ~delegated =
  make_session_on ~branch_id:main_id ~branch_name:"tester/personal"
    ~delegated

let rec contains_sub s sub =
  String.length s >= String.length sub
  && (String.sub s 0 (String.length sub) = sub
      || contains_sub (String.sub s 1 (String.length s - 1)) sub)

(* eval-touching cases run inside the Eio scheduler (Control_rim
   captures Eio effects; rls_floor_test / session_split_test precedent). *)
let in_eio f = Eio_main.run (fun _ -> f ())

(* D1: the loader gate fires before any Store/lib work. *)
let test_delegated_load_library_gate () =
  point_pp_libs_dir ();
  let t = make_session ~delegated:true in
  match S.load_library t "definitely-not-a-package" with
  | Ok msg -> Alcotest.fail (Printf.sprintf "expected denial, got Ok: %s" msg)
  | Error e ->
      Alcotest.(check bool) "denial names the rule" true
        (contains_seq e "seeded packages only");
      Alcotest.(check bool) "denial names the package" true
        (contains_seq e "definitely-not-a-package")

(* runtime lists are cons chains (VList is reader-only sugar), so count
   a proper list through both shapes. *)
let rec proper_list_len (v : Ast.value) : int option =
  match v with
  | Ast.VNil -> Some 0
  | Ast.VPair (_, tl) -> Option.map (fun n -> n + 1) (proper_list_len tl)
  | Ast.VList xs -> Some (List.length xs)
  | _ -> None

(* D2: a seeded package loads env-only; defines bind and persist is
   skipped silently (no DB, no raise). *)
let test_delegated_seeded_load_still_works () =
  point_pp_libs_dir ();
  let t = make_session ~delegated:true in
  (match S.load_library t "std" with
   | Ok msg ->
       Alcotest.(check bool) "loaded summary" true
         (contains_seq msg "loaded ")
   | Error e -> Alcotest.fail ("std failed to load delegated: " ^ e));
   (match S.eval_string_rim t "(list/range 5)" with
    | v ->
        (match proper_list_len v with
         | Some 5 -> ()
         | Some n ->
             Alcotest.fail
               (Printf.sprintf "list/range: expected 5 elems, got %d" n)
         | None ->
             Alcotest.fail
               (Printf.sprintf "list/range: expected a proper list, got %s"
                  (Pricklypear_lisp.Reader.pp_value v)))
    | exception Ast.Interp_error e ->
        Alcotest.fail ("list/range raised: " ^ Ast.pp_interp_error e))

(* D3: reseed-package is denied for delegated sessions (checked before
   the admin branch — a delegated admin still may not reseed). The prim
   reports Error as a VStr, not an exception. *)
let test_delegated_reseed_denied () =
  point_pp_libs_dir ();
  let t = make_session ~delegated:true in
  match S.eval_string_rim t "(reseed-package \"std\")" with
  | Ast.VStr msg ->
      Alcotest.(check bool) "reseed denial" true
        (contains_seq msg "denied for delegated sessions")
  | v ->
      Alcotest.fail
        (Printf.sprintf "expected denial, got: %s"
           (Pricklypear_lisp.Reader.pp_value v))

(* D4: deftype is denied for delegated sessions (schema authority). *)
let test_delegated_deftype_denied () =
  point_pp_libs_dir ();
  let t = make_session ~delegated:true in
  match S.eval_string_rim t "(deftype 'widget 'name)" with
  | exception Ast.Interp_error e ->
      let msg = Ast.pp_interp_error e in
      Alcotest.(check bool) "deftype denial" true
        (contains_sub msg "denied for delegated sessions")
  | v ->
      Alcotest.fail
        (Printf.sprintf "expected denial, got: %s"
           (Pricklypear_lisp.Reader.pp_value v))

(* D5: self sessions (delegated=false) keep the old behavior: the same
   load takes the normal loader path and fails with the plain loader
   error, not the delegation denial. *)
let test_self_load_unchanged () =
  point_pp_libs_dir ();
  let t = make_session ~delegated:false in
  match S.load_library t "definitely-not-a-package" with
  | Ok msg -> Alcotest.fail (Printf.sprintf "expected loader error, got Ok: %s" msg)
  | Error e ->
      Alcotest.(check bool) "plain loader error, no delegation text" true
        (not (contains_seq e "seeded packages only"))

(* D6: THE PRODUCER (auth.delegation-tokens): attach_identity with
   ~delegated ~actor builds the data-only on-behalf-of session: owner
   manifest, actor provenance, denied admin, locked branch. No DB
   (checkout_personal_now false + same username skips branch work). *)
let test_delegated_attach_producer () =
  let t = make_session ~delegated:false in
  S.attach_identity t ~user_id:"tester-uuid" ~username:"tester"
    ~is_admin:true  (* owner is admin — but delegation must NOT inherit it *)
    ~auth_token:None ~checkout_personal_now:false
    ~user_type:"human" ~parent_user_id:None
    ~delegated:true ~actor:(Some "gregor") ();
  Alcotest.(check bool) "delegated flag set" true t.delegated ;
  Alcotest.(check bool) "actor recorded" true
    (t.manifest.actor = Some "gregor") ;
  Alcotest.(check bool) "username stays owner" true
    (t.manifest.username = "tester") ;
  Alcotest.(check bool) "admin NOT inherited" false t.is_admin ;
  Alcotest.(check bool) "branch locked" true t.agent_branch_lock

(* branching.package-updates W1: load-library on a NON-MAIN branch warns
   that it pins the package (i-1790493758: repair scripts loaded the
   release file onto a shared branch and silently reverted in-flight
   pins). No DB: delegated session, seeded package, env-only load — the
   D2 shape, on a non-main store branch. *)
let test_nonmain_load_warns () =
  point_pp_libs_dir ();
  let t =
    make_session_on
      ~branch_id:"00000000-0000-0000-0000-000000000002"
      ~branch_name:"tester/personal" ~delegated:true
  in
  match S.load_library t "std" with
  | Ok msg ->
      Alcotest.(check bool) "warning present" true
        (contains_seq msg "WARNING");
      Alcotest.(check bool) "warning names non-main" true
        (contains_seq msg "non-main");
      Alcotest.(check bool) "warning names the branch" true
        (contains_seq msg "tester/personal");
      Alcotest.(check bool) "load summary still present" true
        (contains_seq msg "loaded ")
  | Error e -> Alcotest.fail ("std failed to load on non-main: " ^ e)

(* W2: system/main loads stay silent. Keyed on the STORE branch, not the
   manifest — the manifest here still says tester/personal while the
   store sits on main, exactly the reseed/with_branch shape (and boot
   seed additionally holds seed_additive). *)
let test_main_load_no_warning () =
  point_pp_libs_dir ();
  let t = make_session ~delegated:true in
  match S.load_library t "std" with
  | Ok msg ->
      Alcotest.(check bool) "no warning on main" true
        (not (contains_seq msg "WARNING"))
  | Error e -> Alcotest.fail ("std failed to load on main: " ^ e)

(* D7: a plain (self) attach resets provenance — actor cleared, admin
   as given, no lock. Attach twice to prove re-attach (cookie +
   delegated interleave) cannot leave stale actor behind. *)
let test_self_attach_clears_actor () =
  let t = make_session ~delegated:false in
  S.attach_identity t ~user_id:"tester-uuid" ~username:"tester"
    ~is_admin:true ~auth_token:None ~checkout_personal_now:false
    ~delegated:true ~actor:(Some "gregor") ();
  S.attach_identity t ~user_id:"tester-uuid" ~username:"tester"
    ~is_admin:true ~auth_token:None ~checkout_personal_now:false ();
  Alcotest.(check bool) "delegated cleared" false t.delegated ;
  Alcotest.(check bool) "actor cleared" true (t.manifest.actor = None) ;
  Alcotest.(check bool) "admin restored" true t.is_admin ;
  Alcotest.(check bool) "lock released" false t.agent_branch_lock

let () =
  Alcotest.run "delegation_loader"
    [ ( "delegation is for data, never for code",
        [ Alcotest.test_case "load-library gate denies non-seeded" `Quick
            (fun () -> in_eio test_delegated_load_library_gate)
        ; Alcotest.test_case "seeded package loads env-only" `Quick
            (fun () -> in_eio test_delegated_seeded_load_still_works)
        ; Alcotest.test_case "reseed-package denied" `Quick
            (fun () -> in_eio test_delegated_reseed_denied)
        ; Alcotest.test_case "deftype denied" `Quick
            (fun () -> in_eio test_delegated_deftype_denied)
        ; Alcotest.test_case "self sessions unchanged" `Quick
            (fun () -> in_eio test_self_load_unchanged)
        ; Alcotest.test_case "attach producer: owner manifest + actor + no admin + lock"
            `Quick test_delegated_attach_producer
        ; Alcotest.test_case "self attach resets provenance" `Quick
            test_self_attach_clears_actor
        ; Alcotest.test_case "non-main load-library warns (pin hazard)" `Quick
            (fun () -> in_eio test_nonmain_load_warns)
        ; Alcotest.test_case "system/main load-library stays silent" `Quick
            (fun () -> in_eio test_main_load_no_warning)
        ] ) ]
