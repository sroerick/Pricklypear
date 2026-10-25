(** Tests for remote/lib/job_worker.ml — the placement gate.

    Pure: no server, no database, no transport. The point of extracting
    [gate] from the loop is exactly this — the rule that decides whether an
    out-of-image worker may run a job is ordinary code over an AST, so it can
    be pinned without standing up an image.

    The high-value cases are the two DECLINES that look runnable:
      - a body that grows (with-transaction …) later must inherit the refusal,
        which it does because the classification is Effect.infer and not a
        curated list of function names;
      - an unreadable def declines instead of raising, because a worker whose
        value is draining a queue must not die on one bad row. *)

open Pricklypear_lisp

let verdict_pp = function
  | Job_worker.Run -> "run"
  | Job_worker.Decline r -> "decline:" ^ r

let check label expected src =
  Alcotest.(check string) label expected
    (verdict_pp (Job_worker.gate (Some src)))

let gate_tests = [
  (* --- the conservative default ------------------------------------- *)
  ("an unresolvable def declines rather than running", `Quick, fun () ->
    Alcotest.(check string) "no source" "decline:def-not-resolvable-locally"
      (verdict_pp (Job_worker.gate None)));

  ("an unparseable def declines rather than raising", `Quick, fun () ->
    (* A worker that raises here stops draining the queue; a decline costs
       one claim cycle and the row goes back for the image runner. *)
    check "garbage" "decline:def-unparseable" "(define (f");

  (* --- a runnable body ---------------------------------------------- *)
  ("a pure body runs", `Quick, fun () ->
    check "arithmetic" "run" "(define (total xs) (fold-left + 0 xs))");

  ("a read-only body runs", `Quick, fun () ->
    (* Reads are lens-eligible: the proxy rim forwards the read prim, the
       result comes back, nothing about it is in-image-only. *)
    check "read" "run" "(define (n) (length (rows \"thing\")))");

  (* --- the two refusals that matter --------------------------------- *)
  ("a body holding with-transaction is refused", `Quick, fun () ->
    (* with-transaction hands a LAMBDA to a server-only prim. Lambdas cannot
       cross the wire, and the txn must be the one the writes land in. *)
    check "txn" "decline:txn-body-in-image-only"
      "(define (f) (with-transaction (lambda () (create \"thing\" 1))))");

  ("a body holding with-step is refused", `Quick, fun () ->
    (* with-step's checkpoint must share the thunk's transaction — that IS
       exactly-once. Honorable in one place only. *)
    check "step" "decline:txn-body-in-image-only"
      "(define (f) (with-transaction (lambda () (with-step \"k\" (lambda () 1)))))");

  ("a body that forks is refused", `Quick, fun () ->
    (* Branch mutation is shared server state this client cannot see
       consistently — same reason as txn, different mechanism. *)
    check "fork" "decline:branch-effect-in-image-only" "(define (f) (fork \"copy\"))");

  ("the refusal is inherited through a called user fn", `Quick, fun () ->
    (* THE REGRESSION THIS PINS: a body can look harmless and reach a txn
       through a helper. A name-allowlist would miss this; Effect.infer with
       known_fn sees it. *)
    let bodies =
      [ ("inner", Reader.read_one "(lambda () (with-transaction (lambda () 1)))");
        ("outer", Reader.read_one "(lambda () (inner))") ]
    in
    let known = Effect.known_fn_of_bindings bodies in
    let verdict =
      Job_worker.gate ~known_fn:known (Some "(define (outer) (inner))")
    in
    Alcotest.(check string) "inherited" "decline:txn-body-in-image-only"
      (verdict_pp verdict));
]

(* --- the loop's ordering ---------------------------------------------
   [tick] is where a decline could wrongly spend an attempt, so its ORDER
   (claim -> decide -> release|run+finish) is pinned here with both sides
   faked. The fake records which channel each expression went down, because
   the protocol verbs MUST NOT reach local_eval: they are server-only, and a
   proxy rim refuses them. *)
type rec_log = {
  mutable server : string list;
  mutable local : string list;
  mutable slept : float list;
  mutable logs : string list;
}

let empty_log () = { server = []; local = []; slept = []; logs = [] }

(* A fake env: [row] is what claim-job returns, [src] what the def resolves
   to, [finish] what the server answers to a terminal write. *)
let starts_with (p : string) (s : string) : bool =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let fake ~row ?(src = None) ?(finish = true) () =
  let lg = empty_log () in
  let env : Job_worker.env =
    { tag = "w1"
    ; branch = "acme/main"
    ; interval = 5.0
    ; server_eval =
        (fun expr ->
          lg.server <- lg.server @ [ expr ];
          if starts_with "(claim-job" expr then row
          else if starts_with "(job-finish" expr then Ast.VBool finish
          else Ast.VBool true)
    ; local_eval =
        (fun expr ->
          lg.local <- lg.local @ [ expr ];
          Ast.VInt 42L)
    ; def_source = (fun _ -> src)
    ; sleep = (fun s -> lg.slept <- lg.slept @ [ s ])
    ; log = (fun m -> lg.logs <- lg.logs @ [ m ])
    }
  in
  (env, lg)

let has_call (prefix : string) (field : string list) =
  List.exists (fun e -> starts_with prefix e) field

let tick_tests =
  let row json = Ast.VStr json in
  [
    ("an empty queue is idle and does nothing else", `Quick, fun () ->
      let env, lg = fake ~row:Ast.VNil () in
      Alcotest.(check bool) "not busy" false (Job_worker.tick env);
      Alcotest.(check int) "one server call (the claim)" 1 (List.length lg.server);
      Alcotest.(check int) "nothing ran locally" 0 (List.length lg.local));

    ("a decline RELEASES and never finishes", `Quick, fun () ->
      (* The whole point of release: nothing ran, so no attempt is spent. If
         this ever sent job-finish the row would march toward 'dead'. *)
      let env, lg =
        fake ~row:(row "{\"id\":\"j1\",\"fn\":\"f\",\"args\":\"null\",\"branch\":\"b\"}")
          ~src:None ()
      in
      Alcotest.(check bool) "handled" true (Job_worker.tick env);
      Alcotest.(check bool) "released" true (has_call "(job-release" lg.server);
      Alcotest.(check bool) "never finished" false (has_call "(job-finish" lg.server);
      Alcotest.(check int) "body never ran" 0 (List.length lg.local));

    ("a txn body is declined, not run", `Quick, fun () ->
      let env, lg =
        fake ~row:(row "{\"id\":\"j1\",\"fn\":\"f\",\"args\":\"null\",\"branch\":\"b\"}")
          ~src:(Some "(define (f) (with-transaction (lambda () 1)))") ()
      in
      Alcotest.(check bool) "handled" true (Job_worker.tick env);
      Alcotest.(check int) "body never ran" 0 (List.length lg.local);
      Alcotest.(check bool) "released" true (has_call "(job-release" lg.server));

    ("a runnable body runs locally and finishes with its value", `Quick, fun () ->
      (* The body must go through local_eval (hybrid placement), NOT back to
         the server wholesale — that is the reason to run it here. *)
      let env, lg =
        fake ~row:(row "{\"id\":\"j1\",\"fn\":\"f\",\"args\":\"null\",\"branch\":\"b\"}")
          ~src:(Some "(define (f) 42)") ()
      in
      Alcotest.(check bool) "handled" true (Job_worker.tick env);
      Alcotest.(check (list string)) "body ran locally" [ "(f)" ] lg.local;
      (* Everything is a quoted literal: id, tag, and the result text. The
         result "42" rides as a STRING whose contents happen to be valid
         jsonb — job-finish's as_json passes a VStr through unquoted. *)
      Alcotest.(check bool) "finished done with the value" true
        (has_call "(job-finish \"j1\" \"w1\" \"done\" \"42\")" lg.server));

    ("a raising body is finished as failed, not dropped", `Quick, fun () ->
      (* A worker that swallows the error leaves the row 'running' until the
         lease lapses. Report it and let the row's retry budget decide. *)
      let env, lg = fake ~row:(row "{\"id\":\"j1\",\"fn\":\"f\",\"args\":\"null\",\"branch\":\"b\"}")
          ~src:(Some "(define (f) 42)") () in
      let env = { env with Job_worker.local_eval = (fun _ -> failwith "boom") } in
      Alcotest.(check bool) "handled" true (Job_worker.tick env);
      Alcotest.(check bool) "reported failed" true
        (has_call "(job-finish \"j1\" \"w1\" \"failed\"" lg.server));

    ("a lost lease is logged rather than silently dropped", `Quick, fun () ->
      let env, lg =
        fake ~row:(row "{\"id\":\"j1\",\"fn\":\"f\",\"args\":\"null\",\"branch\":\"b\"}")
          ~src:(Some "(define (f) 42)") ~finish:false ()
      in
      Alcotest.(check bool) "handled" true (Job_worker.tick env);
      Alcotest.(check bool) "says so" true
        (List.exists
           (fun m ->
             let p = "lease lost" in
             let n = String.length m and k = String.length p in
             let rec f i = i + k <= n && (String.sub m i k = p || f (i + 1)) in
             f 0)
           lg.logs));
  ]
let expr_tests = [
  ("quotes a tag as a string literal", `Quick, fun () ->
    Alcotest.(check string) "plain"
      "(claim-job \"w1\" \"acme/main\")"
      (Job_worker.claim_expr ~tag:"w1" ~branch:"acme/main"));

  ("escapes a quote in a tag", `Quick, fun () ->
    (* A tag reaching the image unescaped would be code, not data. *)
    Alcotest.(check string) "escaped"
      "(job-heartbeat \"id\" \"a\\\"b\")"
      (Job_worker.heartbeat_expr ~id:"id" ~tag:"a\"b"));

  ("passes a result string through as json text", `Quick, fun () ->
    Alcotest.(check string) "json"
      "(job-finish \"id\" \"w1\" \"done\" \"{\\\"n\\\":1}\")"
      (Job_worker.finish_done_expr ~id:"id" ~tag:"w1" ~json:"{\"n\":1}"));

  ("release is the decline verb", `Quick, fun () ->
    Alcotest.(check string) "release"
      "(job-release \"id\" \"w1\")"
      (Job_worker.release_expr ~id:"id" ~tag:"w1"));
]

(* --- call shape must match the image -------------------------------- *)
let call_tests =
  let call args =
    Job_worker.call_src { id = "i"; fn = "f"; args; branch = "b" }
  in
  [
    ("no args fires as a bare call", `Quick, fun () ->
      Alcotest.(check string) "empty" "(f)" (call ""));
    ("an empty json array is no args", `Quick, fun () ->
      Alcotest.(check string) "array" "(f)" (call "[]"));
    ("json null is no args", `Quick, fun () ->
      Alcotest.(check string) "null" "(f)" (call " null "));
    ("a json array splices as one form", `Quick, fun () ->
      Alcotest.(check string) "spliced" "(f [1, 2])" (call "[1, 2]"));
    ("a bare scalar splices positionally", `Quick, fun () ->
      (* Matches image Jobs.eval_src: anything not empty/[]/null is spliced. *)
      Alcotest.(check string) "scalar" "(f 7)" (call "7"));
  ]

(* --- row decoding --------------------------------------------------- *)
let claim_tests = [
  ("decodes a claimed row", `Quick, fun () ->
    match
      Job_worker.claim_of_json
        "{\"id\":\"abc\",\"fn\":\"work\",\"args\":\"[1]\",\"branch\":\"acme/main\"}"
    with
    | None -> Alcotest.fail "expected a claim"
    | Some c ->
      Alcotest.(check string) "id" "abc" c.Job_worker.id;
      Alcotest.(check string) "fn" "work" c.Job_worker.fn;
      Alcotest.(check string) "args" "[1]" c.Job_worker.args;
      Alcotest.(check string) "branch" "acme/main" c.Job_worker.branch);

  ("a structured args value is re-rendered as json text", `Quick, fun () ->
    match
      Job_worker.claim_of_json
        "{\"id\":\"abc\",\"fn\":\"work\",\"args\":[1,\"x\"],\"branch\":\"b\"}"
    with
    | None -> Alcotest.fail "expected a claim"
    | Some c -> Alcotest.(check string) "args" "[1,\"x\"]" c.Job_worker.args);

  ("null claim (empty queue) decodes to None", `Quick, fun () ->
    Alcotest.(check bool) "null" true
      (Job_worker.claim_of_json "null" = None));

  ("a row without an id is not a claim", `Quick, fun () ->
    Alcotest.(check bool) "no id" true
      (Job_worker.claim_of_json "{\"fn\":\"work\"}" = None));
]

let () =
  Alcotest.run "job_worker"
    [ ("gate", gate_tests)
    ; ("tick", tick_tests)
    ; ("expr", expr_tests)
    ; ("call", call_tests)
    ; ("claim", claim_tests) ]
