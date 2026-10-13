(* face hosts (auth.branch-faces): {face}.{user}.{base} host grammar.
   classify_host is pure-over-env (PP_BASE_DOMAIN read at call time), so the
   grammar is pinned with putenv + direct calls. Reserved labels: agent, dev
   (a CLOSED set fixed in code). Anything else (unreserved labels, >=3 labels,
   bad usernames) falls to Host_unknown exactly as before; single-label
   gardens unchanged; Host_legacy when PP_BASE_DOMAIN is unset. *)

module H = Pricklypear_image_lib.Http_core

let with_base_domain base f =
  let had = Sys.getenv_opt "PP_BASE_DOMAIN" in
  let () =
    match base with
    | Some b -> Unix.putenv "PP_BASE_DOMAIN" b
    | None -> Unix.putenv "PP_BASE_DOMAIN" "" in
  let r = f () in
  let () =
    match had with
    | Some v -> Unix.putenv "PP_BASE_DOMAIN" v
    | None -> Unix.putenv "PP_BASE_DOMAIN" "" in
  r

let headers h = [ ("host", h) ]

let is_garden n = function
  | H.Host_garden g -> String.equal g n
  | _ -> false

let is_userbranch u f = function
  | H.Host_userbranch (a, b) -> String.equal a u && String.equal b f
  | _ -> false

let test_grammar () =
  with_base_domain (Some "example.com") (fun () ->
      Alcotest.(check bool) "garden unchanged" true
        (is_garden "alice" (H.classify_host (headers "alice.example.com")));
      Alcotest.(check bool) "apex" true
        (match H.classify_host (headers "example.com") with
         | H.Host_apex -> true
         | _ -> false);
      Alcotest.(check bool) "agent face parses" true
        (is_userbranch "alice" "agent"
           (H.classify_host (headers "agent.alice.example.com")));
      Alcotest.(check bool) "dev face parses" true
        (is_userbranch "alice" "dev"
           (H.classify_host (headers "dev.alice.example.com")));
      Alcotest.(check bool) "unreserved label unknown" true
        (match H.classify_host (headers "evil.alice.example.com") with
         | H.Host_unknown -> true
         | _ -> false);
      Alcotest.(check bool) "three labels unknown" true
        (match H.classify_host (headers "x.y.alice.example.com") with
         | H.Host_unknown -> true
         | _ -> false);
      Alcotest.(check bool) "bad username unknown" true
        (match H.classify_host (headers "agent.bad_.example.com") with
         | H.Host_unknown -> true
         | _ -> false));
  with_base_domain None (fun () ->
      Alcotest.(check bool) "legacy when base unset" true
        (match H.classify_host (headers "agent.alice.example.com") with
         | H.Host_legacy -> true
         | _ -> false))

let () =
  Alcotest.run "face-hosts"
    [ ("grammar", [ Alcotest.test_case "reserved-label grammar" `Quick test_grammar ]) ]
