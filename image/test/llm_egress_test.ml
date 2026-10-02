(** Egress guard for user-configured LLM bases (2026-10 security pass).

    auth.user-llm-keys (/settings/keys) lets any authenticated user set
    the provider BASE; Llm_client shells curl server-side against it.
    egress_pin_arg applies the SAME private-space envelope as the
    Nopales http-get prim and pins the validated address into curl via
    --resolve. Pure tests: IP-literal URLs only, so the resolver never
    hits the network (getaddrinfo answers literals locally). *)

module L = Pricklypear_image_lib.Llm_client

let expect_blocked (label : string) (url : string) : unit =
  match L.egress_pin_arg url with
  | Error e ->
      Alcotest.(check bool) (label ^ ": blocked with a reason") true
        (String.length e > 5)
  | Ok pin ->
      Alcotest.failf "expected %s to be blocked, got Ok %s" url
        (match pin with Some p -> p | None -> "(no pin)")

let expect_pin (url : string) (pin : string) : unit =
  match L.egress_pin_arg url with
  | Error e -> Alcotest.failf "expected %s pinned, got Error %s" url e
  | Ok (Some p) -> Alcotest.(check string) (url ^ " pin") pin p
  | Ok None -> Alcotest.failf "expected %s pinned, got none" url

let expect_no_pin (url : string) : unit =
  match L.egress_pin_arg url with
  | Error e -> Alcotest.failf "expected %s unpinned, got Error %s" url e
  | Ok (Some p) -> Alcotest.failf "expected %s unpinned, got pin %s" url p
  | Ok None -> ()

let tests =
  [ ( "llm-egress: loopback/private/link-local bases are blocked"
    , `Quick
    , fun () ->
        expect_blocked "v4 loopback" "http://127.0.0.1:8000/v1";
        expect_blocked "localhost" "http://localhost:8000/v1";
        expect_blocked "rfc1918" "http://10.1.2.3/v1";
        expect_blocked "rfc1918 172" "http://172.16.0.1/v1";
        expect_blocked "rfc1918 192" "http://192.168.1.1/v1";
        expect_blocked "link-local metadata" "http://169.254.169.254/latest";
        expect_blocked "v6 loopback" "http://[::1]:8000/v1";
        expect_blocked "v6 ula" "http://[fd00::1]/v1";
        expect_blocked "v6 link-local" "http://[fe80::1]/v1" )
  ; ( "llm-egress: public IP-literal bases pin the validated address"
    , `Quick
    , fun () ->
        expect_pin "https://1.1.1.1/openai/v1" "--resolve '1.1.1.1:443:1.1.1.1'";
        expect_pin "http://8.8.8.8/v1" "--resolve '8.8.8.8:80:8.8.8.8'";
        expect_pin "http://1.1.1.1:8000/v1" "--resolve '1.1.1.1:8000:1.1.1.1'" )
  ; ( "llm-egress: opt-out env is honored (one shared switch)"
    , `Quick
    , fun () ->
        Unix.putenv "PP_NET_EGRESS_UNRESTRICTED" "1";
        (try expect_no_pin "http://127.0.0.1:8000/v1"
         with exn ->
           Unix.putenv "PP_NET_EGRESS_UNRESTRICTED" "0";
           raise exn);
        Unix.putenv "PP_NET_EGRESS_UNRESTRICTED" "0";
        (* restored: guard active again *)
        expect_blocked "restored" "http://10.1.2.3/v1" )
  ]

let () = Alcotest.run "pricklypear-llm-egress" [ ("egress", tests) ]
