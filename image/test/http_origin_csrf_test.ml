(** security.origin-authenticated-verbs: the origin/CSRF gate, the CSP
    policy and the nonce injection are pure functions in [Http_core], so
    they are pinned here without a server or a database. *)

open Pricklypear_image_lib

let contains ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  go 0

let check msg expected actual = Alcotest.(check bool) msg expected actual

let tests =
  [ ( "origin: same-origin mutation allowed"
    , `Quick
    , fun () ->
        check "same" true
          (Http_core.same_origin_ok
             [ ("host", "pp.example"); ("origin", "https://pp.example") ]) )
  ; ( "origin: cross-origin mutation rejected"
    , `Quick
    , fun () ->
        check "cross" false
          (Http_core.same_origin_ok
             [ ("host", "pp.example"); ("origin", "https://evil.example") ]) )
  ; ( "origin: cookie-borne mutation with no origin/referer rejected"
    , `Quick
    , fun () ->
        check "absent" false
          (Http_core.same_origin_ok [ ("host", "pp.example") ]) )
  ; ( "origin: referer fallback accepted"
    , `Quick
    , fun () ->
        check "referer" true
          (Http_core.same_origin_ok
             [ ("host", "pp.example")
             ; ("referer", "https://pp.example/notes?slug=x") ]) )
  ; ( "csrf: cookie-borne kernel POST needs the custom header"
    , `Quick
    , fun () ->
        check "missing" true
          (Http_core.csrf_violation ~meth:"POST" ~path:"/api/eval"
             ~headers:[] ~session_cookie:true);
        check "present" false
          (Http_core.csrf_violation ~meth:"POST" ~path:"/api/eval"
             ~headers:[ ("x-pp-csrf", "1") ] ~session_cookie:true) )
  ; ( "csrf: Basic/Bearer clients carry no cookie and are unaffected"
    , `Quick
    , fun () ->
        check "no cookie" false
          (Http_core.csrf_violation ~meth:"POST" ~path:"/api/eval"
             ~headers:[] ~session_cookie:false) )
  ; ( "csrf: reads and non-kernel paths are unaffected"
    , `Quick
    , fun () ->
        check "get" false
          (Http_core.csrf_violation ~meth:"GET" ~path:"/api/eval"
             ~headers:[] ~session_cookie:true);
        check "ui path" false
          (Http_core.csrf_violation ~meth:"POST" ~path:"/notes"
             ~headers:[] ~session_cookie:true) )
  ; ( "csp: full default-src policy admits exactly the request nonce"
    , `Quick
    , fun () ->
        let p = Http_core.csp_policy ~nonce:"abc123" () in
        check "default-src" true (contains ~needle:"default-src 'self'" p);
        check "script nonce" true
          (contains ~needle:"script-src 'self' 'nonce-abc123'" p);
        check "style nonce" true
          (contains ~needle:"style-src 'self' 'nonce-abc123'" p);
        check "no unsafe-inline" false (contains ~needle:"unsafe-inline" p) )
  ; ( "csp: security_headers carries the policy"
    , `Quick
    , fun () ->
        match
          List.assoc_opt "content-security-policy" (Http_core.security_headers ())
        with
        | Some v ->
            check "default-src" true (contains ~needle:"default-src 'self'" v)
        | None -> Alcotest.fail "no content-security-policy header" )
  ; ( "nonce: inline script and style tagged, external script left alone"
    , `Quick
    , fun () ->
        let html =
          "<html><style>a{}</style><script src=\"/a.js\"></script>\
           <script>1</script></html>"
        in
        let out = Http_core.inject_nonce ~nonce:"n1" html in
        check "style tagged" true
          (contains ~needle:"<style nonce=\"n1\">" out);
        check "inline script tagged" true
          (contains ~needle:"<script nonce=\"n1\">" out);
        check "external untouched" true
          (contains ~needle:"<script src=\"/a.js\">" out) )
  ]

let () = Alcotest.run "http_origin_csrf" [ ("origin-csrf", tests) ]
