;; libs/tuna/lib.pp -- FED3 PP-half: thin PP client over tuna's host API.
;;
;; tuna = the tree substrate at pricklypear.club (repo ~/tuna-deriv; book
;; tuna.borg + borg/*.borg). This client is the "PP is a client of the tree
;; surface before it is a peer of the chain" half of FED3.
;;
;; LAW (borg/federation.borg FED3, pre-registered failure 3): the library
;; NEVER sets actor fields. tuna derives the actor from the bearer token, so
;; a bridge write is always attributed to the identity that owns the bearer.
;; Nothing here can forge attribution, by construction.
;;
;; v0 scope: tree surface (get/put/cas/list/log/state) + ns fork, explicit
;; config. The fed surface (fed-value/fed-ops) deliberately waits for a PP
;; sha256 prim: FED1/FED2's client laws are rehash-verify-and-reject, and an
;; unverifiable client would absorb - exactly what the book forbids.
;; Store-backed, identity-keyed config (bearer parity) lands in v1.

;; -- config + transport -------------------------------------------------

(define tuna/cfg
  (lambda (base bearer)
    "Config for the tuna prims: {base, bearer}. base like
https://pricklypear.club (no trailing slash); bearer is a tuna identity
token. Pure: callers pass the cfg to every call; nothing here is stored."
    (dict-set* "{}"
               (list "base" (as-str base) "bearer" (as-str bearer)))))

(define tuna/check
  (lambda (tag r)
    "Transport + tuna envelope discipline. The http result dict carries
ok/status/body/err: a transport failure raises with tag + err + status; a
delivered body parses as JSON and raises if tuna answered {error: ...}
(j_err paths, including the 409 CAS conflict). Returns tuna's parsed body."
    (if (string-eq (as-str (dict-get r "ok")) "true")
        (let ((j (json-parse (dict-get r "body"))))
          (if (null? j)
              (error (string-append "tuna " tag ": bad body HTTP "
                                    (as-str (dict-get r "status")) " - "
                                    (as-str (dict-get r "err"))))
              (if (null? (dict-get j "error"))
                  j
                  (error (string-append "tuna " tag ": HTTP "
                                        (as-str (dict-get r "status")) " - "
                                        (as-str (dict-get j "error")))))))
        (error (string-append "tuna " tag ": "
                              (as-str (dict-get r "err")) " HTTP "
                              (as-str (dict-get r "status")))))))

(define tuna/url
  (lambda (cfg path)
    "Absolute tuna URL for an API path."
    (string-append (dict-get cfg "base") path)))

(define tuna/headers
  (lambda (cfg)
    "Headers JSON for bearer-authed tuna calls."
    (string-append "{\"Content-Type\": \"application/json\", \"Authorization\": \"Bearer \""
                   (json-escape (dict-get cfg "bearer"))
                   "\"}")))

(define tuna/get
  (lambda (cfg path)
    "Bearer GET an API path (query string included in path; prefixes must
be URL-safe - tuna prefixes are lowercase alnum + dots)."
    (tuna/check path (http-get (tuna/url cfg path) (tuna/headers cfg)))))

(define tuna/post
  (lambda (cfg path body)
    "Bearer POST a JSON body string to an API path."
    (tuna/check path (http-post (tuna/url cfg path) (tuna/headers cfg) body))))

;; -- tree surface (POST bodies; tuna answers bare JSON) ------------------

(define tuna/tree-get
  (lambda (cfg path)
    "Read one path: {path, value_ternary, value_hash, version, owner}.
A missing path raises (tuna journals the failed read - denials surface,
never drop)."
    (tuna/post cfg "/api/tree/get"
               (string-append "{\"path\":\"" (json-escape path) "\"}"))))

(define tuna/tree-put
  (lambda (cfg path value-ternary)
    "Unconditional put of a canonical ternary value: 201 ->
{path, version, value_hash}. value_ternary must be tuna-canonical
complete-tree ternary ('0' leaf / '1<child>' / '2<left><right>'; '20' is
a parse error at offset 2, not a value)."
    (tuna/post cfg "/api/tree/put"
               (string-append "{\"path\":\"" (json-escape path)
                              "\",\"value_ternary\":\"" (json-escape value-ternary)
                              "\"}"))))

(define tuna/tree-cas
  (lambda (cfg path value-ternary expected-version)
    "Compare-and-swap on the path version: 201 -> {path, version,
value_hash}; a moved head raises with tuna's 409 'tree put conflict:
version mismatch' (the denial is journaled by tuna)."
    (tuna/post cfg "/api/tree/put"
               (string-append "{\"path\":\"" (json-escape path)
                              "\",\"value_ternary\":\"" (json-escape value-ternary)
                              "\",\"expected_version\":" (as-str expected-version)
                              "}"))))

(define tuna/tree-list
  (lambda (cfg prefix)
    "Entries under a prefix (tuna default limit 1000): {entries:
[{path, value_hash, version, owner, updated_at}]}."
    (tuna/post cfg "/api/tree/list"
               (string-append "{\"prefix\":\"" (json-escape prefix) "\"}"))))

(define tuna/tree-log
  (lambda (cfg prefix)
    "Ops chain for a prefix: {ops: [{seq, path, op, value_hash,
prev_version, version, actor, ts, op_hash}]}. Clients fold ops (rewind
surface); every row's actor is the true caller."
    (tuna/get cfg (string-append "/api/tree/log?prefix=" prefix))))

(define tuna/tree-state
  (lambda (cfg prefix)
    "Folded state under a prefix: {prefix, at_seq, entries: [{path,
value_hash, version}]}."
    (tuna/get cfg (string-append "/api/tree/state?prefix=" prefix))))

(define tuna/ns-fork
  (lambda (cfg src dst)
    "Copy a whole namespace prefix to another: 201 -> {src, dst, copied}."
    (tuna/post cfg "/api/ns/fork"
               (string-append "{\"src\":\"" (json-escape src)
                              "\",\"dst\":\"" (json-escape dst) "\"}"))))
