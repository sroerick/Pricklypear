;; Shared dune rules for the surface binaries (agent/lib + remote/lib —
;; both at repository depth 2, so the ../../ deps below resolve
;; identically from either). Included verbatim by both agent/lib/dune
;; and remote/lib/dune (i-1788671587-9): vendored htmx assets + the
;; Nopales package sources the HTML surfaces boot-load, embedded as
;; OCaml strings so each binary is self-contained regardless of cwd.
;; Real sources: image/static/*.js + libs/html/lib.pp + libs/css/lib.pp
;; + libs/filmstrip/lib.pp + libs/hyper/lib.pp.

(rule
 (targets assets.ml)
 (deps ../../image/static/htmx.min.js
        ../../image/static/hx-multipart.js
        ../../image/static/htmx-2-compat.min.js)
 (action
  (with-stdout-to assets.ml
   (progn
    (echo "(* Auto-generated: vendored htmx assets baked into the binary. *)")
    (echo "(* Do not edit by hand. *)")
    (echo "let htmx_min = {asset|")
    (cat ../../image/static/htmx.min.js)
    (echo "|asset}")
    (echo "let hx_multipart = {asset|")
    (cat ../../image/static/hx-multipart.js)
    (echo "|asset}")
    (echo "let htmx_2_compat = {asset|")
    (cat ../../image/static/htmx-2-compat.min.js)
    (echo "|asset}")))))

;; The pp-sync header record — (package NAME (export …)) — is a
;; directive the IMAGE intercepts pre-eval (image/lib/session.ml); the
;; agent's bare evaluator has no such intercept, so the filmstrip embed
;; strips it first (awk balance-walk; header contains no strings, only
;; the export symbol list).
(rule
 (targets filmstrip_nohdr.pp)
 (deps ../../libs/filmstrip/lib.pp)
 (action
  (bash
   "awk '/^\\(package /{skip=1; d=gsub(/\\(/,\"\")-gsub(/\\)/,\"\"); next} skip{d+=gsub(/\\(/,\"\")-gsub(/\\)/,\"\"); if(d==0){skip=0}; next} {print}' ../../libs/filmstrip/lib.pp > filmstrip_nohdr.pp")))

(rule
 (targets libs_source.ml)
 (deps ../../libs/html/lib.pp ../../libs/hyper/lib.pp ../../libs/css/lib.pp filmstrip_nohdr.pp)
 (action
  (with-stdout-to libs_source.ml
   (progn
    (echo "(* Auto-generated: embedded Nopales package sources. *)")
    (echo "(* Do not edit by hand. *)")
    (echo "let html_lib = {pp|")
    (cat ../../libs/html/lib.pp)
    (echo "|pp}")
    ;; filmstrip-shell (borge/agent.borg): the glochid shell IS a
    ;; filmstrip — libs/css backs filmstrip/css + the shell boots on the
    ;; agent route table. Load order (surface.ml): html, css, filmstrip,
    ;; glochid layer, agent surface.
    (echo "let css_lib = {pp|")
    (cat ../../libs/css/lib.pp)
    (echo "|pp}")
    (echo "let hyper_lib = {pp|")
    (cat ../../libs/hyper/lib.pp)
    (echo "|pp}")
    (echo "let filmstrip_lib = {pp|")
    (cat filmstrip_nohdr.pp)
    (echo "|pp}")))))
