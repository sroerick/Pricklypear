;; Shared dune rules for the surface binaries (agent/lib + remote/lib —
;; both at repository depth 2, so the ../../ deps below resolve
;; identically from either). Included verbatim by both agent/lib/dune
;; and remote/lib/dune (i-1788671587-9): vendored htmx assets + the
;; Nopales package sources the HTML surfaces boot-load, embedded as
;; OCaml strings so each binary is self-contained regardless of cwd.
;; Real sources: image/static/*.js + libs/html/lib.pp + libs/hyper/lib.pp.

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

(rule
 (targets libs_source.ml)
 (deps ../../libs/html/lib.pp ../../libs/hyper/lib.pp)
 (action
  (with-stdout-to libs_source.ml
   (progn
    (echo "(* Auto-generated: embedded Nopales package sources. *)")
    (echo "(* Do not edit by hand. *)")
    (echo "let html_lib = {pp|")
    (cat ../../libs/html/lib.pp)
    (echo "|pp}")
    (echo "let hyper_lib = {pp|")
    (cat ../../libs/hyper/lib.pp)
    (echo "|pp}")))))
