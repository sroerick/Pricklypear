(lang dune 3.23)

(name pricklypear)

(package
 (name pricklypear-lisp)
 (description "Pure Scheme-flavored Lisp-1 interpreter for Pricklypear (no I/O)"))

(package
 (name pricklypear-common)
 (description "Shared OCaml types for Pricklypear (manifest, branch, protocol, auth_types)"))

(package
 (name pricklypear-image)
 (description "Pricklypear image: a single OCaml unix binary — Lisp interpreter, REPL, htmx, Postgres"))
