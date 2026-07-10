(lang dune 3.23)

(name pricklypear)

(package
 (name pricklypear-lisp)
 (description "Pure Scheme-flavored Lisp-1 interpreter for Pricklypear (no I/O)"))

(package
 (name pricklypear-common)
 (description "Shared OCaml types between Pricklypear supervisor, auth, and image"))

(package
 (name pricklypear-auth)
 (description "Auth unikernel for Pricklypear: credential verification, session tokens"))

(package
 (name pricklypear-supervisor)
 (description "Supervisor unikernel for Pricklypear: fleet management, SSH front door"))

(package
 (name pricklypear-image)
 (description "Per-user image unikernel for Pricklypear: Lisp interpreter, REPL, htmx"))
