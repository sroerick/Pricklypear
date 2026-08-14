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

(package
 (name pricklypear-conformance)
 (description "Nopales core conformance case data shared by the alcotest and js_of_ocaml browser-target runners"))

(package
 (name pricklypear-pure-prims)
 (description "Pure Nopales primitives shared by the image and the js_of_ocaml browser client"))

(package
 (name pricklypear-cli)
 (description "Laptop/agent CLIs: pp-eval, pp-sync (HTTP clients for the live image; no Python)"))

(package
 (name pricklypear-agent)
 (description "Local Nopales-native coding agent (pi-inspired): minimal agent loop, Nopales plugins, HTML code browser. Sibling of the image, not part of it — see borge/agent.borg"))
