# Mirage Solo5 spike — Phase 1 (probe)

Goal of this spike (iteration 27): confirm the Mirage-native stack is
reachable before committing to a full image unikernel refactor.

## Phase 1 probe scope

The current `image/bin/unikernel.ml` is NOT a mirage unikernel — it's a
plain `Lwt_main.run (Boot.start)` program using POSIX-flavored libs
(`pgx_lwt` + custom `Io_unix` shim, `httpaf-lwt-unix`, `lwt.unix`).
The spec's [transport-choices] promises the Mirage-native stack
(`pgx_lwt_mirage`, httpaf-over-mirage).

Phase 1 probe questions:
1. Can `mirage configure` parse a minimal `config.ml` on the poohstack
   switch (mirage 4.11.0)?
2. Does `Mirage_main.Make` round-trip — i.e. does a generated `main.ml`
   compile against `mirage-unix` and `mirage-runtime`?
3. Is `pgx_lwt_mirage` reachable from a generated unikernel (it requires
   the `Tcpip.Stack.V4V6` functor signature)?

If all of Phase 1 succeeds, Phase 2 (a later iteration) would refactor
the image unikernel to use `pgx_lwt_mirage` + a Mirage-native http
server (NOT `httpaf-mirage` — that package does not exist on this
switch's repo; the mirage http path is `gluten-mirage` + `httpaf`, also
not installed).

## Findings recorded in iteration 27

- `pgx_lwt_mirage` 2.2 IS installed on poohstack. Its `Make` functor
  signature: `Make (RANDOM) (TIME) (MCLOCK) (PCLOCK) (STACK : Tcpip.Stack.V4V6)
  : sig val connect : STACK.t -> (module Pgx_lwt.S) end`.
- `mirage` 4.11.0 installed. `mirage-unix` 5.0.1 installed.
- `httpaf-mirage` does NOT exist as a package on this opam repo.
  `gluten-mirage` exists but is NOT installed. The spec's claim of
  `httpaf-mirage` was a spec inaccuracy (filed for a future plan-pass).
- Solo5 stack (`mirage-solo5`, `ocaml-solo5`, `solo5-bindings-hvt`,
  `mirage-net-solo5`) all NOT installed — would require a heavy
  switch modification + cross-compiler install.
- AGENTS.md "Do not add dependencies lightly" + "Do not change
  dune files or go.mod without discussion" applies → installing the
  Solo5 stack for a probe is out of scope for this iteration.

## What this spike proves

A minimal mirage unikernel `config.ml` + `unikernel.ml` can be
configured against the `unix` target on poohstack. This is the
foundation: Phase 2 would replace `image/bin/unikernel.ml`'s
`Lwt_main.run` entry with `Mirage_main.Make` over a `mirage/unix`
target, and Phase 3 would target `mirage-solo5` (once installed).

See the rerunnable probe command in the iteration 27 entry of
`.ralph/build-pricklypear.md`.
