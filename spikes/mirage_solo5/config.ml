open Mirage

(* Minimal Phase 1 probe: prove mirage configure + the unix target
   round-trips, and that Mirage_main.Make can be invoked with the
   device graph mirage-runtime ships. This is the smallest "mirage
   unikernel" that can build from the config side — two clocks only
   (ptime + mtime), no network yet (that's a later phase 2/3 concern).

   PROBE RESULT (iteration 27):
     *) `mirage configure -t unix` SUCCEEDS — generates mirage/main.ml
        + dune.build.
     *) The generated `mirage/main.ml` correctly applies
        `Unikernel.Hello(Mirage_ptime)(Mirage_mtime)` and wires the
        Mirage runtime (sleep, logs, bootvar, RNG, etc).
     *) `pgx_lwt_mirage` 2.2 IS installed on poohstack. Its `Make`
        functor signature: `Make (RANDOM) (TIME) (MCLOCK) (PCLOCK)
        (STACK : Tcpip.Stack.V4V6) : val connect : STACK.t ->
        (module Pgx_lwt.S)` — requires a real Tcpip.Stack.V4V6 device,
        only satisfiable inside a real mirage unikernel.
     *) `dune build` FAILS with 6 missing opam packages on poohstack:
          mirage-bootvar.unix, mirage-crypto-rng-mirage, mirage-logs,
          mirage-ptime.unix, mirage-sleep.unix, cmdliner-stdlib.
        These are Mirage 4.x runtime packages; installing them is a
        heavy switch modification (6+ new deps) and is OUT OF SCOPE
        for the spike per AGENTS.md "Do not add dependencies lightly".
        Such an install needs explicit user approval.
     *) `httpaf-mirage` does NOT exist as a package. The mirage http
        path is via `gluten-mirage` + `httpaf`, also not installed.
     *) Solo5 stack (mirage-solo5, ocaml-solo5, solo5-bindings-hvt,
        mirage-net-solo5) all absent from poohstack.

   To re-run the probe:
     cd spikes/mirage_solo5
     eval $(opam env --switch=poohstack --set-switch)
     mirage configure -t unix   # succeeds — generates main.ml + dune.build
     dune build                # FAILS mirage-sleep.unix not found

   See README.md in this folder + the iteration 27 entry of
   .ralph/build-pricklypear.md for the full findings + next steps. *)
let () =
  register "pp_mirage_spike"
    [
      main
        "Unikernel.Hello"
        (ptime @-> mtime @-> job)
        $ default_ptime $ default_mtime
    ]
