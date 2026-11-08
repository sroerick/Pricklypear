#!/usr/bin/env bash
# Cloud Agent install — build the Pricklypear image and all packages.
#
# The heavy, stable prerequisites are baked into the environment base
# snapshot, NOT installed here:
#   * apt: opam, postgresql-16, ksh, libargon2-dev, libwayland-dev,
#          wayland-protocols, libxkbcommon-dev, build tooling
#   * the `poohstack` opam switch (OCaml 5.4.1) with every opam dependency
#     already compiled (httpun pinned to 0.1.0 for the flush ABI, pgx,
#     eio, lwt, pure-html, argon2, mirage-crypto, tls-eio, wayland, ...)
#
# This step only (re)builds the checked-out source tree, so it stays fast
# and idempotent. `dune build` is a cheap no-op when nothing changed.
set -euo pipefail

cd "$(dirname "$0")/.."

eval "$(opam env --switch=poohstack --set-switch)"

dune build @all
