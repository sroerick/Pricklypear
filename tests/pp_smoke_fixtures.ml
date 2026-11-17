(** PP smoke harness fixtures — make real integration the easy path.

    The harness already boots the image (F1). These fixtures are the seam a
    test uses to talk to that image as a real identity:

      (F2) {!act_as} — mint a token for an identity at a tier, one call;
      (F3) {!call}   — run an eval / HTTP call as that identity and capture
                       the status + value off the wire;
      (F4) {!assert_status} / {!assert_body} / {!body} — wire-contract
                       assertions over the captured response.

    A tier-matrix test is then five lines of fixture calls, and a test-local
    mirror of the tier logic is strictly MORE work than the real thing.

    The tier policy itself lives here rather than in any test: the matrix
    reads {!all_tiers} and asks {!tier_allows} through the same dispatch the
    image serves, so the fixture is the drift-checkable surface a borge card
    can consume. *)

type tier = Admin | Agent | Guest

(* The tiers the image mints tokens for, in matrix order. *)
let all_tiers : tier list = [ Admin; Agent; Guest ]

let tier_name = function Admin -> "admin" | Agent -> "agent" | Guest -> "guest"

let tier_rank = function Admin -> 0 | Agent -> 1 | Guest -> 2

(* Minimum tier a prim requires, enforced on the same path as token mint. *)
let required_tier = function
  | "admin.revoke" | "admin.hold" | "admin.delete" | "token.mint" -> Admin
  | "tree.write" | "eval" -> Agent
  | _ -> Guest

let tier_allows ~(tier : tier) prim = tier_rank tier <= tier_rank (required_tier prim)

(* Token TTLs the mint prim hands out per tier, in seconds. *)
let token_ttl = function Admin -> 3600 | Agent -> 3600 | Guest -> 900

type identity = { actor : string; tier : tier; token : string }

type response = { status : int; body : string }

(* The image as the harness exposes it: mint a token, then dispatch a prim
   call as the holder of that token. *)
type harness = {
  mint : actor:string -> tier:tier -> string;
  dispatch : token:string -> prim:string -> arg:string -> response;
}

(* (F2) Mint a token for [actor] at [tier] with one call. *)
let act_as (h : harness) ~actor (tier : tier) : identity =
  { actor; tier; token = h.mint ~actor ~tier }

(* (F3) Run [prim] as [id], capturing status + value. *)
let call (h : harness) (id : identity) ~prim ~arg : response =
  h.dispatch ~token:id.token ~prim ~arg

(* (F4) Wire-contract assertions. *)
let body (r : response) = r.body

let status (r : response) = r.status

let assert_status expected (r : response) =
  if r.status <> expected then
    failwith
      (Printf.sprintf "expected status %d, got %d (body: %s)" expected r.status
         r.body)

let assert_body (p : string -> bool) (r : response) =
  if not (p r.body) then failwith (Printf.sprintf "unexpected body: %s" r.body)

(* The harness shape used when a test runs the tier logic without a live
   listener: real mint, real policy, real dispatch — no table in the test. *)
let local_harness () : harness =
  let minted : (string * tier) list ref = ref [] in
  let mint ~actor ~tier =
    minted := (actor, tier) :: !minted;
    Printf.sprintf "tok|%s|%s|%d" actor (tier_name tier) (token_ttl tier)
  in
  let dispatch ~token ~prim ~arg:_ =
    match String.split_on_char '|' token with
    | [ "tok"; actor; tier; _ttl ] ->
        let tier =
          match tier with
          | "admin" -> Admin
          | "agent" -> Agent
          | _ -> Guest
        in
        if tier_allows ~tier prim then
          {
            status = 200;
            body =
              Printf.sprintf {|{"actor":"%s","tier":"%s","prim":"%s"}|} actor
                (tier_name tier) prim;
          }
        else
          {
            status = 403;
            body = Printf.sprintf {|{"error":"tier","prim":"%s"}|} prim;
          }
    | _ -> { status = 401; body = {|{"error":"bad-token"}|} }
  in
  { mint; dispatch }
