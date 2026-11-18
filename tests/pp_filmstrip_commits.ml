(** Pins for the monorepo commit-history filmstrip pane (wyo-preview).

    The pane is read-only, public and non-interactive, lists only the last
    N commits (N pinned by the spec pass), and reads the spec-pinned source
    (the bare repo's git log). A dropped constant, a dropped source pin, or
    an anchor/htmx creeping into a commit row is a regression:

      (a) N stays pinned at 20 (filmstrip/commit_count);
      (b) the data source stays the bare repo on disk (a path CONSTANT, so
          the pane survives MR4 -- no cwd, no session);
      (c) a rendered commit row carries rev, author, date and subject, and
          carries NO anchor, htmx attribute or form (non-interactive).

    The sources are copies in the build dir (dune deps), so these reads
    never touch the source tree. *)

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let contains ~needle hay =
  let n = String.length needle in
  let h = String.length hay in
  let rec go i = i + n <= h && (String.equal (String.sub hay i n) needle || go (i + 1)) in
  n = 0 || go 0

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n" name
  end

let () =
  let lib = read "../libs/filmstrip/filmstrip.ml" in
  check "commit count pinned to 20" (contains ~needle:"let commit_count = 20" lib);
  check "data source is the bare repo (path constant)"
    (contains ~needle:"let commit_repo = \"/home/git/srv/git/wyo-preview.git\"" lib);
  check "git log reads the pinned git-dir"
    (contains ~needle:"git --git-dir=%s log -n %d" lib);
  let row = Filmstrip_shared.commit_html
      { Filmstrip_shared.rev = "abc1234"; author = "roerick"; date = "2026-11-16";
        subject = "the subject" } in
  check "row carries the rev" (contains ~needle:"abc1234" row);
  check "row carries the author" (contains ~needle:"roerick" row);
  check "row carries the date" (contains ~needle:"2026-11-16" row);
  check "row carries the subject" (contains ~needle:"the subject" row);
  check "row is not interactive (no anchor)" (not (contains ~needle:"<a " row));
  check "row is not interactive (no htmx)" (not (contains ~needle:"hx-" row));
  check "row is not interactive (no form)" (not (contains ~needle:"<form" row));
  if !failures > 0 then exit 1
