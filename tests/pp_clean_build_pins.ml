(** Regression pins for the clean-Linux build (card 32160ff1).

    Each assertion is one of the four blockers the image hit building from a
    bare Debian/opam box; a dropped pin or a dropped README line is a
    regression, so make it fail here rather than on the next fresh machine:

      (a) httpun resolves to 0.2.0 — its renamed unit variants stop
          image/lib/http_agent.ml compiling — unless pinned to 0.1.0;
      (b) the argon2 binding dlsyms the main program, so boot needs
          LD_PRELOAD=libargon2.so.1;
      (c) pgx has no SCRAM, so PG must take password_encryption = md5;
      (d) the todo seed registers through the chat lookbook seam
          (chat/lookbook-add), which must resolve from the file that defines
          lookbook/todo-seed.

    The files are dune deps of this test, so they are copies in the build
    dir and these reads never touch the source tree. *)

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

let check name hay needle =
  if not (contains ~needle hay) then begin
    incr failures;
    Printf.eprintf "FAIL %s: missing %S\n" name needle
  end

let () =
  let pins = read "../scripts/opam-pins.txt" in
  (* (a) the httpun pair the image's unit-literal matches need. *)
  check "opam pins" pins "httpun 0.1.0";
  check "opam pins" pins "httpun-eio 0.1.0";
  let readme = read "../README.md" in
  (* (b) argon2 dlsym workaround. *)
  check "README" readme "LD_PRELOAD=libargon2.so.1";
  (* (c) md5 password_encryption for pgx. *)
  check "README" readme "password_encryption";
  check "README" readme "md5";
  (* (d) todo seeds through the chat lookbook seam, which only resolves if
     chat still defines it (a rename there re-breaks fresh-DB boot). *)
  check "libs/todo/lib.pp" (read "../libs/todo/lib.pp") "lookbook-add";
  check "libs/chat/lib.pp" (read "../libs/chat/lib.pp") "define chat/lookbook-add";
  (* (e) card 75272c4a: a comment posted from a card detail page must come
     back to the SAME card, not the list. The composer rides htmx (the
     #todo-main outerHTML swap) and the plain-POST fallback keeps the card
     id in the redirect, so neither arm can drop you on the main list. *)
  let todo_lib = read "../libs/todo/lib.pp" in
  check "libs/todo/lib.pp" todo_lib "todo-comment-form";
  check "libs/todo/lib.pp" todo_lib "(string-append \"&id=\" (as-str id))";
  (* (f) card 8b8405: every comment write stamps author + generation on the
     row, an unresolvable author is REFUSED (never stamped anonymous), and a
     row that predates the stamp still reads as author-unknown. Dropping any
     of the three is a regression. *)
  check "libs/todo/lib.pp" todo_lib "comment refused: no resolvable author";
  check "libs/todo/lib.pp" todo_lib "\"author\" who";
  check "libs/todo/lib.pp" todo_lib "\"generation\" (as-str (gen-now))";
  check "libs/todo/lib.pp" todo_lib "todo/datum/feed-author";
  check "libs/todo/lib.pp" todo_lib "todo-feed-acted-by";
  check "libs/todo/lib.pp" todo_lib "todo-feed-gen";
  (* (g) card c1d1842e: the todo list's view selector carries a doasq entry
     alongside its sibling synthetic views, in the SAME three-field shape
     (value, label, selected) with no empty value or placeholder label. *)
  check "libs/todo/lib.pp" todo_lib
    "(html/option \"doasq\" \"doasq\" (string-eq view \"doasq\"))";
  (* ... and the nav link list itself carries it, in the same
     (view key label extra) shape as everything / from-notes / archived,
     with every field supplied (no empty placeholder in the new entry). *)
  check "libs/todo/lib.pp" todo_lib
    "(todo/nav-link view \"doasq\" \"doasq\" \"approvals\")";
  if !failures > 0 then exit 1
