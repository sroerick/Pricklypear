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
  (* (d) todo seeds through the chat lookbook seam. *)
  check "libs/todo/lib.pp" (read "../libs/todo/lib.pp") "lookbook-add";
  if !failures > 0 then exit 1
