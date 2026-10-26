(* hostkit/chat_stream_test.ml — the harness chat's REPL rule must not
   swallow chat prose.

   POST /chat treated any prompt whose first non-space char was "(" as
   Nopales and evaluated it. A long chat body that merely opened with a
   parenthesis was therefore fed to the reader, which bounced on a quoted
   word followed by a period — the closing quote ends a string, so the next
   "." stands alone and the reader reports "dotted-pair notation (.) is not
   supported" — or on an unbalanced quote (the rest of the line lexes as
   symbols). A bogus parse error for something that was never code.

   A prompt is REPL input when it READS as exactly one complete form AND
   that form's operator is callable (a kernel special form, or a prim /
   defined name in the shared env). Prose that happens to lex as one
   list — "(I can't reproduce this. It's odd.)" is a call of the unbound
   word `I` because the apostrophe is quote sugar — is chat, not code. *)

let check name input expected =
  Alcotest.(check bool) name expected (Chat_stream.repl_prompt input)

(* The glochid host refines the syntactic gate with the env: a head is
   callable when it is a kernel special form or a bound prim/def. *)
let callable name =
  Pricklypear_lisp.Eval.is_special_form name
  || List.mem name [ "+"; "list"; "*" ]

let check_known name input expected =
  Alcotest.(check bool) name expected
    (Chat_stream.repl_prompt ~known:callable input)

(* >500 chars of prose whose apostrophes make it lex as ONE list: the
   residual shape after the reader-error fix. *)
let long_apostrophe_prose =
  "(I can't reproduce this. It's odd and long enough that the body crosses \
   five hundred characters, which is exactly when this used to bounce. He \
   said it wasn't happening, then it was. The reader walked the prose as \
   code and reported something bogus instead of reaching the model. Nothing \
   here is an expression; it is one person writing to another, and it exists \
   only to be long enough to reproduce the report. I don't think it's broken. \
   We can't see the bug yet, but it's there, and I'm sure it'll show up again \
   the next time someone pastes a paragraph into the box.)"

(* >500 chars of ordinary prose that opens with a paren and carries quotes:
   the shape that used to bounce. *)
let long_prose =
  "(Quick note on the \"chat\" bug. The body below is long enough to cross \
   five hundred characters, which is exactly when this used to bounce. He \
   typed \"hello\". Then he kept writing, and the reader walked the prose as \
   code. It hit a sentence period after a quoted word and reported a \
   dotted-pair parse error that had nothing to do with what was typed. It \
    should reach the model as chat, not be rejected as broken Lisp. Nothing \
   in this paragraph is an expression; it is one person writing to another, \
   and the paragraph exists only to be long enough to reproduce the report.)"

let tests =
  [ ("chat: a single form is REPL input", `Quick, fun () ->
      check "(+ 1 2)" "(+ 1 2)" true)
  ; ("chat: a define is REPL input", `Quick, fun () ->
      check "(define (f x) (* x 2))" "(define (f x) (* x 2))" true)
  ; ("chat: leading whitespace still reads", `Quick, fun () ->
      check "  (list 1 2)" "  (list 1 2)" true)
  ; ("chat: plain prose is not REPL input", `Quick, fun () ->
      check "hello there" "hello there" false)
  ; ("chat: empty prompt is not REPL input", `Quick, fun () ->
      check "" "" false)
  ; ("chat: quoted word + period does not raise", `Quick, fun () ->
      check "(he said \"stop\". Then we left.)"
        "(he said \"stop\". Then we left.)" false)
  ; ("chat: an unbalanced quote does not raise", `Quick, fun () ->
      check "(it's a \"quoted phrase that never closes)"
        "(it's a \"quoted phrase that never closes)" false)
  ; ("chat: a form followed by prose is not REPL input", `Quick, fun () ->
      check "(+ 1 2) and then some prose." "(+ 1 2) and then some prose."
        false)
  ; ("chat: 500+ char prose with quotes is not REPL input", `Quick, fun () ->
      Alcotest.(check bool) "long enough" true (String.length long_prose > 500);
      check "long prose" long_prose false)
  ; ("chat: prose with apostrophes is not REPL input", `Quick, fun () ->
      check_known "apostrophes" "(I can't reproduce this. It's odd.)" false)
  ; ("chat: apostrophe prose without a lone dot is not REPL", `Quick, fun () ->
      check_known "apostrophes only" "(I can't reproduce this it is odd)" false)
  ; ("chat: 500+ char apostrophe prose is not REPL input", `Quick, fun () ->
      Alcotest.(check bool) "long enough" true
        (String.length long_apostrophe_prose > 500);
      check_known "long apostrophe prose" long_apostrophe_prose false)
  ; ("chat: a callable head is still REPL input", `Quick, fun () ->
      check_known "(+ 1 2)" "(+ 1 2)" true)
  ; ("chat: a special form is still REPL input", `Quick, fun () ->
      check_known "define" "(define (f x) (* x 2))" true)
  ; ("chat: a list headed by an unknown word is chat", `Quick, fun () ->
      check_known "unknown head" "(hello world)" false)
  ]

let () = Alcotest.run "chat_stream" [ ("repl-prompt", tests) ]
