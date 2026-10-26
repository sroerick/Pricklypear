(* hostkit/chat_stream_test.ml — the harness chat's REPL rule must not
   swallow chat prose.

   POST /chat treated any prompt whose first non-space char was "(" as
   Nopales and evaluated it. A long chat body that merely opened with a
   parenthesis was therefore fed to the reader, which bounced on a quoted
   word followed by a period — the closing quote ends a string, so the next
   "." stands alone and the reader reports "dotted-pair notation (.) is not
   supported" — or on an unbalanced quote (the rest of the line lexes as
   symbols). A bogus parse error for something that was never code.

   A prompt is REPL input when it READS as exactly one complete form. *)

let check name input expected =
  Alcotest.(check bool) name expected (Chat_stream.repl_prompt input)

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
  ]

let () = Alcotest.run "chat_stream" [ ("repl-prompt", tests) ]
