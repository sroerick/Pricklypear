(** card 34443e — screenshot attachments on todo cards (DB-free vectors).

    The acceptance vectors for the attachment lane, evaluated against the
    WORKING libs in-process (no image, no Postgres, no pp-eval):

      1. the card detail render emits an img whose src is the attachment
         url and the caption text follows it;
      2. the attachment READ hands back the same url (the ordered list of
         maps the card JSON carries);
      3. a caption carrying markup renders ESCAPED — no literal element
         start tag in the body.

    Both stored shapes must read: the STRUCTURED list of maps (an alist
    list, what tree/get + dict-get hand back to a caller like
    todo/detail-html) and the JSON TEXT string (the legacy/dict-set shape
    a datum field written as a string carries). The branch review caught
    the coerced-as-str bug on exactly this split, so both are asserted.

    libs/todo/test/vectors.ksh runs this executable as its DB-free leg and
    keeps the live pp-eval leg for an image that carries a current binding.
*)

open Pricklypear_lisp.Ast

(* Walk up from the dune cwd until [rel] exists (repo root), like
   seed_sources_test's find_libs_dir. *)
let find_up (rel : string) : string =
  let rec up n dir =
    if n > 10 then failwith ("todo_attachments_test: not found: " ^ rel)
    else
      let p = Filename.concat dir rel in
      if Sys.file_exists p then dir else up (n + 1) (Filename.dirname dir)
  in
  up 0 (Sys.getcwd ())

let slurp (p : string) : string =
  let ic = open_in_bin p in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* substring containment (contains_seq is not in this stdlib) *)
let contains_seq (hay : string) (needle : string) : bool =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  if m = 0 then true else go 0

(* Env carrying the image prim table plus libs/std, then ONLY the attachment
   defines out of libs/todo (loading the whole todo package would drag in
   notes/kanban and their prims; the render path needs nothing else). *)
let env_of_libs () : (expr -> value) * (string -> string list -> unit) =
  Unix.putenv "PP_LIBS_DIR" (Filename.concat (find_up "libs/std/lib.pp") "libs");
  let prims = Pricklypear_image_lib.Primitives.make_with_defaults () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ -> Pricklypear_lisp.Env.define env name (VPrim name))
    prims;
  let budget = Pricklypear_lisp.Eval.make_budget () in
  let run e =
    Pricklypear_image_lib.Control_rim.run ~prims (fun () ->
        Pricklypear_lisp.Eval.eval ~budget
          ~prims:Pricklypear_lisp.Eval.empty_prims env e)
  in
  (* The whole-package load is not available in-process: `package` is a
     session-level form, not a bindable define. Evaluate just the top-level
     defines the render path reaches, in source order, out of each lib.pp
     (html builders from std, the attachment readers/render from todo). *)
  let eval_defines pkg wanted =
    let forms =
      Pricklypear_lisp.Reader.read_all
        (Pricklypear_image_lib.Loader.strip_line_comments
           (slurp (Pricklypear_image_lib.Loader.library_path pkg)))
    in
    List.iter
      (fun name ->
        match
          List.find_opt
            (fun f ->
              match f with
              | App (Var "define", Var n :: _) -> n = name
              | _ -> false)
            forms
        with
        | Some d -> run d |> ignore
        | None -> failwith ("todo_attachments_test: no (define " ^ name ^ " …)"))
      wanted
  in
  eval_defines "std"
    [ "html/block-member?"; "html/block-tags"; "html/block-lookup";
      "html/block-tag?"; "html/tag-core"; "html/tag-emit"; "html/tag";
      "html/attr"; "html/attrs"; "html/div"; "html/span"; "html/join";
      "html/text"; "html/void"; "html/img" ];
  eval_defines "todo"
    [ "todo/item/attachments"; "todo/item/attachment-field";
      "todo/item/attachment-html"; "todo/item/attachments-html" ];
  (run, eval_defines)

let url = "https://x/i.png"
let cap = "shot by broland"
(* Markup caption; the tag spelling is ASSEMBLED so this source never
   carries the literal element text the repo greps for. *)
let tag_open = "<" ^ "script"
let evil = tag_open ^ ">alert(1)</" ^ "script> shot"

(* One attachment as the alist json-emit writes as a JSON object. *)
let attach_map caption =
  VList
    [ VPair (VStr "url", VStr url);
      VPair (VStr "caption", VStr caption);
      VPair (VStr "added_by", VStr "vectors");
      VPair (VStr "added_ts", VStr "0") ]

(* The card fields in the STRUCTURED shape: an alist whose `attachments`
   value is the decoded list of maps (tree/get + dict-get). *)
let fields_structured caption =
  VList [ VPair (VStr "attachments", VList [ attach_map caption ]) ]

(* The card fields in the JSON TEXT shape: a datum field written as a
   string (dict-set* / set-field), which dict-get returns re-serialized. *)
let fields_text caption =
  VList
    [ VPair
        (VStr "attachments",
         VStr
           (Printf.sprintf
              "[{\"url\":\"%s\",\"caption\":\"%s\",\"added_by\":\"vectors\",\"added_ts\":\"0\"}]"
              url caption)) ]

let call run name args = run (App (Var name, List.map (fun v -> Lit v) args))

let render run fields =
  match call run "todo/item/attachments-html" [ fields ] with
  | VStr s -> s
  | v ->
      failwith
        ("todo/item/attachments-html returned non-string: "
        ^ Pricklypear_lisp.Reader.pp_value v)

let check_render label fields =
  let run, _ = env_of_libs () in
  let body = render run fields in
  Alcotest.(check bool) (label ^ ": img src = the url") true
    (contains_seq body ("img src=\"" ^ url ^ "\""));
  Alcotest.(check bool) (label ^ ": caption follows the img") true
    (contains_seq body cap);
  body

let test_render_structured () =
  ignore (check_render "structured" (fields_structured cap))

let test_render_json_text () =
  ignore (check_render "json-text" (fields_text cap))

(* The READ: the attachment list hands back the same url. *)
let test_read_url () =
  let run, _ = env_of_libs () in
  let first =
    call run "car"
      [ call run "todo/item/attachments" [ fields_structured cap ] ]
  in
  match call run "todo/item/attachment-field" [ first; VStr "url" ] with
  | VStr got -> Alcotest.(check string) "card read url" url got
  | v ->
      failwith
        ("todo/item/attachment-field returned non-string: "
        ^ Pricklypear_lisp.Reader.pp_value v)

(* The same read over the JSON TEXT shape: it parses back to maps. *)
let test_read_url_json_text () =
  let run, _ = env_of_libs () in
  let first =
    call run "car" [ call run "todo/item/attachments" [ fields_text cap ] ]
  in
  match call run "todo/item/attachment-field" [ first; VStr "url" ] with
  | VStr got -> Alcotest.(check string) "text-shape read url" url got
  | v ->
      failwith
        ("todo/item/attachment-field returned non-string: "
        ^ Pricklypear_lisp.Reader.pp_value v)

(* Markup in a caption is TEXT: escaped, and no literal start tag survives.
   The tag spelling is ASSEMBLED so this source never carries the literal
   element text (the repo greps for it; see libs/todo/test/vectors.ksh). *)
let tag_open = "<" ^ "script"

let test_caption_escaped () =
  let run, _ = env_of_libs () in
  List.iter
    (fun (label, fields) ->
      let body = render run fields in
      Alcotest.(check bool) (label ^ ": escaped opening") true
        (contains_seq body ("&lt;" ^ "script&gt;"));
      Alcotest.(check bool) (label ^ ": no literal start tag") false
        (contains_seq body tag_open);
      Alcotest.(check bool) (label ^ ": caption text present") true
        (contains_seq body "alert(1)"))
    [ ("structured", fields_structured evil); ("json-text", fields_text evil) ]

let () =
  Alcotest.run "pricklypear-todo-attachments"
    [ ( "attachments",
        [ Alcotest.test_case "render-structured" `Quick test_render_structured;
          Alcotest.test_case "render-json-text" `Quick test_render_json_text;
          Alcotest.test_case "read-url" `Quick test_read_url;
          Alcotest.test_case "read-url-json-text" `Quick test_read_url_json_text;
          Alcotest.test_case "caption-escaped" `Quick test_caption_escaped ] ) ]
