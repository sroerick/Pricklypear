(** On-demand LLM complexity analysis + cache in data_rows. *)
module Ast = Pricklypear_lisp.Ast
module Reader = Pricklypear_lisp.Reader
module Comp = Pricklypear_lisp.Complexity
module Expl = Pricklypear_lisp.Explain

let sha1_hex (s : string) : string =
  let d = Digestif.SHA1.digest_string s in
  Digestif.SHA1.to_hex d

let ast_hash (e : Ast.expr) : string = sha1_hex (Reader.pp_expr e)

let heuristic_json ~(fn_name : string) (e : Ast.expr) : Yojson.Safe.t =
  let cx = Comp.classify ~fn_name e in
  `Assoc
    [
      ("complexity", `String (Comp.badge cx));
      ("confidence", `String "heuristic");
      ("rationale", `String "static pattern match (no LLM)");
      ("source", `String "heuristic");
    ]

let strip_code_fence (s : string) : string =
  let s = String.trim s in
  if String.length s >= 3 && String.starts_with ~prefix:"```" s then
    let rest =
      match String.index_opt s '\n' with
      | None -> s
      | Some i -> String.sub s (i + 1) (String.length s - i - 1)
    in
    let rest = String.trim rest in
    if String.ends_with ~suffix:"```" rest then
      String.sub rest 0 (String.length rest - 3) |> String.trim
    else rest
  else s

(** Extract a balanced `{...}` object from prose. Prefers the last complete
    object that looks like our schema (has "complexity"), since models often
    emit a truncated content object first and a full one later in reasoning. *)
let extract_json_object (s : string) : string option =
  let s = strip_code_fence s in
  let len = String.length s in
  let extract_at start =
    let rec scan i depth in_str escape =
      if i >= len then None
      else
        let c = s.[i] in
        if escape then scan (i + 1) depth in_str false
        else if in_str then
          if c = '\\' then scan (i + 1) depth true true
          else if c = '"' then scan (i + 1) depth false false
          else scan (i + 1) depth true false
        else
          match c with
          | '"' -> scan (i + 1) depth true false
          | '{' -> scan (i + 1) (depth + 1) false false
          | '}' ->
              if depth <= 1 then Some (String.sub s start (i - start + 1))
              else scan (i + 1) (depth - 1) false false
          | _ -> scan (i + 1) depth false false
    in
    scan (start + 1) 1 false false
  in
  let rec collect i acc =
    if i >= len then List.rev acc
    else if s.[i] = '{' then
      match extract_at i with
      | Some blob -> collect (i + 1) (blob :: acc)
      | None -> collect (i + 1) acc
    else collect (i + 1) acc
  in
  let blobs = collect 0 [] in
  let has_complexity b =
    try
      match Yojson.Safe.from_string b with
      | `Assoc fields -> List.mem_assoc "complexity" fields
      | _ -> false
    with _ -> false
  in
  match List.rev (List.filter has_complexity blobs) with
  | b :: _ -> Some b (* last schema-shaped object *)
  | [] -> (match List.rev blobs with b :: _ -> Some b | [] -> None)

let parse_llm_json (raw : string) : (Yojson.Safe.t, string) result =
  let try_parse s =
    try Some (Yojson.Safe.from_string (String.trim s)) with _ -> None
  in
  match try_parse (strip_code_fence raw) with
  | Some (`Assoc _ as j) -> Ok j
  | Some _ ->
      Error "analyze: LLM JSON was not an object"
  | None ->
      (match extract_json_object raw with
       | Some blob ->
           (match try_parse blob with
            | Some (`Assoc _ as j) -> Ok j
            | Some _ -> Error "analyze: embedded JSON was not an object"
            | None ->
                Error
                  ("analyze: LLM response not JSON: "
                   ^ String.sub raw 0 (min 200 (String.length raw))))
       | None ->
           Error
             ("analyze: LLM response not JSON: "
              ^ String.sub raw 0 (min 200 (String.length raw))))

let cache_get (store : Store.t) ~(hash : string) : Yojson.Safe.t option =
  match Crud.find_row store ~type_name:"function_analysis" ~field:"cache_hash" ~value:hash with
  | Ast.VNil -> None
  | Ast.VPair (Ast.VStr _id, Ast.VStr fields_json) ->
      (try
         let j = Yojson.Safe.from_string fields_json in
         Some (Yojson.Safe.Util.member "result" j)
       with _ -> None)
  | Ast.VStr fields_json ->
      (* some paths return fields only *)
      (try
         let j = Yojson.Safe.from_string fields_json in
         match j with
         | `Assoc _ ->
             (match Yojson.Safe.Util.member "result" j with
              | `Null -> Some j
              | r -> Some r)
         | _ -> None
       with _ -> None)
  | _ -> None

let cache_put (store : Store.t) ~(author : string) ~(fn_name : string)
    ~(hash : string) ~(result : Yojson.Safe.t) : unit =
  let fields =
    `Assoc
      [
        ("fn", `String fn_name);
        ("cache_hash", `String hash);
        ("result", result);
        ("when", `String (string_of_float (Unix.gettimeofday ())));
      ]
  in
  let fields_json = Yojson.Safe.to_string fields in
  ignore
    (Crud.create store ~type_name:"function_analysis" ~fields_json ~author)

let resolve_llm_creds (store : Store.t) ~(user_id : string option) :
    Llm_client.creds option =
  match user_id with
  | Some uid ->
      (match Auth_store.get_llm_key_secret store ~user_id:uid with
       | Some (key, meta) ->
           Some
             {
               Llm_client.api_key = key;
               api_base =
                 (match meta.Auth_store.base with
                  | Some b when b <> "" -> b
                  | _ -> Llm_client.api_base ());
               model =
                 (match meta.Auth_store.model with
                  | Some m when m <> "" -> m
                  | _ -> Llm_client.model ());
             }
       | None ->
           let k = Llm_client.api_key () in
           if k = "" then None
           else
             Some
               {
                 api_key = k;
                 api_base = Llm_client.api_base ();
                 model = Llm_client.model ();
               })
  | None ->
      let k = Llm_client.api_key () in
      if k = "" then None
      else
        Some
          {
            api_key = k;
            api_base = Llm_client.api_base ();
            model = Llm_client.model ();
          }

let is_placeholder (s : string) : bool =
  let t = String.trim s in
  t = "" || t = "..." || t = "O(...)" || t = "TODO"
  || (String.length t > 0 && String.length t <= 3
      && String.for_all (fun c -> c = '.') t)

let normalize_result ~(model : string) ~(source : string) (j : Yojson.Safe.t)
    : (Yojson.Safe.t, string) result =
  let open Yojson.Safe.Util in
  let str_field key =
    match member key j with
    | `String s -> s
    | `Null -> ""
    | other -> Yojson.Safe.to_string other
  in
  let complexity =
    match str_field "complexity" with
    | "" -> str_field "big_o"
    | s -> s
  in
  let complexity =
    if complexity = "" then str_field "time_complexity" else complexity
  in
  let confidence =
    match str_field "confidence" with "" -> "medium" | s -> s
  in
  let rationale =
    match str_field "rationale" with
    | "" ->
        (match str_field "reason" with
         | "" -> str_field "explanation"
         | s -> s)
    | s -> s
  in
  if is_placeholder complexity || is_placeholder rationale then
    Error
      ("analyze: LLM returned placeholder fields (complexity="
       ^ complexity ^ ")")
  else
    Ok
      (`Assoc
         [
           ("complexity", `String complexity);
           ("confidence", `String confidence);
           ("rationale", `String rationale);
           ("notes", `String (str_field "notes"));
           ("source", `String source);
           ("model", `String model);
           ("label", `String "LLM estimate");
         ])

let heuristic_fallback ~(model : string) ~(fallback : Yojson.Safe.t)
    ~(note : string) : Yojson.Safe.t =
  let open Yojson.Safe.Util in
  `Assoc
    [
      ("complexity", member "complexity" fallback);
      ("confidence", `String "low");
      ("rationale", member "rationale" fallback);
      ("notes", `String note);
      ("source", `String "heuristic+llm-fallback");
      ("model", `String model);
      ("label", `String "heuristic (LLM fallback)");
    ]

let llm_analyze ~(fn_name : string) (e : Ast.expr) ~(creds : Llm_client.creds)
    ~(fallback : Yojson.Safe.t) : (Yojson.Safe.t, string) result =
  let pp = Reader.pp_expr e in
  let system =
    "You estimate asymptotic time complexity for Scheme-family Lisp. \
     Respond with a JSON object only. Required string fields: \
     complexity (e.g. O(1), O(n), O(n log n), O(n^2), O(2^n), unknown), \
     confidence (low|medium|high), rationale (one concrete sentence about \
     THIS function), notes (optional). Never use ellipsis placeholders."
  in
  let user =
    Printf.sprintf
      "Analyze this function and fill real values (not ...).\nname: %s\nsource:\n%s\n"
      fn_name pp
  in
  match
    Llm_client.with_creds creds (fun () ->
        (* GLM spends many tokens on message.reasoning; keep headroom so
           message.content JSON is not truncated mid-object. *)
        Llm_client.call ~temperature:0.0 ~max_tokens:2048 ~json_object:true
          ~system ~user ())
  with
  | Error e -> Error e
  | Ok raw ->
      (match parse_llm_json raw with
       | Ok j ->
           (match
              normalize_result ~model:creds.Llm_client.model ~source:"llm" j
            with
            | Ok out -> Ok out
            | Error err ->
                Ok
                  (heuristic_fallback ~model:creds.Llm_client.model ~fallback
                     ~note:(err ^ "; raw snippet: "
                            ^
                            let s = String.trim raw in
                            if String.length s > 180 then String.sub s 0 180 ^ "…"
                            else s)))
       | Error _parse_err ->
           let snippet =
             let s = String.trim raw in
             if String.length s > 240 then String.sub s 0 240 ^ "…" else s
           in
           Ok
             (heuristic_fallback ~model:creds.Llm_client.model ~fallback
                ~note:("LLM did not return usable JSON; showing heuristic. "
                       ^ snippet)))

let analyze_function (store : Store.t) ~(author : string)
    ~(user_id : string option) ~(fn_name : string) ~(force_llm : bool)
    : (Yojson.Safe.t, string) result =
  match Store.lookup_function store fn_name with
  | None -> Error ("analyze: no function " ^ fn_name)
  | Some (expr, _, _) ->
      let hash = ast_hash expr in
      (match cache_get store ~hash with
       | Some j when not force_llm -> Ok j
       | _ ->
           let heur = heuristic_json ~fn_name expr in
           if not force_llm then Ok heur
           else
             (match resolve_llm_creds store ~user_id with
              | None ->
                  Error
                    "analyze: no LLM key configured (set user key at \
                     /settings/keys or LLM_API_KEY)"
              | Some creds ->
                  (match llm_analyze ~fn_name expr ~creds ~fallback:heur with
                   | Error e -> Error e
                   | Ok j ->
                       cache_put store ~author ~fn_name ~hash ~result:j;
                       Ok j)))

let explain_function (store : Store.t) ~(fn_name : string)
    : (string, string) result =
  match Store.lookup_function store fn_name with
  | None -> Error ("explain: no function " ^ fn_name)
  | Some (expr, _, _) ->
      let layers = Expl.peel ~fn_name expr in
      Ok (String.concat "\n\n" (Expl.layers_to_strings layers))

let result_to_vstr = function
  | Ok (`String s) -> Ast.VStr s
  | Ok j -> Ast.VStr (Yojson.Safe.to_string j)
  | Error e -> Ast.VStr e
