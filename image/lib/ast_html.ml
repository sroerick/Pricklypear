(** Pure_html AST pretty-printer with recursion/tail CSS classes. *)
open Pure_html
module Ast = Pricklypear_lisp.Ast
module Rec = Pricklypear_lisp.Recursion
module Reader = Pricklypear_lisp.Reader

let class_for ~(info : Rec.recursion_info) ~(mutual : string list)
    ~(path : int list) ~(callee : string option) : string list =
  let cls = ref [ "ast-node" ] in
  (match callee with
   | Some n when n = "" -> ()
   | Some n when List.exists (fun s -> s.Rec.callee = n && s.path = path) info.self_sites
     ->
       cls := "ast-self-ref" :: !cls;
       if Rec.is_tail_self_path info path then cls := "ast-tail" :: !cls
       else cls := "ast-non-tail" :: !cls
   | Some n when List.mem n mutual -> cls := "ast-mutual" :: !cls
   | _ -> ());
  !cls

let join_class xs = String.concat " " xs

let rec node_of ~(info : Rec.recursion_info) ~(mutual : string list)
    ~(path : int list) (e : Ast.expr) : node =
  match e with
  | Ast.Var n -> HTML.span [ HTML.class_ "ast-var" ] [ txt "%s" n ]
  | Ast.Lit v -> HTML.span [ HTML.class_ "ast-lit" ] [ txt "%s" (Reader.pp_value v) ]
  | Ast.Quote e' ->
      HTML.span [ HTML.class_ "ast-quote" ]
        [ txt "'"; node_of ~info ~mutual ~path:(0 :: path) e' ]
  | Ast.App (Ast.Var callee, args) ->
      let cls =
        class_for ~info ~mutual ~path:(List.rev path) ~callee:(Some callee)
      in
      let kids =
        txt "("
        :: HTML.span [ HTML.class_ "ast-head" ] [ txt "%s" callee ]
        :: (match args with
            | [] -> [ txt ")" ]
            | _ ->
                List.concat
                  (List.mapi
                     (fun i a ->
                       [
                         txt " ";
                         node_of ~info ~mutual ~path:(i :: path) a;
                       ])
                     args)
                @ [ txt ")" ])
      in
      HTML.span [ HTML.class_ "%s" (join_class cls) ] kids
  | Ast.App (f, args) ->
      HTML.span [ HTML.class_ "ast-app" ]
        (txt "("
         :: node_of ~info ~mutual ~path:(0 :: path) f
         :: List.concat
              (List.mapi
                 (fun i a ->
                   [ txt " "; node_of ~info ~mutual ~path:((i + 1) :: path) a ])
                 args)
         @ [ txt ")" ])
  | Ast.If (t, th, el) ->
      HTML.span [ HTML.class_ "ast-if" ]
        [
          txt "(if ";
          node_of ~info ~mutual ~path:(0 :: path) t;
          txt " ";
          node_of ~info ~mutual ~path:(1 :: path) th;
          txt " ";
          node_of ~info ~mutual ~path:(2 :: path) el;
          txt ")";
        ]
  | Ast.Seq (a, b) ->
      HTML.span [ HTML.class_ "ast-seq" ]
        [
          txt "(seq ";
          node_of ~info ~mutual ~path:(0 :: path) a;
          txt " ";
          node_of ~info ~mutual ~path:(1 :: path) b;
          txt ")";
        ]
  | Ast.Let (n, v, body) ->
      HTML.span [ HTML.class_ "ast-let" ]
        [
          txt "(let ((";
          txt "%s" n;
          txt " ";
          node_of ~info ~mutual ~path:(0 :: path) v;
          txt ")) ";
          node_of ~info ~mutual ~path:(1 :: path) body;
          txt ")";
        ]
  | Ast.Letrec (bs, body) ->
      let bind_nodes =
        List.concat
          (List.mapi
             (fun i (n, e') ->
               [
                 txt "(";
                 txt "%s" n;
                 txt " ";
                 node_of ~info ~mutual ~path:(i :: path) e';
                 txt ") ";
               ])
             bs)
      in
      HTML.span [ HTML.class_ "ast-letrec" ]
        ([ txt "(letrec (" ]
         @ bind_nodes
         @ [
             txt ") ";
             node_of ~info ~mutual ~path:(List.length bs :: path) body;
             txt ")";
           ])
  | Ast.Lambda (ps, body) ->
      HTML.span [ HTML.class_ "ast-lambda" ]
        [
          txt "(lambda (";
          txt "%s" (String.concat " " ps);
          txt ") ";
          node_of ~info ~mutual ~path:(0 :: path) body;
          txt ")";
        ]
  | Ast.Prim (n, args) ->
      HTML.span [ HTML.class_ "ast-prim" ]
        (txt "(%s" n
         :: (match args with
             | [] -> [ txt ")" ]
             | _ ->
                 List.concat
                   (List.mapi
                      (fun i a ->
                        [
                          txt " ";
                          node_of ~info ~mutual ~path:(i :: path) a;
                        ])
                      args)
                 @ [ txt ")" ]))
  | Ast.Define (n, body) ->
      HTML.span [ HTML.class_ "ast-define" ]
        [
          txt "(define ";
          txt "%s" n;
          txt " ";
          node_of ~info ~mutual ~path:(0 :: path) body;
          txt ")";
        ]

let pp_expr_html ?(fn_name = "") ?(mutual = []) (e : Ast.expr) : node =
  let info =
    if fn_name = "" then
      {
        Rec.is_recursive = false;
        tail_recursive = false;
        self_sites = [];
        callees = [];
      }
    else Rec.analyze_self ~fn_name e
  in
  let body = Rec.unwrap_fn_body e in
  HTML.pre [ HTML.class_ "bb-source ast-walk" ]
    [ node_of ~info ~mutual ~path:[] body ]

let recursion_badges ~(fn_name : string) (e : Ast.expr) : node list =
  let info = Rec.analyze_self ~fn_name e in
  let bits = ref [] in
  if info.is_recursive then begin
    bits :=
      HTML.span
        [
          HTML.class_ "badge %s"
            (if info.tail_recursive then "ast-tail" else "ast-non-tail");
        ]
        [
          txt "%s"
            (if info.tail_recursive then "tail-recursive"
             else "recursive (non-tail)");
        ]
      :: !bits
  end;
  List.rev !bits
