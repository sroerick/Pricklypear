(** Language special forms — syntax handled by the evaluator, not
    bound as env lambdas or registered prims.

    Browsable from the image Functions UI as package [(special-forms)].
    Keep this catalog in sync with [Eval.is_special_form] / eval_special_form. *)

type entry = {
  name : string;
  summary : string;
  arity : string;
  examples : string;
}

let catalog : entry list =
  [
    {
      name = "seq";
      summary =
        "Evaluate body expressions left-to-right for effects; return \
         the last body's value. Empty (seq) returns nil.";
      arity = "(seq e1 e2 … en)  — 0 or more bodies";
      examples = "(seq (define x 1) (define y 2) y)  →  2";
    };
    {
      name = "if";
      summary =
        "Conditional: evaluate test, then then-branch or else-branch \
         (not both). Non-nil non-false is truthy; only #f and () are falsey.";
      arity = "(if test then else)";
      examples = "(if #t 1 2)  →  1";
    };
    {
      name = "lambda";
      summary =
        "Create a closure. Params are symbols; body is a single expr \
         (use seq for multi-step bodies).";
      arity = "(lambda (p1 p2 …) body)";
      examples = "((lambda (x) (+ x 1)) 41)  →  42";
    };
    {
      name = "let";
      summary =
        "Bind names in a child env, then evaluate body. Bindings are \
         evaluated in the outer env (not sequential let*).";
      arity = "(let ((name val) …) body)";
      examples = "(let ((y 5)) y)  →  5";
    };
    {
      name = "letrec";
      summary =
        "Mutual recursive bindings (AST form). App-form letrec from \
         surface text is not fully supported yet.";
      arity = "(letrec ((name val) …) body)  — AST / partial surface";
      examples = "prefer lambda + define for most code";
    };
    {
      name = "define";
      summary =
        "Bind name to value in the current env (and image persists \
         top-level defines to function_defs). Optional docstring \
         (string literal) is store metadata via function_defs.doc; \
         read back with (function-doc name). Name is not evaluated.";
      arity =
        "(define name expr [\"doc\"]) | (define (name p…) [\"doc\"] body…)";
      examples =
        "(define double (lambda (x) \"2x\" (+ x x)))  ; or trailing \"doc\"";
    };
    {
      name = "quote";
      summary =
        "Return the argument as data without evaluating it. Reader \
         sugar 'x expands to (quote x).";
      arity = "(quote expr)  or  'expr";
      examples = "'(a b c)  →  list of symbols/strings as values";
    };
    {
      name = "prim";
      summary =
        "Call a registered primitive by name with one evaluated arg \
         (legacy/low-level path; prefer normal (name …) calls).";
      arity = "(prim name arg)";
      examples = "(prim add-one 41) when add-one is registered";
    };
    {
      name = "with-transaction";
      summary =
        "Run a zero-arg lambda inside a dynamic DB transaction \
         (image-layer __with_transaction__ prim). Nested txs error.";
      arity = "(with-transaction (lambda () body…))";
      examples = "(with-transaction (lambda () (create 'note …)))";
    };
    {
      name = "quasiquote";
      summary =
        "Template quote with unquote / unquote-splicing. Reader sugar `x.";
      arity = "(quasiquote template)  or  `template";
      examples = "`(a ,@'(b c) d)";
    };
    {
      name = "defmacro";
      summary =
        "Define a syntactic macro (unevaluated args; body returns code as data).";
      arity = "(defmacro name (params) body)";
      examples = "(defmacro when (t b) `(if ,t ,b ()))";
    };
    {
      name = "gensym";
      summary = "Fresh symbol name string for unhygienic macros.";
      arity = "(gensym)  or  (gensym prefix)";
      examples = "(gensym) → g$1";
    };
    {
      name = "cond";
      summary =
        "Multi-branch conditional. First truthy test's body wins. \
         Use else or #t as final catch-all. Empty cond → nil.";
      arity = "(cond (test body) …)";
      examples = "(cond ((null? xs) 'empty) (#t 'full))";
    };
    {
      name = "and";
      summary =
        "Short-circuit AND. (and) → #t. Returns last value if all truthy, \
         else first falsey (#f or nil).";
      arity = "(and e1 e2 …)";
      examples = "(and #t 1 2) → 2";
    };
    {
      name = "or";
      summary =
        "Short-circuit OR. (or) → #f. Returns first truthy value, else last.";
      arity = "(or e1 e2 …)";
      examples = "(or #f 0 'x) → 0 (0 is truthy)";
    };
  ]

let names : string list = List.map (fun e -> e.name) catalog

let find (name : string) : entry option =
  List.find_opt (fun e -> e.name = name) catalog

(** (name, one-line summary) for package index tables. *)
let summaries : (string * string) list =
  List.map (fun e -> (e.name, e.summary)) catalog
