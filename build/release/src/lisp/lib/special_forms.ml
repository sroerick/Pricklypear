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

(** Static catalog of special-form entries, one per special form {{name}},
    documenting its summary, arity, and an example. Serves as reference
    text for the Functions UI; keep in sync with the evaluator. *)
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
    {
      name = "begin";
      summary =
        "Alias of seq: evaluate body expressions left-to-right; value of \
         the last. (begin) → nil.";
      arity = "(begin e1 e2 … en)  — 0 or more bodies";
      examples = "(begin (define x 1) (+ x 1))  →  2";
    };
    {
      name = "match";
      summary =
        "Pattern dispatch: evaluate the scrutinee, try clauses in order, \
         first matching pattern binds its variables and evaluates that \
         clause's body. No match → error.";
      arity = "(match scrut (pat body…) …)";
      examples =
        "(match '(a b) ((list 'a x) x) (_ 'none))  →  b";
    };
    {
      name = "guard";
      summary =
        "Evaluate body; if a Lisp error is raised, call the handler lambda \
         (0- or 1-arg; 1-arg receives an (error msg kind) list). \
         Budget-exceeded always propagates.";
      arity = "(guard handler-lambda body…)";
      examples =
        "(guard (lambda (e) 'caught) (/ 1 0))  →  caught";
    };
    {
      name = "error";
      summary =
        "Raise a Lisp error: (error msg), (error kind msg), or evaluated \
         argument forms. Catchable by guard.";
      arity = "(error msg)  |  (error kind msg)";
      examples = "(error \"bad input\")  ; kind defaults to user_error";
    };
    {
      name = "with-step";
      summary =
        "Job checkpoint boundary: run a zero-arg lambda; in a job the \
         result is memoized under (job-id, name), outside a job it is a \
         plain call. See borge/jobs.borg step-checkpoints.";
      arity = "(with-step \"name\" (lambda () body…))";
      examples = "(with-step \"fetch\" (lambda () (http-get url)))";
    };
    {
      name = "macroexpand-1";
      summary =
        "Expand ONE macro level: returns the expansion as data (list of \
         symbols/values). Non-macro forms return reified unchanged.";
      arity = "(macroexpand-1 form)";
      examples =
        "(macroexpand-1 (when #t 1))  →  (if #t 1 ())  ; no quote: the \
         form is captured unevaluated";
    };
    {
      name = "macroexpand";
      summary =
        "Expand macro call sites repeatedly until the head is no longer a \
         macro; returns the fully expanded form as data.";
      arity = "(macroexpand form)";
      examples =
        "(macroexpand (unless #f 1))  ; unless = (when (not t) b) → (if …)";
    };
    {
      name = "apply";
      summary =
        "First-class application: call f with the elements of args-list as \
         arguments. Works on closures and registered prims.";
      arity = "(apply f args-list)";
      examples = "(apply + (list 1 2 3))  →  6";
    };
    {
      name = "list/map";
      summary =
        "Map f over the list (OCaml loop + apply; one eval step total, \
         not per element).";
      arity = "(list/map f xs)";
      examples = "(list/map (lambda (x) (* x x)) '(1 2 3))  →  (1 4 9)";
    };
    {
      name = "list/filter";
      summary =
        "Keep elements for which the predicate is truthy (OCaml loop + \
         apply).";
      arity = "(list/filter p xs)";
      examples = "(list/filter (lambda (x) (< x 3)) '(1 2 3 4))  →  (1 2)";
    };
    {
      name = "list/foldl";
      summary =
        "Left fold: (f acc x) over xs from the left, starting from init.";
      arity = "(list/foldl f init xs)";
      examples = "(list/foldl (lambda (a x) (+ a x)) 0 '(1 2 3))  →  6";
    };
    {
      name = "list/foldr";
      summary =
        "Right fold: foldr f init [a;b;c] = f a (f b (f c init)).";
      arity = "(list/foldr f init xs)";
      examples = "(list/foldr cons () '(1 2 3))  →  (1 2 3)";
    };
    {
      name = "delay";
      summary =
        "Build a lazy thunk from a body expr without evaluating it (see \
         lazy-env-thunks). Var lookups auto-force thunks; force explicitly.";
      arity = "(delay body)";
      examples = "(define t (delay (expensive)))  ; nothing runs yet";
    };
    {
      name = "force";
      summary =
        "Force a thunk: evaluate its body once, memoize the result, return \
         it. Forcing an already-forced thunk returns the memoized value.";
      arity = "(force thunk)";
      examples = "(force t)  ; runs (expensive) now, once";
    };
    {
      name = "unquote";
      summary =
        "Inside quasiquote, substitute the evaluated expr at depth 1 \
         (reader sugar ,x). Error outside quasiquote.";
      arity = "(unquote expr)  or  ,expr";
      examples = "`(a ,( + 1 2))  →  (a 3)";
    };
    {
      name = "unquote-splicing";
      summary =
        "Inside a quasiquoted list, splice the evaluated list's elements \
         (reader sugar ,@x). Error outside quasiquote/list context.";
      arity = "(unquote-splicing expr)  or  ,@expr";
      examples = "`(a ,@'(b c) d)  →  (a b c d)";
    };
  ]

(** The [name] of every entry in [catalog], in catalog order. *)
let names : string list = List.map (fun e -> e.name) catalog

(** Look up a special-form entry by [name]; [None] if not in [catalog]. *)
let find (name : string) : entry option =
  List.find_opt (fun e -> e.name = name) catalog

(** (name, one-line summary) for package index tables. *)
let summaries : (string * string) list =
  List.map (fun e -> (e.name, e.summary)) catalog
