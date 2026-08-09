;; package pp -- client optimistic-preview dispatch + render.
;; Loaded into the browser evaluator (client/bin/interp.js) AFTER the
;; package sources (structed, outline, html, bible). Spec:
;; borge/structural-editing.borg client-eval.
;;
;; pp/dispatch-op mirrors the PURE branches of notes/outline/apply-op
;; (the ops whose fns are pure outline/* + structed/*). It calls the
;; SAME functions the server calls — no reimplementation of the op
;; algebra. The set-*/eval branches use request-param (impure) and are
;; not previewable; they return the tree unchanged so the client falls
;; back to server-only for those.

(define pp/dispatch-op
  (lambda
    (op tree nid)
    (cond
      ((string-eq op "add-child")
        (outline/add-child-by-id tree nid (outline/node (gensym "n") "New node")))
      ((string-eq op "add-sibling")
        (outline/insert-sibling tree nid (outline/node (gensym "n") "New node")))
      ((string-eq op "delete")
        (if (string-eq (as-str nid) "root") tree (outline/delete tree nid)))
      ((string-eq op "move-up") (outline/move-up tree nid))
      ((string-eq op "move-down") (outline/move-down tree nid))
      ((string-eq op "promote")
        (if (string-eq (as-str nid) "root") tree (outline/promote tree nid)))
      ((string-eq op "demote")
        (if (string-eq (as-str nid) "root") tree (outline/demote tree nid)))
      ((string-eq op "cycle-todo")
        (outline/update tree nid (lambda (n) (outline/cycle-todo n))))
      ((string-eq op "archive")
        (if (string-eq (as-str nid) "root") tree
          (outline/update tree nid (lambda (n) (outline/archive n)))))
      ((string-eq op "unarchive")
        (if (string-eq (as-str nid) "root") tree
          (outline/update tree nid (lambda (n) (outline/unarchive n)))))
      ((string-eq op "slurp-forward")
        (if (string-eq (as-str nid) "root") tree
          (car (structed/slurp-forward outline/kids outline/set-kids tree nid))))
      ((string-eq op "barf-forward")
        (if (string-eq (as-str nid) "root") tree
          (car (structed/barf-forward outline/kids outline/set-kids tree nid))))
      (else tree))))

;; pp/preview: apply the op to the tree and render via outline/html-lite
;; (the same render lib the server uses, minus action buttons — the
;; action buttons are re-added when the server's authoritative render
;; morphs in). Returns the HTML string. Spec: client-eval OPTIMISTIC
;; EXECUTION MODEL steps 1-3.
(define pp/preview
  (lambda
    (slug tree op nid)
    (let ((new-tree (pp/dispatch-op op tree nid)))
      (if (equal? new-tree tree)
        ""
        (outline/html-lite slug new-tree #f)))))
