;; package bi/dashboard -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define bi/dashboard
  (lambda
    ()
    (seq (bi/seed-sample-data) (bi/dashboard-html))))

(define bi/dashboard/preview-note
  (lambda
    ()
    "Branch preview: (fork \"bi-preview\") (checkout \"\226\128\166/bi-preview\") then edit bi/dashboard-html; merge when ready. Metrics functions stay on main and are read-only to layout agents."))

(define bi/dashboard/publish
  (lambda
    (source-body prompt)
    (seq
      (install-function
        "bi/dashboard-html"
        (as-str source-body))
      (create
        (quote dashboard-revision)
        (dict-set*
          "{}"
          (list
            "name"
            "bi/dashboard-html"
            "source"
            (as-str source-body)
            "prompt"
            (as-str prompt)
            "author"
            (whoami)
            "created_at"
            (date-today))))
      "published")))

(define bi/dashboard/read
  (lambda
    ()
    (dict-set*
      "{}"
      (list
        "metrics"
        (bi/metrics-snapshot)
        "source"
        (bi/dashboard/source)
        "route"
        "/bi"))))

(define bi/dashboard/source
  (lambda () (function-source "bi/dashboard-html")))

