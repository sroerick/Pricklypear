;; package music -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push
;;
;; Little music player. Track rows live in data_rows (type music-track,
;; media_id -> media_blobs); playlist rows (type music-playlist) hold a
;; comma-separated track_ids order. Byte storage is media_blobs; <audio>
;; streams via GET /media/<id> with the normal cookie session — the
;; kernel serve path gives Range/206 seeking for free (storage.media).
;;
;; FLOWS (spec borge/music.borg)
;;   1. upload:   POST /music multipart (file,title,artist,album)
;;                -> (request-multipart) -> (media-put-raw) -> track row
;;   2. play:     ?play=<track-id> (or POST action=play) loads the
;;                row's media into the player card
;;   3. playlist: POST /music action=pl-create|pl-add|pl-delete
;;   4. delete:   POST /music action=delete track=<id> (row + its blob)
;;
;; Kernel prims this package depends on (borge/image.borg
;; htmx-interface + borge/storage.borg media): request-multipart,
;; media-put-raw, media-url, media-delete, plus the Range/206 serve
;; path on GET /media/<id>.
;;
;; Signatures verified live 2026-08-31: string-split is (s sep); get
;; returns the fields dict; rows returns (id . fields) pairs; html/p
;; takes only body; html/p* takes (attrs body); html/select is
;; (name attrs body); html/option is (value label selected?).

(define music/blank?
  (lambda (s) "True when s is null or an empty string."
    (if (null? s) #t (string-eq (as-str s) ""))))

(define music/nonblank
  (lambda (s fallback) "s when nonblank, else fallback."
    (if (music/blank? s) (as-str fallback) s)))

;; ---- model ----

(define music/fields
  (lambda (title artist album media-id)
    "Build a track fields JSON string."
    (dict-set*
      "{}"
      (list
        "title" (as-str title)
        "artist" (as-str artist)
        "album" (as-str album)
        "media_id" (as-str media-id)
        "notes" ""))))

(define music/pl-fields
  (lambda (title ids-str) "Build playlist fields JSON."
    (dict-set*
      "{}"
      (list "title" (as-str title) "track_ids" (as-str ids-str)))))

(define music/create
  (lambda (title artist album media-id)
    "Create a music-track row; returns the row id."
    (create (quote music-track)
            (music/fields title artist album media-id))))

(define music/all
  (lambda () "All track rows on the branch, up to 500."
    (rows (quote music-track) 500)))

(define music/playlists
  (lambda () "All playlist rows, up to 100."
    (rows (quote music-playlist) 100)))

(define music/by-id
  (lambda (id) "Get one track fields dict by id, null when missing."
    (get (quote music-track) (as-str id))))

(define music/pl-create
  (lambda (title) "Create an empty playlist row; returns the row id."
    (create (quote music-playlist) (music/pl-fields title ""))))

(define music/pl-add
  (lambda (pl-id track-id)
    "Append a track id to a playlist; returns the row id."
    (let
      ((f (get (quote music-playlist) (as-str pl-id))))
      (let
        ((old (as-str (dict-get f "track_ids"))))
        (let
          ((next
            (if (music/blank? old)
              (as-str track-id)
              (string-append old "," (as-str track-id)))))
          (update
            (quote music-playlist)
            (as-str pl-id)
            (music/pl-fields (as-str (dict-get f "title")) next)))))))

(define music/pl-tracks
  (lambda (pl-id)
    "Resolve a playlist to track fields dicts in track_ids order;
dangling ids (deleted tracks) are skipped."
    (let
      ((f (get (quote music-playlist) (as-str pl-id))))
      (if (null? f) ()
        (list/filter
          (lambda (r) (not (null? r)))
          (list/map
            (lambda (tid) (get (quote music-track) (as-str tid)))
            (string-split (as-str (dict-get f "track_ids")) ",")))))))

(define music/pl-delete
  (lambda (pl-id) "Delete a playlist row by id."
    (delete (quote music-playlist) (as-str pl-id))))

;; ---- upload (flow 1) ----

(define music/guess-type
  (lambda (filename)
    "Guess a whitelisted audio type by extension; audio/mpeg fallback."
    (let
      ((name (string-downcase (as-str filename))))
      (if (string-ends-with? name ".mp3") "audio/mpeg"
        (if (string-ends-with? name ".m4a") "audio/mp4"
          (if (string-ends-with? name ".ogg") "audio/ogg"
            (if (string-ends-with? name ".flac") "audio/flac"
              (if (string-ends-with? name ".wav") "audio/wav"
                "audio/mpeg"))))))))

(define music/part
  (lambda (parts name)
    "First multipart part matching name: (name filename ct data)."
    (if (null? parts) ()
      (if (string-eq (as-str (car (car parts))) name)
        (car parts)
        (music/part (cdr parts) name)))))

(define music/part-str
  (lambda (parts name)
    "Text value (element 4) of a multipart part by field name.
    Parts are (name filename content-type data); text parts carry the
    value in data with filename/content-type nil."
    (let
      ((p (music/part parts name)))
      (if (null? p) "" (as-str (car (cdr (cdr (cdr p)))))))))

(define music/upload-multipart
  (lambda (parts)
    "Store the file part via media-put-raw; returns media id or ()."
    (let
      ((p (music/part parts "file")))
      (if (null? p) ()
        (let
          ((filename
            (if (null? (car (cdr p))) "" (as-str (car (cdr p)))))
            (ct
              (if (null? (car (cdr (cdr p))))
                ""
                (as-str (car (cdr (cdr p))))))
            (data (as-str (car (cdr (cdr (cdr p)))))))
          (let
            ((type (music/nonblank ct (music/guess-type filename))))
            (media-put-raw type data filename)))))))

;; ---- player (flow 2) ----

(define music/media-url
  (lambda (media-id) "The /media/<uuid> stream path for a media id."
    (as-str (media-url (as-str media-id)))))

(define music/audio-tag
  (lambda (media-id)
    "The <audio controls> element for a media id."
    (render-html
      (html/tag
        "audio"
        (html/attrs
          (list
            (html/attr "class" "music-player")
            (html/attr "controls" "controls")
            (html/attr "preload" "none")
            (html/attr "src" (music/media-url media-id))))
        (string-append
          "Your browser does not support audio. "
          (html/tag "a" (html/attr "href" (music/media-url media-id)) "Raw file"))))))

(define music/sel-id
  (lambda ()
    "Selected track id: ?play=<id> on GET, or the POST action=play track."
    (let
      ((p (request-param "play")))
      (if (null? p)
        (if (string-eq (as-str (request-param "action")) "play")
          (request-param "track")
          ())
        p))))

(define music/player-html
  (lambda ()
    "The player card: selected track title + <audio>, or a hint."
    (let
      ((sel (music/sel-id)))
      (let
        ((f (if (null? sel) () (music/by-id sel))))
        (let
          ((body
            (if (null? f)
              (html/p* (html/attr "class" "muted")
                       (html/text "Pick a track and press Play."))
              (html/join
                (list
                  (html/p*
                    (html/attr "class" "muted")
                    (html/text
                      (string-append
                        (music/nonblank (as-str (dict-get f "title")) "Untitled")
                        " — "
                        (music/nonblank (as-str (dict-get f "artist")) "unknown"))))
                  (music/audio-tag (as-str (dict-get f "media_id"))))))))
          (html/div
            (html/attr "class" "card")
            (html/join (list (html/h2 "Player") body))))))))

;; ---- rendering ----

(define music/style-html
  (lambda ()
    "Small style block for the player page."
    (html/tag
      "style"
      ""
      (html/join
        (list
          ".music-table{width:100%;border-collapse:collapse}"
          ".music-table td,.music-table th{padding:.35rem .5rem;text-align:left;border-bottom:1px solid var(--border,#ddd)}"
          ".music-player{width:100%;max-width:32rem;display:block;margin-top:.5rem}")))))

(define music/action-form
  (lambda (pairs label)
    "One inline hx-post form from (action . extra-pairs) pairs."
    (html/form
      "POST"
      "/music"
      (html/attrs
        (list
          (html/attr "style" "display:inline")
          (html/attr "hx-post" "/music")
          (html/attr "hx-target" "#music-main")
          (html/attr "hx-swap" "outerHTML")))
      (html/join
        (append
          (list (html/hidden "action" (as-str (car pairs))))
          (map (lambda (kv) (html/hidden (as-str (car kv)) (as-str (car (cdr kv)))))
               (cdr pairs))
          (list (html/button (html/attr "type" "submit") label)))))))

(define music/row-html
  (lambda (row)
    "Render one track row with title, artist, album, play + delete."
    (let
      ((id (as-str (car row))) (f (cdr row)))
      (let
        ((title (music/nonblank (as-str (dict-get f "title")) id))
          (artist (music/nonblank (as-str (dict-get f "artist")) "—"))
          (album (music/nonblank (as-str (dict-get f "album")) "—")))
        (html/tag
          "tr"
          ""
          (html/join
            (list
              (html/tag "td" "" (html/text title))
              (html/tag "td" "" (html/text artist))
              (html/tag "td" "" (html/text album))
              (html/tag
                "td"
                ""
                (html/join
                  (list
                    (music/action-form
                      (list "play" (list "track" id)) "Play")
                    " "
                    (music/action-form
                      (list "delete" (list "track" id)) "Del")))))))))))

(define music/table-html
  (lambda (rows-list)
    "The track table, or an empty-state line."
    (if (null? rows-list)
      (html/p* (html/attr "class" "muted")
               (html/text "No tracks yet — upload one below."))
      (html/tag
        "table"
        (html/attr "class" "music-table")
        (html/join
          (list
            (html/tag
              "thead"
              ""
              (html/tag
                "tr"
                ""
                (html/join
                  (list
                    (html/tag "th" "" (html/text "Title"))
                    (html/tag "th" "" (html/text "Artist"))
                    (html/tag "th" "" (html/text "Album"))
                    (html/tag "th" "" "")))))
            (html/tag
              "tbody"
              ""
              (html/join (list/map music/row-html rows-list)))))))))

(define music/upload-form
  (lambda ()
    "The upload card: file + optional title/artist/album."
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Upload")
          (html/form
            "POST"
            "/music"
            (html/attrs
              (list
                ;; File inputs REQUIRE multipart encoding: a plain form
                ;; urlencodes the filename only, and htmx does the same
                ;; unless hx-encoding is set.
                (html/attr "enctype" "multipart/form-data")
                (html/attr "hx-encoding" "multipart/form-data")
                (html/attr "hx-post" "/music")
                (html/attr "hx-target" "#music-main")
                (html/attr "hx-swap" "outerHTML")
                (html/attr
                  "hx-on::after-request"
                  "if(event.detail.successful){this.reset()}")))
            (html/join
              (list
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "file")
                      (html/attr "name" "file")
                      (html/attr "required" "required")
                      (html/attr "accept"
                                 "audio/*,.mp3,.ogg,.flac,.wav,.m4a,.aac,.webm"))))
                " "
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "text")
                      (html/attr "name" "title")
                      (html/attr "placeholder" "Title"))))
                " "
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "text")
                      (html/attr "name" "artist")
                      (html/attr "placeholder" "Artist (optional)"))))
                " "
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "text")
                      (html/attr "name" "album")
                      (html/attr "placeholder" "Album (optional)"))))
                " "
                (html/button (html/attr "type" "submit") "Upload")
                (html/p*
                  (html/attr "class" "muted")
                  (html/text
                    "audio/mpeg ogg flac wav mp4 aac webm, up to 64 MiB."))))))))))

(define music/pl-row-html
  (lambda (row track-options)
    "One playlist line: title (n tracks) + add-track select + delete."
    (let ((id (as-str (car row))) (f (cdr row)))
      (let ((title (music/nonblank (as-str (dict-get f "title")) "Playlist"))
            (count (count (string-split (as-str (dict-get f "track_ids")) ","))))
        (html/p*
          ""
          (html/join
            (list
              (html/strong title)
              (html/text (string-append " (" (as-str count) " tracks) "))
              (html/form
                "POST"
                "/music"
                (html/attrs
                  (list
                    (html/attr "style" "display:inline")
                    (html/attr "hx-post" "/music")
                    (html/attr "hx-target" "#music-main")
                    (html/attr "hx-swap" "outerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "pl-add")
                    (html/hidden "pl" id)
                    (html/select "track" "" track-options)
                    " "
                    (html/button (html/attr "type" "submit") "Add"))))
              " "
              (music/action-form
                (list "pl-delete" (list "pl" id))
                "Delete playlist"))))))))

(define music/track-options-html
  (lambda (tracks)
    "An <option> per track for the playlist add-select."
    (html/join
      (list/map
        (lambda (row)
          (html/option
            (as-str (car row))
            (music/nonblank (as-str (dict-get (cdr row) "title")) "Untitled")
            #f))
        tracks))))

(define music/playlist-html
  (lambda ()
    "Playlists card: per-playlist rows + create form."
    (let ((pls (music/playlists)))
      (let ((opts (music/track-options-html (music/all))))
        (html/div
          (html/attr "class" "card")
          (html/join
            (append
              (list (html/h2 "Playlists"))
              (if (null? pls)
                (list (html/p* (html/attr "class" "muted")
                               (html/text "No playlists yet.")))
                (list/map (lambda (row) (music/pl-row-html row opts)) pls))
              (list
                (html/form
                  "POST"
                  "/music"
                  (html/attrs
                    (list
                      (html/attr "hx-post" "/music")
                      (html/attr "hx-target" "#music-main")
                      (html/attr "hx-swap" "outerHTML")))
                  (html/join
                    (list
                      (html/hidden "action" "pl-create")
                      (html/input
                        (html/attrs
                          (list
                            (html/attr "type" "text")
                            (html/attr "name" "title")
                            (html/attr "placeholder" "New playlist name")
                            (html/attr "required" "required"))))
                      " "
                      (html/button (html/attr "type" "submit") "Create"))))))))))))

(define music/main-html
  (lambda (status)
    "Player + library + playlists + upload; the #music-main fragment.
Status is a string or ()."
    (let
      ((status-html
        (if (music/blank? status) ()
          (html/p* (html/attr "class" "muted") (html/text (as-str status))))))
      (html/div
        (html/attr "id" "music-main")
        (html/join
          (list
            (html/div
              (html/attr "class" "card")
              (html/join
                (list (html/h2 "Player") status-html (music/player-html))))
            (html/h2 "Library")
            (music/table-html (music/all))
            (music/playlist-html)
            (music/upload-form)))))))

;; ---- dispatch ----

(define music/handle-post
  (lambda ()
    "Dispatch a POST action; returns a short status string.
upload (default, multipart) / play / delete / pl-create / pl-add /
pl-delete. Delete also removes the track's media blob — the bytes
were uploaded for that track."
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "play") "playing")
        ((string-eq action "delete")
         (let ((track-id (as-str (request-param "track"))))
           (let ((f (get (quote music-track) track-id)))
             (seq
               (when (not (null? f))
                 (let ((mid (as-str (dict-get f "media_id"))))
                   (unless (music/blank? mid) (media-delete mid))))
               (delete (quote music-track) track-id)
               (string-append "deleted " track-id)))))
        ((string-eq action "pl-create")
         (let ((t (music/nonblank (as-str (request-param "title")) "Playlist")))
           (seq (music/pl-create t) (string-append "playlist created: " t))))
        ((string-eq action "pl-add")
         (seq
           (music/pl-add
             (as-str (request-param "pl"))
             (as-str (request-param "track")))
           "added to playlist"))
        ((string-eq action "pl-delete")
         (seq
           (music/pl-delete (as-str (request-param "pl")))
           "playlist deleted"))
         (else
          ;; default: multipart upload (no action param). Guard the
          ;; content-type: a urlencoded body (form without enctype,
          ;; curl without headers) would make request-multipart
          ;; raise -- answer with a status string, not a 500.
          (let
            ((ct (string-downcase (as-str (request-header "content-type")))))
            (if (string-contains? ct "multipart/form-data")
              (let ((parts (request-multipart)))
                (let ((media-id (music/upload-multipart parts))
                      (title (music/part-str parts "title"))
                      (artist (music/part-str parts "artist"))
                      (album (music/part-str parts "album")))
                  (if (null? media-id)
                    "upload failed: no audio file part"
                    (seq
                      (music/create
                        (music/nonblank title "Untitled")
                        (music/nonblank artist "")
                        (music/nonblank album "")
                        (as-str media-id))
                      (string-append
                        "uploaded "
                        (music/nonblank title "Untitled"))))))
              "upload failed: form did not send multipart data")))))))

(define music/page
  (lambda ()
    "GET /music renders library+player; POST handles upload/actions."
    (if (string-eq (request-method) "POST")
      (let
        ((status (music/handle-post)))
        (if (hx-request?)
          (render-html (music/main-html status))
          (redirect
            (string-append "/music?msg=" (url-encode (as-str status))))))
      (html/page
        (list
          (music/style-html)
          (html/h1 "Music")
          (html/p*
            (html/attr "class" "muted")
            (html/text
              "Tracks on data_rows (type music-track); audio streams from media_blobs with Range/206 seek."))
          (music/main-html (as-str (request-param "msg"))))))))
