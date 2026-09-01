;; package music -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push
;;
;; Little music player. Track rows live in data_rows (type music-track,
;; media_id -> media_blobs); playlist rows (type music-playlist) hold a
;; comma-separated track_ids order. Byte storage is media_blobs; <audio>
;; streams via GET /media/<id> with the normal cookie session — the
;; kernel serve path gives Range/206 seeking for free (storage.media).
;;
;; FLOWS
;;   1. upload:   POST /music  (multipart file, title, artist)
;;                -> media-put -> music-track row -> title row on page
;;   2. play:     GET /music  renders player card + track table;
;;                ?play=<track-id> loads that track into the player
;;   3. playlist: POST /music action=pl-create title=...
;;                POST /music action=pl-add pl=<id> track=<id>
;;   4. delete:   POST /music action=delete track=<id>
;;
;; Notes: get/create/update/delete take the fields DICT (not the pair);
;; string-split is (string-split s sep). Verified on prod 2026-08-31.

(define music/blank?
  (lambda (s) "True when s is null or an empty string."
    (if (null? s) #t (string-eq (as-str s) ""))))

(define music/fields
  (lambda
    (title artist album media-id)
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
    (dict-set* "{}" (list "title" (as-str title) "track_ids" (as-str ids-str)))))

(define music/create
  (lambda
    (title artist album media-id)
    "Create a music-track row; returns the row id."
    (create
      (quote music-track)
      (music/fields title artist album media-id))))

(define music/all
  (lambda () "All track rows on the branch, up to 500."
    (rows (quote music-track) 500)))

(define music/playlists
  (lambda () "All playlist rows, up to 100."
    (rows (quote music-playlist) 100)))

(define music/by-id
  (lambda (id) "Get one track row (fields dict) by id, null when missing."
    (get (quote music-track) (as-str id))))

(define music/pl-create
  (lambda (title)
    "Create an empty playlist row; returns the row id."
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
    "Resolve a playlist to track fields dicts in track_ids order."
    (let
      ((f (get (quote music-playlist) (as-str pl-id))))
      (if (null? f) ()
        (list/filter
          (lambda (r) (not (null? r)))
          (list/map
            (lambda (tid) (get (quote music-track) (as-str tid)))
            (string-split (as-str (dict-get f "track_ids")) ",")))))))

(define music/media-url
  (lambda (media-id) "The /media/<uuid> stream path for a media id."
    (string-append "/media/" (as-str media-id))))

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

(define music/row-html
  (lambda (row)
    "Render one track row with title, artist/album, play and delete actions."
    (let
      ((id (as-str (car row))) (f (cdr row)))
      (let
        ((title (music/nonblank (as-str (dict-get f "title")) id))
          (artist (as-str (dict-get f "artist")))
          (album (as-str (dict-get f "album"))))
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
                          (html/hidden "action" "play")
                          (html/hidden "track" id)
                          (html/button
                            (html/attr "type" "submit")
                            "Play")))
                    " "
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
                          (html/hidden "action" "delete")
                          (html/hidden "track" id)
                          (html/button
                            (html/attr "type" "submit")
                            "Del")))))))))))))))

(define music/nonblank
  (lambda (s fallback) "s when nonblank, else fallback."
    (if (music/blank? s) (as-str fallback) s)))

(define music/table-html
  (lambda (rows-list)
    "The track table, or an empty-state line."
    (if (null? rows-list)
      (html/p* (html/attr "class" "muted") (html/text "No tracks yet — upload one below."))
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
                      (html/attr "accept" "audio/*,.mp3,.ogg,.flac,.wav,.m4a,.aac,.webm")
                      (html/attr "required" "required"))))
                " "
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "text")
                      (html/attr "name" "title")
                      (html/attr "method" "")
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
                    "audio/mpeg ogg flac wav mp4 up to 64 MiB (see /media)."))))))))))

(define music/handle-post
  (lambda ()
    "Dispatch a POST action: upload / create / pl-create / pl-add / delete. Returns a short status string."
    (let
      ((action (as-str (request-param "action"))))
      (if (string-eq action "delete")
        (let
          ((track-id (as-str (request-param "track"))))
          (let
            ((f (get (quote music-track) track-id)))
            (seq
              (when (not (null? f))
                (delete (quote music-track) track-id))
              "deleted")))
        (if (string-eq action "pl-create")
          (let
            ((t (music/nonblank (as-str (request-param "title")) "Playlist")))
            (seq (music/pl-create t) (string-append "playlist " t " created")))
          (if (string-eq action "pl-add")
            (seq
              (music/pl-add
                (as-str (request-param "pl"))
                (as-str (request-param "track")))
              "added to playlist")
            ;; default: multipart upload (no action param)
            (let
              ((parts (multipart)))
              (let
                ((media-id (music/upload-multipart parts))
                  (title (music/part-str parts "title"))
                  (artist (music/part-str parts "artist"))
                  (album (music/part-str parts "album")))
                (if (null? media-id)
                  "upload failed: no audio file"
                  (seq
                    (music/create
                      (music/nonblank title "Untitled")
                      artist
                      album
                      (as-str media-id))
                    "uploaded"))))))))))

(define music/part-str
  (lambda (parts name)
    "Text value of a multipart part by field name, empty when missing."
    (let
      ((p (music/part parts name)))
      (if (null? p) "" (as-str (car p))))))

(define music/part
  (lambda (parts name)
    "First part whose field name matches: (name filename content-type data)."
    (if (null? parts) ()
      (if (string-eq (as-str (car (car parts))) name)
        (car parts)
        (music/part (cdr parts) name)))))

(define music/upload-multipart
  (lambda (parts)
    "Store the file part via media-put; returns media id or ()."
    (let
      ((p (music/part parts "file")))
      (if (null? p) ()
        (let
          ((fn (as-str (car (cdr p))))
            (ct (as-str (car (cdr (cdr p)))))
            (data (as-str (car (cdr (cdr (cdr p)))))))
          (let
            ((type
              (if (music/blank? ct)
                (music/guess-type fn)
                ct)))
            (media-put type (as-str data) fn)))))))

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

(define music/main-html
  (lambda ()
    "Player + table + upload form, htmx-targeted fragment root."
    (let
      ((rows-list (music/all))
        (play-id (as-str (request-param "play")))
        (msg (as-str (request-param "msg"))))
      (let
        ((sel
          (if (music/blank? play-id) ()
            (get (quote music-track) play-id)))
          (status
            (if (music/blank? msg) ()
              (html/p* (html/attr "class" "muted") (html/text msg)))))
        (let
          ((player
            (if (null? sel)
              (html/p* (html/attr "class" "muted") (html/text "Pick a track and press Play."))
              (music/audio-tag (as-str (dict-get (cdr sel) "media_id"))))))
          (html/div
            (html/attr "id" "music-main")
            (html/join
              (list
                (html/div
                  (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Player")
                      (if (null? status) "" status)
                      player)))
                (html/h2 "Library")
                (music/table-html rows-list)
                (music/upload-form)))))))))

(define music/page
  (lambda ()
    "GET /music renders library+player; POST handles upload/actions."
    (if (string-eq (request-method) "POST")
      (let
        ((status (music/handle-post)))
        (seq
          (if (hx-request?)
            (render-html (music/main-html))
            (redirect "/music?msg=%s")))
        )
      (html/page
        (list
          (html/h1 "Music")
          (html/p*
            (html/attr "class" "muted")
            (html/text
              "Tracks on data_rows (type music-track); audio streams from media_blobs with Range/206 seek."))
          (music/main-html))))
    )
  )
