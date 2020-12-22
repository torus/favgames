(define-module appmain
  (export app-start!))

(select-module appmain)

(use gauche.collection)
(use gauche.threads)

(use srfi-27) ; random

(use rfc.http)
(use rfc.json)
(use dbi)

(use scheme.vector)
(use scheme.set)

(use sxml.tools)
(use text.tree)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)
(add-load-path "./gosh-modules/dbd-sqlite3")
(use dbd.sqlite3)

(use violet)
(use config)

;;
;; Application
;;

(define (create-page user-id . children)
  `(html
    (@ (lang "en"))
    (head
     (meta (@ (charset "utf-8")))
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
     (meta (@ (name "description") (content "")))
     (meta (@ (name "author") (content "Mark Otto, Jacob Thornton, and Bootstrap contributors")))
     (title "Starter Template · Bootstrap")
     (link (@
            (rel "stylesheet")
            (integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
            (href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
            (crossorigin "anonymous")))
     (style
         (string-append
          ".bd-placeholder-img {"
          "  font-size: 1.125rem;"
          "  text-anchor: middle;"
          "  -webkit-user-select: none;"
          "  -moz-user-select: none;"
          "  -ms-user-select: none;"
          "  user-select: none;"
          "}"
          "@media (min-width: 768px) {"
          "  .bd-placeholder-img-lg {"
          "    font-size: 3.5rem;"
          "  }"
          "}"
          ))
     (link (@ (rel "stylesheet") (href "/static/starter-template.css"))))
    (body
     (div (@ (id "fb-root")) "")
     (script (@ (async "async") (defer "defer") (crossorigin "anonymous")
                (src "https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.3&appId=468063727261207&autoLogAppEvents=1")) "")
     (nav (@ (class "navbar navbar-expand-md navbar-dark bg-dark fixed-top"))
          (a (@ (href "/") (class "navbar-brand")) "Favorite Games")
          (button
           (@
            (type "button")
            (data-toggle "collapse")
            (data-target "#navbarsExampleDefault")
            (class "navbar-toggler")
            (aria-label "Toggle navigation")
            (aria-expanded "false")
            (aria-controls "navbarsExampleDefault"))
           (span (@ (class "navbar-toggler-icon"))))
          (div (@ (id "navbarsExampleDefault") (class "collapse navbar-collapse"))
               (ul (@ (class "navbar-nav"))
                   (li (@ (class "nav-item"))
                       ,(if user-id
                            `(a (@ (href ,#"/favs/~user-id"))
                                ,(x->string user-id))
                            `(a (@ (href "/login"))
                                "ログイン"))))
               (form
                (@ (class "form-inline my-2 my-lg-0")
                   (action "/"))
                (input (@ (type "text") (placeholder "Search") (class "form-control mr-sm-2")
                          (aria-label "Search")
                          (name "q")))
                (button (@ (type "submit") (class "btn btn-secondary my-2 my-sm-0"))
                        "Search"))
               ))
     (main
      (@ (role "main") (class "container"))
      ,@children)
     (script (@ (src "/static/script.js")) "")
     (script (@
              (src "https://code.jquery.com/jquery-3.4.1.min.js")
              (integrity "sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=")
              (crossorigin "anonymous"))
             "")
     (script (@
              (src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js")
              (integrity "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1")
              (crossorigin "anonymous"))
             "")
     (script (@
              (src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js")
              (integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM")
              (crossorigin "anonymous"))
             "")))
  )



(define (get-random)
  (call-with-input-file "/dev/random"
    (^p
     (let* ((ch (read-char p))
            (result (if (char? ch)
                        (let ((num (char->integer ch)))
                          (thread-sleep! (/ num 1000))
                          num)
                        (x->string ch))))
       result))))

(define (fetch-alt-names await vec)
  (let ((alt-name-alist (get-alt-names await (vector->list vec))))
    (map (^[entry] (let ((json (cdr entry))) (cdr (assoc "name" json)))) alt-name-alist)))

(define (cdr-or-empty x)
  (if (pair? x) (cdr x) #()))

(define (search-result-entry await user-id search-key)
  (lambda (json)
    (let ((id (cdr (assoc "id" json)))
          (name (cdr (assoc "name" json))))
      `(tr (th ,(get-display-name await json))
           (td ,(if user-id
                    (let ((button-id #"add-button-~id"))
                      `(button (@ (type "button")
                                  (class "btn btn-primary text-nowrap")
                                  (id ,button-id)
                                  (onclick ,#"addGame(\"~id\", \"~button-id\")"))
                               "おきにいりに追加"))
                    `(button (@ (type "button") (class "btn btn-primary text-nowrap")
                                (disabled "disabled"))
                             "おきにいりに追加")))))))

(define (home-page await search-key user-id)
  (if (> (string-length search-key) 0)
      (await (lambda ()
               (let-values (((status header body)
                             (http-post "api.igdb.com"
                                        "/v4/games/"
                                        #"search \"~search-key\"; fields id,alternative_names,name;"
                                        :client-id twitch-clinet-id
										:authorization #"Bearer ~twitch-access-token"
                                        :secure #t
                                        )))
                 `((p ,#"Search requested: ~search-key")
                   ,(if (equal? status "200")
                        (let ((json (parse-json-string body)))
                          `((h2 "検索結果")
                            (table (@ (class "table"))
                                   (tr (th "名前") (td ""))
                                   ,(vector->list
                                     (vector-map (search-result-entry await user-id search-key)
                                                 json)))))
                        `("ERROR"
                          (pre ,body)))))))
      `((h2 "ゲームを探す")
        (form
        (@ (class "form-inline my-2 my-lg-0") (action "/"))
        (input (@ (type "text") (placeholder "Search") (class "form-control mr-sm-2")
                  (aria-label "Search")
                  (name "q")))
        (button (@ (type "submit") (class "btn btn-secondary my-2 my-sm-0")) "Search")))))

;; curl '' \
;; -d 'fields alternative_name,character,collection,company,description,game,name,person,platform,popularity,published_at,test_dummy,theme;' \
;; -H 'user-key: API_KEY' \
;; -H 'Accept: application/json'

(define (get-session key)
  (let ((rset (dbi-do *sqlite-conn* "SELECT user_id FROM sessions WHERE session_key = ?" '() key)))
    (if (zero? (size-of rset))
        #f
        (let ((row (find-min rset)))
          (dbi-close rset)
          (vector-ref row 0)))))

(define (fb-login-button)
  '(div (@ (class "fb-login-button")
           (onlogin "onlogin")
           (data-width "")
           (data-size "large")
           (data-button-type "continue_with")
           (data-auto-logout-link "false")
           (data-use-continue-as "false"))
        ""))

(define (get-user-id-from-cookie await req)
  (let* ((session-cookie (request-cookie-ref req "sesskey"))
         (user-id (and session-cookie
                       (await (cut get-session (cadr session-cookie))))))
    user-id))

(define-http-handler "/"
  (^[req app]
    (let-params req ([search-key "q:q" :default ""])
                (violet-async
                 (^[await]
                   (let* ((session-cookie (request-cookie-ref req "sesskey"))
                          (user-id (and session-cookie
                                        (await (cut get-session (cadr session-cookie))))))

                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             (get-user-id-from-cookie await req)
                                             (home-page await search-key user-id)
                                             ))))))))))


; recommendation
; select f1.user_id u, f1.game_id g1, f2.game_id g2 from favorites f0, favorites f1, favorites f2 where f0.user_id = 3 and f1.user_id = f2.user_id and g1 = f0.game_id and g1 <> g2;

(define (get-favs user-id)
  (dbi-do *sqlite-conn*
          "SELECT game_id FROM favs WHERE user_id = ?"
          '() user-id))

(define (get-data-from-igdb await end-point id-list)
  (if (null? id-list)
      ()
      (await (^[]
               (let-values (((status header body)
                             (let ((ids (string-join (map x->string id-list) ", ")))
                               (http-post "api.igdb.com"
                                          #?=#"/v4~end-point"
                                          #"fields *; where id = (~ids);"
                                          :client-id twitch-clinet-id
										  :authorization #"Bearer ~twitch-access-token"
                                          :secure #t
                                          ))))
                 (if (equal? status "200")
                     (let ((json (parse-json-string body)))
                       (vector->list (vector-map (^j
                                                  (let ((game-id (cdr (assoc "id" j))))
                                                    (cons game-id j))) json)))
                     (raise (list status body)
                      ))
                 )))))

(define (get-data-from-cache await table id)
  (let ((rset (dbi-do *sqlite-conn*
                      #"SELECT data FROM ~table WHERE id = ?"
                '() id)))
    (if (zero? #?=(size-of rset))
        #f
        (let ((row (find-min rset)))
          (dbi-close rset)
          (read-from-string (vector-ref row 0))))))

(define (put-data-to-cache await table alist)
  (let loop ((alist alist))
    (if (null? alist)
        'done
        (let* ((id-and-data (car alist))
               (id (car id-and-data))
               (data (cdr id-and-data)))
          (await (^[]
                   (dbi-do *sqlite-conn*
                           #"INSERT OR IGNORE INTO ~table (id, data) VALUES (?, ?)"
                           '() id (write-to-string data))))
          (loop (cdr alist))))))

(define (get-data-from-cache/missing-ids await table ids)
  (let loop ((ids ids)
             (alist ())
             (missing-ids ()))
    (if (null? ids)
        (values alist missing-ids)
        (let* ((id (car ids))
               (info (get-data-from-cache await table id)))
          (if info
              (loop (cdr ids) (acons id info alist) missing-ids)
              (loop (cdr ids) alist (cons id missing-ids)))))))

(define (get-game-details await ids)
  (let*-values (((alist-cache missing-ids)
                 (get-data-from-cache/missing-ids await "cache_games" ids))
                ((alist-igdb)
                 (get-data-from-igdb await "/games/" missing-ids)))
    (put-data-to-cache await "cache_games" alist-igdb)
    (append alist-cache alist-igdb)))

(define (get-alt-names await ids)
  (let*-values (((alt-name-alist-cache missing-ids)
                 (get-data-from-cache/missing-ids await "cache_alternative_names" ids))
                ((alt-name-alist-igdb)
                 (get-data-from-igdb await "/alternative_names/" missing-ids)))
    (put-data-to-cache await "cache_alternative_names" alt-name-alist-igdb)
    (append alt-name-alist-cache alt-name-alist-igdb)))

;;

(define (get-display-name await game-detail)
  (let* ((alt-name-ids (cdr-or-empty (assoc "alternative_names" game-detail)))
         (alt-names #?=(get-alt-names await (vector->list alt-name-ids)))
         (japanese-title (filter (^[entry]
                                   (let* ((dat (cdr entry))
                                          (comment (assoc "comment" dat)))
                                     (and comment (string=? (cdr comment) "Japanese title"))))
                                 alt-names)))
    (cdr (assoc "name" (if (null? japanese-title)
                           game-detail
                           #?=(car japanese-title))))))

(define (render-fav-entry await game-detail)
  `(tr (td ,(get-display-name await game-detail))))

(define (render-favs await user-id)
  (let*-values (((rset) (await (cut get-favs user-id)))
                ((game-ids) (map (^[row] (vector-ref row 0)) rset)))
    (dbi-close rset)
    `((h2 "おきにいりのゲーム")
      (table (@ (class "table"))
            ,@(map (^[game]
                     (let ((game-id (car game))
                           (game-detail (cdr game)))
                       (render-fav-entry await game-detail)))
                   (get-game-details await game-ids))))))

(define-http-handler #/^\/favs\/(\d+)/
  (^[req app]
    (let-params req ([user-id "p:1" :convert x->integer])
                (violet-async
                 (^[await]
                   (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             (get-user-id-from-cookie await req)
                                             (render-favs await user-id)))))
                   )))))

(define-http-handler "/add"
  (with-post-json
   (lambda (req app)
     (violet-async
      (^[await]
        (let* ((session-cookie (request-cookie-ref req "sesskey"))
               (user-id (and session-cookie
                             (await (cut get-session (cadr session-cookie)))))
               (json (request-param-ref req "json-body"))
               (game-id (cdr (assoc "game" json))))
          (dbi-do *sqlite-conn*
                  "INSERT OR IGNORE INTO favs (game_id, user_id) VALUES (?, ?)"
                  '() game-id user-id)

          (respond/ok req #"OK")
          ))))))

(define (profile-form await user-id)
  `((h2 "プロフィールの編集")
    (form (@ (onsubmit "return submitProfile(this)"))
     (div (@ (class "form-group"))
          (label (@ (for "name-input")) "名前")
          (input (@ (type "text")
                    (class "form-control")
                    (id "name-input")
                    (aria-describedby "name-help")))
          (small (@ (id "name-help")
                    (class "form-text text-muted"))
                 "公開されても良い名前を書いてください。"))
     (button (@ (type "submit") (class "btn btn-primary")) "保存")))
  )

(define (get-profile await user-id)
  (await (^[]
		   (let ((rset (dbi-do *sqlite-conn*
							   "SELECT name FROM user_profile WHERE user_id = ?"
							   '() user-id)))
			 (let ((name (if (zero? (size-of rset))
							 "ななしさん"
							 (vector-ref (find-min rset) 0) )))
			   (acons 'name name ()))
			 ))))

(define (profile-page await user-id)
  (let ((prof #?=(get-profile await user-id)))
	`((h2 "プロフィール")
	  (p "名前：" ,(cdr (assq 'name prof))))))

(define-http-handler "/profile"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((session-cookie (request-cookie-ref req "sesskey"))
              (user-id (and session-cookie
                            (await (cut get-session (cadr session-cookie))))))
         (respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 user-id
                                 (if user-id
                                     #?=(profile-page await user-id)
                                     '(p (a (@ (href "/login")
                                               "ログインしてください。")))))))))))))

(define-http-handler "/profile/form"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((session-cookie (request-cookie-ref req "sesskey"))
               (user-id (and session-cookie
                             (await (cut get-session (cadr session-cookie))))))
         (respond/ok req (cons "<!DOCTYPE html>"
                                   (sxml:sxml->html
                                    (create-page
                                     (get-user-id-from-cookie await req)
                                     (if user-id
                                         (profile-form await user-id)
                                         '(p (a (@ (href "/login")
                                                   "ログインしてください。")))))))))))))

(define-http-handler "/profile/update"
  (with-post-json
   (^[req app]
     (violet-async
      (^[await]
        (let* ((session-cookie (request-cookie-ref req "sesskey"))
               (user-id (and session-cookie
                             (await (cut get-session (cadr session-cookie)))))
               (json (request-param-ref req "json-body"))
               (name (cdr (assoc "name" json))))
          (if user-id
              (begin
                (await (^[]
                         (dbi-do *sqlite-conn*
                                 "INSERT OR REPLACE INTO user_profile (user_id, name) VALUES (?, ?)"
                                 '() user-id name)))
                (respond/ok req "done"))
              (respond/ok req "not logged in"))))))))

(define-http-handler #/^\/static\// (file-handler))

(define (make-session-key!)
  (let ((key (random-integer #x10000000000000000)))
    (format #f "~16,'0x" key)))

(define-http-handler '(GET) "/login"
  (^[req app]
    ))

(define-http-handler "/login"
  (with-post-json
   (lambda (req app)
     (if (eq? 'POST (request-method req))
         (let* ((json (request-param-ref req "json-body"))
                (fb-id (cdr (assoc "userID" json)))
                (rset (dbi-do *sqlite-conn*
                              "SELECT user_id FROM facebook_user_auths WHERE facebook_id = ?"
                              '() fb-id))
                (user-id #f)
                (session-key (make-session-key!)))
           (if (zero? (size-of rset))
               (begin
                 (dbi-do *sqlite-conn* "INSERT INTO users (user_id) VALUES (NULL)" '())
                 (set! user-id (sqlite3-last-id *sqlite-conn*))
                 (dbi-do *sqlite-conn*
                         "INSERT INTO facebook_user_auths (facebook_id, user_id) VALUES (?, ?)"
                         '() fb-id user-id))
               (for-each (^r (set! user-id (vector-ref r 0))) rset))

           (dbi-close rset)

           (dbi-do *sqlite-conn*
                   "INSERT INTO sessions (session_key, user_id) VALUES (?, ?)"
                   '() session-key user-id)

           (response-cookie-add! req "sesskey" session-key)
           (respond/ok req #"OK ~session-key"))

         ;; GET method
         (respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 #f
                                 (fb-login-button)
                                 )))))
     )))

(define *sqlite-conn* #f)

(define (execute-query str)
  (dbi-execute (dbi-prepare *sqlite-conn* str)))

(define (execute-query-tree tree)
  (execute-query (tree->string tree)))

(define (create-tables)
  (execute-query-tree '("CREATE TABLE IF NOT EXISTS users ("
                        " user_id INTEGER PRIMARY KEY,"
                        " display_name TEXT"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS facebook_user_auths ("
                        " facebook_id INTEGER PRIMARY KEY,"
                        " user_id INTEGER"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS favs ("
                        " game_id INTEGER PRIMARY KEY,"
                        " user_id INTEGER"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS sessions ("
                        " session_key TEXT PRIMARY KEY,"
                        " user_id INTEGER"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS cache_games ("
                        " id INTEGER PRIMARY KEY,"
                        " data TEXT"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS cache_alternative_names ("
                        " id INTEGER PRIMARY KEY,"
                        " data TEXT"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS user_profile ("
                        " user_id INTEGER PRIMARY KEY,"
                        " name TEXT"
                        ")"))

  'ok)

(define-http-handler "/admin/setup"
  (^[req app]
    (violet-async
     (^[await]
       (await create-tables)
       (respond/ok req (cons "<!DOCTYPE html>"
                             (sxml:sxml->html
                                          (create-page
                                           (get-user-id-from-cookie await req)
                                           '(p "done")
                                           ))))))))

(define (app-start!)
  (random-source-randomize! default-random-source)
  (let ((conn (dbi-connect "dbi:sqlite3:favgame-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")))
