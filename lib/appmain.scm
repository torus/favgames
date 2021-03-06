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

(use makiki)
;; rheingau-use doesn't work here.. :(
(add-load-path "./gosh-modules/dbd-sqlite3")
(use dbd.sqlite3)

(use violet)
(use config)

;;
;; Application
;;

(define (navbar user-id)
  `(nav (@ (class "navbar is-success")
           (role "navigation")
           (aria-label "main navigation"))
        (div (@ (class "navbar-brand"))
             (a (@ (class "navbar-item")
                   (href "/"))
                (h1 (@ (class "is-size-3")) "FAVGAM"))

             (a (@ (role "button")
                   (class "navbar-burger")
                   (aria-label "menu")
                   (aria-expanded "false")
                   (data-target "navbar-main"))
                (span (@ (aria-hidden "true")) "")
                (span (@ (aria-hidden "true")) "")
                (span (@ (aria-hidden "true"))) "")
             )

        (div (@ (class "navbar-menu") (id "navbar-main"))
             (div (@ (class "navbar-start"))
                  ,(if user-id
                       `(a (@ (class "navbar-item")
                              (href ,#"/favs/~user-id"))
                           "おきにいり")
                       `(a (@ (class "navbar-item")
                              (href ,#"/login"))
                           "ログイン・新規登録"))
                  (a (@ (class "navbar-item")
                        (href ,#"https://github.com/torus/favgames/wiki")
                        (target "_blank")
                        (rel "noopener noreferrer"))
                     "つかいかた"))
             (div (@ (class "navbar-end"))
                  (a (@ (class "navbar-item")
                        (href "/profile"))
                     "プロフィール")))))

(define (create-page title user-id . children)
  `(html
    (@ (lang "en"))
    (head
     (meta (@ (charset "utf-8")))
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
     (meta (@ (name "description") (content "Share your favorite games!")))
     (meta (@ (name "author") (content "Toru Hisai @ Seaknot Studios GK")))

     (link (@ (rel "stylesheet") (href "/bulma/css/bulma.css")))

     (title ,title)
)
    (body
     (div (@ (id "fb-root")) "")
     (script (@ (async "async") (defer "defer") (crossorigin "anonymous")
                (src "https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.3&appId=468063727261207&autoLogAppEvents=1")) "")

     ,(navbar user-id)

     (div (@ (class "level"))
          "")
     (div (@ (class "container"))
          ,@children)

     (footer (@ (class "footer"))
             (div (@ (class "content"))
                  (p (strong "FAVGAM") " by "
                     (a (@ (href "https://seaknot.dev"))
                        "Seaknot Studio. ")
                     ,(fas-icon "gamepad")
                     " The game database provided by "
                     (a (@ (href "https://www.igdb.com")) "IGDB."))))


     (script (@ (src "/static/script.js")) "")
     (script (@ (defer "defer")
                (src "https://use.fontawesome.com/releases/v5.14.0/js/all.js"))
             "")

     ;; <!-- Global site tag (gtag.js) - Google Analytics -->
     (script (@ (src "https://www.googletagmanager.com/gtag/js?id=G-FJJLN0G19Q")) "")
     (script "  window.dataLayer = window.dataLayer || [];"
             "  function gtag(){dataLayer.push(arguments);}"
             "  gtag('js', new Date());"
             "  gtag('config', 'G-FJJLN0G19Q');"
             )
     )))

(define (fetch-alt-names await vec)
  (let ((alt-name-alist (get-alt-names await (vector->list vec))))
    (map (^[entry] (let ((json (cdr entry))) (cdr (assoc "name" json)))) alt-name-alist)))

(define (cdr-or-empty x)
  (if (pair? x) (cdr x) #()))

(define (cdr-or-false x)
  (if (pair? x) (cdr x) #f))

(define (already-added-button)
  `(button (@ (type "button") (class "button is-primary text-nowrap")
              (disabled "disabled"))
           ,(fas-icon "heart")))

(define (add-button game-id user-id)
  (if user-id
      (let ((button-id #"add-button-~game-id"))
        `(button (@ (type "button")
                    (class "button is-primary text-nowrap")
                    (id ,button-id)
                    (onclick ,#"addGame(\"~game-id\", \"~button-id\")"))
                 ,(far-icon "heart")))
      `(button (@ (type "button") (class "button is-primary text-nowrap")
                  (disabled "disabled"))
               ,(far-icon "heart"))))

(define (search-result-entry await user-id search-key)
  (lambda (json)
    (let ((id (cdr (assoc "id" json)))
          (name (cdr (assoc "name" json))))
      `(tr (th ,(render-game-title-with-link await json))
           (td ,(if user-id
                    (let ((button-id #"add-button-~id"))
                      `(button (@ (type "button")
                                  (class "button is-primary text-nowrap")
                                  (id ,button-id)
                                  (onclick ,#"addGame(\"~id\", \"~button-id\")"))
                               ,(far-icon "heart")))
                    `(button (@ (type "button") (class "button is-primary text-nowrap")
                                (disabled "disabled"))
                             ,(far-icon "heart"))))))))

(define (fas-icon name)
  `(span (@ (class "icon"))
         (i (@ (class ,#"fas fa-~name")) "")))

(define (far-icon name)
  `(span (@ (class "icon"))
         (i (@ (class ,#"far fa-~name")) "")))

(define (get-owned-proc await user-id game-ids)
  (define (id-list ids)
	(string-join (map x->string game-ids) ", "))
  (let ((owned-games
		 (map (^[row] (vector-ref row 0))
			  (dbi-do *sqlite-conn*
					  #`"SELECT game_id FROM favs WHERE user_id = ? AND game_id IN (,(id-list game-ids))"
					  '() user-id))))
	(lambda (game-id)
	  (find (^x (eq? game-id x)) owned-games))))

(define (search-games await user-id search-key)
  (let-values (((status header body)
                (http-post "api.igdb.com"
                           "/v4/games/"
                           #"search \"~search-key\"; fields id; limit 12;"
                           :client-id twitch-clinet-id
                           :authorization #"Bearer ~twitch-access-token"
                           :secure #t
                           )))
    `(,(search-input search-key)
      ,(if (equal? status "200")
           (let* ((json (parse-json-string body))
				  (game-ids (vector->list (vector-map (^x (cdr (assoc "id" x))) json))))
			 (render-games-in-tile await game-ids
								   (get-owned-proc await user-id game-ids) user-id))
           `("ERROR"
             (pre ,body))))))

(define (search-input pre-input)
  `(div (@ (class "block"))
        (form (@ (action "/"))
              (div (@ (class "field has-addons"))
                   (div (@ (class "control"))
                        (input (@ (type "text")
                                  (placeholder "ゲームタイトル")
                                  (class "input")
                                  (aria-label "Search")
                                  (name "q")
                                  (value ,pre-input))))
                   (div (@ (class "control"))
                        (button (@ (type "submit")
                                   (class "button is-info"))
                                ,(fas-icon "search")))))))

(define (newly-added-games await user-id)
  (let* ((rset
          (if user-id
              (dbi-do *sqlite-conn*
                       (string-append
                        "SELECT games.game_id, x.user_id owned FROM games"
                        " LEFT OUTER JOIN"
                        " (SELECT game_id, user_id FROM favs WHERE user_id = ?) x"
                        " ON games.game_id = x.game_id"
                        " ORDER BY added_at DESC LIMIT 18")
                       '() user-id)
              (dbi-do *sqlite-conn*
                       (string-append
                        "SELECT games.game_id, NULL owned FROM games"
                        " ORDER BY added_at DESC LIMIT 18")
                       '())))
         (game-ids (reverse (map (^[row] (vector-ref row 0)) rset)))
         (owneds (reverse (map (^[row] (cons (vector-ref row 0)
                                             (vector-ref row 1))) rset))))

    `(div (@ (class "block"))
          (h3 (@ (class "title is-3")) "最近追加されたゲーム")
          ,(render-games-in-tile await game-ids owneds user-id))))

(define (home-page await search-key user-id)
  `((h3 (@ (class "title is-3")) "ゲームを探す")
    ,(if (> (string-length search-key) 0)
      (await (^[] (search-games await user-id search-key)))
      `(,(search-input "")
        ,(newly-added-games await user-id)))))

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
                                             "おきにいりゲーム"
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
               (print #"Calling API: ~end-point ~id-list")
               (let-values (((status header body)
                             (let ((ids (string-join (map x->string id-list) ", ")))
                               (http-post "api.igdb.com"
                                          #"/v4~end-point"
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
                     (begin
                       (print #"API Error: ~end-point ~id-list ~|status|:\n~body")
                       ())
                     )
                 )))))

(define (get-data-from-cache await table id)
  (let ((rset (dbi-do *sqlite-conn*
                      #"SELECT data FROM ~table WHERE id = ?"
                '() id)))
    (if (zero? (size-of rset))
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

(define (get-covers await ids)
  (let*-values (((alist-cache missing-ids)
                 (get-data-from-cache/missing-ids await "cache_covers" ids))
                ((alist-igdb)
                 (get-data-from-igdb await "/covers/" (filter (^x x) missing-ids))))
    (put-data-to-cache await "cache_covers" alist-igdb)
    (append alist-cache alist-igdb)))

;;

(define (get-display-name await game-detail)
  (let* ((alt-name-ids (cdr-or-empty (assoc "alternative_names" game-detail)))
         (alt-names (get-alt-names await (vector->list alt-name-ids)))
         (japanese-title (filter (^[entry]
                                   (let* ((dat (cdr entry))
                                          (comment (assoc "comment" dat)))
                                     (and comment (string=? (cdr comment) "Japanese title"))))
                                 alt-names)))
    (cdr (assoc "name" (if (null? japanese-title)
                           game-detail
                           (car japanese-title))))))

(define (render-game-title-with-link await game-detail)
  `(a (@ (href ,(cdr-or-empty (assoc "url" game-detail)))
         (target "_blank")
         (rel "noopener noreferrer"))
      ,(get-display-name await game-detail)))

(define (image-url image-id)
  #"https://images.igdb.com/igdb/image/upload/t_cover_big/~|image-id|.jpg")

(define (render-fav-entry await game-detail)
  `((figure (@ (class "image is-3x4"))
            ,(let* ((cover-id (cdr-or-false (assoc "cover" game-detail)))
                    (covers (if (not cover-id)
                                #f
                                (get-covers await (list cover-id))))
                    (cover-url (if (pair? covers)
                                   (let ((id (cdr (assoc "image_id" (car covers)))))
                                     (image-url id))
                                   "/static/noimage.png")))
               `(img (@ (src ,cover-url)))))
    (p ,(render-game-title-with-link await game-detail))))

(define (split-by-6 lst)
  (if (null? lst)
      ()
      (cons
       (take* lst 6)
       (split-by-6 (drop* lst 6)))))

(define (render-games-in-tile await game-ids owneds visiters-user-id)
  (define owned?
    (cond ((pair? owneds)
           (lambda (game-id) (cdr (assq game-id owneds))))
		  ((procedure? owneds) owneds)
          (owneds
           (lambda (game-id) #t))
          (else
           (lambda (game-id) #f))))
  (let ((rows (split-by-6 (get-game-details await game-ids))))
    `(div (@ (class "tile is-vertical"))
          ,@(map (^[cols]
                   `(div (@ (class "tile is-ancestor"))
                         ,@(map (^[game]
                                  (let ((game-id (car game))
                                        (game-detail (cdr game)))
                                    `(div (@ (class "tile is-parent is-2"))
                                          (div (@ (class "tile is-child box"))
                                               ,(render-fav-entry await game-detail)
                                               ,(if (owned? game-id)
                                                    (already-added-button)
                                                    (add-button game-id visiters-user-id)
                                                    )))
                                    ))
                                cols)))
                 rows)))
  )

(define (render-favs await user-id visiters-user-id)
  (let* ((rset (await (cut get-favs user-id)))
         (game-ids (map (^[row] (vector-ref row 0)) rset))
         (owneds (if (eq? user-id visiters-user-id) #t #f)))
    (dbi-close rset)
    (let ((prof (get-profile await user-id)))
      `((h2 (@ (class "title")) ,#"~(cdr (assoc 'name prof)) のおきにいりゲーム")
        ,(render-games-in-tile await game-ids owneds visiters-user-id)))))

(define-http-handler #/^\/favs\/(\d+)/
  (^[req app]
    (let-params req ([user-id "p:1" :convert x->integer])
                (violet-async
                 (^[await]
                   (let* ((session-cookie (request-cookie-ref req "sesskey"))
                          (visiters-user-id
                           (and session-cookie
                                (await (cut get-session (cadr session-cookie))))))
                     (respond/ok req (cons "<!DOCTYPE html>"
                                           (sxml:sxml->html
                                            (create-page
                                             "おきにいり"
                                             (get-user-id-from-cookie await req)
                                             (render-favs await user-id visiters-user-id))))))
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
          (dbi-do *sqlite-conn*
                  "INSERT OR REPLACE INTO games (game_id, added_at) VALUES (?, strftime('%s', 'now'))"
                  '() game-id)

          (respond/ok req #"OK")
          ))))))

(define (profile-form await user-id)
  (let ((prof (get-profile await user-id)))
    `((h2 (@ (class "title")) "プロフィールの編集")
      (form (@ (onsubmit "return submitProfile(this)"))
            (div (@ (class "field"))
                 (label (@ (class "label")
                           (for "name-input")) "名前")
                 (div (@ (class "control"))
                      (input (@ (type "text")
                                (class "input")
                                (id "name-input")
                                (aria-describedby "name-help")
                                (value ,(cdr (assoc 'name prof))))))
                 (p (@ (id "name-help")
                       (class "help"))
                    "公開されても良い名前を書いてください。"))
            (div (@ (class "field"))
                 (div (@ (class "control"))
                      (button (@ (type "submit")
                                 (class "button is-primary text-nowrap"))
                              "保存"))))))
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
  (let ((prof (get-profile await user-id)))
    `((h2 "プロフィール")
      (p "名前：" ,(cdr (assq 'name prof)))
      (p (a (@ (href "/profile/form")) "プロフィールを更新する")))))

(define (require-login req)
  (let ((url (request-path req)))
    `(p (a (@ (href ,#"/login?redirect=~url"))
           "ログインしてください。"))))

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
                                 "プロフィール"
                                 user-id
                                 (if user-id
                                     (profile-page await user-id)
                                     (require-login req)))))))))))

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
                                     "プロフィールの更新"
                                     (get-user-id-from-cookie await req)
                                     (if user-id
                                         (profile-form await user-id)
                                         (require-login req)))))))))))

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
(define-http-handler #/^\/bulma\// (file-handler :root "./node_modules"))

(define (make-session-key!)
  (let ((key (random-integer #x10000000000000000)))
    (format #f "~16,'0x" key)))

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
                                 "ログイン"
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
  (execute-query-tree '("INSERT OR IGNORE INTO users (user_id, display_name)"
                        " VALUES (6502, '6502')"))

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

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS cache_covers ("
                        " id INTEGER PRIMARY KEY,"
                        " data TEXT"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS user_profile ("
                        " user_id INTEGER PRIMARY KEY,"
                        " name TEXT"
                        ")"))
  (execute-query-tree '("INSERT OR IGNORE INTO user_profile (user_id, name)"
                        " VALUES (6502, '6502')"))


  (execute-query-tree '("CREATE TABLE IF NOT EXISTS games ("
                        " game_id INTEGER PRIMARY KEY,"
                        " added_at INTEGER"
                        ")"))
  (execute-query-tree '("CREATE INDEX IF NOT EXISTS games_added_at ON games ("
                        " added_at"
                        ")"))

  (execute-query-tree '("CREATE TABLE IF NOT EXISTS schema_updates ("
                        " version INTEGER PRIMARY KEY,"
						" date INTEGER NOT NULL"
                        ")"))
  (let ((rset (dbi-do *sqlite-conn*
                         "SELECT version FROM schema_updates ORDER BY version DESC LIMIT 1"
                         '())))
	(if (zero? (size-of rset))
		(begin
		  (execute-query-tree '("CREATE TABLE favs_new ("
								" game_id INTEGER NOT NULL,"
								" user_id INTEGER NOT NULL"
								")"))
		  (execute-query-tree '("INSERT INTO favs_new (game_id, user_id)"
								" SELECT * FROM favs"))
		  (execute-query-tree '("DROP TABLE favs"))
		  (execute-query-tree '("ALTER TABLE favs_new RENAME TO favs"))
		  (execute-query-tree '("INSERT INTO schema_updates (version, date)"
								" VALUES (1, strftime('%s', 'now'))")))
		(let ((version (vector-ref (find-min rset) 0)))
		  #?=version)
		)
)

  'ok)

(define-http-handler "/admin/setup"
  (^[req app]
    (violet-async
     (^[await]
       (await create-tables)
       (respond/ok req (cons "<!DOCTYPE html>"
                             (sxml:sxml->html
                              (create-page
                               "初期設定"
                               (get-user-id-from-cookie await req)
                               '(p "done")
                               ))))))))

(define (app-start!)
  (random-source-randomize! default-random-source)
  (let ((conn (dbi-connect "dbi:sqlite3:favgame-sqlite3.db")))
    (set! *sqlite-conn* conn)
    (print "Sqlite connected")))
