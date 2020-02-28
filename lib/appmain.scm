(define-module appmain
  (export app-start!))

(select-module appmain)

(use gauche.threads)
(use rfc.http)
(use rfc.json)

(use scheme.vector)

(use sxml.tools)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(use violet)
(use config)

;;
;; Application
;;

(define (create-page . children)
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
          (a (@ (href "#") (class "navbar-brand")) "Favorite Games")
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
                   (li (@ (class "nav-item active"))
                       (a (@ (href "#") (class "nav-link"))
                          "Home " (span (@ (class "sr-only")) "(current)")))
                   (li (@ (class "nav-item")) (a (@ (href "#") (class "nav-link")) "Link"))
                   (li (@ (class "nav-item"))
                       (a (@
                           (tabindex "-1") (href "#") (class "nav-link disabled")
                           (aria-disabled "true"))
                          "Disabled"))
                   (li (@ (class "nav-item dropdown"))
                       (a (@ (id "dropdown01") (href "#")
                             (data-toggle "dropdown")
                             (class "nav-link dropdown-toggle")
                             (aria-haspopup "true")
                             (aria-expanded "false"))
                          "Dropdown")
                       (div (@ (class "dropdown-menu") (aria-labelledby "dropdown01"))
                            (a (@ (href "#") (class "dropdown-item")) "Action")
                            (a (@ (href "#") (class "dropdown-item")) "Another action")
                            (a (@ (href "#") (class "dropdown-item")) "Something else here"))))
               (form
                (@ (class "form-inline my-2 my-lg-0")
                   (action "/"))
                (input (@ (type "text") (placeholder "Search") (class "form-control mr-sm-2")
                          (aria-label "Search")
                          (name "q")))
                (button (@ (type "submit") (class "btn btn-secondary my-2 my-sm-0"))
                        "Search"))))
     (main
      (@ (role "main") (class "container"))
      ,@children)
     (script (@ (src "/static/script.js")) "")
     (script (@
              (src "https://code.jquery.com/jquery-3.3.1.slim.min.js")
              (integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo")
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

(define (get-alt-name await id)
  (let-values (((status header body)
                (http-post "api-v3.igdb.com"
                           "/alternative_names/"
                           #"fields *; where id=~id;"
                           :user-key api-key
                           :secure #t
                           )))
    (let ((json (parse-json-string body)))
      (cdr (assoc "name" (vector-ref json 0)))
      )))

(define (fetch-alt-name await vec)
  (vector-fold (^[a b] (string-append (get-alt-name await b) " / " a)) "" vec))

(define (search-result-entry await)
  (lambda (json)
    (let ((id (cdr (assoc "id" json)))
          (name (cdr (assoc "name" json)))
          (alt-name-ids (cdr (assoc "alternative_names" json))))
      (let ((alt-names (fetch-alt-name await alt-name-ids)))
        `(li (a (@ (href ,#"/games/~id")) ,name) ,alt-names))
      )))

(define (home-page await search-key)
  (if (> (string-length search-key) 0)
      (await (lambda ()
               (let-values (((status header body)
                             (http-post "api-v3.igdb.com"
                                        "/games/"
                                        #"search \"~search-key\"; fields id,alternative_names,name;"
                                        :user-key api-key
                                        :secure #t
                                        )))
                 `((p ,#"Search requested: ~search-key")
                   (pre ,(x->string status))
                   ,(if (equal? status "200")
                        (let ((json (parse-json-string body)))
                          (list 'ul
                                (vector->list (vector-map (search-result-entry await) json))))
                        `("ERROR"
                          (pre ,body))))))
                 )
      `(p "Hey!")
      )
  )

;; curl '' \
;; -d 'fields alternative_name,character,collection,company,description,game,name,person,platform,popularity,published_at,test_dummy,theme;' \
;; -H 'user-key: API_KEY' \
;; -H 'Accept: application/json'

(define-http-handler "/"
  (^[req app]
    (let-params req ([search-key "q:q" :default ""])
                (violet-async
                 (^[await]
                   (respond/ok req (cons "<!DOCTYPE html>"
                                         (sxml:sxml->html
                                          (create-page
                                           (home-page await search-key)
                                           '(div (@ (class "fb-login-button")
                                                    (onlogin "onlogin")
                                                    (data-width "")
                                                    (data-size "large")
                                                    (data-button-type "continue_with")
                                                    (data-auto-logout-link "false")
                                                    (data-use-continue-as "false")))

                                           ))))
                   )))))

(define-http-handler #/^\/static\// (file-handler))

(define (app-start!)
  )
