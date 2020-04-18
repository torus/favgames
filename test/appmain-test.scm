(use gauche.test)

(test-start "appmain")

(add-load-path "./lib/")
(use violet)
(use appmain)

(test-module 'appmain)
