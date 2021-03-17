(barrel
 (name "favgam")
 (version "0.0.1")
 (dependencies
  #;(from-git "makiki"
            (repo "https://github.com/shirok/Gauche-makiki.git")
            (branch "use-connection"))
  (from-git/build "dbd-sqlite3"
                  (repo "https://github.com/mhayashi1120/Gauche-dbd-sqlite3.git"))))
