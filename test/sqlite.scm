(use gauche.test)
(use gauche.collection)

(test-start "dbd.sqlite3")

(use dbi)
(add-load-path "./gosh-modules/dbd-sqlite3")
;(load "./gosh-modules/dbd-sqlite3/dbd/sqlite3")
(use dbd.sqlite3)
;(require 'dbd.sqlite3)
(test-module 'dbd.sqlite3)

(define connection #f)

(define (cleanup-test)
  (define (remove-file file)
    (when (file-exists? file)
      (sys-unlink file)))
  (remove-file "test.db"))

(cleanup-test)

(test* "dbi-connect"
       <sqlite3-connection>
       (let1 c (dbi-connect "dbi:sqlite3:test.db")
         (set! connection c)
         (class-of c)))

(test* "Creating test table"
       #f
       (dbi-open?
        (dbi-execute
         ;; http://www.sqlite.org/datatype3.html
         ;;TODO NUMERIC
         (dbi-prepare connection
                      "CREATE TABLE tbl1(id INTEGER, name TEXT, image NONE, rate REAL);"))))

(test* "Checking insert common fields"
       #f
       (dbi-open?
        (dbi-execute
         (dbi-prepare connection
                      "INSERT INTO tbl1 VALUES(1, 'name 1', x'0101', 0.8);"))))

(define (select-rows sql . params)
  (let1 rset (apply dbi-do connection sql '() params)
    (unwind-protect
     (map identity rset)
     (dbi-close rset))))

(test* "Checking current inserted values"
       '(#(1 "name 1" #u8(1 1) 0.8))
       (select-rows "SELECT id, name, image, rate FROM tbl1 ORDER BY id ASC;"))
