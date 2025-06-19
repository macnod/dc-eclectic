;; Run these tests in the repl with
;;   (prove:run #P"dc-eclectic-tests.lisp")
;; or, from the shell with
;;   make test
;;
;; Change 19
;;

(in-package :cl-user)

;; (require :dc-eclectic)
(pushnew (truename ".") asdf:*central-registry* :test #'equal)
(asdf:load-system :dc-eclectic)

(require :prove)
(defpackage :dc-eclectic-tests (:use :cl :prove :dc-eclectic :dc-ds :cl-ppcre))
(in-package :dc-eclectic-tests)

(defun top-of-second ()
  (loop with reference-timestamp = (timestamp-string)
        for timestamp = (timestamp-string)
        while (string= timestamp reference-timestamp)
        finally (return timestamp)))

(defun run-tests ()
  (prove:run #P"dc-eclectic-tests.lisp"))

(plan 152)

;; universal
(let* ((universal-time (get-universal-time))
       (unix-time (universal-time-to-unix-time universal-time)))
  (is (unix-time-to-universal-time unix-time) universal-time
      "unix-time-to-universal-time")
  (is (universal-time-to-unix-time universal-time) unix-time
      "universal-time-to-unix-time")
  (ok (zerop (universal-time-to-unix-time
              (unix-time-to-universal-time 0)))
      "unix-time universal-time round trip")
  (ok (<= (get-unix-time) (get-unix-time))
      "In consecutive calls to get-unix-time, first call <= second call"))

(is (to-ascii "a") "a"
    "to-ascii with single-ascii-character string")
(is (to-ascii "abc") "abc"
    "to-ascii with multi-ascii-character string")
(is (to-ascii "ñ") "?"
    "to-ascii with single-non-ascii-character string")
(is (to-ascii "ñ¼½¾") "????"
    "to-ascii with multi-non-ascii-character string")
(is (to-ascii "ñ¼½¾abc") "????abc"
    "to-ascii with multi-non-ascii-character string followed by ascii")
(is (to-ascii "ñx¼" :replacement-char #\Space) " x "
    "to-ascii with custom replacement char")
(is (to-ascii (vector (code-char 129) #\x (code-char 130))
              :printable-only nil)
    "x"
    "to-ascii chars above high with printable-only nil")
(is (to-ascii (vector (code-char 29) #\x (code-char 28))
              :printable-only nil)
    "x"
    "to-ascii chars below min with printable-only nil")
(is (to-ascii (vector (code-char 1000) #\y (code-char 1001))
              :printable-only nil)
    "?y?"
    "to-ascii non-ascii chars with printable-only nil")

(is (flatten (list 1 2 (list 3 (list 4 (list 5 6) 7) 8) 9))
    (list 1 2 3 4 5 6 7 8 9)
    "flatten nested list of integers")

(is (flatten (list 1 2 3))
    (list 1 2 3)
    "flatten already-flat list of integers")

(is (flatten
     (list "one" "two"
           (list "three" (list "four" (list "five" "six") "seven") "eight")))
    (list "one" "two" "three" "four" "five" "six" "seven" "eight")
    "flatten nested list of strings")

(let ((reference-time (unix-time-to-universal-time 1689698048)))
  (is (timestamp-string :universal-time reference-time)
      "2023-07-18T16:34:08"
      "timestamp-string with reference time (in utc, default format)")
  (is (timestamp-string :universal-time (- reference-time (* 7 3600))
                        :timezone -7)
      "2023-07-18T16:34:08"
      "timestamp-string with reference time (in pst, default format)")
  (is (timestamp-string :universal-time reference-time
                        :format "%Y-%M-%D")
      "2023-07-18"
      "timestamp-string with reference time (%Y-%M-%D)")
  (is (timestamp-string :universal-time reference-time
                        :format "%h:%m:%s")
      "16:34:08"
      "timestamp-string with reference time (%h:%m:%s)")
  (is (timestamp-string :universal-time reference-time
                        :format "%Y%s")
      "202308"
      "timestamp-string with reference time (%Y%s)")
  (is (timestamp-string :universal-time reference-time
                        :format "Year=%Y; Month=%M; Day=%D")
      "Year=2023; Month=07; Day=18"
      "timestamp-string with reference time (Year=%Y; Month=%M; Day=%D)"))

;; join-paths tests
(is (join-paths nil) "" "join-paths with nil")
(is (join-paths nil "a" "b" "c") "a/b/c" "join-paths with nil and strings")
(is (join-paths "/" nil "a" "b" "c") "/a/b/c"
    "join-paths with '/', nil, and strings")
(is (join-paths "a") "a" "join-paths with single string")
(is (join-paths "/a") "/a" "join-paths with single string that starts with /")
(is (join-paths "/") "/" "join-paths with single /")
(is (join-paths "a" "b" "c") "a/b/c" "join-paths with multiple strings")
(is (join-paths "/a" "b" "c") "/a/b/c"
    "join-paths with multiple strings starting with /a")
(is (join-paths "/" "a" "b" "c") "/a/b/c"
    "join-paths with multiple strings starting with /")
(is (join-paths "a" "b" "c" nil) "a/b/c"
    "join-paths with multiple strings and nil")
(is (join-paths "a" "b" "c" "") "a/b/c"
    "join-paths with multiple strings ending in empty string")
(is (join-paths "a" "b" "c" "/") "a/b/c"
    "join-paths with multiple strings ending in /")
(is (join-paths "" "a" "b" "c") "a/b/c"
    "join-paths with multiple strings starting with empty string")
(is (join-paths "a" "" "b" "c") "a/b/c"
    "join-paths with multiple strings containing empty string")
(is (join-paths "/" "a" 1 "b") "/a/1/b" "join-paths with one non-string")
(is (join-paths "/a" 1 "b") "/a/1/b" "join-paths with one non-string")
(is (join-paths 1 2 3) "1/2/3" "join-paths with all non-string")

;; path-only
(is-error (path-only nil) 'error "path-only with nil")
(is (path-only "") "./" "path-only with empty string")
(is (path-only "/") "./" "path-only with /")
(is (path-only "/a/b/c/file.txt") "/a/b/c"
    "path-only with /a/b/c/file.txt")
(is (path-only "a/b/c/file.txt") "a/b/c"
    "path-only with a/b/c/file.txt")
(is (path-only "file.txt") "./" "path-only with file.txt")

;; filename-only
(is (filename-only nil) "" "filename-only with nil")
(is (filename-only "/") "" "filename-only with /")
(is (filename-only "/a/b/c/file.txt") "file.txt"
    "filename-only with /a/b/c/file.txt")
(is (filename-only "/file.txt") "file.txt"
    "filename-only with /file.txt")
(is (filename-only "file.txt") "file.txt"
    "filename-only with file.txt")
(is (filename-only "") "" "filename-only with empty string")
(is (filename-only "file.txt/") "" "filename-only with file.txt/")

;; sorted-hash representation
(is (comparable-hash-dump
     (ds '(:map :one 1 :two 2 :three 3 :four 4 :five 5)))
    '((:five 5) (:four 4) (:one 1) (:three 3) (:two 2))
    "comparable-hash-dump with keyword keys")
(is (comparable-hash-dump
     (ds '(:map :one 1 :two 2 :three 3 :four 4 :five 5))
     :f-sort #'string>)
    (reverse '((:five 5) (:four 4) (:one 1) (:three 3) (:two 2)))
    "comparable-hash-dump with keyword keys, descending")
(is (comparable-hash-dump
     (ds '(:map  5 1   3 2   4 3   1 4   2 5)))
    '((1 4) (2 5) (3 2) (4 3) (5 1))
    "comparable-hash-dump with integer keys")
(is (comparable-hash-dump
     (ds '(:map  5 1   3 2   4 3   1 4   2 5))
     :f-sort #'<
     :f-make-sortable #'identity)
    '((1 4) (2 5) (3 2) (4 3) (5 1))
    "comparable-hash-dump with integer keys, numeric sort")
(is (comparable-hash-dump
     (ds '(:map  5 1   3 2   4 3   1 4   2 5))
     :f-sort #'>
     :f-make-sortable #'identity)
    (reverse '((1 4) (2 5) (3 2) (4 3) (5 1)))
    "comparable-hash-dump with integer keys, descending numeric sort")

;; hashify-list
(is (comparable-hash-dump
     (hashify-list (list :one :two :three :two :one))
     :flat t)
    '(:one 2 :three 1 :two 2)
    "hashify-list with keyword keys")
(is (comparable-hash-dump
     (hashify-list '(1 2 3 2 1))
     :flat t
     :f-sort #'<
     :f-make-sortable #'identity)
    '(1 2 2 2 3 1)
    "hashify-list with integer keys and numeric sort")
(is (comparable-hash-dump
     (hashify-list '("one" "two" "three") :method :index))
    '(("one" 0) ("three" 2) ("two" 1))
    "hashify-list index method")
(is (comparable-hash-dump
     (hashify-list '((:id "a" :first "Alice" :last "Adams" :age 100)
                     (:id "b" :first "Bob" :last "Baker" :age 101)
                     (:id "c" :first "Claire" :last "Clark" :age 102))
                   :method :index
                   :plist-key :id))
    '(("a" (:id "a" :first "Alice" :last "Adams" :age 100))
      ("b" (:id "b" :first "Bob" :last "Baker" :age 101))
      ("c" (:id "c" :first "Claire" :last "Clark" :age 102)))
    "hashify-list index method plist-key")
(is (comparable-hash-dump
     (hashify-list '(:one 1 :two 2 :three 3 :one 11)
                   :method :plist)
     :flat t)
    '(:one 11 :three 3 :two 2)
    "hashify-list plist method")
(is (comparable-hash-dump
     (hashify-list '((:one 1) (:two 2) (:three 3))
                   :method :alist)
     :flat t)
    '(:one 1 :three 3 :two 2)
    "hashify-list alist method")
(is (comparable-hash-dump
     (hashify-list '(:one 1 :two 2 :three 3)
                   :method :plist
                   :f-key (lambda (k) (format nil "~a" k)))
     :flat t)
    '("ONE" 1 "THREE" 3 "TWO" 2)
    "hashify-list plist method, stringified keys")
(is (comparable-hash-dump
     (hashify-list '(:one 1 :two 2 :three 3)
                   :method :plist
                   :f-value (lambda (r c v)
                              (declare (ignore r c))
                              (format nil "~a" v)))
     :flat t)
    '(:one "1" :three "3" :two "2")
    "hashify-list plist method, strigified values")
(is (comparable-hash-dump
     (hashify-list '((:a 11 :b 12 :c 13)
                     (:a 21 :b 22 :c 23)
                     (:a 31 :b 32 :c 33)
                     (:a 41 :b 12 :c 43))
                   :method :count
                   :plist-key :b))
    '(((:a 21 :b 22 :c 23) 1) ((:a 31 :b 32 :c 33) 1) ((:a 41 :b 12 :c 43) 2))
    "hashify-list list of plists, method count, specifying plist-key")

;; hash-string
(is (hash-string "Donnie")
    (format nil "~{~a~}"
            '("2daf3ad277939e55520a61187c73abc3"
              "f09b8759ce41469d67ec7fd8f6930c6c"
              "e943809d1c37312838cef4b40665aa2e"
              "02803cc0a206c97bb8a476b1d681ca95"))
    "hash-string")
(is (hash-string "Donnie" :size 32) 
  "2daf3ad277939e55520a61187c73abc3"
  "hash-string truncated to 32 characters")
(is (hash-string "Donnie" :salt "abc" :size 16)
  "f524abffea9ae753"
  "hash-string with salt and truncated to 16 characters")
  
(is (hash-hmac-256 "secret" "Donnie")
    "b81c15aafc9935e7138b5c09fc775e66275739370493c06051fea29f5cc6c32a"
    "hash-hmac-256")


;; distinct-elements
(is (sort (distinct-elements '(1 2 3 4 3 2 1)) #'<)
    '(1 2 3 4)
    "distinct-elements integers")
(is (sort (distinct-elements '("abc" "def" "ghi" "jkl" "abc" "def" "ghi" "jkl"))
          #'string<)
    '("abc" "def" "ghi" "jkl")
    "distinct-elements with strings")
(is (sort
     (mapcar (lambda (x) (format nil "~a" x))
             (distinct-elements '("one" 1 "two" 2 "three" 3 "two" 2 "one" 1)))
     #'string<)
    '("1" "2" "3" "one" "three" "two")
    "distinct-elements with mixed types")
(is (sort (distinct-elements '((:a 1 :b 2 :c 3)
                               (:a 4 :b 5 :c 6)
                               (:a 7 :b 8 :c 9)
                               (:a 1 :b 2 :c 3)
                               (:a 4 :b 5 :c 6)
                               (:a 7 :b 8 :c 9))
                             :key :b)
          #'< :key (lambda (x) (getf x :b)))
    '((:a 1 :b 2 :c 3)
      (:a 4 :b 5 :c 6)
      (:a 7 :b 8 :c 9))
    "distinct-elements for list with key")
(is (distinct-elements "donnie") "donie" "distinct-elements string")

;; hash-values and hash-keys
(let ((h (ds '(:map :a 1 :b 2 :c 3))))
  (is (sort (hash-values h) #'<)
      '(1 2 3)
      "hash-values")
  (is (sort (hash-keys h) #'string<)
      '(:a :b :c)
      "hash-keys"))

;; shuffle
(let* ((original-list (range 1 100))
       (original-vector (apply #'vector (range 1 100)))
       (shuffled-list (shuffle original-list))
       (shuffled-vector (shuffle original-vector)))
  (isnt original-list shuffled-list
        "shuffled <> original")
  (is (length original-list) (length shuffled-list)
      "original and shuffled same length")
  (is (comparable-hash-dump (hashify-list original-list))
      (comparable-hash-dump (hashify-list shuffled-list))
      "original and shuffled same elements, same count per element")
  (is (comparable-hash-dump
       (hashify-list (map 'list 'identity original-vector)))
      (comparable-hash-dump
       (hashify-list (map 'list 'identity shuffled-vector)))
      "original and shuffled vectors same elements, same count per element"))

;; range
(is (range 1 3) '(1 2 3) "range 1-3")
(is (range 1 5 :step 2) '(1 3 5) "range 1-5 by 2")
(is (range 1 5 :filter #'evenp) '(2 4) "range 1-5 even")
(is (loop for a from 1 to 100
          for range = (range 1 10 :shuffle t)
          collect (car range) into heads
          finally (return
                    (loop with ref = (car heads)
                          for head in heads thereis (not (= head ref)))))
    t
    "range shuffled")

;; permutations
(is (all-permutations '(1 2 3))
    '((1 2 3)
      (1 3 2)
      (2 1 3)
      (2 3 1)
      (3 1 2)
      (3 2 1))
    "all-permutations distinct integers")
(is (all-permutations-of-string "one")
    '("one" "oen" "noe" "neo" "eon" "eno")
    "all-permutations-of-string")
(is (existing-permutations-of-string
     "one" (ds '(:map "one" t "neo" t "eon" t)))
    '("one" "neo" "eon")
    "existing-permutations-of-string")
(is (n-gram-strings "ab" 3)
    '("aaa" "aab" "aba" "abb" "baa" "bab" "bba" "bbb")
    "n-gram-strings")
(is (existing-n-gram-strings
     "abc" 3 (ds '(:map "aba" t "bbc" t "cab" t "hello" t "one" t "two" t)))
    '("aba" "bbc" "cab")
    "existing-n-gram-strings")

;; verify-string
(is (verify-string "Donnie" "Donnie") t
    "verify-string 1")
(is (verify-string "Donnie" "^D.+e$") t
    "verify-string 2")
(is (verify-string "Donnie" "^Do") nil
    "verify-string 3")
(is (verify-string "Donnie" "e$") nil
    "verify-string 4")
(is (verify-string "Donnie" "^d.+") nil
    "verify-string 5")
(is (verify-string "Donnie" "^d.+" :ignore-case t) t
    "verify-string 6")
(is (verify-string "Donnie" "^.+$") t
    "verify-string 7")

;; plistp
(ok (plistp '(:one 1 :two 2 :three 3)))
(ok (not (plistp '(:one 1 :two :three 3))))

;; file-exists-p and directory-exists-p
(let* ((root "/tmp/dc-eclectic-tests/")
       (file-name-1 (format nil "~afile-1.txt" root))
       (file-name-2 (format nil "~afile-2.txt" root))
       (dir-name-1 (format nil "~adir-1/" root))
       (dir-name-2 (format nil "~adir-2/" root)))
  (when (directory-exists-p root)
    (uiop:delete-directory-tree (pathname root) :validate t))
  (ensure-directories-exist dir-name-1)
  (with-open-file (out file-name-1 :direction :output :if-exists :supersede)
    (write-line "Hello World!" out))
  (ok (file-exists-p file-name-1)
      (format nil "file exists: ~a" file-name-1))
  (ok (not (file-exists-p file-name-2))
      (format nil "file does not exists: ~a" file-name-2))
  (ok (not (file-exists-p dir-name-1))
      (format nil "file does not exist (it's a directory): ~a" dir-name-1))
  (ok (not (file-exists-p dir-name-2))
      (format nil "file does not exist: ~a" dir-name-2))
  (ok (directory-exists-p dir-name-1)
      (format nil "directory exists: ~a" dir-name-1))
  (ok (not (directory-exists-p dir-name-2))
      (format nil "directory does not exist: ~a" dir-name-2))
  (ok (not (directory-exists-p file-name-1))
      (format nil "file exists, but it's not a directory: ~a" file-name-1))
  (ok (not (directory-exists-p file-name-2))
      (format nil "file does not exist as is not a directory: ~a" file-name-2))
  (uiop:delete-directory-tree (pathname root) :validate t))

;; index-of-max
(ok (null (index-of-max nil)) "index-of-max nil")
(ok (null (index-of-max (vector))) "index-of-max empty vector")
(ok (zerop (index-of-max '(0))) "index-of-max single element")
(ok (zerop (index-of-max (vector 0))) "index-of-max single element vector")
(is (index-of-max '(0 1)) 1 "index-of-max two elements")
(is (index-of-max (vector 0 1)) 1 "index-of-max two elements vector")
(ok (zerop (index-of-max '(1 0))) "index-of-max two elements reverse")
(ok (zerop (index-of-max (vector 1 0)))
    "index-of-max two elements reverse vector")
(is (index-of-max '(-1 1 0)) 1 "index-of-max three elements")
(is (index-of-max (vector -1 1 0)) 1 "index-of-max three elements vector")
(ok (zerop (index-of-max '(-1 -2 -3 -9))) "index-of-max all negative")
(ok (zerop (index-of-max (vector -1 -2 -3 -9)))
    "index-of-max all negative vector")
(is (index-of-max '(-1 -2 -3 0)) 3 "index-of-max one zero")
(is (index-of-max (vector -1 -2 -3 0)) 3 "index-of-max one zero vector")
(is (index-of-max '(-9 -2 -3 -1)) 3 "index-of-max all negative reverse")
(is (index-of-max (vector -9 -2 -3 -1)) 3
    "index-of-max all negative reverse vector")

;; choose-one
(let* ((list '(1 2 3 4 5))
       (rstate (make-random-state (reference-random-state)))
       (a (choose-one list rstate)))
  (ok (loop for a from 1 to 10
            for choice = (choose-one list)
            always (and (= (truncate choice) choice)
                        (>= choice 1)
                        (<= choice 5)))
      "choose-one always returns an element of seq")
  (is 1 a "first call to choose-one with rstate returns same element")
  (ok (null (choose-one nil)) "choose-one of nil is nil")
  (ok (loop for a from 1 to 10
            always (= (choose-one (list a)) a))
      "choose-one of a single-value list is always the single value"))

;; choose-some
(ok (loop with list = '(1 2 3 4 5)
          for a from 1 to 20
          for choice = (choose-some list 1)
          always (and (= (length choice) 1)
                      (>= (car choice) 1)
                      (<= (car choice) 5)))
    "choose-some seq 1 always returns a single value from the sequence")
(is (choose-some '(1 2 3 4 5) 5) '(1 2 3 4 5)
    "choose-some seq (length seq) always returns a copy of the sequence.")
(let ((some (sort (choose-some '(1 2 3 4 5) 4) #'<)))
  (is (sort (distinct-elements some) #'<) some
      "choose-some returns elements from SEQ that have distinct indexes")
  (ok (loop for a in some always (and (>= a 1) (<= a 5)))
      "choose-some returns elements from SEQ")
  (ok (equal 4 (length some))
      "choose-some returns the correct number of elements"))
(ok (null (choose-some '(1 2 3 4 5) 0))
    "choose-some returns nil if N = 0")
(ok (null (choose-some '(1 2 3 4 5) -1))
    "choose-some returns nil if N < 0")
(ok (null (choose-some nil 2))
    "choose-some returns nil if SEQ is empty")
(let ((some (sort (choose-some (vector 1 2 3) 2) #'<)))
  (ok (or (equalp some (vector 1 2))
          (equalp some (vector 2 3))
          (equalp some (vector 1 3)))
      "choose-some works with vectors"))


;; log-entry
(let ((timestamp (top-of-second)))
  (is (format nil "~a ~a" timestamp "Hello World")
      (log-entry "Hello World")
      "entry - format string only")
  (is (format nil "~a ~a" timestamp "Hello 10 times, Donnie")
      (log-entry "Hello ~d times, ~a" 10 "Donnie")
      "entry - format string with 2 parameters"))

;; log-it
(let* ((timestamp (top-of-second))
       (filepath "/tmp/neurons.log")
       (stream (open filepath
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)))
  (log-it stream "~a ~a ~a" "one" "two" "three")
  (log-it stream "Hello World!")
  (log-it stream "~{~d~^, ~}" (list 1 2 3 4 5))
  (close stream)
  (let ((log (uiop:read-file-lines "/tmp/neurons.log")))
    (is (mapcar (lambda (s) (format nil "~a ~a" timestamp s))
                (list "one two three"
                      "Hello World!"
                      "1, 2, 3, 4, 5"))
        log
        (format nil "log-it to ~a" filepath))))

;; spew and slurp
(let* ((string "hello world")
        (filename "/tmp/hello.txt"))
  (spew string filename)
  (ok (uiop:file-exists-p filename) "spew created file")
  (is (slurp filename) string "slurp returns spewed string")
  (delete-file filename))

;; freeze and thaw
(let* ((original (list 1 "one" :one t nil '(2 3 4)))
        (frozen (freeze original))
        (thawed (thaw frozen)))
  (is (length original) (length thawed) "Thawed list has correct length")
  (is original thawed "Thawed list is the same as original list"))

;; freeze and spew, then slurp and and thaw
(let* ((filename "/tmp/hello.txt")
        (original (list "hello" :world)))
  (spew (freeze original) filename)
  (let ((thawed (thaw (slurp filename))))
    (is thawed original)
    (is (car thawed) "hello" "First element of thawed list is \"hello\"")
    (is (second thawed) :world "Second element of thawed list is :world")))

;; split-n-trim
(let* ((s1 "one two three")
        (s2 "  one     two three   ")
        (s3 "123one456 789two10 1112three14   ")
        (p1-actual (split-n-trim s1))
        (p2-actual (split-n-trim s2))
        (p3-actual (split-n-trim s3 :fat "\\d+"))
        (expected (list "one" "two" "three")))
  (is p1-actual expected "split-n-trim words separated by single space")
  (is p2-actual expected "split-n-trim words separated by multiple spaces")
  (is p3-actual expected 
    "split-n-trim words separated by spaces, trimming digits from each word"))

;; trim
(is "hello" (trim "      hello       ") "trim spaces around a string")
(is "hello" (trim "01234hello67890" "\\d+") "trim digits around a string")
(is "hello" (trim "	 	hello 	") "trim whitespace")

;; All Done!
(finalize)
