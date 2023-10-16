;; Run these tests in the repl with
;;   (prove:run #P"/home/macnod/common-lisp/dc-eclectic/dc-eclectic-tests.lisp")
;; or, from the shell with
;;   make test

(in-package :cl-user)
(require :dc-eclectic)
(require :prove)
(defpackage :dc-eclectic-tests (:use :cl :prove :dc-eclectic))
(in-package :dc-eclectic-tests)

(defun run-tests ()
  (prove:run #P"dc-eclectic-tests.lisp"))

(plan 188)

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

(let ((timestamp (loop ;; Get a timestamp that starts at the beginning of
                       ;; a second. This will give us a whole second to
                       ;; run the log-entry tests.
                       with reference-timestamp = (timestamp-string)
                       for timestamp = (timestamp-string)
                       while (string= timestamp reference-timestamp)
                       finally (return timestamp))))
  ;; log-entry tests
  (is (log-entry "hello world")
      (format nil "~a hello world~%" timestamp)
      "log-entry with 1 string")
  (is (log-entry "hello" "world")
      (format nil "~a helloworld~%" timestamp)
      "log-entry with 2 strings")
  (is (log-entry "hello " "beautiful " "world")
      (format nil "~a hello beautiful world~%" timestamp)
      "log-entry with 3 strings")

  ;; log-entries tests
  (is (log-entries "one" "two" "three")
      (format nil "~{~a ~a~%~}" (loop for m in (list "one" "two" "three")
                                      append (list timestamp m)))
      "log-entries with 3 strings")
  (is (log-entries (list "one" "two" "three"))
      (format nil "~{~a ~a~%~}" (loop for m in (list "one" "two" "three")
                                      append (list timestamp m)))
      "log-entries with 3 strings in a list")
  (is (log-entries (list "one" "two" (list "three" (list "four" "five")) "six"))
      (format nil "~{~a ~a~%~}"
              (loop for m in (list "one" "two" "three" "four" "five" "six")
                    append (list timestamp m)))
      "log-entries with multiple strings in a deeply-nested list")
  (is (with-output-to-string (s)
        (write-log-entry s "line 1")
        (write-log-entry s "line 2"))
      (format nil "~a line 1~%~a line 2~%" timestamp timestamp)
      "write-log-entry 2 lines"))

;; join-paths tests
(is (join-paths nil) "" "join-paths with nil")
(is (join-paths nil "a" "b" "c") "a/b/c" "join-paths with nil and strings")
(is (join-paths "/" nil "a" "b" "c") "/a/b/c"
    "join-paths with /, nil and strings")
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
(is-error (join-paths "a" 1 "b") 'error "join-paths with one non-string")
(is-error (join-paths 1 2 3) 'error "join-paths with all non-string")

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

;; ds
(is (ds nil) nil "ds with nil")
(is (ds "") "" "ds with empty string")
(is (ds 1) 1 "ds with integer")
(is (ds 1/2) 1/2 "ds with fraction")
(is (ds "hello") "hello" "ds with string")
(is (ds '(:list 1 2 3)) '(1 2 3) "ds with list of integers")
(ok (equalp (ds '(:array 1 2 3)) (vector 1 2 3)) "ds with array of integers")
(let ((list-of-maps (list (make-hash-table :test 'equal)
                          (make-hash-table :test 'equal)))
      (ds (ds '(:list (:map :a 1 :b 2) (:map :a 3 :b 4)))))
  (setf (gethash :a (first list-of-maps)) 1)
  (setf (gethash :b (first list-of-maps)) 2)
  (setf (gethash :a (second list-of-maps)) 3)
  (setf (gethash :b (second list-of-maps)) 4)
  (let ((s1 (loop for h in list-of-maps
                  collect (loop for k being the hash-keys of h
                                  using (hash-value v)
                                collect (format nil "~a=~a" k v))
                    into kv-pairs
                  finally (return (format nil "~{~a~^, ~}" kv-pairs))))
        (s2 (loop for h in ds
                  collect (loop for k being the hash-keys of h
                                  using (hash-value v)
                                collect (format nil "~a=~a" k v))
                    into kv-pairs
                  finally (return (format nil "~{~a~^, ~}" kv-pairs)))))
    (is s1 s2 "ds with list of maps")))
(let ((ds (ds '(:map :a (:list 1 2 3) :b (:list 4 5 6))))
      (map-of-lists (make-hash-table :test 'equal)))
  (setf (gethash :a map-of-lists) '(1 2 3))
  (setf (gethash :b map-of-lists) '(4 5 6))
  (is (format nil "~a" (ds-list ds))
      (format nil "~a" (ds-list map-of-lists))
      "ds with map of lists"))
(is-error (ds '(:map :a 1 :b)) 'type-error "ds with map")
(is-error (ds (list 1)) 'type-error "ds with list of single integer")
(is-error (ds '(:vector 1 2 3)) 'type-error "ds with vector of integers")

;; ds-list
(is (ds-list (ds nil)) nil "ds-list (ds nil)")
(is (ds-list (ds "")) "" "ds-list (ds empty string)")
(is (ds-list (ds 1)) 1 "ds-list (ds integer)")
(is (ds-list (ds 1/2)) 1/2 "ds-list (ds fraction)")
(is (ds-list (ds "hello")) "hello" "ds-list (ds string)")
(is (ds-list (ds '(:list 1 2 3)))
    '(:list 1 2 3)
    "ds-list (ds list of integers)")
(is (ds-list (ds '(:list (:map :a 1 :b 2) (:map :a 3 :b 4))))
    '(:list (:map :a 1 :b 2) (:map :a 3 :b 4))
    "ds-list (ds list of maps)")
(is (ds-list (ds '(:map :a (:list 1 2 3) :b (:list 4 5 6))))
    '(:map :a (:list 1 2 3) :b (:list 4 5 6))
    "ds-list (ds map of lists)")

;; ds-get
(let ((ds (ds '(:map
                :a (:list 1 2 3)
                :b (:list 4 5 (:array 6 7 8))
                :c "nine"))))
  (is (ds-get ds :c) "nine" "ds-get with :c")
  (is (ds-get ds :a 0) 1 "ds-get with :a 0")
  (is (ds-get ds :a 1) 2 "ds-get with :a 1")
  (is (ds-get ds :a 2) 3 "ds-get with :a 2")
  (is (ds-get ds :a 3) nil "ds-get with :a 3")
  (is (ds-get ds :b 0) 4 "ds-get with :b 0")
  (is (ds-get ds :b 1) 5 "ds-get with :b 1")
  (is (ds-get ds :b 3) nil "ds-get with :b 3")
  (is (ds-get ds :b 2 0) 6 "ds-get with :b 2 0")
  (is (ds-get ds :b 2 1) 7 "ds-get with :b 2 1")
  (is (ds-get ds :b 2 2) 8 "ds-get with :b 2 2")
  (is (ds-get ds :b 2 3) nil "ds-get with :b 2 3"))

;; ds-paths
(let ((ds-1 (ds '(:list
                  (:map :name "Donnie" :age 55 :phone "919-429-1371")
                  (:map :name "Tracy" :age 41 :phone "650-622-1491")
                  (:map :name "Graydon" :age 8 :phone "n/a"))))
      (ds-2 (ds '(:map
                  :donnie (:map :name "Donnie" :age 55 :phone "919-429-1371")
                  :tracy (:map :name "Tracy" :age 41 :phone "650-622-1491")
                  :graydon (:map :name "Graydon" :age 8 :phone "n/a"))))
      (ds-3 (ds '(:list 1 2 (:map :a 1 :b 2 :c (:map :three 3 :four 4 :five
                                                (:list 5 6 7)))))))
  (is (ds-paths ds-1) '((0 :name) (0 :age) (0 :phone)
                        (1 :name) (1 :age) (1 :phone)
                        (2 :name) (2 :age) (2 :phone))
      "ds-paths with list of maps")
  (is (ds-paths ds-2) '((:donnie :name) (:donnie :age) (:donnie :phone)
                        (:tracy :name) (:tracy :age) (:tracy :phone)
                        (:graydon :name) (:graydon :age) (:graydon :phone))
      "ds-paths with map of maps")
  (is (ds-paths (ds '(:list 1 2 3))) '((0) (1) (2)) "ds-paths for '(1 2 3)")
  (is (ds-paths (ds 1)) (list nil) "ds-paths for (ds 1)")
  (is (ds-paths (ds "one")) (list nil) "ds-paths for (ds \"one\")")
  (is (ds-paths nil) (list nil) "ds-paths for (ds nil)")
  (is (ds-paths 1) (list nil) "ds-paths for 1")
  (is (ds-paths "one") (list nil) "ds-paths for \"one\"")
  (is (ds-paths nil) (list nil) "ds-paths for nil")
  (is (mapcar #'car (ds-paths (ds '(:map :one 1 :two 2 :three 3))))
      (list :one :two :three)
      "ds-paths with a simple map")
  (is (ds-paths ds-3)
      '((0) (1) (2 :A) (2 :B) (2 :C :THREE) (2 :C :FOUR)
        (2 :C :FIVE 0) (2 :C :FIVE 1) (2 :C :FIVE 2))
      "ds-paths with unbalanced tree"))

;; ds-type
(is (ds-type (ds '(:map :a 1))) 'hash-table "ds-type hash-table")
(is (ds-type (ds '(:list 1 2 3))) 'sequence "ds-type sequence (list)")
(is (ds-type (ds '(:array 4 5 6))) 'sequence "ds-type sequence (array)")
(is (ds-type (ds '(:map "hello" (:list 1 2 3) "world" (:list 4 5 6))))
    'hash-table "ds-type (hash of lists)")
(is (ds-type (ds '(:list (:map :one 1 :two 2)
                   (:map :one 3 :two 4))))
    'sequence "ds-type (list of hashes)")
(ok (not (member (ds-type (ds 1)) '(hash-table sequence)))
    "ds-type (ds 1) is not a hash-table or a sequence")
(ok (not (member (ds-type (ds "")) '(hash-table sequence)
                 :test 'equal))
    "ds-type (ds \"\") is not a hash-table or a sequence")
(ok (not (member (ds-type 1) '(hash-table sequence)))
    "ds-type 1 is not a hash-table or a sequence")
(is (ds-type 1) 'bit "ds-type 1 is a bit")
(is (ds-type "abc") 'string "ds-type \"abc\" is a string")

;; ds-set
(let ((ds (ds '(:map :a 1 :b 2 :c 3
                :d (:list 4 5 (:map :six 6 :seven 7 "eight" 8
                                    :nine (:list 9 10 11)))))))
  (ds-set ds :a 5)
  (is (ds-get ds :a) 5 "ds-set :a 5")
  (ds-set ds :b 6)
  (is (ds-get ds :b) 6 "ds-set :b 6")
  (ds-set ds :c 7)
  (is (ds-get ds :c) 7 "ds-set :c 7")
  (ds-set ds '(:d 0) 4.5)
  (is (ds-get ds :d 0) 4.5 "ds-set :d 0 4.5")
  (ds-set ds '(:d 2 :seven) 7.5)
  (is (ds-get ds :d 2 :seven) 7.5 "ds-set :d 2 :seven 7.5")
  (ds-set ds '(:d 2 "eight") 8.5)
  (is (ds-get ds :d 2 "eight") 8.5 "ds-set :d 2 \"eight\" 8.5")
  (ds-set ds '(:d 2 :nine 2) 11.5)
  (is (ds-get ds :d 2 :nine 2) 11.5 "ds-set :d 2 :nine 2 11.5")
  (is (ds-list ds)
      (ds-list
       (ds '(:map :a 5 :b 6 :c 7 :d
             (:list 4.5 5
              (:map :six 6
                    :seven 7.5
               "eight" 8.5
               :nine (:list 9 10 11.5))))))
      "ds-set all changes")
  (ds-set ds '(:d 2 :nine 2) (ds '(:map :eleven 11 :twelve 12)))
  (is (ds-get ds :d 2 :nine 2 :eleven) 11 "ds-set :d 2 :nine 2 :eleven 11")
  (is (ds-get ds :d 2 :nine 2 :twelve) 12 "ds-set :d 2 :nine 2 :twelve 12")
  (ds-set ds '(:e :f :g) 1)
  (is (ds-get ds :e :f :g) 1 "ds-set atom non-existing map path 1")
  (ds-set ds '(:h 1) 2)
  (is-error (ds-get ds '(:h 1)) 'simple-error
            "ds-get keys must be integers, strings, or keywords 1")
  (is (ds-get ds :h 1) 2 "ds-set atom non-existing map/list path 2")
  (is-error (ds-set ds '(:i 1 '(:j :k :l) :m 2) 3) 'simple-error
            "ds-set keys must be integers, strings, or keywords 2")
  (ds-set ds '(:l 1 :m 2 :n) 4)
  (is (ds-get ds :l 1 :m 2 :n) 4 "ds-set atom non-existing map/list path 3"))
(let ((ds (ds '(:list 1 1 3))))
  (ds-set ds 1 2)
  (is (ds-get ds 1) 2 "ds-set 1 2")
  (ds-set ds 4 5)
  (is (ds-get ds 4) 5 "ds-set 4 5"))
(let ((ds (ds '(:array 1 1 3))))
  (ds-set ds 1 2)
  (is (ds-get ds 1) 2 "ds-set 1 2")
  (is-error (ds-set ds 3 4) 'simple-error
            "Non-list sequence extension is not supported 1")
  (is-error (ds-set ds 5 6) 'simple-error
            "Non-list sequence extension is not supported 2"))

;; ds-paths
(let ((ds (ds '(:map :a 1 :b 2 :c 3
                :d (:list 4 5 (:map :six 6 :seven 7 "eight" 8
                                    :nine (:list 9 10 11)))))))
  (is (ds-paths ds)
      '((:a) (:b) (:c) (:d 0) (:d 1) (:d 2 :six) (:d 2 :seven)
        (:d 2 "eight") (:d 2 :nine 0) (:d 2 :nine 1) (:d 2 :nine 2))
      "ds-paths with a nested data structure")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 0))) 1
      "path #1 leads to 1")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 1))) 2
      "path #2 leads to 2")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 2))) 3
      "path #3 leads to 3")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 3))) 4
      "path #4 leads to 4")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 4))) 5
      "path #5 leads to 5")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 5))) 6
      "path #6 leads to 6")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 6))) 7
      "path #7 leads to 7")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 7))) 8
      "path #8 leads to 8")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 8))) 9
      "path #9 leads to 9")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 9))) 10
      "path #10 leads to 10")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 10))) 11
      "path #11 leads to 11"))

;; ds-clone
(let* ((ds (ds '(:map :a 1 :b 2)))
       (ds-1 (ds-clone ds))
       (ds-2 '(:map :a (:map b (:map :c (:list 1 2 3))) :aa 4))
       (ds-3 (ds-clone ds-2)))
  (is (ds-list ds) (ds-list ds-1)
      "ds-clone clone is an exact copy of the original")
  (ds-set ds-1 :a 3)
  (isnt (ds-list ds) (ds-list ds-1)
        "ds-clone original doesn't change when clone changes")
  (is (ds-list ds-2) (ds-list ds-3)
      "ds-clone clone is an exact copy of the deeply-nested original"))

;; ds-merge
(let* ((ds-1 (ds '(:map :a 1 :b 2))))
  (is (ds-list (ds-merge ds-1 (ds '(:map :c 3 :d 4))))
      '(:map :a 1 :b 2 :c 3 :d 4)
      "ds-merge (:a 1 :b 2) (:c 3 :d 4)")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a 3))))
      '(:map :a 3 :b 2)
      "ds-merge (:a 1 :b 2) (:a 3)")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a 3 :b 4))))
      '(:map :a 3 :b 4)
      "ds-merge (:a 1 :b 2) (:a 3 :b 4)")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a 3 :c 5))))
      '(:map :a 3 :b 2 :c 5)
      "ds-merge (:a 1 :b 2) (:a 3 :c 5)")
  (is (ds-list (ds-merge ds-1
                         (ds '(:map :b (:list 1 2 3)))
                         (ds '(:map :b (:list 1 4 3)))))
      '(:map :a 1 :b (:list 1 4 3))
      "ds-merge (:a 1 :b 2) (:b (:list 1 2 3)) (:b (:list 1 4 3))")
  (is (ds-list (ds-merge ds-1 (ds '(:map :b (:list 1 2 3)))))
      '(:map :a 1 :b (:list 1 2 3))
      "ds-merge (:a 1 :b 2) (:b (1 2 3))")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a (:map :aa 11 :ab 12)))))
      '(:map :a (:map :aa 11 :ab 12) :b 2)
      "ds-merge (:a 1 :b 2) (:a (:map :aa 11 :ab 12))"))

;; ds-from-json
(let* ((json-1 "{\"a\":1,\"b\":2,\"c\":[1,2,3],\"d\":{\"e\":4,\"f\":5}}")
       (json-2 (format nil "{\"a\":1,\"b\":2,\"c\":[1,2,~a],\"d\":\"five\"}"
                       "{\"e\":4,\"f\":[\"one\",\"two\",\"three\"]}"))
       (ds-1 (ds-from-json json-1))
       (ds-2 (ds-from-json json-2)))
  (is (ds-list ds-1)
      '(:map "a" 1 "b" 2 "c" (:list 1 2 3) "d" (:map "e" 4 "f" 5))
      "ds-from-json map with ints, a string, a nested list, a nested map")
  (is (ds-to-json ds-1) json-1 "ds-to-json")
  (is (ds-list ds-2)
      '(:map
        "a" 1
        "b" 2
        "c" (:list 1 2 (:map
                        "e" 4
                        "f" (:list "one" "two" "three")))
        "d" "five")
      "ds-from-json with more deeply nested structures")
  (is (ds-get ds-2 "c" 2 "f" 1) "two"
      "ds-get against a deeply-nested ds-to-json structure"))

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
     (hashify-list '("one" "two" "three")
                   :method :index)
     :flat t
     :f-sort #'<
     :f-make-sortable #'identity)
    '(1 "one" 2 "two" 3 "three")
    "hashify-list index method")
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
(is (hash-hmac-256 "secret" "Donnie")
    "b81c15aafc9935e7138b5c09fc775e66275739370493c06051fea29f5cc6c32a"
    "hash-hmac-256")


;; distinct-elements
(is (sort (distinct-elements '(1 2 3 4 3 2 1)) #'<)
    '(1 2 3 4)
    "distinct-elements with integers")
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

(finalize)
