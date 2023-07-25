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

(plan 50)

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

(let ((timestamp (loop ;; this gives us a whole second with the same timestamp
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
      (format nil "~{~a ~a~%~}" (loop for m in (list "one" "two" "three" "four" "five" "six")
                                      append (list timestamp m)))
      "log-entries with multiple strings in a deeply-nested list"))

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

(finalize)
