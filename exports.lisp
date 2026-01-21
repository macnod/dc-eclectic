;; exports.lisp

(in-package :dc-eclectic)

(defun check-exports ()
  (let* ((public (public-symbols))
          (exported (exported-symbols))
          (docs (doc-symbols))
          (missing-in-exports (set-difference public exported))
          (stale-exports (set-difference exported public))
          (missing-in-docs (set-difference public docs))
          (stale-docs (set-difference docs public)))
    (list
      :missing-in-exports missing-in-exports
      :stale-exports stale-exports
      :missing-in-docs missing-in-docs
      :stale-docs stale-docs)))

(defun sort-symbols (list)
  (safe-sort list
    :predicate (lambda (a b)
                 (string< (format nil "~a" a) (format nil "~b" b)))))

(defun exported-symbols ()
  "Returns list of symbols exported from DC-ECLECTIC."
  (let (result)
    (do-external-symbols (s (find-package 'dc-eclectic) result) (push s result))
    (sort-symbols result)))

(defun symbol-documentation (s)
  "Returns the documentation string for symbol S."
  (or
    (documentation s 'variable)
    (documentation s 'function)))

(defun documented-symbols ()
  "Returns a list of documented symbols in DC-ECLECTIC."
  (let (result)
    (do-symbols (s (find-package 'dc-eclectic) result)
      (let ((doc (symbol-documentation s)))
        (when (and
                doc
                (or (boundp s) (find-class s nil) (fboundp s))
                (re:scan "^(\\[public\\]|\\[private\\]) " doc))
          (push s result))))
    (sort-symbols result)))

(defun public-symbols ()
  "Returns a list of the symbols in this package with a documentation string
that starts with '[public] '."
  (remove-if-not
    (lambda (s) (re:scan "^\\[public\\] " (symbol-documentation s)))
    (documented-symbols)))

(defun private-symbols ()
  "Returns a list of the symbols in this package with a documentation string
that starts with '[private] '."
  (remove-if-not
    (lambda (s) (re:scan "^\\[private\\] " (symbol-documentation s)))
    (documented-symbols)))

(defun doc-symbols ()
  (let ((lines (re:split
                 "\\n"
                 (slurp (join-paths
                          (asdf:system-relative-pathname :dc-eclectic #P"")
                          "docs.lisp")))))
    (loop for line in lines
      for symbol-line = (re:scan
                          (format nil "~{~a~^|~}"
                            (list
                              " function\\)"
                              " variable\\)"
                              " macro\\)"))
                          line)
      for symbol-string = (when symbol-line
                            (car
                              (remove-if
                                (lambda (s) (zerop (length s)))
                                (re:split "[ ()]+" line))))
      for symbol = (when symbol-string
                     (find-symbol (string-upcase symbol-string)))
      when symbol collect symbol into symbols
      finally (return (sort-symbols symbols)))))
