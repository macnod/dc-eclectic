(defpackage :dc-eclectic
  (:use :cl :cl-ppcre :trivial-utf-8 :sb-thread :sb-ext)
  (:import-from :ironclad
   :ascii-string-to-byte-array
                :byte-array-to-hex-string
   :digest-sequence
                :sha512)
  (:export
   distinct-elements
   distinct-values
   ds
   ds-clone
   ds-from-json
   ds-get
   ds-keys
   ds-list
   ds-merge
   ds-set
   ds-to-json
   ds-type
   filename-only
   flatten
   get-unix-time
   hash-hmac-256
   hash-string
   join-paths
   log-entry
   log-entries
   path-only
   range
   run-tests
   shuffle
   timestamp-string
   to-ascii
   universal-time-to-unix-time
   unix-time-to-universal-time
   write-log-entry
   ))
