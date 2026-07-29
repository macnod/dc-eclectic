(asdf:defsystem :dc-eclectic
  :description "Functions that I use in most of my programs."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:babel
                :cl-base64
                :cl-csv
                :cl-ppcre
                :dc-dlist
                :dc-ds
                :dc-time
                :ironclad
                :mgl-pax
                :p-log
                :trivial-utf-8
                :yason)
  :serial t
  :components ((:file "dc-eclectic-package")
                (:file "dc-eclectic")
                (:file "encoder")
                (:file "reference-random-state")
                (:file "docs")
                (:file "exports")))
