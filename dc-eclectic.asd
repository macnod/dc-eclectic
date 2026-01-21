(asdf:defsystem :dc-eclectic
  :description "Functions that I use in most of my programs."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:dc-time
                :p-log
                :cl-ppcre
                :yason
                :ironclad
                :trivial-utf-8
                :cl-base64
                :cl-csv
                :babel
                :mgl-pax
                :dc-ds)
  :serial t
  :components ((:file "dc-eclectic-package")
                (:file "dc-eclectic")
                (:file "encoder")
                (:file "reference-random-state")
                (:file "docs")
                (:file "exports")))
