(asdf:defsystem :dc-eclectic
  :description "Functions that I use in most of my programs."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:cl-ppcre
                :yason
                :uiop
                :ironclad
                :trivial-utf-8
                :cl-base64
                :cl-csv
                :babel
                :dc-ds)
  :serial t
  :components ((:file "dc-eclectic-package")
                (:file "dc-eclectic")
                (:file "encoder")
                (:file "reference-random-state")))
