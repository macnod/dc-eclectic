TESTS_FILE="$(HOME)/common-lisp/dc-eclectic/dc-eclectic-tests.lisp"
ROSWELL=/usr/bin/ros
# Reporter can be list dot tap or fiveam.
REPORTER=list

.PHONY: test

install:
	$(ROSWELL) install cl-ppcre
	$(ROSWELL) install yason
	$(ROSWELL) install ironclad
	$(ROSWELL) install trivial-utf-8
	$(ROSWELL) install cl-csv
	$(ROSWELL) install prove
	$(ROSWELL) install macnod/dc-ds
	$(ROSWELL) install macnod/dc-dlist

test:
	$(ROSWELL) run -- \
	  --eval "(require :prove)" \
	  --eval "(prove:run #P\"$(TESTS_FILE)\" :reporter :$(REPORTER))" \
	  --non-interactive
