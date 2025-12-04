TESTS_FILE="./dc-eclectic-tests.lisp"
REPORTER=list
ROSWELL_PREFIX=$(HOME)/.local
ROSWELL=$(ROSWELL_PREFIX)/bin/ros
ROSWELL_SRC_DIR=roswell
ROSWELL_REPO=https://github.com/roswell/roswell.git

.PHONY: setup install-dependencies test

setup: install-roswell install-dependencies

install-roswell:
	curl -L https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb
	sudo dpkg -i roswell.deb
	$(ROSWELL) install sbcl/2.5.10
	$(ROSWELL) use sbcl/2.5.10

install-dependencies:
	$(ROSWELL) install cl-ppcre
	$(ROSWELL) install yason
	$(ROSWELL) install ironclad
	$(ROSWELL) install trivial-utf-8
	$(ROSWELL) install cl-csv
	$(ROSWELL) install babel
	$(ROSWELL) install cl-base64
	$(ROSWELL) install fiveam
	$(ROSWELL) install macnod/dc-ds
	$(ROSWELL) install macnod/dc-dlist

test:
	$(ROSWELL) run -- \
	--eval "(asdf:load-system :fiveam)" \
	--eval "(load #P\"dc-eclectic-tests.lisp\")" \
	--eval "(setf fiveam:*test-dribble* t)" \
	--eval "(sb-ext:unlock-package :it.bese.fiveam)" \
	--eval "(progn (let ((results (fiveam:run!))) (if (or (null results) (and (not (eq results t)) (> (slot-value results 'it.bese.fiveam::failed) 0))) (sb-ext:exit :code 1) (sb-ext:exit :code 0))))" \
	--quit
