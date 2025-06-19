TESTS_FILE="./dc-eclectic-tests.lisp"
LISP=/usr/bin/sbcl
REPORTER=list
QUICKLISP_URL=https://beta.quicklisp.org/quicklisp.lisp
QUICKLISP_ASC_URL=https://beta.quicklisp.org/quicklisp.lisp.asc
ROSWELL=$(HOME)/.local/bin/ros
ROSWELL_SRC_DIR=roswell

.PHONY: setup install-dependencies test

setup: install-roswell install-dependencies

install-roswell:
	# Build from source if Roswell is not present
	if ! command -v ros >/dev/null 2>&1; then \
		git clone -b release https://github.com/roswell/roswell.git $(ROSWELL_SRC_DIR); \
		cd $(ROSWELL_SRC_DIR); \
		sh bootstrap; \
		./configure --prefix=$(HOME)/.local; \
		make; \
		make install; \
		cd ..; \
		rm -rf $(ROSWELL_SRC_DIR); \
		$(ROSWELL) setup; \
	fi

install-dependencies:
	# Install dependencies
	$(ROSWELL) install cl-ppcre
	$(ROSWELL) install yason
	$(ROSWELL) install ironclad
	$(ROSWELL) install trivial-utf-8
	$(ROSWELL) install cl-csv
	$(ROSWELL) install prove
	$(ROSWELL) install macnod/dc-ds
	$(ROSWELL) install macnod/dc-dlist

test:
	$(ROSWELL) run -- --eval "(require :prove)" \
		--eval "(unless (prove:run (truename \"$(TESTS_FILE)\") :reporter :$(REPORTER)) (sb-ext:exit :code 1))" \
		--quit
