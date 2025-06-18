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
	# Download Quicklisp installer and signature
	curl -O $(QUICKLISP_URL)
	curl -O $(QUICKLISP_ASC_URL)
	# Verify Quicklisp signature
	gpg --keyserver keys.openpgp.org --recv-keys 307965AB028B5FF7
	gpg --verify quicklisp.lisp.asc quicklisp.lisp
	# Install roswell build dependencies and build from source if not present
	if ! command -v ros >/dev/null 2>&1; then \
	  git clone -b release https://github.com/roswell/roswell.git $(ROSWELL_SRC_DIR); \
	  cd $(ROSWELL_SRC_DIR); \
	  sh bootstrap; \
	  ./configure --prefix=$(HOME)/.local; \
	  make; \
	  sudo make install; \
	  cd ..; \
	  rm -rf $(ROSWELL_SRC_DIR); \
	  $(ROSWELL) setup; \
	fi
	# Clean up Quicklisp installer files
	rm -f quicklisp.lisp quicklisp.lisp.asc

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
