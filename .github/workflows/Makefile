TESTS_FILE="dc-eclectic-tests.lisp"
LISP=/usr/bin/sbcl
REPORTER=list
QUICKLISP_URL=https://beta.quicklisp.org/quicklisp.lisp
QUICKLISP_ASC_URL=https://beta.quicklisp.org/quicklisp.lisp.asc
QUICKLISP_DIR=$(HOME)/quicklisp
ROSWELL=/usr/local/bin/ros
ROSWELL_SRC_DIR=roswell

.PHONY: setup test

setup:
	# Download Quicklisp installer and signature
	curl -O $(QUICKLISP_URL)
	curl -O $(QUICKLISP_ASC_URL)
	# Verify Quicklisp signature
	gpg --keyserver keys.openpgp.org --recv-keys 307965AB028B5FF7
	gpg --verify quicklisp.lisp.asc quicklisp.lisp
	# Install Quicklisp
	$(LISP) --no-userinit --no-sysinit --load quicklisp.lisp \
	  --eval "(quicklisp-quickstart:install)" \
	  --eval "(ql:add-to-init-file)" \
	  --quit
	# Install roswell build dependencies and build from source if not present
	if ! command -v ros >/dev/null 2>&1; then \
	  git clone -b release https://github.com/roswell/roswell.git $(ROSWELL_SRC_DIR); \
	  cd $(ROSWELL_SRC_DIR); \
	  sh bootstrap; \
	  ./configure; \
	  make; \
	  sudo make install; \
	  cd ..; \
	  rm -rf $(ROSWELL_SRC_DIR); \
	  $(ROSWELL) setup; \
	fi
	# Install Quicklisp dependencies
	$(LISP) --no-userinit --no-sysinit \
	  --eval "(load \"$(QUICKLISP_DIR)/setup.lisp\")" \
	  --eval "(ql:quickload :cl-ppcre)" \
	  --eval "(ql:quickload :yason)" \
	  --eval "(ql:quickload :ironclad)" \
	  --eval "(ql:quickload :trivial-utf-8)" \
	  --eval "(ql:quickload :cl-csv)" \
	  --quit
	# Install dc-ds and dc-dlist using roswell
	$(ROSWELL) install macnod/dc-ds
	$(ROSWELL) install macnod/dc-dlist
	# Clean up Quicklisp installer files
	rm -f quicklisp.lisp quicklisp.lisp.asc

test: setup
	$(LISP) --no-userinit --no-sysinit \
	  --eval "(load \"$(QUICKLISP_DIR)/setup.lisp\")" \
	  --eval "(ql:quickload :prove)" \
	  --eval "(require :prove)" \
	  --eval "(prove:run #P\"$(TESTS_FILE)\" :reporter :$(REPORTER))" \
	  --quit
