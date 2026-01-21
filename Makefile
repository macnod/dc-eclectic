ROSWELL_VERSION := v23.10.14.114
SBCL_VERSION := 2.5.10
TEST_FILE := dc-eclectic-tests.lisp
ROSWELL_BASE_URL := https://github.com/roswell/roswell/releases/download
ROSWELL_FULL_URL := $(ROSWELL_BASE_URL)/$(ROSWELL_VERSION)/roswell_$(subst v,,$(ROSWELL_VERSION))-1_amd64.deb

install-roswell:
	@if ! which ros > /dev/null 2>&1; then \
		echo "Roswell not found. Installing..."; \
		curl -L $(ROSWELL_FULL_URL) --output roswell.deb; \
		sudo dpkg -i roswell.deb; \
		ros install sbcl-bin/$(SBCL_VERSION); \
		ros use sbcl-bin/$(SBCL_VERSION); \
		echo "Roswell installation complete."; \
	else \
		echo "Roswell already installed. Skipping..."; \
	fi
	touch $@

install-dependencies:
	ros install cl-ppcre
	ros install yason
	ros install uiop
	ros install ironclad
	ros install trivial-utf-8
	ros install cl-base64
	ros install cl-csv
	ros install babel
	ros install fiveam
	ros install mgl-pax
	ros install macnod/dc-ds
	ros install macnod/dc-time
	ros install macnod/p-log

test:
	ros run -- --disable-debugger --load "$(TEST_FILE)" --quit

.PHONY: install-roswell install-dependencies test
.DEFAULT_GOAL := test
