ROSWELL_PREFIX=$(HOME)/.local
ROSWELL=$(ROSWELL_PREFIX)/bin/ros
INSTALLED_SYSTEMS=$(HOME)/.roswell/lisp/quicklisp/dists/quicklisp/software
INSTALLED_LOCAL=$(HOME)/.roswell/local-projects

APT_PACKAGES=automake \
             build-essential \
             curl \
             git \
             gnupg \
             libcurl4-openssl-dev \
             zlib1g-dev

CL_PACKAGES=cl-ppcre \
            yason \
            ironclad \
            trivial-utf-8 \
            cl-csv \
            fiveam \
            cl-base64 \
            babel \
            macnod/dc-ds \
            macnod/dc-dlist

.PHONY: all setup install-apt-packages install-roswell install-dependencies test clean

all: setup test

setup: install-apt-packages install-roswell install-dependencies

install-apt-packages: $(APT_PACKAGES)

$(APT_PACKAGES):
	@dpkg-query -l --no-pager $@ >/dev/null || sudo apt install $@ -y

install-roswell:
	curl -L https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb
	sudo dpkg -i roswell.deb
	# if ! [ -f $(ROSWELL) ]; then \
	# 	git clone https://github.com/roswell/roswell.git roswell; \
	# 	cd roswell; \
	# 	sh bootstrap; \
	# 	./configure --prefix=$(ROSWELL_PREFIX); \
	# 	make; \
	# 	make install; \
	# 	cd ..; \
	# 	rm -rf roswell; \
	# 	$(ROSWELL) setup; \
	# fi

install-dependencies: $(CL_PACKAGES)

$(CL_PACKAGES):
	@if ! [ -f "$(echo $@ | tr "/" "-").installed" ]; then \
		$(ROSWELL) install $@; \
		echo "Writing $(echo $@ | tr '/' '-').installed"; \
		touch "$(echo $@ | tr '/' '-').installed"; \
	else \
		echo "$@ is already installed"; \
	fi

test:
	$(ROSWELL) run -- \
	--eval "(asdf:load-system :fiveam)" \
	--eval "(load #P\"dc-eclectic-tests.lisp\")" \
	--eval "(setf fiveam:*test-dribble* t)" \
	--eval "(sb-ext:unlock-package :it.bese.fiveam)" \
	--eval "(progn (let ((results (fiveam:run!))) (if (or (null results) (and (not (eq results t)) (> (slot-value results 'it.bese.fiveam::failed) 0))) (sb-ext:exit :code 1) (sb-ext:exit :code 0))))" \
	--quit

clean:
	rm -f *.installed
