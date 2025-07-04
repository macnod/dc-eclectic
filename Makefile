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
            prove \
            macnod/dc-ds \
            macnod/dc-dlist

.PHONY: all setup install-apt-packages install-roswell install-dependencies test clean

all: setup test

setup: install-apt-packages install-roswell install-dependencies

install-apt-packages: $(APT_PACKAGES)

$(APT_PACKAGES):
	@dpkg-query -l --no-pager $@ >/dev/null || sudo apt install $@ -y

install-roswell:
	if ! [ -f $(ROSWELL) ]; then \
		git clone https://github.com/roswell/roswell.git roswell; \
		cd roswell; \
		sh bootstrap; \
		./configure --prefix=$(ROSWELL_PREFIX); \
		make; \
		make install; \
		cd ..; \
		rm -rf roswell; \
		$(ROSWELL) setup; \
	fi

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
	--eval "(require :prove)" \
	--eval "(prove:run #P\"dc-eclectic-tests.lisp\" :reporter :list)" \
	--non-interactive

clean:
	rm -f *.installed
