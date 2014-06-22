INSTALL = install
RSYNC := rsync
RSYNCFLAG := -az --delete
RMR := $(RM) -R
ECHO := echo
FIND := find

KERNEL := $(shell uname)

ifeq ($(KERNEL),Linux)
  init-files := init.el init-linux.el init-x.el
endif
ifeq ($(KERNEL),Darwin)
  init-files := init.el init-mac.el
endif

init-dir := ~/.emacs.d

.PHONY: all init site-lisp install-init install-site-lisp

all: init site-lisp src

init: $(addsuffix c,$(init-files))

site-lisp:
	$(MAKE) -C site-lisp

install: install-init install-site-lisp

install-init:
	$(INSTALL) -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) $(init-files) $(addsuffix c,$(init-files)) $(init-dir)/

install-site-lisp:
	$(MAKE) -C site-lisp install

clean: clean-init
	$(MAKE) -C site-lisp clean
	$(MAKE) -C src clean

clean-init:
	$(RM) *.elc
