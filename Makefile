INSTALL = install
RSYNC := rsync
RSYNCFLAG := -avz --delete
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

all: init site-lisp

init: $(addsuffix c,$(init-files))

site-lisp:
	$(MAKE) -C site-lisp

install: install-init install-site-lisp

install-init:
	$(INSTALL) -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) $(init-files) $(addsuffix c,$(init-files)) $(init-dir)/

install-site-lisp:
	$(MAKE) -C site-lisp install

get-elpa:
	$(RSYNC) $(RSYNCFLAG) $(init-dir)/elpa/* elpa/

install-elpa:
	$(RSYNC) $(RSYNCFLAG) elpa/* $(init-dir)/elpa/

clean: clean-init
	$(MAKE) -C site-lisp clean

clean-init:
	$(RM) *.elc

include emacslisp.mk
