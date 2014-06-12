INSTALL = install
RSYNC := rsync
RSYNCFLAG := -az --delete
EMACS := emacs
RMR := $(RM) -R
ECHO := echo
FIND := find

COMPILE.el := $(EMACS) -batch -script ~/.emacs.d/init.el -f batch-byte-compile
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

install: install-init install-site-lisp install-etc

install-init:
	install -d $(init-dir)
	$(RSYNC) $(RSYNCFLAG) $(init-files) $(addsuffix c,$(init-files)) $(init-dir)/

install-etc:
	$(RSYNC) $(RSYNCFLAG) -r etc share $(init-dir)/

install-site-lisp:
	$(MAKE) -C site-lisp install

clean: clean-init
	$(MAKE) -C site-lisp clean

clean-init:
	$(RM) *.elc

%.elc: %.el
	$(COMPILE.el) $<
