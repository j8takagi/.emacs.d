INSTALL = install
RSYNC := rsync
RSYNCFLAG := -avz --delete
RMR := $(RM) -R
ECHO := echo
EMACS := emacs
FIND := find

KERNEL := $(shell uname)

ifeq ($(KERNEL),Linux)
  init-files := init.el init-linux.el init-x.el
endif
ifeq ($(KERNEL),Darwin)
  init-files := init.el init-mac.el
endif

emacs-dir := ~/.emacs.d

.PHONY: all init site-lisp install-init install-site-lisp

all: init site-lisp

init: $(addsuffix c,$(init-files))

site-lisp:
	$(MAKE) -C site-lisp

$(emacs-dir):
	$(INSTALL) -d $(emacs-dir)

get-elpa:
	$(RSYNC) $(RSYNCFLAG) $(emacs-dir)/elpa/* elpa/

elpa:
	$(EMACS) -batch -l recompile-elpa.el

install: install-init install-site-lisp

install-init: ~/.emacs.d
	$(RSYNC) $(RSYNCFLAG) $(init-files) $(addsuffix c,$(init-files)) $(emacs-dir)/

install-site-lisp: ~/.emacs.d
	$(MAKE) -C site-lisp install

install-elpa: elpa ~/.emacs.d
	$(RSYNC) $(RSYNCFLAG) elpa/* $(emacs-dir)/elpa/

clean: clean-init
	$(MAKE) -C site-lisp clean

clean-init:
	$(RM) *.elc

include emacslisp.mk
