ECHO := echo
EMACS := emacs
FIND := find
INSTALL = install
MKDIR := mkdir
RMR := $(RM) -R
RSYNC := rsync
RSYNCFLAG := -avz --delete

emacs-dir := ~/.emacs.d

.PHONY: all init site-lisp install install-init.d install-init.sys.d install-site-lisp

all: init init.d init.sys.d site-lisp

init: init.elc

init.d:
	$(MAKE) -C $@

init.sys.d:
	$(MAKE) -C $@

site-lisp:
	$(MAKE) -C $@

get-elpa:
	$(RSYNC) $(RSYNCFLAG) $(emacs-dir)/elpa/ elpa/

elpa:
	$(EMACS) -batch -l recompile-elpa.el

install: install-init install-init.d install-init.sys.d install-site-lisp

install-init: ~/.emacs.d init
	$(RSYNC) $(RSYNCFLAG) init.el init.elc $(emacs-dir)/

install-init.d: init.d
	$(MAKE) -C init.d install

install-init.sys.d: init.sys.d
	$(MAKE) -C init.sys.d install

install-site-lisp: ~/.emacs.d/site-lisp
	$(MAKE) -C site-lisp install

install-elpa: elpa ~/.emacs.d/elpa
	$(RSYNC) $(RSYNCFLAG) elpa/* $(emacs-dir)/elpa/

$(emacs-dir):
	$(MKDIR) $@

clean: clean-init clean-init.d clean-init.sys.d clean-site-lisp

clean-init:
	$(RM) *.elc

clean-init.d:
	$(MAKE) -C init.d clean

clean-init.sys.d:
	$(MAKE) -C init.sys.d clean

clean-site-lisp:
	$(MAKE) -C site-lisp clean

include emacslisp.mk
