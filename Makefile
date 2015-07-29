CP := cp
ECHO := echo
EMACS := emacs
GREPV := grep -v
INSTALL = install
MKDIR := mkdir
RMR := $(RM) -R
RSYNC := rsync
RSYNCFLAG := -avz --delete

COMPILE.el := $(EMACS) -batch -l set-load-path.el -l init.el -f batch-byte-compile
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
emacs-dir := ~/.emacs.d

.PHONY: all init site-lisp install install-init.d install-init.sys.d install-site-lisp

all: init init.d init.sys.d insert site-lisp

init: init.elc

init.d:
	$(MAKE) -C $@

init.sys.d:
	$(MAKE) -C $@

insert:
	$(MAKE) -C $@

site-lisp:
	$(MAKE) -sC $@

get-abbrev:
	$(RSYNC) $(RSYNCFLAG) $(emacs-dir)/.abbrev_defs ./

install: install-init install-abbrev install-init.d install-init.sys.d install-site-lisp install-insert

install-init: $(emacs-dir) $(emacs-dir)/init.el~ init
	$(RSYNC) $(RSYNCFLAG) init.el init.elc $(emacs-dir)/ | $(CLEAN.rsync)

install-abbrev: $(emacs-dir)
	$(RSYNC) $(RSYNCFLAG) .abbrev_defs $(emacs-dir)/ | $(CLEAN.rsync)

$(emacs-dir)/init.el~: $(emacs-dir)/init.el
	$(CP) $< $@

install-init.d: init.d
	$(MAKE) -C init.d install

install-init.sys.d: init.sys.d
	$(MAKE) -C init.sys.d install

install-site-lisp: $(emacs-dir)/site-lisp
	$(MAKE) -C site-lisp install

install-insert:
	$(MAKE) -C insert install

$(emacs-dir):
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean: clean-init clean-init.d clean-init.sys.d clean-insert clean-site-lisp

clean-init:
	$(RM) *.elc

clean-init.d:
	$(MAKE) -C init.d clean

clean-init.sys.d:
	$(MAKE) -C init.sys.d clean

clean-insert:
	$(MAKE) -C insert clean

clean-site-lisp:
	$(MAKE) -C site-lisp clean
