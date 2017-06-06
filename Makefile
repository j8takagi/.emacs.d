CP := cp -v
ECHO := echo
EMACS := emacs
GREPV := grep -v
INSTALL = install
MKDIR := mkdir
TEST := test
RMR := $(RM) -R
RSYNC := rsync
RSYNCFLAG := -avz --delete

COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
INSTALLDIR := ~/.emacs.d

.PHONY: all init site-lisp install install-init.sys.d install-site-lisp check test

all: init init.sys.d insert install-site-lisp

init: init.elc

init.sys.d:
	$(MAKE) -C $@

insert:
	$(MAKE) -C $@

site-lisp:
	$(MAKE) -sC $@

get-abbrev:
	$(RSYNC) $(RSYNCFLAG) $(INSTALLDIR)/abbrev_defs ./

install: install-init install-init.sys.d install-site-lisp install-insert

install-init: $(INSTALLDIR) $(INSTALLDIR)/init.el~ init
	@$(RSYNC) $(RSYNCFLAG) init.el init.elc $(INSTALLDIR)/ | $(CLEAN.rsync)

$(INSTALLDIR)/init.el~: $(INSTALLDIR)/init.el
	@$(CP) $< $@

install-init.sys.d: init.sys.d $(INSTALLDIR)/init.sys.d
	@$(MAKE) -sC init.sys.d install

$(INSTALLDIR)/init.sys.d: $(INSTALLDIR)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

install-site-lisp: site-lisp $(INSTALLDIR)/site-lisp
	@$(MAKE) -sC site-lisp install

$(INSTALLDIR)/site-lisp: $(INSTALLDIR)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

install-insert: insert $(INSTALLDIR)/insert
	@$(MAKE) -sC insert install

$(INSTALLDIR)/insert: $(INSTALLDIR)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

$(INSTALLDIR):
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

%.elc: %.el
	$(COMPILE.el) $<

check: test

test:
	$(MAKE) -sC test

distclean: clean

clean: init-clean init.sys.d-clean insert-clean site-lisp-clean

init-clean:
	$(RM) *.elc

init.d-clean:
	$(MAKE) -C init.d clean

init.sys.d-clean:
	$(MAKE) -C init.sys.d clean

insert-clean:
	$(MAKE) -C insert clean

site-lisp-clean:
	$(MAKE) -C site-lisp clean
