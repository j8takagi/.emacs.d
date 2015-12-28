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

COMPILE.el := $(EMACS) -batch -l set-load-path.el -l init.el -f batch-byte-compile
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
emacs-dir := ~/.emacs.d

.PHONY: all init site-lisp install install-init.d install-init.sys.d install-site-lisp

all: init init.d init.sys.d insert install-site-lisp

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
	$(RSYNC) $(RSYNCFLAG) $(emacs-dir)/abbrev_defs ./

install: install-init install-init.d install-init.sys.d install-site-lisp install-insert

install-init: $(emacs-dir) $(emacs-dir)/init.el~ init
	@$(RSYNC) $(RSYNCFLAG) init.el init.elc $(emacs-dir)/ | $(CLEAN.rsync)

$(emacs-dir)/init.el~: $(emacs-dir)/init.el
	@$(CP) $< $@

install-init.d: init.d $(emacs-dir)/init.d
	@$(MAKE) -sC init.d install

$(emacs-dir)/init.d: $(emacs-dir)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

install-init.sys.d: init.sys.d $(emacs-dir)/init.sys.d
	@$(MAKE) -sC init.sys.d install

$(emacs-dir)/init.sys.d: $(emacs-dir)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

install-site-lisp: site-lisp $(emacs-dir)/site-lisp
	@$(MAKE) -sC site-lisp install

$(emacs-dir)/site-lisp: $(emacs-dir)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

install-insert: insert $(emacs-dir)/insert
	@$(MAKE) -sC insert install

$(emacs-dir)/insert: $(emacs-dir)
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

$(emacs-dir):
	@(if $(TEST) ! -d $@; then $(MKDIR) $@; fi)

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
