CP := cp
ECHO := echo
EMACS := emacs
FIND := find
INSTALL = install
MKDIR := mkdir
RMR := $(RM) -R
RSYNC := rsync
RSYNCFLAG := -avz --delete

COMPILE.el := $(EMACS) -batch -l set-load-path.el -l init.el -f batch-byte-compile

emacs-dir := ~/.emacs.d

.PHONY: all init elpa site-lisp install install-init.d install-init.sys.d install-site-lisp install-elpa

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

install: install-init install-abbrev install-init.d install-init.sys.d install-site-lisp install-insert

install-init: $(emacs-dir) $(emacs-dir)/init.el~ init
	$(RSYNC) $(RSYNCFLAG) init.el init.elc $(emacs-dir)/

install-abbrev: $(emacs-dir)
	$(RSYNC) $(RSYNCFLAG) .abbrev_defs $(emacs-dir)/

$(emacs-dir)/init.el~: $(emacs-dir)/init.el
	$(CP) $< $@

install-init.d: init.d
	$(MAKE) -C init.d install

install-init.sys.d: init.sys.d
	$(MAKE) -C init.sys.d install

install-site-lisp: $(emacs-dir)/site-lisp
	$(MAKE) -C site-lisp install

install-insert: $(emacs-dir)/insert
	$(RSYNC) $(RSYNCFLAG) insert/* $(emacs-dir)/insert/

install-elpa: elpa $(emacs-dir)/elpa
	$(RSYNC) $(RSYNCFLAG) elpa/* $(emacs-dir)/elpa/

$(emacs-dir):
	$(MKDIR) $@

$(emacs-dir)/insert:
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean: clean-init clean-init.d clean-init.sys.d clean-site-lisp

clean-init:
	$(RM) *.elc

clean-init.d:
	$(MAKE) -C init.d clean

clean-init.sys.d:
	$(MAKE) -C init.sys.d clean

clean-site-lisp:
	$(MAKE) -C site-lisp clean
