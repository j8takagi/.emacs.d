PACKAGENAME := view-mode-vi-bindings

EMACS := emacs
COMPILE.el := $(EMACS) -batch -l ../../set-load-path.el -f batch-byte-compile

ELFILES := $(wildcard *.el)
ELCFILES := $(addsuffix c,$(ELFILES))

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*"
GREPV := grep -v
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)\|\(./\)"
ECHO := echo

INSTALL-SITELISP := ~/.emacs.d/site-lisp

.PHONY: all install clean

all: $(ELCFILES)

$(INSTALL-SITELISP)/$(PACKAGENAME):
	(MKDIR) $@

install: $(INSTALL-SITELISP) all
	@($(ECHO) "$(PACKAGENAME) -> $(INSTALL-SITELISP)/$(PACKAGENAME)"; $(RSYNC) $(RSYNCFLAG) $(RSYNCEXCLUDE) ./ $(INSTALL-SITELISP)/$(PACKAGENAME)/ | $(CLEAN.rsync))

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)

distclean: clean