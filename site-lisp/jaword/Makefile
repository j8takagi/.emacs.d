PACKAGENAME := jaword

EMACS := emacs
COMPILE.el := $(EMACS) -batch -f batch-byte-compile

ELFILES := $(wildcard *.el)
ELCFILES := $(addsuffix c,$(ELFILES))

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*"

SED := sed

INSTALL-SITELISP := ~/.emacs.d/site-lisp

CLEAN.rsync := $(SED) -e 's%sending incremental file list%Install $(PACKAGENAME) to $(INSTALL-SITELISP)%' -e '/^sent/d' -e '/^total/d' -e 's~^\./~~' -e '/^$$/d'

.PHONY: all install clean

all: $(ELCFILES)

$(INSTALL-SITELISP)/$(PACKAGENAME):
	(MKDIR) $@

install: $(INSTALL-SITELISP) all
	@($(RSYNC) $(RSYNCFLAG) $(RSYNCEXCLUDE) ./ $(INSTALL-SITELISP)/$(PACKAGENAME)/ | $(CLEAN.rsync))

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)

distclean: clean
