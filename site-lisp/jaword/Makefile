PACKAGENAME := jaword

EMACS := emacs
COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile

ELFILES := $(filter-out set-compile.el,$(wildcard *.el))
ELCFILES := $(addsuffix c,$(ELFILES))

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*" --exclude ".git*" --exclude "ChangeLog" --exclude "Readme*" --exclude *.gif --exclude tinysegmenter.el*

SED := sed

INSTALL-SITELISP := ~/.emacs.d/site-lisp

CLEAN.rsync := $(SED) -e 's%sending incremental file list%Install $(PACKAGENAME) to $(INSTALL-SITELISP)%' -e '/^sent/d' -e '/^total/d' -e 's~^\./~~' -e '/^$$/d'

.PHONY: all install clean

all: $(ELCFILES)

install: all
	@($(RSYNC) $(RSYNCFLAG) $(RSYNCEXCLUDE) ./ $(INSTALL-SITELISP)/$(PACKAGENAME)/ | $(CLEAN.rsync))

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)

distclean: clean
