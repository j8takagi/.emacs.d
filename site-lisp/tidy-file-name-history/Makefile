PACKAGENAME := tidy-file-name-history

EMACS := emacs
COMPILE.el := $(EMACS) -batch -f batch-byte-compile

ELFILES := $(wildcard *.el)
ELCFILES := $(addsuffix c,$(ELFILES))

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*"
GREPV := grep -v
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)\|\(./\)"
ECHO := echo
MKDIR := mkdir -p
TEST := test

INSTALL-SITELISP := ~/.emacs.d/site-lisp

.PHONY: all install clean

all: $(ELCFILES)

$(INSTALL-SITELISP)/$(PACKAGENAME): $(INSTALL-SITELISP)
	@(if $(TEST) ! -e $@; then $(MKDIR) $@; fi)

install: $(INSTALL-SITELISP)/$(PACKAGENAME) all
	@($(ECHO) "Installing $(PACKAGENAME) to $(INSTALL-SITELISP)/$(PACKAGENAME)"; $(RSYNC) $(RSYNCFLAG) $(RSYNCEXCLUDE) ./ $(INSTALL-SITELISP)/$(PACKAGENAME)/ | $(CLEAN.rsync))

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)

distclean: clean
