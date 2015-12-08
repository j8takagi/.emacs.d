sitelisp-dir ?= ~/.emacs.d/site-lisp

EMACS := emacs
RSYNC := rsync
RSYNCFLAG := -avz
GREPV := grep -v

CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"

SITELISPS := $(shell sh -c "ls -F | grep /")

.PHONY: all install clean

all: subdirs.elc
	for dir in $(SITELISPS); do $(MAKE) -C $$dir all; done

install: all
	$(RSYNC) $(RSYNCFLAG) --exclude "Makefile*" --delete ./ $(sitelisp-dir)/ | $(CLEAN.rsync)

clean:
	for dir in $(SITELISPS); do $(MAKE) -C $$dir clean; done

COMPILE.el := $(EMACS) -batch -script ~/.emacs.d/init.el -f batch-byte-compile

%.elc: %.el
	$(COMPILE.el) $<
