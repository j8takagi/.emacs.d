sitelisp-dir ?= ~/.emacs.d/site-lisp

EMACS := emacs
RSYNC := rsync

SITELISPS := $(shell sh -c "ls -F | grep /")

.PHONY: all install clean

all: subdirs.elc
	for dir in $(SITELISPS); do $(MAKE) -C $$dir all; done

install:
	$(RSYNC) -az --exclude "Makefile*" --delete ./ $(sitelisp-dir)/

clean:
	for dir in $(SITELISPS); do $(MAKE) -C $$dir clean; done 

COMPILE.el := $(EMACS) -batch -script ~/.emacs.d/init.el -f batch-byte-compile

%.elc: %.el
	$(COMPILE.el) $<
