sitelisp-dir ?= ~/.emacs.d/site-lisp

RSYNC := rsync

SITELISPS := $(shell sh -c "ls -F | grep /")

.PHONY: all install clean

all:
	for dir in $(SITELISPS); do $(MAKE) -C $$dir all; done

install:
	$(RSYNC) -az --exclude "Makefile*" --delete ./ $(sitelisp-dir)/

clean:
	for dir in $(SITELISPS); do $(MAKE) -C $$dir clean; done 
