SITELISPS := $(shell sh -c "ls -F | grep /")
INSTALL-SITELISP := ~/.emacs.d/site-lisp

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*"
GREPV := grep -v
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)\|\(\./\)"

.PHONY: all install clean distclean

all:
	@($(foreach dir,$(SITELISPS),$(MAKE) -sC $(dir) all; ))

install: all
	@($(RSYNC) -auvz --delete --existing --ignore-non-existing ./ $(INSTALL-SITELISP)/ | $(CLEAN.rsync))
	@($(foreach dir,$(SITELISPS),$(MAKE) -sC $(dir) install; ))

clean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) clean; )

distclean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) distclean; )
