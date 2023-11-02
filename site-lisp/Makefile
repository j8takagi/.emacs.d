SITELISPS := $(shell sh -c "ls -F | grep /")
INSTALL-SITELISP := ~/.emacs.d/site-lisp

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*"

SED := sed
PRINTF := printf

.PHONY: all install clean distclean

all:
	@($(foreach dir,$(SITELISPS),$(MAKE) -sC $(dir) all; ))

install: all
	@($(foreach dir,$(SITELISPS),$(PRINTF) $(dir) && $(MAKE) -sC $(dir) install; ))

clean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) clean; )

distclean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) distclean; )
