SITELISPS := $(shell sh -c "ls -F | grep /")
INSTALL-SITELISP := ~/.emacs.d/site-lisp

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*"

SED := sed
PRINTF := printf
CP := cp
INSTALL := install
TOUCH := touch

.PHONY: all install new clean distclean

all:
	@($(foreach dir,$(SITELISPS),$(MAKE) -sC $(dir) all; ))

install: all
	@($(foreach dir,$(SITELISPS),$(PRINTF) $(dir) && $(MAKE) -sC $(dir) install; ))

new:
	$(INSTALL) -d $(TARGET)/; $(SED) -e "1s/TARGET := /TARGET := $(TARGET)/" makefile.template >$(TARGET)/Makefile; $(TOUCH) $(TARGET)/$(TARGET).el; $(INSTALL) -d $(TARGET)/test

clean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) clean; )

distclean:
	$(foreach dir,$(SITELISPS),$(MAKE) -C $(dir) distclean; )
