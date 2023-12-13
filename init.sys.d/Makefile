EMACS := emacs
RSYNC := rsync
MKDIR := mkdir
GREPV := grep -v
RSYNCFLAG := -avz --delete

CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"
COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile

NATIVE-COMPILE.el := $(EMACS) -batch -l native-compile.el -f init-native-compile

INSTALL := install -C -v

ELFILES := $(addsuffix .el,$(TARGET))
ELCFILES := $(addsuffix c,$(ELFILES))

MKDIR := mkdir
SED := sed
RMR := $(RM) -r

EMACSDIR := ~/.emacs.d
INSTALLDIR := $(EMACSDIR)/init.sys.d

KERNEL := $(shell uname)
ifeq ($(KERNEL),Linux)
  OSFILES := $(wildcard init-linux*.el init-x*.el)
endif
ifeq ($(KERNEL),Darwin)
  OSFILES := $(wildcard init-ns*.el init-darwin*.el)
endif

HOST := $(shell hostname -s)
HOSTFILES := $(wildcard init-$(HOST)*.el)

ELFILES := $(OSFILES) $(HOSTFILES)
ELCFILES := $(addsuffix c,$(OSFILES) $(HOSTFILES))

.PHONY: all install clean

all: $(ELCFILES)

install: all $(INSTALLDIR)
	$(foreach TGT,$(ELFILES),$(INSTALL) $(TGT) $(INSTALLDIR)/$(TGT);)
	$(foreach TGT,$(ELCFILES),$(INSTALL) $(TGT) $(INSTALLDIR)/$(TGT);)

uninstall: distclean
	$(RMR) $(INSTALLDIR)

$(INSTALLDIR):
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) $(ELCFILES)

distclean: clean
