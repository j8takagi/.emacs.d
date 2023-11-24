TARGET := fontset-set fontsize-set list-fonts-display list-fontfamilies-display xlfd-at
TARGETDIR := font-utilities

EMACS := /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile

NATIVE-COMPILE.el := $(EMACS) -batch -l native-compile.el -l set-compile.el -f init-native-compile

INSTALL := install -C -v

ELFILES := $(addsuffix .el,$(TARGET))
ELCFILES := $(addsuffix c,$(ELFILES))

MKDIR := mkdir
SED := sed

INSTALL-BASE := ~/.emacs.d/site-lisp
INSTALL-DIR := $(INSTALL-BASE)/$(TARGETDIR)/

.PHONY: all native-compile install clean

all: $(ELCFILES) native-compile

install: $(INSTALL-DIR) all
	$(foreach TGT,$(ELFILES),$(INSTALL) $(TGT) $(INSTALL-DIR)/$(TGT);)
	$(foreach TGT,$(ELCFILES),$(INSTALL) $(TGT) $(INSTALL-DIR)/$(TGT);)

$(INSTALL-DIR):
	$(INSTALL) -d $(INSTALL-DIR)

%.elc: %.el
	$(COMPILE.el) $<

native-compile: $(ELFILES)
	$(NATIVE-COMPILE.el) $^

check:
	$(MAKE) -sC test

clean:
	$(RM) $(ELCFILES)

distclean: clean
