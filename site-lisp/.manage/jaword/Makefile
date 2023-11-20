TARGET := jaword
TARGETDIR := $(TARGET)

EMACS := emacs
COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile

NATIVE-COMPILE.el := $(EMACS) -batch -l set-compile.el -l native-compile.el -f init-native-compile

ELFILES := $(addsuffix .el,$(TARGET))
ELCFILES := $(addsuffix c,$(ELFILES))

RSYNC := rsync
RSYNCFLAG := -avz --delete
RSYNCEXCLUDE := --exclude "Makefile*" --exclude "set-compile.el*" --exclude "native-compile.el*" --exclude "test" --exclude ".git*" --exclude "ChangeLog" --exclude "README*" --exclude "Readme*" --exclude "*.gif"

MKDIR := mkdir
SED := sed

INSTALL-SITELISP := ~/.emacs.d/site-lisp

INSTALL-DIR := $(INSTALL-SITELISP)/$(TARGETDIR)

CLEAN.rsync := $(SED) -e 's%sending incremental file list%Install $(TARGET) to $(INSTALL-SITELISP)%' -e '/^sent/d' -e '/^total/d' -e 's~^\./~~' -e '/^$$/d'

.PHONY: all native-compile install clean

all: $(ELCFILES) native-compile

install: all $(INSTALL-DIR)
	@($(RSYNC) $(RSYNCFLAG) $(RSYNCEXCLUDE) ./ $(INSTALL-DIR) | $(CLEAN.rsync))

$(INSTALL-DIR):
	$(MKDIR) $@

%.elc: %.el
	$(COMPILE.el) $<

native-compile: $(addsuffix .el,$(TARGET))
	$(NATIVE-COMPILE.el) $^

check:
	$(MAKE) -sC test

clean:
	$(RM) $(ELCFILES)

distclean: clean
