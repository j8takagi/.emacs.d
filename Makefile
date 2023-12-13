CP := cp -v
ECHO := echo
ifeq ($(shell uname -s),Darwin)
    EMACS := /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
else
    EMACS := emacs
endif
GREPV := grep -v
INSTALL = install -C -v
MKDIR := mkdir
TEST := test
RMR := $(RM) -R

COMPILE.el := $(EMACS) -batch -l set-compile.el -f batch-byte-compile
NATIVE-COMPILE.el := $(EMACS) -batch -l set-compile.el -l native-compile.el -f init-native-compile
CLEAN.rsync := $(GREPV) "^\(sent\)\|\(total\)\|\(sending\)"

INSTALL-DIR := ~/.emacs.d/

INIT-FILES := init early-init
INIT-EL-FILES := $(addsuffix .el,$(INIT-FILES))
INIT-ELC-FILES := $(addsuffix .elc,$(INIT-FILES))


.PHONY: all init native-compile site-lisp install install-init.sys.d install-site-lisp check test

all: init native-compile init.sys.d insert install-site-lisp

init: $(INIT-EL-FILES) native-compile

native-compile: $(INIT-EL-FILES)
	$(NATIVE-COMPILE.el) $^

init.sys.d:
	$(MAKE) -C $@

insert:
	$(MAKE) -C $@

site-lisp:
	$(MAKE) -sC $@

save-abbrev:
	$(EMACS) -batch -l save-abbrev.el

install: install-init install-init.sys.d install-site-lisp install-insert save-abbrev

install-init: $(INSTALL-DIR) init
	$(INSTALL) $(INIT-EL-FILES) $(INIT-ELC-FILES) $(INSTALL-DIR)

install-init.sys.d: init.sys.d $(INSTALL-DIR)/init.sys.d
	$(MAKE) -sC init.sys.d install

$(INSTALL-DIR)/init.sys.d: $(INSTALL-DIR)
	$(INSTALL) -d $@

install-site-lisp: site-lisp $(INSTALL-DIR)/site-lisp
	@$(MAKE) -sC site-lisp install

$(INSTALL-DIR)/site-lisp: $(INSTALL-DIR)
	$(INSTALL) -d $@

install-insert: insert $(INSTALL-DIR)/insert
	@$(MAKE) -sC insert install

$(INSTALL-DIR)/insert: $(INSTALL-DIR)
	$(INSTALL) -d $@


$(INSTALL-DIR):
	$(INSTALL) -d $@

%.elc: %.el
	$(COMPILE.el) $<

check: test

test:
	$(MAKE) -sC test

distclean: clean

clean: init-clean init.sys.d-clean insert-clean site-lisp-clean

init-clean:
	$(RM) *.elc

init.d-clean:
	$(MAKE) -C init.d clean

init.sys.d-clean:
	$(MAKE) -C init.sys.d clean

insert-clean:
	$(MAKE) -C insert clean

site-lisp-clean:
	$(MAKE) -C site-lisp clean

uninstall-all:
	$(RMR) ~/.emacs.d/
