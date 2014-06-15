prefix ?= /usr/local
elispdir ?= ~/.emacs.d/site-lisp
installdir ?= $(elispdir)/exopen

.PHONY: install clean

TARGETS := exopen-mode.el

EMACS := emacs
COMPILE.el := $(EMACS) -batch -f batch-byte-compile
INSTALL := install

all: $(addsuffix c,$(TARGETS))

install: $(TARGETS) $(addsuffix c,$(TARGETS))
	$(INSTALL) -d $(installdir)
	$(INSTALL) $^ $(installdir)/

uninstall:
	$(RM) $(prefix $(installdir)/,$(TARGETS)) $(suffix c,$(prefix $(installdir)/,$(TARGETS)))

clean:
	$(RM) *.elc

%.elc: %.el
	$(COMPILE.el) $<
