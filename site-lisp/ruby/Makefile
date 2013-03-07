prefix ?= /usr/local
elispdir ?= ~/.emacs.d/site-lisp
installdir ?= $(elispdir)/ruby

.PHONY: install clean

EMACS := emacs
COMPILE.el := $(EMACS) -batch -f batch-byte-compile
INSTALL := install

TARGETS = $(wildcard *.el)

$(addsuffix c,$(TARGETS)):

install: $(TARGETS) $(addsuffix c,$(TARGETS))
	$(INSTALL) -d $(installdir)
	$(INSTALL) $^ $(installdir)/

%.elc: %.el
	$(COMPILE.el) $<

clean:
	$(RM) *.elc
