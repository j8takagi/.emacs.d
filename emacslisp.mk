EMACS := emacs

COMPILE.el := $(EMACS) -batch -f batch-byte-compile

%.elc: %.el
	$(COMPILE.el) $<
