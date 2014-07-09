EMACS := emacs

COMPILE.el := $(EMACS) -batch -l init.el -f batch-byte-compile

%.elc: %.el
	$(COMPILE.el) $<
