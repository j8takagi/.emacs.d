EMACS := emacs

COMPILE.el := $(EMACS) -batch -l set-load-path.el -f batch-byte-compile

%.elc: %.el
	$(COMPILE.el) $<
